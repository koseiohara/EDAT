#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD=${TEST_BUILD:-"$ROOT/tests/.build"}
MODDIR="$BUILD/mod"
OBJDIR="$BUILD/obj"
BINDIR="$BUILD/bin"

make_variable() {
  sed -n "s/^$1[[:space:]]*=[[:space:]]*//p" "$2" | head -n 1
}

# The configured Makefile is authoritative. TEST_FC/TEST_CC are explicit
# escape hatches; ambient FC/CC are deliberately ignored so a login-shell
# setting cannot select a compiler different from the configured build.
if [ -n "${TEST_FC:-}" ]; then
  FC=$TEST_FC
elif [ -f "$ROOT/Makefile" ]; then
  FC=$(make_variable FC "$ROOT/Makefile")
else
  echo "ERROR: $ROOT/Makefile is missing; configure the project first." >&2
  echo "       Alternatively set TEST_FC explicitly." >&2
  exit 2
fi

if [ -n "${TEST_CC:-}" ]; then
  CC=$TEST_CC
elif [ -f "$ROOT/Makefile" ]; then
  CC=$(make_variable CC "$ROOT/Makefile")
else
  CC=cc
fi

[ -n "$FC" ] || { echo "ERROR: FC is empty in $ROOT/Makefile" >&2; exit 2; }
[ -n "$CC" ] || CC=cc

compiler_banner=$($FC --version 2>/dev/null | head -n 1 || true)
case "$compiler_banner" in
  *GNU*|*GFortran*)
    compiler_family=gnu
    DEFAULT_TEST_FFLAGS='-O0 -g -Wall -Wextra -fcheck=all -fbacktrace'
    ;;
  *Intel*|*ifort*|*ifx*)
    compiler_family=intel
    DEFAULT_TEST_FFLAGS='-O0 -g -warn all -check all -traceback'
    ;;
  *)
    echo "ERROR: unsupported Fortran compiler: $FC" >&2
    echo "Compiler banner: $compiler_banner" >&2
    exit 2
    ;;
esac

# Do not inherit FFLAGS/CFLAGS from the parent environment. In particular,
# configured include paths may point at src/*.mod produced by another compiler.
TEST_FFLAGS=${TEST_FFLAGS:-$DEFAULT_TEST_FFLAGS}
TEST_CFLAGS=${TEST_CFLAGS:--O0 -g}

FORTRAN_SOURCES="
  pairwise_sum
  edat_math
  edat_float
  edat_sort
  edat_caseconverter
  edat_string
  integral_sp
  integral_dp
  integral_qp
  derivative_sp
  derivative_dp
  derivative_qp
  edat_met
  edat_binio
"

TEST_PROGRAMS="
  test_math
  test_float_string_sort
  test_met_derivative
  test_met_integral
  test_binio
  test_extended
"

fc_compile() {
  src=$1
  obj=$2
  case $compiler_family in
    gnu)
      # TEST_FFLAGS intentionally undergoes shell word splitting.
      # shellcheck disable=SC2086
      "$FC" $TEST_FFLAGS -J"$MODDIR" -I"$MODDIR" -c "$src" -o "$obj"
      ;;
    intel)
      # shellcheck disable=SC2086
      "$FC" $TEST_FFLAGS -module "$MODDIR" -I"$MODDIR" -c "$src" -o "$obj"
      ;;
  esac
}

fc_link() {
  output=$1
  shift
  # shellcheck disable=SC2086
  "$FC" $TEST_FFLAGS -I"$MODDIR" "$@" -o "$output"
}

rm -rf "$BUILD"
mkdir -p "$MODDIR" "$OBJDIR" "$BINDIR"

printf '%s\n' "Fortran compiler: $FC" "Compiler version: $compiler_banner" "C compiler: $CC" "Build directory: $BUILD"

# shellcheck disable=SC2086
# shellcheck disable=SC2086
"$CC" $TEST_CFLAGS -c "$ROOT/src/c_qsort.c" -o "$OBJDIR/c_qsort.o"

for source_name in $FORTRAN_SOURCES; do
  fc_compile "$ROOT/src/$source_name.f90" "$OBJDIR/$source_name.o"
done
fc_compile "$ROOT/tests/test_support.f90" "$OBJDIR/test_support.o"

OBJECTS="
$OBJDIR/c_qsort.o
$OBJDIR/pairwise_sum.o
$OBJDIR/edat_math.o
$OBJDIR/edat_float.o
$OBJDIR/edat_sort.o
$OBJDIR/edat_caseconverter.o
$OBJDIR/edat_string.o
$OBJDIR/integral_sp.o
$OBJDIR/integral_dp.o
$OBJDIR/integral_qp.o
$OBJDIR/derivative_sp.o
$OBJDIR/derivative_dp.o
$OBJDIR/derivative_qp.o
$OBJDIR/edat_met.o
$OBJDIR/edat_binio.o
$OBJDIR/test_support.o
"

for test_name in $TEST_PROGRAMS; do
  fc_link "$BINDIR/$test_name" "$ROOT/tests/$test_name.f90" $OBJECTS
  "$BINDIR/$test_name"
done

fc_link "$BINDIR/test_negative_binio" "$ROOT/tests/test_negative_binio.f90" $OBJECTS
for mode in record recl missing; do
  if "$BINDIR/test_negative_binio" "$mode" >"$BUILD/negative_$mode.log" 2>&1; then
    echo "FAIL: negative binio case '$mode' unexpectedly succeeded" >&2
    exit 1
  fi
done
echo "test_negative_binio: PASS"

cat > "$BUILD/external_consumer.f90" <<'EOF_EXTERNAL'
program external_consumer
  use, intrinsic :: iso_fortran_env, only: real64
  use EDAT_Math, only: mean
  implicit none
  if (mean([1.0_real64, 3.0_real64]) /= 2.0_real64) error stop 1
end program external_consumer
EOF_EXTERNAL
fc_link "$BINDIR/external_consumer" "$BUILD/external_consumer.f90" $OBJECTS
"$BINDIR/external_consumer"
echo "test_external_consumer: PASS"
