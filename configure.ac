
AC_PREREQ([2.69])

AC_INIT([EDAT], [0.1.0], [https://github.com/koseiohara/EDAT/issues])

AC_CONFIG_SRCDIR([src/edat_math.f90])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([
  foreign
  1.13
  silent-rules
  subdir-objects
])

AC_PROG_CC
AC_PROG_FC
AC_PROG_RANLIB

AC_LANG_PUSH([Fortran])
AC_FC_FREEFORM
AC_LANG_POP([Fortran])

AC_CONFIG_FILES([
  Makefile
  src/Makefile
])

AC_OUTPUT


