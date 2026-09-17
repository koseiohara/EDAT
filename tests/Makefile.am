AM_FCFLAGS = -I$(top_builddir)/src $(OPENMP_FCFLAGS)

noinst_LIBRARIES = libtestsupport.a
libtestsupport_a_SOURCES = test_support.F90

check_PROGRAMS = \
  test_math \
  test_math_multidim \
  test_float_string_sort \
  test_met_derivative \
  test_met_integral \
  test_binio \
  test_extended \
  test_negative_binio \
  test_negative_math \
  test_external_consumer


test_math_SOURCES = test_math.F90
test_math_multidim_SOURCES = test_math_multidim.F90
test_float_string_sort_SOURCES = test_float_string_sort.F90
test_met_derivative_SOURCES = test_met_derivative.F90
test_met_integral_SOURCES = test_met_integral.F90
test_binio_SOURCES = test_binio.F90
test_extended_SOURCES = test_extended.F90
test_negative_binio_SOURCES = test_negative_binio.F90
test_negative_math_SOURCES = test_negative_math.F90
test_external_consumer_SOURCES = test_external_consumer.F90

TESTS = \
  test_math \
  test_math_multidim \
  test_float_string_sort \
  test_met_derivative \
  test_met_integral \
  test_binio \
  test_extended \
  test_external_consumer

LDADD = libtestsupport.a $(top_builddir)/src/libedat.a

test_negative_binio_LDADD = $(top_builddir)/src/libedat.a
test_negative_math_LDADD = $(top_builddir)/src/libedat.a
test_external_consumer_LDADD = $(top_builddir)/src/libedat.a

POSITIVE_TEST_OBJECTS = \
  test_math.$(OBJEXT) \
  test_math_multidim.$(OBJEXT) \
  test_float_string_sort.$(OBJEXT) \
  test_met_derivative.$(OBJEXT) \
  test_met_integral.$(OBJEXT) \
  test_binio.$(OBJEXT) \
  test_extended.$(OBJEXT)

$(POSITIVE_TEST_OBJECTS): test_support.$(OBJEXT) $(top_builddir)/src/libedat.a

test_negative_binio.$(OBJEXT) \
test_negative_math.$(OBJEXT) \
test_external_consumer.$(OBJEXT): $(top_builddir)/src/libedat.a

check-local: test_negative_binio$(EXEEXT) test_negative_math$(EXEEXT)
	@set -e; \
	while IFS='|' read mode expected; do \
	  test -n "$$mode" || continue; \
	  log="negative_binio_$$mode.log"; \
	  if ./test_negative_binio$(EXEEXT) "$$mode" >"$$log" 2>&1; then \
	    echo "FAIL: negative binio case '$$mode' unexpectedly succeeded" >&2; \
	    exit 1; \
	  fi; \
	  if ! grep -F "$$expected" "$$log" >/dev/null 2>&1; then \
	    echo "FAIL: negative binio case '$$mode' did not report expected error" >&2; \
	    echo "Expected: $$expected" >&2; \
	    cat "$$log" >&2; \
	    exit 1; \
	  fi; \
	done < $(srcdir)/negative_binio_cases.txt; \
	echo "test_negative_binio: PASS"
	@set -e; \
	while IFS='|' read mode expected; do \
	  test -n "$$mode" || continue; \
	  log="negative_math_$$mode.log"; \
	  if ./test_negative_math$(EXEEXT) "$$mode" >"$$log" 2>&1; then \
	    echo "FAIL: negative math case '$$mode' unexpectedly succeeded" >&2; \
	    exit 1; \
	  fi; \
	  if ! grep -F "$$expected" "$$log" >/dev/null 2>&1; then \
	    echo "FAIL: negative math case '$$mode' did not report expected error" >&2; \
	    echo "Expected: $$expected" >&2; \
	    cat "$$log" >&2; \
	    exit 1; \
	  fi; \
	done < $(srcdir)/negative_math_cases.txt; \
	echo "test_negative_math: PASS"

CLEANFILES = \
  *.mod \
  negative_binio_*.log \
  negative_math_*.log \
  negative.tmp

EXTRA_DIST = negative_binio_cases.txt negative_math_cases.txt
