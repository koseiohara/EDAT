# EDAT test suite

This directory contains a standalone test suite for the public EDAT routines.
It does not depend on the project's Autotools configuration.

## Running the tests

The suite uses exactly one Fortran compiler per run. By default, it reads
`FC` and `CC` from the configured project-root `Makefile`, so the tests use
the same compiler family as the project build. All test-generated files are
written below `tests/.build`.

From the project root:

```sh
./tests/run_tests.sh
```

An explicit compiler can be supplied when required:

```sh
TEST_FC=ifort TEST_CC=icc ./tests/run_tests.sh
```

Do not compile the tests with a different compiler from the project build.
Fortran module files are compiler-specific. Cross-compiler testing belongs in
CI, where the project and tests must both be rebuilt independently for every
compiler.

Optional overrides are:

- `TEST_FC`: explicit Fortran compiler override; normally omit it.
- `TEST_CC`: explicit C compiler override; normally omit it.
- `TEST_FFLAGS`: test-only Fortran flags. Ambient `FFLAGS` is ignored.
- `TEST_CFLAGS`: test-only C flags. Ambient `CFLAGS` is ignored.
- `TEST_BUILD`: test build directory; default is `tests/.build`.

## Test organization

### `test_math.f90`

Checks every array size from 0 through 150. The suite covers:

- high-precision summation;
- mean;
- population and sample variance;
- covariance;
- correlation;
- mismatched input sizes;
- `real32`, `real64`, and `real128` entry points;
- cancellation-sensitive input.

### `test_float_string_sort.f90`

Checks:

- scalar and elemental `isclose` behavior;
- upper- and lower-case conversion;
- integer, `real32`, and `real64` sorting for sizes 0 through 65.

### `test_met_derivative.f90`

Checks derivatives of concrete fields on several grid sizes:

- linear functions on nonuniform grids;
- sine and cosine combinations;
- periodic and nonperiodic longitude handling;
- increasing and decreasing pressure coordinates;
- rejection of repeated latitude coordinates.

Expected values account for the coordinate used by the implementation. In
particular, the horizontal derivative routines return derivatives with respect
to longitude or latitude in radians; they do not apply an Earth-radius factor.

### `test_met_integral.f90`

Checks independently accumulated quadrature results:

- meridional integration includes the `cos(latitude)` weight;
- vertical integration includes the triangle between pressure zero and the
  top model level;
- vertical integration includes full-layer trapezoids;
- the partial surface layer follows the current implementation rule.

### `test_binio.f90`

Checks:

- one- and two-dimensional input;
- record stepping and record reset;
- 32-bit and 64-bit record counters;
- byte-order conversion;
- the implementation behavior that floating-point binary payloads are stored
  as `real32` values.

## Test style

Each program has a short top-level list of test cases. Detailed setup and
expected-value calculations are placed in named internal subroutines so that a
reader can identify the intent of each test before reading its mechanics.

## Extended coverage

`test_extended.f90` adds regression checks for:

- empty and one-element statistics, sample covariance, negative and degenerate correlations;
- all supported floating-point kinds for generic mathematical and meteorological APIs;
- `isclose` boundaries, zero, NaN, infinity, and kind dispatch;
- empty, fixed-width, and punctuation-preserving string conversion;
- sorting order plus exact multiset preservation, extrema, signed zero, reverse order, and noncontiguous sections;
- derivative point-count and duplicate-coordinate errors, descending coordinates, surface-adjacent derivatives, underground masking, and custom `undef`;
- nearest meridional bounds, reported valid bounds, reversed bounds, descending coordinates, duplicate coordinates, and vertical surface-position cases;
- binary I/O for scalar and ranks 1-5, `real32`/`real64`/`real128` payload conversion, mixed-case actions, record stepping, and reset precedence;
- direct byte-order reversal rather than only testing that conversion is self-inverse.

`test_negative_binio.f90` is run as a subprocess. Nonzero termination is required for invalid record numbers, invalid record lengths, and missing input files.

The main runner also compiles and runs an external consumer program against the generated module files and objects.

## Axis-reversal invariance tests

The extended suite also checks coordinate-axis reversal explicitly:

- nonperiodic zonal derivatives on ascending and descending longitude axes are compared after reversing the output order;
- meridional integrals on ascending and descending latitude axes must agree numerically;
- vertical integrals on ascending and descending pressure axes must agree numerically.

In each case the coordinate array and the corresponding data dimension are reversed together.
