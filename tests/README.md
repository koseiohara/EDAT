# EDAT test suite

This directory contains the test suite for the public EDAT routines.
The suite is integrated with the project's Autotools build.

## Running the tests

The test suite is integrated with Automake. Configure the project, then run
`make check` from the project root. The configured Fortran and C compilers are
used for both the library and the tests.

```sh
./configure
make check
```

To test with a different compiler, configure the complete project with that
compiler and rebuild from a clean tree. Fortran module files are
compiler-specific, so library and test objects must not be mixed across
compiler families. Cross-compiler testing belongs in CI, with an independent
configure and build for each compiler.

`make check` builds the test programs, runs the normal test programs through
Automake's test harness, checks the negative BinIO and Math cases in separate
processes, and verifies the expected BinIO and Math error messages.

## Test organization

### `test_math.F90`

Checks every array size from 0 through 150. The suite covers:

- high-precision summation;
- mean;
- population and sample variance;
- covariance;
- correlation;
- `real32`, `real64`, and `real128` entry points;
- cancellation-sensitive input.

### `test_float_string_sort.F90`

Checks:

- scalar and elemental `isclose` behavior;
- upper- and lower-case conversion;
- integer, `real32`, and `real64` sorting for sizes 0 through 65.

### `test_met_derivative.F90`

Checks derivatives of concrete fields on several grid sizes:

- linear functions on nonuniform grids;
- sine and cosine combinations;
- periodic and nonperiodic longitude handling;
- increasing and decreasing pressure coordinates;
- rejection of repeated latitude coordinates.

Expected values account for the coordinate used by the implementation. In
particular, the horizontal derivative routines return derivatives with respect
to longitude or latitude in radians; they do not apply an Earth-radius factor.

### `test_met_integral.F90`

Checks independently accumulated quadrature results:

- meridional integration includes the `cos(latitude)` weight;
- vertical integration includes the triangle between pressure zero and the
  top model level;
- vertical integration includes full-layer trapezoids;
- the partial surface layer follows the current implementation rule.

### `test_binio.F90`

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

`test_extended.F90` adds regression checks for:

- empty and one-element statistics, sample covariance, negative and degenerate correlations;
- all supported floating-point kinds for generic mathematical and meteorological APIs;
- `isclose` boundaries, zero, NaN, infinity, and kind dispatch;
- empty, fixed-width, and punctuation-preserving string conversion;
- sorting order plus exact multiset preservation, extrema, signed zero, reverse order, and noncontiguous sections;
- derivative point-count and duplicate-coordinate errors, descending coordinates, surface-adjacent derivatives, underground masking, and custom `undef`;
- nearest meridional bounds, reported valid bounds, reversed bounds, descending coordinates, duplicate coordinates, and vertical surface-position cases;
- binary I/O for scalar and ranks 1-5, `real32`/`real64`/`real128` payload conversion, mixed-case actions, record stepping, and reset precedence;
- direct byte-order reversal rather than only testing that conversion is self-inverse.

`test_negative_binio.F90` is run as a subprocess. The suite requires nonzero termination and verifies the expected error message for invalid record numbers, invalid record lengths, and missing input files.

`make check` also compiles and runs an external consumer program against the built EDAT library and generated module files.

## Axis-reversal invariance tests

The extended suite also checks coordinate-axis reversal explicitly:

- nonperiodic zonal derivatives on ascending and descending longitude axes are compared after reversing the output order;
- meridional integrals on ascending and descending latitude axes must agree numerically;
- vertical integrals on ascending and descending pressure axes must agree numerically.

In each case the coordinate array and the corresponding data dimension are reversed together.

## Math multidimensional and negative coverage

`test_math_multidim.F90` checks `sum_hp` against the intrinsic `sum` using the
same array and the same `dim` value. It covers every valid `dim` for ranks 2
through 10 and includes zero-extent arrays for both reduction and output
extents.

`test_negative_math.F90` is executed by `make check` in separate subprocesses.
It verifies invalid `dim` rejection and array-shape mismatch rejection for
`covariance` and `corrcoef`, including mismatches whose total element counts are
equal.
