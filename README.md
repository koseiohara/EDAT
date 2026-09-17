# Elementary Data Analysis Toolkit (EDAT)

A basic data analysis toolkit for Fortran.

## Install and build
Install the source code by cloning this repository:
```sh
git clone https://github.com/koseiohara/EDAT.git
cd EDAT
```

### Build
Bootstrap the build system from a source checkout, then configure with the desired compilers:

```sh
autoreconf --install --force

# ifort
./configure --prefix=$HOME/FortranLib FC="ifort" CC="icc" FCFLAGS="-O3 -traceback -warn all -convert little_endian -assume byterecl" CFLAGS="-O3 -Wall"
# gfortran
./configure --prefix=$HOME/FortranLib FC="gfortran" CC="gcc" FCFLAGS="-O3 -Wall -fconvert=little-endian" CFLAGS="-O3 -Wall"

make
make check
make install
```

`make check` runs the complete test suite.


## Tools
- [edat_math](#math)
    - [Parameters](#math-parameters)
    - [corrcoef](#math-corrcoef)
    - [covariance](#math-covariance)
    - [variance](#math-variance)
    - [mean](#math-mean)
    - [sum_hp](#math-sum-hp)
- [edat_sort](#sort)
    - [quick_sort](#sort-quick-sort)
- [edat_string](#string)
    - [to_upper](#string-to-upper)
    - [to_lower](#string-to-lower)
- [edat_float](#float)
    - [isclose](#float-isclose)
- [edat_met](#met)
    - [Parameters](#met-parameters)
    - [potential_temperature](#met-potential-temperature)
    - [meridionalIntegral](#met-meridionalIntegral)
    - [verticalIntegral](#met-verticalIntegral)
    - [zonalDerivative](#met-zonalDerivative)
    - [meridionalDerivative](#met-meridionalDerivative)
    - [verticalDerivative](#met-verticalDerivative)
- [edat_binio](#binio)
    - [finfo](#binio-finfo)
    - [finfo constructor](#binio-finfo-constructor)
    - [fclose](#binio-fclose)
    - [fread](#binio-fread)
    - [fwrite](#binio-fwrite)
    - [get_record](#binio-get-record)
    - [reset_record](#binio-reset-record)
    - [endian_converter](#binio-endian-converter)


## edat_math<a id="math"></a>
edat_math provides useful parameters and tools for mathematical analysis.

### Parameters<a id="math-parameters"></a>
```fortran
real(rk), parameter :: M_E        = 2.718281828459045235360287471352662498_rk       ! e
real(rk), parameter :: M_LOG2E    = 1.442695040888963407359924681001892137_rk       ! log_2 e
real(rk), parameter :: M_LOG10E   = 0.434294481903251827651128918916605082_rk       ! log_10 e
real(rk), parameter :: M_LN2      = 0.693147180559945309417232121458176568_rk       ! log_e 2
real(rk), parameter :: M_LN10     = 2.302585092994045684017991454684364208_rk       ! log_e 10
real(rk), parameter :: M_PI       = 3.141592653589793238462643383279502884_rk       ! pi
real(rk), parameter :: M_PI_2     = 1.570796326794896619231321691639751442_rk       ! pi/2
real(rk), parameter :: M_PI_4     = 0.785398163397448309615660845819875721_rk       ! pi/4
real(rk), parameter :: M_1_PI     = 0.318309886183790671537767526745028724_rk       ! 1/pi
real(rk), parameter :: M_2_PI     = 0.636619772367581343075535053490057448_rk       ! 2/pi
real(rk), parameter :: M_2_SQRTPI = 1.128379167095512573896158903121545172_rk       ! 2/sqrt(pi)
real(rk), parameter :: M_SQRT2    = 1.414213562373095048801688724209698079_rk       ! sqrt(2)
real(rk), parameter :: M_SQRT1_2  = 0.707106781186547524400844362104849039_rk       ! 1/sqrt(2)
```
`rk` is the local kind parameter and specifies quadruple precision.
The variable names and their values are identical to those defined in the math.h header of the C language.  
In addition to these parameters, `qnorm` is defined.
`qnorm` is a 99-element array.
`qnorm(i)` is the value $x$ such that for a standard normal variable $X$, $P(−x < X < x) = i/100$.
This array is used for interval estimation of normally distributed data.
```fortran
real(rk), parameter :: qnorm(99)=[0.01253346951_rk, &  !! 01
                                & 0.02506890826_rk, &  !! 02
                                & 0.03760828766_rk, &  !! 03
                                  .
                                  .
                                  .
                                & 1.95996398454_rk, &  !! 95
                                & 2.05374891063_rk, &  !! 96
                                & 2.17009037758_rk, &  !! 97
                                & 2.32634787404_rk, &  !! 98
                                & 2.57582930355_rk  ]  !! 99
```

### corrcoef<a id="math-corrcoef"></a>
```fortran
corrcoef(array1, array2 [, dim])
```
Returns the correlation coefficient between `array1` and `array2`.

`array1` and `array2` must have the same shape and must be the same type: `real32`, `real64`, or `real128`.
Arrays of rank 1 through 7 are supported.

If `dim` is omitted, the correlation coefficient is computed over all elements of the arrays and the result is a scalar.
If `dim` is specified, the correlation coefficient is computed along dimension `dim`.
For arrays of rank greater than 1, the result has rank one less than the input arrays.

`dim` must be between 1 and the rank of the input arrays.
An invalid `dim` or a mismatch between the shapes of `array1` and `array2` causes error termination.

### covariance<a id="math-covariance"></a>
```fortran
covariance(array1, array2 [, sample])
covariance(array1, array2, dim [, sample])
```
Returns the covariance between `array1` and `array2`.

`array1` and `array2` must have the same shape and must be the same type: `real32`, `real64`, or `real128`.
Arrays of rank 1 through 7 are supported.

If `dim` is omitted, the covariance is computed over all elements of the arrays and the result is a scalar.
If `dim` is specified, the covariance is computed along dimension `dim`.
For arrays of rank greater than 1, the result has rank one less than the input arrays.

`dim` must be between 1 and the rank of the input arrays.
An invalid `dim` or a mismatch between the shapes of `array1` and `array2` causes error termination.

If `sample` is provided and `sample=.TRUE.`, the return value is the sample covariance.
Otherwise, the return value is the population covariance.

### variance<a id="math-variance"></a>
```fortran
variance(array [, sample])
variance(array, dim [, sample])
```
Returns the variance of `array`.

`array` must be `real32`, `real64`, or `real128`.
Arrays of rank 1 through 7 are supported.

If `dim` is omitted, the variance is computed over all elements of `array` and the result is a scalar.
If `dim` is specified, the variance is computed along dimension `dim`.
For arrays of rank greater than 1, the result has rank one less than `array`.

`dim` must be between 1 and the rank of `array`.
An invalid `dim` causes error termination.

This routine can compute the variance of data very precisely because [sum_hp](#math-sum-hp) is used.
If `sample` is provided and `sample=.TRUE.`, the return value is the sample variance.
Otherwise, the return value is the population variance.

### mean<a id="math-mean"></a>
```fortran
mean(array [, dim])
```
Returns the mean of `array`.

`array` must be `real32`, `real64`, or `real128`.
Arrays of rank 1 through 10 are supported.

If `dim` is omitted, the mean is computed over all elements of `array` and the result is a scalar.
If `dim` is specified, the mean is computed along dimension `dim`.
For arrays of rank greater than 1, the result has rank one less than `array`.

`dim` must be between 1 and the rank of `array`.
An invalid `dim` causes error termination.

This routine can compute the average of data very precisely because [sum_hp](#math-sum-hp) is used.

### sum_hp<a id="math-sum-hp"></a>
```fortran
sum_hp(array [, dim])
```
Returns the sum of `array`.

`array` must be `real32`, `real64`, or `real128`.
Arrays of rank 1 through 10 are supported.

If `dim` is omitted, all elements of `array` are summed and the result is a scalar.
If `dim` is specified, the elements are summed along dimension `dim`.
For arrays of rank greater than 1, the result has rank one less than `array`.

`dim` must be between 1 and the rank of `array`.
An invalid `dim` causes error termination.

This function can compute the sum of data more precisely than the built-in function `sum()` because the pairwise-sum algorithm is used.


## edat_sort<a id="sort"></a>
edat_sort provides a sorting subroutine.

### quick_sort<a id="sort-quick-sort"></a>
```fortran
call quick_sort(array)
```
Sorts `array` in ascending order.
`array` must be a rank-1 array of `int32`, `real32`, or `real64`.
This subroutine is a wrapper of `qsort` defined in the `stdlib.h` header of the C language.


## edat_string<a id="string"></a>
edat_string provides some routines for manipulating strings.

### to_upper<a id="string-to-upper"></a>
```fortran
pure elemental function to_upper(input) result(output)
    character(*), intent(in) :: input
    character(len(input)) :: output
```
Converts lowercase letters to uppercase, leaving all other characters unchanged.

### to_lower<a id="string-to-lower"></a>
```fortran
pure elemental function to_lower(input) result(output)
    character(*), intent(in) :: input
    character(len(input)) :: output
```
Converts uppercase letters to lowercase, leaving all other characters unchanged.


## edat_float<a id="float"></a>
edat_float provides some routines for manipulating floating-point data.

### isclose<a id="float-isclose"></a>
```fortran
isclose(a, b [, rel_tol, abs_tol])
```
Returns `.TRUE.` if `a` and `b` are close.  
`a`, `b`, `rel_tol`, and `abs_tol` must have the same type: `real32`, `real64`, or `real128`.  
`isclose` is elemental and accepts scalar or conformable array arguments.
The default value of `abs_tol` is `0.`.
The default value of `rel_tol` depends on the type of the arguments.

| Kind | Default `rel_tol` |
|------|-------------------|
| `real32`  | `1.E-4`  |
| `real64`  | `1.E-13` |
| `real128` | `1.E-31` |


## edat_met<a id="met"></a>
edat_met is a module for meteorology.

### Parameters<a id="met-parameters"></a>
```fortran
real(rk), parameter :: GRAV        = 9.80665_rk         ! Gravitational Acceleration [m/s^2]
real(rk), parameter :: EarthRadius = 6.3710E+6_rk       ! Radius of the Earth [m]

real(rk), parameter :: GasConstant = 287.04_rk          ! Gas Constant for Dry Air [J/K/kg] #used for p=rhoRT
real(rk), parameter :: Cp          = 1004._rk           ! Specific Heat for Dry Air at Constant Pressure [J/K/kg]
real(rk), parameter :: Cv          = Cp-GasConstant     ! Specific Heat for Dry Air at Constant Volume [J/K/kg]
real(rk), parameter :: Lq          = 2.507E+6_rk        ! Latent Heat of vaporization [J/kg]
```
`rk` is the local kind parameter and specifies quadruple precision.

### potential_temperature<a id="met-potential-temperature"></a>
```fortran
potential_temperature(T, P)
```
Returns potential temperature.  
`T` and `P` are temperature [K] and pressure [Pa], respectively.
They must have the same type: `real32`, `real64`, or `real128`.
`potential_temperature` is elemental and accepts scalar or conformable array arguments.

### meridionalIntegral<a id="met-meridionalIntegral"></a>
```fortran
call meridionalIntegral(lat, field, south, north, output, status [, valid_south, valid_north])
```
Returns a meridionally integrated field.

`lat`, `field`, `south`, `north`, `output`, `valid_south`, and `valid_north` must be the same type: `real32`, `real64`, or `real128`.

`lat` contains latitude [rad] and must have shape `[ny]`.
`field` is the target field and must have shape `[nx,ny,nz]`.
`south` and `north` are the southern and northern limits of the integral [rad].
`output` must have shape `[nx,nz]`.
`status` is positive if the computation succeeds.
`valid_south` and `valid_north` are optional outputs containing the actual southern and northern limits of the integral.

If `south` and `north` exist in `lat`, `valid_south` and `valid_north` are equal to `south` and `north`.
Otherwise, the closest latitudes are chosen as `valid_south` and `valid_north` and as the limits of integration.

| Status | Cause |
|--------|-------|
| -1 | Inconsistency of input array shapes |
| -2 | Integral axis is not a monotone sequence |
| -3 | Array size is too small to compute integral |

### verticalIntegral<a id="met-verticalIntegral"></a>
```fortran
call verticalIntegral(lev, field, psfc, output, status)
```
Returns a vertically integrated field.

`lev`, `field`, `psfc`, and `output` must be the same type: `real32`, `real64`, or `real128`.

`lev` contains vertical levels [Pa] or [hPa] and must have shape `[nz]`.
`field` is the target field and must have shape `[nx,ny,nz]`.
`psfc` contains surface pressure [Pa] or [hPa] and must have shape `[nx,ny]`.
`output` must have shape `[nx,ny]`.
`status` is positive if the computation succeeds.

| Status | Cause |
|--------|-------|
| -1 | Inconsistency of input array shapes |
| -2 | Integral axis is not a monotone sequence |
| -3 | Array size is too small to compute integral |

### zonalDerivative<a id="met-zonalDerivative"></a>
```fortran
call zonalDerivative(lon, input, output [, periodic, status])
```
Returns the zonal derivative.

`lon`, `input`, and `output` must be the same type: `real32`, `real64`, or `real128`.

`lon` contains longitude [rad] and must have shape `[nx]`.
`input` is the target field and must have shape `[nx,ny,nz]`.
`output` must have shape `[nx,ny,nz]`.
`periodic` is an optional logical argument specifying whether the longitude boundary is periodic.
`status` is an optional integer output and is positive if the computation succeeds.

Both west-to-east and east-to-west longitude are acceptable.  
The derivative is computed with a central difference method at interior grid points.
If `periodic=.FALSE.`, the eastern and western boundaries are computed with a one-sided difference method.

| Status | Cause |
|--------|-------|
| -1 | Inconsistency of input array shapes |
| -2 | Derivative axis is not a monotone sequence |
| -3 | Array size is too small to compute derivative |

### meridionalDerivative<a id="met-meridionalDerivative"></a>
```fortran
call meridionalDerivative(lat, input, output [, status])
```
Returns the meridional derivative.

`lat`, `input`, and `output` must be the same type: `real32`, `real64`, or `real128`.

`lat` contains latitude [rad] and must have shape `[ny]`.
`input` is the target field and must have shape `[nx,ny,nz]`.
`output` must have shape `[nx,ny,nz]`.
`status` is an optional integer output and is positive if the computation succeeds.

Both north-to-south and south-to-north latitude are acceptable.  
The derivative is computed with a central difference method at interior grid points.
The northern and southern boundaries are computed with a one-sided difference method.

| Status | Cause |
|--------|-------|
| -1 | Inconsistency of input array shapes |
| -2 | Derivative axis is not a monotone sequence |
| -3 | Array size is too small to compute derivative |

### verticalDerivative<a id="met-verticalDerivative"></a>
```fortran
call verticalDerivative(lev, input, psfc, output [, undef, status])
```
Returns the vertical derivative.

`lev`, `input`, `psfc`, `output`, and `undef` must be the same type: `real32`, `real64`, or `real128`.

`lev` contains vertical levels [Pa] or [hPa] and must have shape `[nz]`.
`input` is the target field and must have shape `[nx,ny,nz]`.
`psfc` contains surface pressure [Pa] or [hPa] and must have shape `[nx,ny]`.
`output` must have shape `[nx,ny,nz]`.
`undef` is an optional value used to fill points below the surface. Its default value is `-999.E+30` in the corresponding floating-point kind.
`status` is an optional integer output and is positive if the computation succeeds.

Both upper-to-lower and lower-to-upper levels are acceptable.  
The derivative is computed with a central difference method at interior grid points.
The lower and upper boundaries are computed with a one-sided difference method.

| Status | Cause |
|--------|-------|
| -1 | Inconsistency of input array shapes |
| -2 | Derivative axis is not a monotone sequence |
| -3 | Array size is too small to compute derivative |


## edat_binio<a id="binio"></a>
edat_binio is a module for performing input and output of headerless binary files.

### finfo<a id="binio-finfo"></a>
```fortran
type finfo
    private
    integer        :: unit
    character(128) :: file
    character(16)  :: action
    integer(int64) :: record
    integer(int64) :: recl
    integer(int64) :: recstep
contains
    generic, public :: get_record => get_record32, get_record64
    generic, public :: fread => fread_ss, fread_sd, fread_sq, &
                               ...
                               fread_5s, fread_5d, fread_5q
    generic, public :: fwrite => fwrite_ss, fwrite_sd, fwrite_sq, &
                                ...
                                fwrite_5s, fwrite_5d, fwrite_5q
    procedure, pass, public :: fclose
    procedure, pass, public :: reset_record
    ...
end type finfo
```

#### unit
Unit number for a file.

#### file
File name for reading or writing.

#### action
`READ`, `WRITE`, or `READWRITE`.

#### record
The next record for reading or writing.

#### recl
Record length in bytes.

#### recstep
Increment applied to `record` after every read or write.
`record` is automatically updated by this value.

### finfo constructor<a id="binio-finfo-constructor"></a>
```fortran
self = finfo(file=file, action=action, record=record, recl=recl, recstep=recstep [, unit=unit])
```
Constructs a `finfo` object and opens the specified direct-access unformatted file.

`file` and `action` are character arguments.
`record`, `recl`, and `recstep` must be `integer(int32)` or `integer(int64)`.
`unit` is optional.

`record` specifies the initial record and must be greater than zero.
`recl` specifies the record length and must be greater than zero.
`recstep` specifies the increment applied to the record number after every read or write.

It is strongly recommended not to provide `unit`.
If `unit` is provided, its value is used as the unit number.
Otherwise, the unit number is automatically assigned.

### fclose<a id="binio-fclose"></a>
```fortran
call self%fclose()
```
Closes the file associated with `self`.

### fread<a id="binio-fread"></a>
```fortran
call self%fread(input_data)
```
Reads data from the current record and then increments the record number by `recstep`.

`input_data` may be a scalar or a rank-1 through rank-5 array.
It must be `real32`, `real64`, or `real128`.
The binary payload is read as `real32` and converted to the kind of `input_data`.

### fwrite<a id="binio-fwrite"></a>
```fortran
call self%fwrite(output_data)
```
Writes data to the current record and then increments the record number by `recstep`.

`output_data` may be a scalar or a rank-1 through rank-5 array.
It must be `real32`, `real64`, or `real128`.
Regardless of the kind of `output_data`, the binary payload is written as `real32`.

### get_record<a id="binio-get-record"></a>
```fortran
call self%get_record(record)
```
Returns the next record number to be read or written.

`record` must be `integer(int32)` or `integer(int64)`.

### reset_record<a id="binio-reset-record"></a>
```fortran
call self%reset_record(increment=increment)
call self%reset_record(newrecord=newrecord)
```
Resets the next record to be read or written.

`increment` and `newrecord` must be `integer(int32)` or `integer(int64)`.
If `increment` is provided, its value is added to the current record.
If `newrecord` is provided, the record is changed to that value.
If both are provided, only `increment` is used.
At least one of `increment` and `newrecord` must be provided.

### endian_converter<a id="binio-endian-converter"></a>
```fortran
call endian_converter(a)
```
Reverses the byte order of `a`.

`a` must be `real32`.
`endian_converter` is elemental and accepts a scalar or an array.



