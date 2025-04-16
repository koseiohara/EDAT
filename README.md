# Elementary Data Analysis Toolkit (EDAT)

A basic data analysis toolkit for Fortran.

## Install and build
Install the source code by cloning this repository:
```sh
git clone https://github.com/koseiohara/EDAT.git
cd EDAT
```
This library can be built by Makefile.

### Makefile
Rewrite the Makefile for your environment:
```sh
cd src
vim Makefile
```
You can change the definitions of `DIR`, `FC`, `CC`, `FFLAG`, and `CFLAGS`.
`${DIR}/lib` and `${DIR}/include` are needed.  
After making these settings, execute the makefile
```sh
make
make install
```
`libedat.a` and `*.mod` will be made and copied to `${DIR}/lib` and `${DIR}/include`, respectively.


## Tools
- [edat_math](#math)
    - [Parameters](#math-parameters)
    - [corrcoef](#math-corrcoef)
    - [covariance](#math-covariance)
    - [variance](#math-covariance)
    - [mean](#math-mean)
    - [sum_hp](#math-sum-hp)
- [edat_sort](#sort)
    - [quick_sort](#sort-quick-sort)
- [edat_string](#string)
    - [to_upper](#string-to-upper)
    - [to_lower](#string-to-lower)
- [edat_met](#met)
    - [Parameters](#met-parameters)
    - [potential_temperature](#met-potential-temperature)
- [edat_binio](#binio)
    - [finfo](#binio-finfo)
    - [fopen](#binio-fopen)
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
`rk` is the local kind parameter, specifies quadruple precision.
The variable names and their values are identical to those defined in the math.h header of the C language.  
In addition to these parameters, `qnorm` is defined.
qnorm is a 99-element array.
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
pure function corrcoef(n, array1, array2) result(output)
    integer, intent(in) :: n
    real, intent(in) :: array1(n)
    real, intent(in) :: array2(n)
```
Returns the correlation coefficient between `array1` and `array2`.
Both arrays must be the same type, `real32`, `real64`, or `real128`.

### covariance<a id="math-covariance"></a>
```fortran
pure function covariance(n, array1, array2, sample) result(output)
    integer, intent(in) :: n
    real   , intent(in) :: array1(n)
    real   , intent(in) :: array2(n)
    logical, intent(in), optional :: sample
```
Returns the covariance between `array1` and `array2`.
Both arrays must be the same type, `real32`, `real64`, or `real128`.  
If `sample` is provided and `sample=.TRUE.`, the return value is the sample covariance. Otherwise, the return value is the population covariance.

### variance<a id="math-variance"></a> 
```fortran
pure function variance(n, array, sample) result(output)
    integer, intent(in) :: n
    real   , intent(in) :: array(n)
    logical, intent(in), optional :: sample
```
Returns the variance of `array`.
`array` must be `real32`, `real64`, or `real128`.  
This routine can compute the variance of data very precisely because [sum_hp](#math-sum-hp) is used.
If `sample` is provided and `sample=.TRUE.`, the return value is the sample variance. Otherwise, the return value is the population variance.

### mean<a id="math-mean"></a>
```fortran
pure function mean(n, array) result(output)
    integer, intent(in) :: n
    real   , intent(in) :: array(n)
```
Returns the mean of `array`.
`array` must be `real32`, `real64`, or `real128`.  
This routine can compute the average of data very precisely because [sum_hp](#math-sum-hp) is used.

### sum_hp<a id="math-sum-hp"></a>
```fortran
pure function sum_hp(n, array) result(output)
    integer, intent(in) :: n
    real   , intent(in) :: array(n)
```
Returns the sum of `array`.
`array` must be `real32`, `real64`, or `real128`.  
This function can compute the sum of data more precisely than the built-in function `sum()` because the pairwise-sum algorithm is used.


## edat_sort<a id="sort"></a>
`edat_sort` provides sorting subroutine.

### quick_sort<a id="sort-quick-sort"></a>
```fortran
subroutine quick_sort(n, array)
    integer, intent(in)    :: n
    real   , intent(inout) :: array(n)  !! integer array is also acceptable
```
Returns sorted array of `array`.
`array` must be `real32`, `real64`, or `int32`.
This subroutine is a wrapper of qsort defined in the `stdlib.h` header of the C language.


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


## edat_met<a id="met"></a>
edat_met is a module for meteorology.

### Parameters<a id="met-parameters"></a>
```fortran
real(rk), parameter :: GRAV        = 9.80665_rk         ! Gravitational Acceleration [m/s^2]
real(rk), parameter :: EarthRadius = 6.3710E+6_rk       ! Radius of the Earth [m]

real(rk), parameter :: GasConstant = 287.04_rk          ! Gas Constant for Dry Air [J/K/kg] #used for p=rhoRT
real(rk), parameter :: Cp          = 1004._rk           ! Specific Heat for Dry Air at Constant Pressure [J/K/kg]
real(rk), parameter :: Cv          = Cp-GasConstant     ! Specific Heat for Dry Air at Constant Volume [J/K/kg]
real(rk), parameter :: Lq          = 2.507E+6_rk        ! Latent Heat of vaporication [J/kg]
```
`rk` is the local kind parameter, specifies quadruple precision.

### potential_temperature<a id="met-potential-temperature"></a>
```fortran
pure elemental function potential_temperature(T, P) result(output)
    real, intent(in) :: T
    real, intent(in) :: P
```
Returns potential temperature.  
`T` and `P` are temperature[K] and pressure[Pa], respectively.
Both of them must be the same type, `real32`, `real64`, or `real128`.  


## edat_binio<a id="binio"></a>
edat_binio is a module for performing input and output of no-header binary files.

### finfo<a id="binio-finfo"></a>
```fortran
type finfo
    private
    integer        :: unit
    character(128) :: file
    character(16)  :: action
    integer        :: record
    integer        :: recl
    integer        :: recstep
end type finfo
```
#### unit
Unit number for a file.
#### file
File name for reading or writing.
#### action
`READ`, `WRITE`, or `READWRITE`.
#### record
The initial record for reading or writing.
#### recl
Record length (byte).
#### recstep
Increment to `record` at every reading or writing.
`record` will be automatically updated with this value.

### fopen<a id="binio-fopen"></a>
```fortran
subroutine fopen(ftype, unit, file, action, record, recl, recstep)
    type(finfo) , intent(out) :: ftype
    integer     , intent(in), optional :: unit
    character(*), intent(in) :: file
    character(*), intent(in) :: action
    integer     , intent(in) :: record
    integer     , intent(in) :: recl
    integer     , intent(in) :: recstep
```
Open a file.
This subroutine initializes `ftype` with the provided arguments.  
It is strongly recommended not to provide `unit`.
If you provide `unit` as an argument, its value will be used for the unit number.
Otherwise, the unit number will be automatically decided.

### fclose<a id="binio-fclose"></a>
```fortran
subroutine fclose(ftype)
    type(finfo), intent(inout) :: ftype
```
Close a file.

### fread<a id="binio-fread"></a>
```fortran
subroutine fread(ftype, input_data)
    type(finfo), intent(inout) :: ftype
    real(4)    , intent(out)   :: input_data
```
Read data from a record in a file.
Scalar, 1dim, 2dim, or 3dim arrays are acceptable for `input_data`.
`input_data` must be a `real32` type.

### fwrite<a id="binio-fwrite"></a>
```fortran
subroutine fwrite(ftype, output_data)
    type(finfo), intent(inout) :: ftype
    real       , intent(in)    :: output_data
```
Write data to a record in a file.
Scalar, 1dim, 2dim, or 3dim arrays are acceptable for `output_data`.
`output_data` must be `real32`, `real64`, or `real128`.
Regardless of the precision of `output_data`, the output is in single precision.

### get_record<a id="binio-get-record"></a>
```fortran
subroutine get_record(ftype, record)
    type(finfo), intent(in)  :: ftype
    integer    , intent(out) :: record
```
Return the next record you read or write.

### reset_record<a id="binio-reset-record"></a>
```fortran
subroutine reset_record(ftype, increment, newrecord)
    type(finfo), intent(inout) :: ftype
    integer    , intent(in)   , optional :: increment
    integer    , intent(in)   , optional :: newrecord
```
Reset the next record you read or write.  
If `increment` is provided, its value will be added to the present record.
If `newrecord` is provided, the record will be changed to the value.
If both are provided, only `increment` will be used.

### endian_converter<a id="binio-endian-converter"></a>
```fortran
pure elemental subroutine endian_converter(rawOre)
    real(4), intent(inout) :: rawOre
```
Convert the endian of `rawOre`.  
This subroutine accepts a floating-point value and returns the same value with its byte order reversed. 
This conversion enables correct interpretation of the value when it is stored in an endianness different from that of the running system independent of environment.



