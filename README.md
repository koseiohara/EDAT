# Elementary Data Analysis Toolkit (EDAT)

A basic data analysis toolkit for Fortran.

## Install and build
Install the source code by cloning this repository:
```sh
git clone https://github.com/koseiohara/EDAT.git
cd EDAT
```
This library can be builded by Makefile.

### Makefile
Rewrite the Makefile for your environment:
```sh
cd src
vim Makefile
```
You can change the definisions of `DIR`, `FC`, and `FLAG`.
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


## edat_math<a id="math"></a>
edat_math provides useful parameters and tools for marthmatical analysis.

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







