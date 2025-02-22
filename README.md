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
```
`libedat.a` will be made.

Copy `libedat.a` and `*.mod` to `${DIR}/lib` and `${DIR}/include`, respectively:
```sh
make install
```


## Tools
- [edat_math](#edat-math)
    - [Parameters](#edat-math-parameters)
    - [corrcoef](#edat-math-corrcoef)
    - [covariance](#edat-math-covariance)
    - [variance](#edat-math-covariance)
    - [mean](#edat-math-mean)
    - [sum_hp](#edat-math-sum-hp)

