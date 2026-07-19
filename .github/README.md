# GitHub Actions CI

`workflows/ci.yml` rebuilds EDAT independently with each supported compiler and
then runs the complete test suite with that same compiler.

Current matrix:

- GNU Fortran (`gfortran`)
- Intel Fortran (`ifx`)
- Intel Fortran Classic (`ifort`)

The jobs intentionally use separate GitHub-hosted runners. Fortran `.mod` files
and object files are never shared between compiler jobs.

The library build uses `make --jobs=1` because the current Automake dependency
graph does not fully describe all Fortran module dependencies. The test runner
also creates its generated files only under `tests/.build/`.
