

module derivative

    use, intrinsic :: iso_fortran_env, only : real32, real64, real128

    implicit none

    private
    public :: zonalDerivative, meridionalDerivative, verticalDerivative

    interface zonalDerivative
        module procedure :: zonalDerivative_sp
        module procedure :: zonalDerivative_dp
        module procedure :: zonalDerivative_qp
    end interface zonalDerivative

    interface meridionalDerivative
        module procedure :: meridionalDerivative_sp
        module procedure :: meridionalDerivative_dp
        module procedure :: meridionalDerivative_qp
    end interface meridionalDerivative

    interface verticalDerivative
        module procedure :: verticalDerivative_sp
        module procedure :: verticalDerivative_dp
        module procedure :: verticalDerivative_qp
    end interface verticalDerivative

    contains


#define RK real32
#define DEFAULT -999.E+30_real32
#define XDERIV zonalDerivative_sp
#define YDERIV meridionalDerivative_sp
#define ZDERIV verticalDerivative_sp
#include "derivative_core.F90"
#undef RK
#undef DEFAULT
#undef XDERIV
#undef YDERIV
#undef ZDERIV

#define RK real64
#define DEFAULT -999.E+30_real64
#define XDERIV zonalDerivative_dp
#define YDERIV meridionalDerivative_dp
#define ZDERIV verticalDerivative_dp
#include "derivative_core.F90"
#undef RK
#undef DEFAULT
#undef XDERIV
#undef YDERIV
#undef ZDERIV

#define RK real128
#define DEFAULT -999.E+30_real128
#define XDERIV zonalDerivative_qp
#define YDERIV meridionalDerivative_qp
#define ZDERIV verticalDerivative_qp
#include "derivative_core.F90"
#undef RK
#undef DEFAULT
#undef XDERIV
#undef YDERIV
#undef ZDERIV

end module derivative



