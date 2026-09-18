

module integral

    use, intrinsic :: iso_fortran_env, only : real32, real64, real128

    implicit none

    private
    public :: meridionalIntegral, verticalIntegral

    interface meridionalIntegral
        module procedure meridionalIntegral_sp
        module procedure meridionalIntegral_dp
        module procedure meridionalIntegral_qp
    end interface meridionalIntegral

    interface verticalIntegral
        module procedure verticalIntegral_sp
        module procedure verticalIntegral_dp
        module procedure verticalIntegral_qp
    end interface verticalIntegral

    contains

#define RK real32
#define YINT meridionalIntegral_sp
#define ZINT verticalIntegral_sp
#include "integral_core.F90"
#undef RK
#undef YINT
#undef ZINT

#define RK real64
#define YINT meridionalIntegral_dp
#define ZINT verticalIntegral_dp
#include "integral_core.F90"
#undef RK
#undef YINT
#undef ZINT

#define RK real128
#define YINT meridionalIntegral_qp
#define ZINT verticalIntegral_qp
#include "integral_core.F90"
#undef RK
#undef YINT
#undef ZINT

end module integral

