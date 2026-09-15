

module pairwise_sum
    use, intrinsic :: iso_fortran_env, only : real32, real64

    implicit none

    private
    public :: sum_hp

    contains


#define RK real32
#define RESIZE resize_sp
#define CORE sum_hp_core_sp
#include 'pairwise_sum_core.F90'
#undef RK
#undef RESIZE
#undef CORE

#define RK real64
#define RESIZE resize_dp
#define CORE sum_hp_core_dp
#include 'pairwise_sum_core.F90'
#undef RK
#undef RESIZE
#undef CORE

end module pairwise_sum


