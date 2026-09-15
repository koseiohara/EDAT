

module pairwise_sum
    use, intrinsic :: iso_fortran_env, only : ik=>int64, real32, real64

    implicit none

    private
    public :: sum_hp

    interface sum_hp
        module procedure sum_hp_1_sp
        module procedure sum_hp_full_2_sp
        module procedure sum_hp_full_3_sp
        module procedure sum_hp_full_4_sp
        module procedure sum_hp_full_5_sp
        module procedure sum_hp_full_6_sp
        module procedure sum_hp_full_7_sp
        module procedure sum_hp_full_8_sp
        module procedure sum_hp_full_9_sp
        module procedure sum_hp_full_10_sp
        module procedure sum_hp_dim_2_sp
        module procedure sum_hp_dim_3_sp
        module procedure sum_hp_dim_4_sp
        module procedure sum_hp_dim_5_sp
        module procedure sum_hp_dim_6_sp
        module procedure sum_hp_dim_7_sp
        module procedure sum_hp_dim_8_sp
        module procedure sum_hp_dim_9_sp
        module procedure sum_hp_dim_10_sp
        module procedure sum_hp_1_dp
        module procedure sum_hp_full_2_dp
        module procedure sum_hp_full_3_dp
        module procedure sum_hp_full_4_dp
        module procedure sum_hp_full_5_dp
        module procedure sum_hp_full_6_dp
        module procedure sum_hp_full_7_dp
        module procedure sum_hp_full_8_dp
        module procedure sum_hp_full_9_dp
        module procedure sum_hp_full_10_dp
        module procedure sum_hp_dim_2_dp
        module procedure sum_hp_dim_3_dp
        module procedure sum_hp_dim_4_dp
        module procedure sum_hp_dim_5_dp
        module procedure sum_hp_dim_6_dp
        module procedure sum_hp_dim_7_dp
        module procedure sum_hp_dim_8_dp
        module procedure sum_hp_dim_9_dp
        module procedure sum_hp_dim_10_dp
    end interface sum_hp

    contains


#define RK real32
#define RESIZE resize_sp
#define CORE sum_hp_core_sp
#define SUM_HP_1 sum_hp_1_sp
#define SUM_HP_FULL_2 sum_hp_full_2_sp
#define SUM_HP_FULL_3 sum_hp_full_3_sp
#define SUM_HP_FULL_4 sum_hp_full_4_sp
#define SUM_HP_FULL_5 sum_hp_full_5_sp
#define SUM_HP_FULL_6 sum_hp_full_6_sp
#define SUM_HP_FULL_7 sum_hp_full_7_sp
#define SUM_HP_FULL_8 sum_hp_full_8_sp
#define SUM_HP_FULL_9 sum_hp_full_9_sp
#define SUM_HP_FULL_10 sum_hp_full_10_sp
#define SUM_HP_DIM_2 sum_hp_dim_2_sp
#define SUM_HP_DIM_3 sum_hp_dim_3_sp
#define SUM_HP_DIM_4 sum_hp_dim_4_sp
#define SUM_HP_DIM_5 sum_hp_dim_5_sp
#define SUM_HP_DIM_6 sum_hp_dim_6_sp
#define SUM_HP_DIM_7 sum_hp_dim_7_sp
#define SUM_HP_DIM_8 sum_hp_dim_8_sp
#define SUM_HP_DIM_9 sum_hp_dim_9_sp
#define SUM_HP_DIM_10 sum_hp_dim_10_sp
#include "pairwise_sum_core.F90"
#include "pairwise_sum_spec.F90"
#undef RK
#undef RESIZE
#undef CORE
#undef SUM_HP_1
#undef SUM_HP_FULL_2
#undef SUM_HP_FULL_3
#undef SUM_HP_FULL_4
#undef SUM_HP_FULL_5
#undef SUM_HP_FULL_6
#undef SUM_HP_FULL_7
#undef SUM_HP_FULL_8
#undef SUM_HP_FULL_9
#undef SUM_HP_FULL_10
#undef SUM_HP_DIM_2
#undef SUM_HP_DIM_3
#undef SUM_HP_DIM_4
#undef SUM_HP_DIM_5
#undef SUM_HP_DIM_6
#undef SUM_HP_DIM_7
#undef SUM_HP_DIM_8
#undef SUM_HP_DIM_9
#undef SUM_HP_DIM_10

#define RK real64
#define RESIZE resize_dp
#define CORE sum_hp_core_dp
#define SUM_HP_1 sum_hp_1_dp
#define SUM_HP_FULL_2 sum_hp_full_2_dp
#define SUM_HP_FULL_3 sum_hp_full_3_dp
#define SUM_HP_FULL_4 sum_hp_full_4_dp
#define SUM_HP_FULL_5 sum_hp_full_5_dp
#define SUM_HP_FULL_6 sum_hp_full_6_dp
#define SUM_HP_FULL_7 sum_hp_full_7_dp
#define SUM_HP_FULL_8 sum_hp_full_8_dp
#define SUM_HP_FULL_9 sum_hp_full_9_dp
#define SUM_HP_FULL_10 sum_hp_full_10_dp
#define SUM_HP_DIM_2 sum_hp_dim_2_dp
#define SUM_HP_DIM_3 sum_hp_dim_3_dp
#define SUM_HP_DIM_4 sum_hp_dim_4_dp
#define SUM_HP_DIM_5 sum_hp_dim_5_dp
#define SUM_HP_DIM_6 sum_hp_dim_6_dp
#define SUM_HP_DIM_7 sum_hp_dim_7_dp
#define SUM_HP_DIM_8 sum_hp_dim_8_dp
#define SUM_HP_DIM_9 sum_hp_dim_9_dp
#define SUM_HP_DIM_10 sum_hp_dim_10_dp
#include "pairwise_sum_core.F90"
#include "pairwise_sum_spec.F90"
#undef RK
#undef RESIZE
#undef CORE
#undef SUM_HP_1
#undef SUM_HP_FULL_2
#undef SUM_HP_FULL_3
#undef SUM_HP_FULL_4
#undef SUM_HP_FULL_5
#undef SUM_HP_FULL_6
#undef SUM_HP_FULL_7
#undef SUM_HP_FULL_8
#undef SUM_HP_FULL_9
#undef SUM_HP_FULL_10
#undef SUM_HP_DIM_2
#undef SUM_HP_DIM_3
#undef SUM_HP_DIM_4
#undef SUM_HP_DIM_5
#undef SUM_HP_DIM_6
#undef SUM_HP_DIM_7
#undef SUM_HP_DIM_8
#undef SUM_HP_DIM_9
#undef SUM_HP_DIM_10

end module pairwise_sum


