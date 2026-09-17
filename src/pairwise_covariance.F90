

module pairwise_covariance
    use, intrinsic :: iso_fortran_env, only : ik=>int64, real32, real64, real128
    use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
    use :: pairwise_sum,  only : sum_hp
    use :: pairwise_mean, only : mean

    implicit none

    private
    public :: covariance

    interface covariance
        module procedure covariance_1_sp
        module procedure covariance_dim_1_sp
        module procedure covariance_full_2_sp
        module procedure covariance_full_3_sp
        module procedure covariance_full_4_sp
        module procedure covariance_full_5_sp
        module procedure covariance_full_6_sp
        module procedure covariance_full_7_sp
        ! module procedure covariance_full_8_sp
        ! module procedure covariance_full_9_sp
        ! module procedure covariance_full_10_sp
        module procedure covariance_dim_2_sp
        module procedure covariance_dim_3_sp
        module procedure covariance_dim_4_sp
        module procedure covariance_dim_5_sp
        module procedure covariance_dim_6_sp
        module procedure covariance_dim_7_sp
        ! module procedure covariance_dim_8_sp
        ! module procedure covariance_dim_9_sp
        ! module procedure covariance_dim_10_sp
        module procedure covariance_1_dp
        module procedure covariance_dim_1_dp
        module procedure covariance_full_2_dp
        module procedure covariance_full_3_dp
        module procedure covariance_full_4_dp
        module procedure covariance_full_5_dp
        module procedure covariance_full_6_dp
        module procedure covariance_full_7_dp
        ! module procedure covariance_full_8_dp
        ! module procedure covariance_full_9_dp
        ! module procedure covariance_full_10_dp
        module procedure covariance_dim_2_dp
        module procedure covariance_dim_3_dp
        module procedure covariance_dim_4_dp
        module procedure covariance_dim_5_dp
        module procedure covariance_dim_6_dp
        module procedure covariance_dim_7_dp
        ! module procedure covariance_dim_8_dp
        ! module procedure covariance_dim_9_dp
        ! module procedure covariance_dim_10_dp
        module procedure covariance_1_qp
        module procedure covariance_dim_1_qp
        module procedure covariance_full_2_qp
        module procedure covariance_full_3_qp
        module procedure covariance_full_4_qp
        module procedure covariance_full_5_qp
        module procedure covariance_full_6_qp
        module procedure covariance_full_7_qp
        ! module procedure covariance_full_8_qp
        ! module procedure covariance_full_9_qp
        ! module procedure covariance_full_10_qp
        module procedure covariance_dim_2_qp
        module procedure covariance_dim_3_qp
        module procedure covariance_dim_4_qp
        module procedure covariance_dim_5_qp
        module procedure covariance_dim_6_qp
        module procedure covariance_dim_7_qp
        ! module procedure covariance_dim_8_qp
        ! module procedure covariance_dim_9_qp
        ! module procedure covariance_dim_10_qp
    end interface covariance

    contains


#define RK real32
#define COVARIANCE_1 covariance_1_sp
#define COVARIANCE_DIM_1 covariance_dim_1_sp
#define COVARIANCE_FULL_2 covariance_full_2_sp
#define COVARIANCE_FULL_3 covariance_full_3_sp
#define COVARIANCE_FULL_4 covariance_full_4_sp
#define COVARIANCE_FULL_5 covariance_full_5_sp
#define COVARIANCE_FULL_6 covariance_full_6_sp
#define COVARIANCE_FULL_7 covariance_full_7_sp
#define COVARIANCE_FULL_8 covariance_full_8_sp
#define COVARIANCE_FULL_9 covariance_full_9_sp
#define COVARIANCE_FULL_10 covariance_full_10_sp
#define COVARIANCE_DIM_2 covariance_dim_2_sp
#define COVARIANCE_DIM_3 covariance_dim_3_sp
#define COVARIANCE_DIM_4 covariance_dim_4_sp
#define COVARIANCE_DIM_5 covariance_dim_5_sp
#define COVARIANCE_DIM_6 covariance_dim_6_sp
#define COVARIANCE_DIM_7 covariance_dim_7_sp
#define COVARIANCE_DIM_8 covariance_dim_8_sp
#define COVARIANCE_DIM_9 covariance_dim_9_sp
#define COVARIANCE_DIM_10 covariance_dim_10_sp
#include "pairwise_covariance_spec.F90"
#undef RK
#undef COVARIANCE_1
#undef COVARIANCE_DIM_1
#undef COVARIANCE_FULL_2
#undef COVARIANCE_FULL_3
#undef COVARIANCE_FULL_4
#undef COVARIANCE_FULL_5
#undef COVARIANCE_FULL_6
#undef COVARIANCE_FULL_7
#undef COVARIANCE_FULL_8
#undef COVARIANCE_FULL_9
#undef COVARIANCE_FULL_10
#undef COVARIANCE_DIM_2
#undef COVARIANCE_DIM_3
#undef COVARIANCE_DIM_4
#undef COVARIANCE_DIM_5
#undef COVARIANCE_DIM_6
#undef COVARIANCE_DIM_7
#undef COVARIANCE_DIM_8
#undef COVARIANCE_DIM_9
#undef COVARIANCE_DIM_10

#define RK real64
#define COVARIANCE_1 covariance_1_dp
#define COVARIANCE_DIM_1 covariance_dim_1_dp
#define COVARIANCE_FULL_2 covariance_full_2_dp
#define COVARIANCE_FULL_3 covariance_full_3_dp
#define COVARIANCE_FULL_4 covariance_full_4_dp
#define COVARIANCE_FULL_5 covariance_full_5_dp
#define COVARIANCE_FULL_6 covariance_full_6_dp
#define COVARIANCE_FULL_7 covariance_full_7_dp
#define COVARIANCE_FULL_8 covariance_full_8_dp
#define COVARIANCE_FULL_9 covariance_full_9_dp
#define COVARIANCE_FULL_10 covariance_full_10_dp
#define COVARIANCE_DIM_2 covariance_dim_2_dp
#define COVARIANCE_DIM_3 covariance_dim_3_dp
#define COVARIANCE_DIM_4 covariance_dim_4_dp
#define COVARIANCE_DIM_5 covariance_dim_5_dp
#define COVARIANCE_DIM_6 covariance_dim_6_dp
#define COVARIANCE_DIM_7 covariance_dim_7_dp
#define COVARIANCE_DIM_8 covariance_dim_8_dp
#define COVARIANCE_DIM_9 covariance_dim_9_dp
#define COVARIANCE_DIM_10 covariance_dim_10_dp
#include "pairwise_covariance_spec.F90"
#undef RK
#undef COVARIANCE_1
#undef COVARIANCE_DIM_1
#undef COVARIANCE_FULL_2
#undef COVARIANCE_FULL_3
#undef COVARIANCE_FULL_4
#undef COVARIANCE_FULL_5
#undef COVARIANCE_FULL_6
#undef COVARIANCE_FULL_7
#undef COVARIANCE_FULL_8
#undef COVARIANCE_FULL_9
#undef COVARIANCE_FULL_10
#undef COVARIANCE_DIM_2
#undef COVARIANCE_DIM_3
#undef COVARIANCE_DIM_4
#undef COVARIANCE_DIM_5
#undef COVARIANCE_DIM_6
#undef COVARIANCE_DIM_7
#undef COVARIANCE_DIM_8
#undef COVARIANCE_DIM_9
#undef COVARIANCE_DIM_10

#define RK real128
#define COVARIANCE_1 covariance_1_qp
#define COVARIANCE_DIM_1 covariance_dim_1_qp
#define COVARIANCE_FULL_2 covariance_full_2_qp
#define COVARIANCE_FULL_3 covariance_full_3_qp
#define COVARIANCE_FULL_4 covariance_full_4_qp
#define COVARIANCE_FULL_5 covariance_full_5_qp
#define COVARIANCE_FULL_6 covariance_full_6_qp
#define COVARIANCE_FULL_7 covariance_full_7_qp
#define COVARIANCE_FULL_8 covariance_full_8_qp
#define COVARIANCE_FULL_9 covariance_full_9_qp
#define COVARIANCE_FULL_10 covariance_full_10_qp
#define COVARIANCE_DIM_2 covariance_dim_2_qp
#define COVARIANCE_DIM_3 covariance_dim_3_qp
#define COVARIANCE_DIM_4 covariance_dim_4_qp
#define COVARIANCE_DIM_5 covariance_dim_5_qp
#define COVARIANCE_DIM_6 covariance_dim_6_qp
#define COVARIANCE_DIM_7 covariance_dim_7_qp
#define COVARIANCE_DIM_8 covariance_dim_8_qp
#define COVARIANCE_DIM_9 covariance_dim_9_qp
#define COVARIANCE_DIM_10 covariance_dim_10_qp
#include "pairwise_covariance_spec.F90"
#undef RK
#undef COVARIANCE_1
#undef COVARIANCE_DIM_1
#undef COVARIANCE_FULL_2
#undef COVARIANCE_FULL_3
#undef COVARIANCE_FULL_4
#undef COVARIANCE_FULL_5
#undef COVARIANCE_FULL_6
#undef COVARIANCE_FULL_7
#undef COVARIANCE_FULL_8
#undef COVARIANCE_FULL_9
#undef COVARIANCE_FULL_10
#undef COVARIANCE_DIM_2
#undef COVARIANCE_DIM_3
#undef COVARIANCE_DIM_4
#undef COVARIANCE_DIM_5
#undef COVARIANCE_DIM_6
#undef COVARIANCE_DIM_7
#undef COVARIANCE_DIM_8
#undef COVARIANCE_DIM_9
#undef COVARIANCE_DIM_10

end module pairwise_covariance

