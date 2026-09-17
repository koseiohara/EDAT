

module pairwise_variance
    use, intrinsic :: iso_fortran_env, only : ik=>int64, real32, real64, real128
    use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
    use :: pairwise_sum,  only : sum_hp
    use :: pairwise_mean, only : mean

    implicit none

    private
    public :: variance

    interface variance
        module procedure variance_1_sp
        module procedure variance_dim_1_sp
        module procedure variance_full_2_sp
        module procedure variance_full_3_sp
        module procedure variance_full_4_sp
        module procedure variance_full_5_sp
        module procedure variance_full_6_sp
        module procedure variance_full_7_sp
        ! module procedure variance_full_8_sp
        ! module procedure variance_full_9_sp
        ! module procedure variance_full_10_sp
        module procedure variance_dim_2_sp
        module procedure variance_dim_3_sp
        module procedure variance_dim_4_sp
        module procedure variance_dim_5_sp
        module procedure variance_dim_6_sp
        module procedure variance_dim_7_sp
        ! module procedure variance_dim_8_sp
        ! module procedure variance_dim_9_sp
        ! module procedure variance_dim_10_sp
        module procedure variance_1_dp
        module procedure variance_dim_1_dp
        module procedure variance_full_2_dp
        module procedure variance_full_3_dp
        module procedure variance_full_4_dp
        module procedure variance_full_5_dp
        module procedure variance_full_6_dp
        module procedure variance_full_7_dp
        ! module procedure variance_full_8_dp
        ! module procedure variance_full_9_dp
        ! module procedure variance_full_10_dp
        module procedure variance_dim_2_dp
        module procedure variance_dim_3_dp
        module procedure variance_dim_4_dp
        module procedure variance_dim_5_dp
        module procedure variance_dim_6_dp
        module procedure variance_dim_7_dp
        ! module procedure variance_dim_8_dp
        ! module procedure variance_dim_9_dp
        ! module procedure variance_dim_10_dp
        module procedure variance_1_qp
        module procedure variance_dim_1_qp
        module procedure variance_full_2_qp
        module procedure variance_full_3_qp
        module procedure variance_full_4_qp
        module procedure variance_full_5_qp
        module procedure variance_full_6_qp
        module procedure variance_full_7_qp
        ! module procedure variance_full_8_qp
        ! module procedure variance_full_9_qp
        ! module procedure variance_full_10_qp
        module procedure variance_dim_2_qp
        module procedure variance_dim_3_qp
        module procedure variance_dim_4_qp
        module procedure variance_dim_5_qp
        module procedure variance_dim_6_qp
        module procedure variance_dim_7_qp
        ! module procedure variance_dim_8_qp
        ! module procedure variance_dim_9_qp
        ! module procedure variance_dim_10_qp
    end interface variance

    contains


#define RK real32
#define VARIANCE_1 variance_1_sp
#define VARIANCE_DIM_1 variance_dim_1_sp
#define VARIANCE_FULL_2 variance_full_2_sp
#define VARIANCE_FULL_3 variance_full_3_sp
#define VARIANCE_FULL_4 variance_full_4_sp
#define VARIANCE_FULL_5 variance_full_5_sp
#define VARIANCE_FULL_6 variance_full_6_sp
#define VARIANCE_FULL_7 variance_full_7_sp
#define VARIANCE_FULL_8 variance_full_8_sp
#define VARIANCE_FULL_9 variance_full_9_sp
#define VARIANCE_FULL_10 variance_full_10_sp
#define VARIANCE_DIM_2 variance_dim_2_sp
#define VARIANCE_DIM_3 variance_dim_3_sp
#define VARIANCE_DIM_4 variance_dim_4_sp
#define VARIANCE_DIM_5 variance_dim_5_sp
#define VARIANCE_DIM_6 variance_dim_6_sp
#define VARIANCE_DIM_7 variance_dim_7_sp
#define VARIANCE_DIM_8 variance_dim_8_sp
#define VARIANCE_DIM_9 variance_dim_9_sp
#define VARIANCE_DIM_10 variance_dim_10_sp
#include "pairwise_variance_spec.F90"
#undef RK
#undef VARIANCE_1
#undef VARIANCE_DIM_1
#undef VARIANCE_FULL_2
#undef VARIANCE_FULL_3
#undef VARIANCE_FULL_4
#undef VARIANCE_FULL_5
#undef VARIANCE_FULL_6
#undef VARIANCE_FULL_7
#undef VARIANCE_FULL_8
#undef VARIANCE_FULL_9
#undef VARIANCE_FULL_10
#undef VARIANCE_DIM_2
#undef VARIANCE_DIM_3
#undef VARIANCE_DIM_4
#undef VARIANCE_DIM_5
#undef VARIANCE_DIM_6
#undef VARIANCE_DIM_7
#undef VARIANCE_DIM_8
#undef VARIANCE_DIM_9
#undef VARIANCE_DIM_10

#define RK real64
#define VARIANCE_1 variance_1_dp
#define VARIANCE_DIM_1 variance_dim_1_dp
#define VARIANCE_FULL_2 variance_full_2_dp
#define VARIANCE_FULL_3 variance_full_3_dp
#define VARIANCE_FULL_4 variance_full_4_dp
#define VARIANCE_FULL_5 variance_full_5_dp
#define VARIANCE_FULL_6 variance_full_6_dp
#define VARIANCE_FULL_7 variance_full_7_dp
#define VARIANCE_FULL_8 variance_full_8_dp
#define VARIANCE_FULL_9 variance_full_9_dp
#define VARIANCE_FULL_10 variance_full_10_dp
#define VARIANCE_DIM_2 variance_dim_2_dp
#define VARIANCE_DIM_3 variance_dim_3_dp
#define VARIANCE_DIM_4 variance_dim_4_dp
#define VARIANCE_DIM_5 variance_dim_5_dp
#define VARIANCE_DIM_6 variance_dim_6_dp
#define VARIANCE_DIM_7 variance_dim_7_dp
#define VARIANCE_DIM_8 variance_dim_8_dp
#define VARIANCE_DIM_9 variance_dim_9_dp
#define VARIANCE_DIM_10 variance_dim_10_dp
#include "pairwise_variance_spec.F90"
#undef RK
#undef VARIANCE_1
#undef VARIANCE_DIM_1
#undef VARIANCE_FULL_2
#undef VARIANCE_FULL_3
#undef VARIANCE_FULL_4
#undef VARIANCE_FULL_5
#undef VARIANCE_FULL_6
#undef VARIANCE_FULL_7
#undef VARIANCE_FULL_8
#undef VARIANCE_FULL_9
#undef VARIANCE_FULL_10
#undef VARIANCE_DIM_2
#undef VARIANCE_DIM_3
#undef VARIANCE_DIM_4
#undef VARIANCE_DIM_5
#undef VARIANCE_DIM_6
#undef VARIANCE_DIM_7
#undef VARIANCE_DIM_8
#undef VARIANCE_DIM_9
#undef VARIANCE_DIM_10

#define RK real128
#define VARIANCE_1 variance_1_qp
#define VARIANCE_DIM_1 variance_dim_1_qp
#define VARIANCE_FULL_2 variance_full_2_qp
#define VARIANCE_FULL_3 variance_full_3_qp
#define VARIANCE_FULL_4 variance_full_4_qp
#define VARIANCE_FULL_5 variance_full_5_qp
#define VARIANCE_FULL_6 variance_full_6_qp
#define VARIANCE_FULL_7 variance_full_7_qp
#define VARIANCE_FULL_8 variance_full_8_qp
#define VARIANCE_FULL_9 variance_full_9_qp
#define VARIANCE_FULL_10 variance_full_10_qp
#define VARIANCE_DIM_2 variance_dim_2_qp
#define VARIANCE_DIM_3 variance_dim_3_qp
#define VARIANCE_DIM_4 variance_dim_4_qp
#define VARIANCE_DIM_5 variance_dim_5_qp
#define VARIANCE_DIM_6 variance_dim_6_qp
#define VARIANCE_DIM_7 variance_dim_7_qp
#define VARIANCE_DIM_8 variance_dim_8_qp
#define VARIANCE_DIM_9 variance_dim_9_qp
#define VARIANCE_DIM_10 variance_dim_10_qp
#include "pairwise_variance_spec.F90"
#undef RK
#undef VARIANCE_1
#undef VARIANCE_DIM_1
#undef VARIANCE_FULL_2
#undef VARIANCE_FULL_3
#undef VARIANCE_FULL_4
#undef VARIANCE_FULL_5
#undef VARIANCE_FULL_6
#undef VARIANCE_FULL_7
#undef VARIANCE_FULL_8
#undef VARIANCE_FULL_9
#undef VARIANCE_FULL_10
#undef VARIANCE_DIM_2
#undef VARIANCE_DIM_3
#undef VARIANCE_DIM_4
#undef VARIANCE_DIM_5
#undef VARIANCE_DIM_6
#undef VARIANCE_DIM_7
#undef VARIANCE_DIM_8
#undef VARIANCE_DIM_9
#undef VARIANCE_DIM_10

end module pairwise_variance


