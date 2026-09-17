

module pairwise_mean
    use, intrinsic :: iso_fortran_env, only : ik=>int64, real32, real64, real128
    use :: pairwise_sum, only : sum_hp

    implicit none

    private
    public :: mean

    interface mean
        module procedure mean_1_sp
        module procedure mean_full_2_sp
        module procedure mean_full_3_sp
        module procedure mean_full_4_sp
        module procedure mean_full_5_sp
        module procedure mean_full_6_sp
        module procedure mean_full_7_sp
        module procedure mean_full_8_sp
        module procedure mean_full_9_sp
        module procedure mean_full_10_sp
        module procedure mean_dim_2_sp
        module procedure mean_dim_3_sp
        module procedure mean_dim_4_sp
        module procedure mean_dim_5_sp
        module procedure mean_dim_6_sp
        module procedure mean_dim_7_sp
        module procedure mean_dim_8_sp
        module procedure mean_dim_9_sp
        module procedure mean_dim_10_sp
        module procedure mean_1_dp
        module procedure mean_full_2_dp
        module procedure mean_full_3_dp
        module procedure mean_full_4_dp
        module procedure mean_full_5_dp
        module procedure mean_full_6_dp
        module procedure mean_full_7_dp
        module procedure mean_full_8_dp
        module procedure mean_full_9_dp
        module procedure mean_full_10_dp
        module procedure mean_dim_2_dp
        module procedure mean_dim_3_dp
        module procedure mean_dim_4_dp
        module procedure mean_dim_5_dp
        module procedure mean_dim_6_dp
        module procedure mean_dim_7_dp
        module procedure mean_dim_8_dp
        module procedure mean_dim_9_dp
        module procedure mean_dim_10_dp
        module procedure mean_1_qp
        module procedure mean_full_2_qp
        module procedure mean_full_3_qp
        module procedure mean_full_4_qp
        module procedure mean_full_5_qp
        module procedure mean_full_6_qp
        module procedure mean_full_7_qp
        module procedure mean_full_8_qp
        module procedure mean_full_9_qp
        module procedure mean_full_10_qp
        module procedure mean_dim_2_qp
        module procedure mean_dim_3_qp
        module procedure mean_dim_4_qp
        module procedure mean_dim_5_qp
        module procedure mean_dim_6_qp
        module procedure mean_dim_7_qp
        module procedure mean_dim_8_qp
        module procedure mean_dim_9_qp
        module procedure mean_dim_10_qp
    end interface mean

    contains


#define RK real32
#define MEAN_1 mean_1_sp
#define MEAN_FULL_2 mean_full_2_sp
#define MEAN_FULL_3 mean_full_3_sp
#define MEAN_FULL_4 mean_full_4_sp
#define MEAN_FULL_5 mean_full_5_sp
#define MEAN_FULL_6 mean_full_6_sp
#define MEAN_FULL_7 mean_full_7_sp
#define MEAN_FULL_8 mean_full_8_sp
#define MEAN_FULL_9 mean_full_9_sp
#define MEAN_FULL_10 mean_full_10_sp
#define MEAN_DIM_2 mean_dim_2_sp
#define MEAN_DIM_3 mean_dim_3_sp
#define MEAN_DIM_4 mean_dim_4_sp
#define MEAN_DIM_5 mean_dim_5_sp
#define MEAN_DIM_6 mean_dim_6_sp
#define MEAN_DIM_7 mean_dim_7_sp
#define MEAN_DIM_8 mean_dim_8_sp
#define MEAN_DIM_9 mean_dim_9_sp
#define MEAN_DIM_10 mean_dim_10_sp
#include "pairwise_mean_spec.F90"
#undef RK
#undef MEAN_1
#undef MEAN_FULL_2
#undef MEAN_FULL_3
#undef MEAN_FULL_4
#undef MEAN_FULL_5
#undef MEAN_FULL_6
#undef MEAN_FULL_7
#undef MEAN_FULL_8
#undef MEAN_FULL_9
#undef MEAN_FULL_10
#undef MEAN_DIM_2
#undef MEAN_DIM_3
#undef MEAN_DIM_4
#undef MEAN_DIM_5
#undef MEAN_DIM_6
#undef MEAN_DIM_7
#undef MEAN_DIM_8
#undef MEAN_DIM_9
#undef MEAN_DIM_10

#define RK real64
#define MEAN_1 mean_1_dp
#define MEAN_FULL_2 mean_full_2_dp
#define MEAN_FULL_3 mean_full_3_dp
#define MEAN_FULL_4 mean_full_4_dp
#define MEAN_FULL_5 mean_full_5_dp
#define MEAN_FULL_6 mean_full_6_dp
#define MEAN_FULL_7 mean_full_7_dp
#define MEAN_FULL_8 mean_full_8_dp
#define MEAN_FULL_9 mean_full_9_dp
#define MEAN_FULL_10 mean_full_10_dp
#define MEAN_DIM_2 mean_dim_2_dp
#define MEAN_DIM_3 mean_dim_3_dp
#define MEAN_DIM_4 mean_dim_4_dp
#define MEAN_DIM_5 mean_dim_5_dp
#define MEAN_DIM_6 mean_dim_6_dp
#define MEAN_DIM_7 mean_dim_7_dp
#define MEAN_DIM_8 mean_dim_8_dp
#define MEAN_DIM_9 mean_dim_9_dp
#define MEAN_DIM_10 mean_dim_10_dp
#include "pairwise_mean_spec.F90"
#undef RK
#undef MEAN_1
#undef MEAN_FULL_2
#undef MEAN_FULL_3
#undef MEAN_FULL_4
#undef MEAN_FULL_5
#undef MEAN_FULL_6
#undef MEAN_FULL_7
#undef MEAN_FULL_8
#undef MEAN_FULL_9
#undef MEAN_FULL_10
#undef MEAN_DIM_2
#undef MEAN_DIM_3
#undef MEAN_DIM_4
#undef MEAN_DIM_5
#undef MEAN_DIM_6
#undef MEAN_DIM_7
#undef MEAN_DIM_8
#undef MEAN_DIM_9
#undef MEAN_DIM_10

#define RK real128
#define MEAN_1 mean_1_qp
#define MEAN_FULL_2 mean_full_2_qp
#define MEAN_FULL_3 mean_full_3_qp
#define MEAN_FULL_4 mean_full_4_qp
#define MEAN_FULL_5 mean_full_5_qp
#define MEAN_FULL_6 mean_full_6_qp
#define MEAN_FULL_7 mean_full_7_qp
#define MEAN_FULL_8 mean_full_8_qp
#define MEAN_FULL_9 mean_full_9_qp
#define MEAN_FULL_10 mean_full_10_qp
#define MEAN_DIM_2 mean_dim_2_qp
#define MEAN_DIM_3 mean_dim_3_qp
#define MEAN_DIM_4 mean_dim_4_qp
#define MEAN_DIM_5 mean_dim_5_qp
#define MEAN_DIM_6 mean_dim_6_qp
#define MEAN_DIM_7 mean_dim_7_qp
#define MEAN_DIM_8 mean_dim_8_qp
#define MEAN_DIM_9 mean_dim_9_qp
#define MEAN_DIM_10 mean_dim_10_qp
#include "pairwise_mean_spec.F90"
#undef RK
#undef MEAN_1
#undef MEAN_FULL_2
#undef MEAN_FULL_3
#undef MEAN_FULL_4
#undef MEAN_FULL_5
#undef MEAN_FULL_6
#undef MEAN_FULL_7
#undef MEAN_FULL_8
#undef MEAN_FULL_9
#undef MEAN_FULL_10
#undef MEAN_DIM_2
#undef MEAN_DIM_3
#undef MEAN_DIM_4
#undef MEAN_DIM_5
#undef MEAN_DIM_6
#undef MEAN_DIM_7
#undef MEAN_DIM_8
#undef MEAN_DIM_9
#undef MEAN_DIM_10

end module pairwise_mean


