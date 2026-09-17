

module pairwise_corrcoef
    use, intrinsic :: iso_fortran_env, only : ik=>int64, real32, real64, real128
    use, intrinsic :: ieee_arithmetic, only : ieee_quiet_nan, ieee_value
    use :: pairwise_covariance, only : covariance
    use :: pairwise_variance,   only : variance

    implicit none

    private
    public :: corrcoef

    interface corrcoef
        module procedure corrcoef_1_sp
        module procedure corrcoef_full_2_sp
        module procedure corrcoef_full_3_sp
        module procedure corrcoef_full_4_sp
        module procedure corrcoef_full_5_sp
        module procedure corrcoef_full_6_sp
        module procedure corrcoef_full_7_sp
        ! module procedure corrcoef_full_8_sp
        ! module procedure corrcoef_full_9_sp
        ! module procedure corrcoef_full_10_sp
        module procedure corrcoef_dim_2_sp
        module procedure corrcoef_dim_3_sp
        module procedure corrcoef_dim_4_sp
        module procedure corrcoef_dim_5_sp
        module procedure corrcoef_dim_6_sp
        module procedure corrcoef_dim_7_sp
        ! module procedure corrcoef_dim_8_sp
        ! module procedure corrcoef_dim_9_sp
        ! module procedure corrcoef_dim_10_sp
        module procedure corrcoef_1_dp
        module procedure corrcoef_full_2_dp
        module procedure corrcoef_full_3_dp
        module procedure corrcoef_full_4_dp
        module procedure corrcoef_full_5_dp
        module procedure corrcoef_full_6_dp
        module procedure corrcoef_full_7_dp
        ! module procedure corrcoef_full_8_dp
        ! module procedure corrcoef_full_9_dp
        ! module procedure corrcoef_full_10_dp
        module procedure corrcoef_dim_2_dp
        module procedure corrcoef_dim_3_dp
        module procedure corrcoef_dim_4_dp
        module procedure corrcoef_dim_5_dp
        module procedure corrcoef_dim_6_dp
        module procedure corrcoef_dim_7_dp
        ! module procedure corrcoef_dim_8_dp
        ! module procedure corrcoef_dim_9_dp
        ! module procedure corrcoef_dim_10_dp
        module procedure corrcoef_1_qp
        module procedure corrcoef_full_2_qp
        module procedure corrcoef_full_3_qp
        module procedure corrcoef_full_4_qp
        module procedure corrcoef_full_5_qp
        module procedure corrcoef_full_6_qp
        module procedure corrcoef_full_7_qp
        ! module procedure corrcoef_full_8_qp
        ! module procedure corrcoef_full_9_qp
        ! module procedure corrcoef_full_10_qp
        module procedure corrcoef_dim_2_qp
        module procedure corrcoef_dim_3_qp
        module procedure corrcoef_dim_4_qp
        module procedure corrcoef_dim_5_qp
        module procedure corrcoef_dim_6_qp
        module procedure corrcoef_dim_7_qp
        ! module procedure corrcoef_dim_8_qp
        ! module procedure corrcoef_dim_9_qp
        ! module procedure corrcoef_dim_10_qp
    end interface corrcoef

    contains


#define RK real32
#define CORRCOEF_1 corrcoef_1_sp
#define CORRCOEF_FULL_2 corrcoef_full_2_sp
#define CORRCOEF_FULL_3 corrcoef_full_3_sp
#define CORRCOEF_FULL_4 corrcoef_full_4_sp
#define CORRCOEF_FULL_5 corrcoef_full_5_sp
#define CORRCOEF_FULL_6 corrcoef_full_6_sp
#define CORRCOEF_FULL_7 corrcoef_full_7_sp
#define CORRCOEF_FULL_8 corrcoef_full_8_sp
#define CORRCOEF_FULL_9 corrcoef_full_9_sp
#define CORRCOEF_FULL_10 corrcoef_full_10_sp
#define CORRCOEF_DIM_2 corrcoef_dim_2_sp
#define CORRCOEF_DIM_3 corrcoef_dim_3_sp
#define CORRCOEF_DIM_4 corrcoef_dim_4_sp
#define CORRCOEF_DIM_5 corrcoef_dim_5_sp
#define CORRCOEF_DIM_6 corrcoef_dim_6_sp
#define CORRCOEF_DIM_7 corrcoef_dim_7_sp
#define CORRCOEF_DIM_8 corrcoef_dim_8_sp
#define CORRCOEF_DIM_9 corrcoef_dim_9_sp
#define CORRCOEF_DIM_10 corrcoef_dim_10_sp
#include "pairwise_corrcoef_spec.F90"
#undef RK
#undef CORRCOEF_1
#undef CORRCOEF_FULL_2
#undef CORRCOEF_FULL_3
#undef CORRCOEF_FULL_4
#undef CORRCOEF_FULL_5
#undef CORRCOEF_FULL_6
#undef CORRCOEF_FULL_7
#undef CORRCOEF_FULL_8
#undef CORRCOEF_FULL_9
#undef CORRCOEF_FULL_10
#undef CORRCOEF_DIM_2
#undef CORRCOEF_DIM_3
#undef CORRCOEF_DIM_4
#undef CORRCOEF_DIM_5
#undef CORRCOEF_DIM_6
#undef CORRCOEF_DIM_7
#undef CORRCOEF_DIM_8
#undef CORRCOEF_DIM_9
#undef CORRCOEF_DIM_10

#define RK real64
#define CORRCOEF_1 corrcoef_1_dp
#define CORRCOEF_FULL_2 corrcoef_full_2_dp
#define CORRCOEF_FULL_3 corrcoef_full_3_dp
#define CORRCOEF_FULL_4 corrcoef_full_4_dp
#define CORRCOEF_FULL_5 corrcoef_full_5_dp
#define CORRCOEF_FULL_6 corrcoef_full_6_dp
#define CORRCOEF_FULL_7 corrcoef_full_7_dp
#define CORRCOEF_FULL_8 corrcoef_full_8_dp
#define CORRCOEF_FULL_9 corrcoef_full_9_dp
#define CORRCOEF_FULL_10 corrcoef_full_10_dp
#define CORRCOEF_DIM_2 corrcoef_dim_2_dp
#define CORRCOEF_DIM_3 corrcoef_dim_3_dp
#define CORRCOEF_DIM_4 corrcoef_dim_4_dp
#define CORRCOEF_DIM_5 corrcoef_dim_5_dp
#define CORRCOEF_DIM_6 corrcoef_dim_6_dp
#define CORRCOEF_DIM_7 corrcoef_dim_7_dp
#define CORRCOEF_DIM_8 corrcoef_dim_8_dp
#define CORRCOEF_DIM_9 corrcoef_dim_9_dp
#define CORRCOEF_DIM_10 corrcoef_dim_10_dp
#include "pairwise_corrcoef_spec.F90"
#undef RK
#undef CORRCOEF_1
#undef CORRCOEF_FULL_2
#undef CORRCOEF_FULL_3
#undef CORRCOEF_FULL_4
#undef CORRCOEF_FULL_5
#undef CORRCOEF_FULL_6
#undef CORRCOEF_FULL_7
#undef CORRCOEF_FULL_8
#undef CORRCOEF_FULL_9
#undef CORRCOEF_FULL_10
#undef CORRCOEF_DIM_2
#undef CORRCOEF_DIM_3
#undef CORRCOEF_DIM_4
#undef CORRCOEF_DIM_5
#undef CORRCOEF_DIM_6
#undef CORRCOEF_DIM_7
#undef CORRCOEF_DIM_8
#undef CORRCOEF_DIM_9
#undef CORRCOEF_DIM_10

#define RK real128
#define CORRCOEF_1 corrcoef_1_qp
#define CORRCOEF_FULL_2 corrcoef_full_2_qp
#define CORRCOEF_FULL_3 corrcoef_full_3_qp
#define CORRCOEF_FULL_4 corrcoef_full_4_qp
#define CORRCOEF_FULL_5 corrcoef_full_5_qp
#define CORRCOEF_FULL_6 corrcoef_full_6_qp
#define CORRCOEF_FULL_7 corrcoef_full_7_qp
#define CORRCOEF_FULL_8 corrcoef_full_8_qp
#define CORRCOEF_FULL_9 corrcoef_full_9_qp
#define CORRCOEF_FULL_10 corrcoef_full_10_qp
#define CORRCOEF_DIM_2 corrcoef_dim_2_qp
#define CORRCOEF_DIM_3 corrcoef_dim_3_qp
#define CORRCOEF_DIM_4 corrcoef_dim_4_qp
#define CORRCOEF_DIM_5 corrcoef_dim_5_qp
#define CORRCOEF_DIM_6 corrcoef_dim_6_qp
#define CORRCOEF_DIM_7 corrcoef_dim_7_qp
#define CORRCOEF_DIM_8 corrcoef_dim_8_qp
#define CORRCOEF_DIM_9 corrcoef_dim_9_qp
#define CORRCOEF_DIM_10 corrcoef_dim_10_qp
#include "pairwise_corrcoef_spec.F90"
#undef RK
#undef CORRCOEF_1
#undef CORRCOEF_FULL_2
#undef CORRCOEF_FULL_3
#undef CORRCOEF_FULL_4
#undef CORRCOEF_FULL_5
#undef CORRCOEF_FULL_6
#undef CORRCOEF_FULL_7
#undef CORRCOEF_FULL_8
#undef CORRCOEF_FULL_9
#undef CORRCOEF_FULL_10
#undef CORRCOEF_DIM_2
#undef CORRCOEF_DIM_3
#undef CORRCOEF_DIM_4
#undef CORRCOEF_DIM_5
#undef CORRCOEF_DIM_6
#undef CORRCOEF_DIM_7
#undef CORRCOEF_DIM_8
#undef CORRCOEF_DIM_9
#undef CORRCOEF_DIM_10

end module pairwise_corrcoef

