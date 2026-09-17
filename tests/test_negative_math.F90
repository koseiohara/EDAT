program test_negative_math
    use, intrinsic :: iso_fortran_env, only : real64
    use :: EDAT_Math, only : covariance, corrcoef, mean, sum_hp, variance

    implicit none

    character(64) :: mode
    real(real64)  :: x1(4)
    real(real64)  :: y1(3)
    real(real64)  :: x2(2,3)
    real(real64)  :: y2_first(3,3)
    real(real64)  :: y2_second(2,4)
    real(real64)  :: y2_same_size(3,2)

    call get_command_argument(1   , &  !! IN
                            & mode  )  !! OUT

    x1(1:4) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
    y1(1:3) = [1.0_real64, 2.0_real64, 3.0_real64]
    x2(1:2,1:3) = 1.0_real64
    y2_first(1:3,1:3) = 1.0_real64
    y2_second(1:2,1:4) = 1.0_real64
    y2_same_size(1:3,1:2) = 1.0_real64

    select case (trim(mode))
    case ('sum_dim_negative')
        write(*, *) sum_hp(x2(1:2,1:3), -1)
    case ('sum_dim_zero')
        write(*, *) sum_hp(x2(1:2,1:3), 0)
    case ('sum_dim_high')
        write(*, *) sum_hp(x2(1:2,1:3), 3)
    case ('mean_dim_negative')
        write(*, *) mean(x2(1:2,1:3), -1)
    case ('mean_dim_zero')
        write(*, *) mean(x2(1:2,1:3), 0)
    case ('mean_dim_high')
        write(*, *) mean(x2(1:2,1:3), 3)
    case ('variance_dim_negative')
        write(*, *) variance(x2(1:2,1:3), -1)
    case ('variance_dim_zero')
        write(*, *) variance(x2(1:2,1:3), 0)
    case ('variance_dim_high')
        write(*, *) variance(x2(1:2,1:3), 3)
    case ('covariance_dim_negative')
        write(*, *) covariance(x2(1:2,1:3), x2(1:2,1:3), -1)
    case ('covariance_dim_zero')
        write(*, *) covariance(x2(1:2,1:3), x2(1:2,1:3), 0)
    case ('covariance_dim_high')
        write(*, *) covariance(x2(1:2,1:3), x2(1:2,1:3), 3)
    case ('corrcoef_dim_negative')
        write(*, *) corrcoef(x2(1:2,1:3), x2(1:2,1:3), -1)
    case ('corrcoef_dim_zero')
        write(*, *) corrcoef(x2(1:2,1:3), x2(1:2,1:3), 0)
    case ('corrcoef_dim_high')
        write(*, *) corrcoef(x2(1:2,1:3), x2(1:2,1:3), 3)
    case ('covariance_rank1_size')
        write(*, *) covariance(x1(1:4), y1(1:3))
    case ('covariance_rank2_dim1')
        write(*, *) covariance(x2(1:2,1:3), y2_first(1:3,1:3))
    case ('covariance_rank2_dim2')
        write(*, *) covariance(x2(1:2,1:3), y2_second(1:2,1:4))
    case ('covariance_rank2_same_size')
        write(*, *) covariance(x2(1:2,1:3), y2_same_size(1:3,1:2))
    case ('corrcoef_rank1_size')
        write(*, *) corrcoef(x1(1:4), y1(1:3))
    case ('corrcoef_rank2_dim1')
        write(*, *) corrcoef(x2(1:2,1:3), y2_first(1:3,1:3))
    case ('corrcoef_rank2_dim2')
        write(*, *) corrcoef(x2(1:2,1:3), y2_second(1:2,1:4))
    case ('corrcoef_rank2_same_size')
        write(*, *) corrcoef(x2(1:2,1:3), y2_same_size(1:3,1:2))
    case default
        ERROR STOP 'test_negative_math: unknown mode'
    end select

    ERROR STOP 'test_negative_math: negative case unexpectedly succeeded'
end program test_negative_math
