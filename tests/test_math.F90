program test_math
    use, intrinsic :: iso_fortran_env, only : real32, real64, real128
    use :: EDAT_Math, only : covariance, corrcoef, mean, sum_hp, variance
    use :: test_support, only : check, check_close, check_nan, finish_tests

    implicit none

    call test_all_array_sizes
    call test_cancellation
    call test_supported_precisions

    call finish_tests('test_math')

    contains

    subroutine test_all_array_sizes
        integer, parameter :: maximum_size = 150

        real(real64), allocatable :: x(:)
        real(real64), allocatable :: y(:)
        integer                   :: array_size

        do array_size = 0, maximum_size
            allocate(x(array_size), y(array_size))

            call fill_affine_data(x(1:size(x,1)), &  !! OUT
                                & y(1:size(y,1))  )  !! OUT
            call check_statistics_for_size(x(1:size(x,1))         , &  !! IN
                                         & y(1:size(y,1))         , &  !! IN
                                         & array_size  )  !! IN

            deallocate(x, y)
        enddo
    end subroutine test_all_array_sizes


    subroutine fill_affine_data(x, y)
        real(real64), intent(out) :: x(:)
        real(real64), intent(out) :: y(:)

        integer :: index

        do index = 1, size(x)
            x(index) = real(index, real64) - 0.25_real64
            y(index) = 3.0_real64 * x(index) - 2.0_real64
        enddo
    end subroutine fill_affine_data


    subroutine check_statistics_for_size(x, y, array_size)
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: y(:)
        integer, intent(in)      :: array_size

        real(real64)  :: expected_mean
        real(real64)  :: expected_population_covariance
        real(real64)  :: expected_population_variance
        character(80) :: description

        write(description, '(A,I0)') 'sum_hp, size=', array_size
        call check_close(sum_hp(x(1:size(x,1)))     , &  !! IN
                       & sum(x(1:size(x,1)))        , &  !! IN
                       & 2.0e-15_real64, &  !! IN
                       & 2.0e-14_real64, &  !! IN
                       & description     )  !! IN

        if (array_size == 0) then
            call check_close(mean(x(1:size(x,1)))                 , &  !! IN
                           & 0.0_real64              , &  !! IN
                           & 0.0_real64              , &  !! IN
                           & 0.0_real64              , &  !! IN
                           & 'mean of an empty array'  )  !! IN
            return
        endif

        expected_mean = sum(x(1:size(x,1))) / real(array_size, real64)
        expected_population_variance = &
            sum((x(1:size(x,1)) - expected_mean)**2) / real(array_size, real64)
        expected_population_covariance = &
            sum((x(1:size(x,1)) - expected_mean) * (y(1:size(y,1)) - sum(y(1:size(y,1))) / real(array_size, real64))) &
            / real(array_size, real64)

        write(description, '(A,I0)') 'mean, size=', array_size
        call check_close(mean(x(1:size(x,1)))       , &  !! IN
                       & expected_mean , &  !! IN
                       & 3.0e-15_real64, &  !! IN
                       & 3.0e-14_real64, &  !! IN
                       & description     )  !! IN

        write(description, '(A,I0)') 'population variance, size=', array_size
        call check_close(variance(x(1:size(x,1)))                 , &  !! IN
                       & expected_population_variance, &  !! IN
                       & 5.0e-14_real64              , &  !! IN
                       & 5.0e-14_real64              , &  !! IN
                       & description                   )  !! IN

        write(description, '(A,I0)') 'population covariance, size=', array_size
        call check_close(covariance(x(1:size(x,1)), y(1:size(y,1)))              , &  !! IN
                       & expected_population_covariance, &  !! IN
                       & 5.0e-14_real64                , &  !! IN
                       & 5.0e-14_real64                , &  !! IN
                       & description                     )  !! IN

        if (array_size > 1) then
            write(description, '(A,I0)') 'sample variance, size=', array_size
            call check_close(variance(x(1:size(x,1)), sample = .TRUE.)                                , &  !! IN
                           & sum((x(1:size(x,1)) - expected_mean)**2) / real(array_size - 1, real64), &  !! IN
                           & 5.0e-14_real64                                            , &  !! IN
                           & 5.0e-14_real64                                            , &  !! IN
                           & description                                                 )  !! IN

            write(description, '(A,I0)') 'correlation of affine data, size=', array_size
            call check_close(corrcoef(x(1:size(x,1)), y(1:size(y,1))), &  !! IN
                           & 1.0_real64    , &  !! IN
                           & 5.0e-14_real64, &  !! IN
                           & 5.0e-14_real64, &  !! IN
                           & description     )  !! IN
        endif
    end subroutine check_statistics_for_size


    subroutine test_cancellation
        real(real64) :: values(5)

        values(1:size(values,1)) = [1.0e16_real64, -1.0e16_real64, &
                            1.0_real64, 2.0_real64, 3.0_real64]

        call check_close(sum_hp(values(1:size(values,1)))                             , &  !! IN
                       & 6.0_real64                                 , &  !! IN
                       & 0.0_real64                                 , &  !! IN
                       & 0.0_real64                                 , &  !! IN
                       & 'pairwise sum preserves the small residual'  )  !! IN
    end subroutine test_cancellation




    subroutine test_supported_precisions
        integer :: values(5)

        values(1:size(values,1)) = [1, 2, 3, 4, 5]

        call check(abs(real(sum_hp(real(values(1:size(values,1)), real32)), real64) - 15.0_real64) < 1.0e-6_real64, &  !! IN
                 & 'sum_hp supports real32'                                                       )  !! IN

        call check(abs(real(sum_hp(real(values(1:size(values,1)), real128)), real64) - &
            & 15.0_real64) < 1.0e-14_real64, &  !! IN
                 & 'sum_hp supports real128'                                                        )  !! IN
    end subroutine test_supported_precisions

end program test_math
