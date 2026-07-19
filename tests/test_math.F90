program test_math
  use, intrinsic :: iso_fortran_env, only: real32, real64, real128
  use EDAT_Math, only: covariance, corrcoef, mean, sum_hp, variance
  use test_support, only: check, check_close, check_nan, finish_tests
  implicit none

  call test_all_array_sizes
  call test_cancellation
  call test_size_mismatch
  call test_supported_precisions

  call finish_tests('test_math')

contains

  subroutine test_all_array_sizes
    integer, parameter :: maximum_size = 150

    real(real64), allocatable :: x(:)
    real(real64), allocatable :: y(:)
    integer :: array_size

    do array_size = 0, maximum_size
      allocate(x(array_size), y(array_size))

      call fill_affine_data(x, y)
      call check_statistics_for_size(x, y, array_size)

      deallocate(x, y)
    end do
  end subroutine test_all_array_sizes


  subroutine fill_affine_data(x, y)
    real(real64), intent(out) :: x(:)
    real(real64), intent(out) :: y(:)

    integer :: index

    do index = 1, size(x)
      x(index) = real(index, real64) - 0.25_real64
      y(index) = 3.0_real64 * x(index) - 2.0_real64
    end do
  end subroutine fill_affine_data


  subroutine check_statistics_for_size(x, y, array_size)
    real(real64), intent(in) :: x(:)
    real(real64), intent(in) :: y(:)
    integer, intent(in) :: array_size

    real(real64) :: expected_mean
    real(real64) :: expected_population_covariance
    real(real64) :: expected_population_variance
    character(80) :: description

    write(description, '(A,I0)') 'sum_hp, size=', array_size
    call check_close(sum_hp(x), sum(x), 2.0e-15_real64, 2.0e-14_real64, description)

    if (array_size == 0) then
      call check_close(mean(x), 0.0_real64, 0.0_real64, 0.0_real64, &
                       'mean of an empty array')
      return
    end if

    expected_mean = sum(x) / real(array_size, real64)
    expected_population_variance = &
      sum((x - expected_mean)**2) / real(array_size, real64)
    expected_population_covariance = &
      sum((x - expected_mean) * (y - sum(y) / real(array_size, real64))) &
      / real(array_size, real64)

    write(description, '(A,I0)') 'mean, size=', array_size
    call check_close(mean(x), expected_mean, &
                     3.0e-15_real64, 3.0e-14_real64, description)

    write(description, '(A,I0)') 'population variance, size=', array_size
    call check_close(variance(x), expected_population_variance, &
                     5.0e-14_real64, 5.0e-14_real64, description)

    write(description, '(A,I0)') 'population covariance, size=', array_size
    call check_close(covariance(x, y), expected_population_covariance, &
                     5.0e-14_real64, 5.0e-14_real64, description)

    if (array_size > 1) then
      write(description, '(A,I0)') 'sample variance, size=', array_size
      call check_close(variance(x, sample=.true.), &
                       sum((x - expected_mean)**2) / real(array_size - 1, real64), &
                       5.0e-14_real64, 5.0e-14_real64, description)

      write(description, '(A,I0)') 'correlation of affine data, size=', array_size
      call check_close(corrcoef(x, y), 1.0_real64, &
                       5.0e-14_real64, 5.0e-14_real64, description)
    end if
  end subroutine check_statistics_for_size


  subroutine test_cancellation
    real(real64) :: values(5)

    values = [1.0e16_real64, -1.0e16_real64, &
              1.0_real64, 2.0_real64, 3.0_real64]

    call check_close(sum_hp(values), 6.0_real64, &
                     0.0_real64, 0.0_real64, &
                     'pairwise sum preserves the small residual')
  end subroutine test_cancellation


  subroutine test_size_mismatch
    real(real64) :: x(5)
    real(real64) :: y(2)

    x = [1.0e16_real64, -1.0e16_real64, &
         1.0_real64, 2.0_real64, 3.0_real64]
    y = [1.0_real64, 2.0_real64]

    call check_nan(covariance(x, y), &
                   'covariance returns NaN for mismatched array sizes')
  end subroutine test_size_mismatch


  subroutine test_supported_precisions
    integer :: values(5)

    values = [1, 2, 3, 4, 5]

    call check(&
      abs(real(sum_hp(real(values, real32)), real64) - 15.0_real64) &
        < 1.0e-6_real64, &
      'sum_hp supports real32')

    call check(&
      abs(real(sum_hp(real(values, real128)), real64) - 15.0_real64) &
        < 1.0e-14_real64, &
      'sum_hp supports real128')
  end subroutine test_supported_precisions

end program test_math
