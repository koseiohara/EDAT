program test_math
  use, intrinsic :: iso_fortran_env, only: real32, real64, real128
  use EDAT_Math, only: covariance, corrcoef, mean, sum_hp, variance
  use test_support, only: check, check_close, check_nan, finish_tests
  implicit none

  call test_all_array_sizes
  call test_cancellation
  call test_multidimensional_statistics
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
      call check_close(variance(x), 0.0_real64, 0.0_real64, 0.0_real64, &
                       'population variance of an empty array')
      call check_close(variance(x, sample=.true.), 0.0_real64, &
                       0.0_real64, 0.0_real64, &
                       'sample variance of an empty array')
      call check_close(covariance(x, y), 0.0_real64, 0.0_real64, &
                       0.0_real64, 'covariance of empty arrays')
      call check_close(covariance(x, y, sample=.true.), 0.0_real64, &
                       0.0_real64, 0.0_real64, &
                       'sample covariance of empty arrays')
      call check_close(corrcoef(x, y), 0.0_real64, 0.0_real64, &
                       0.0_real64, 'correlation of empty arrays')
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

    if (array_size == 1) then
      call check_nan(variance(x, sample=.true.), &
                     'sample variance of one value is NaN')
      call check_nan(covariance(x, y, sample=.true.), &
                     'sample covariance of one pair is NaN')
      call check_nan(corrcoef(x, y), &
                     'correlation of one pair is NaN')
    end if

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


  subroutine test_multidimensional_statistics
    real(real64) :: values(2,3)
    real(real64) :: related_values(2,3)
    real(real64) :: variance_dim1(3)
    real(real64) :: variance_dim2(2)

    values = reshape([1.0_real64, 2.0_real64, 3.0_real64, &
                      4.0_real64, 5.0_real64, 6.0_real64], shape(values))
    related_values = 2.0_real64 * values + 1.0_real64
    variance_dim1 = variance(values, dim=1)
    variance_dim2 = variance(values, dim=2)

    call check_close(variance(values(:,1), dim=1), 0.25_real64, &
                     5.0e-14_real64, 5.0e-14_real64, &
                     'population variance supports rank-1 dim reduction')
    call check_close(variance(values), 35.0_real64 / 12.0_real64, &
                     5.0e-14_real64, 5.0e-14_real64, &
                     'population variance supports full rank-2 reduction')
    call check_close(variance(values, sample=.true.), 3.5_real64, &
                     5.0e-14_real64, 5.0e-14_real64, &
                     'sample variance supports full rank-2 reduction')
    call check(all(abs(variance_dim1 - 0.25_real64) < 5.0e-14_real64), &
               'population variance supports dim=1 reduction')
    call check(all(abs(variance_dim2 - 8.0_real64 / 3.0_real64) &
                   < 5.0e-14_real64), &
               'population variance supports dim=2 reduction')
    call check(all(abs(variance(values, dim=1, sample=.true.) - 0.5_real64) &
                   < 5.0e-14_real64), &
               'sample variance supports dimensional reduction')

    call check_close(covariance(values(:,1), related_values(:,1), dim=1), &
                     0.5_real64, 5.0e-14_real64, 5.0e-14_real64, &
                     'covariance supports rank-1 dim reduction')
    call check_close(covariance(values, related_values), &
                     35.0_real64 / 6.0_real64, &
                     5.0e-14_real64, 5.0e-14_real64, &
                     'covariance supports full rank-2 reduction')
    call check_close(covariance(values, related_values, sample=.true.), &
                     7.0_real64, 5.0e-14_real64, 5.0e-14_real64, &
                     'sample covariance supports full rank-2 reduction')
    call check(all(abs(covariance(values, related_values, dim=1) &
                       - 0.5_real64) < 5.0e-14_real64), &
               'covariance supports dim=1 reduction')
    call check(all(abs(covariance(values, related_values, dim=2) &
                       - 16.0_real64 / 3.0_real64) < 5.0e-14_real64), &
               'covariance supports dim=2 reduction')
    call check(all(abs(covariance(values, related_values, dim=1, &
                                  sample=.true.) - 1.0_real64) &
                   < 5.0e-14_real64), &
               'sample covariance supports dimensional reduction')

    call check_close(corrcoef(values(:,1), related_values(:,1), dim=1), &
                     1.0_real64, 5.0e-14_real64, 5.0e-14_real64, &
                     'corrcoef supports rank-1 dim reduction')
    call check_close(corrcoef(values, related_values), 1.0_real64, &
                     5.0e-14_real64, 5.0e-14_real64, &
                     'corrcoef supports full rank-2 reduction')
    call check(all(abs(corrcoef(values, related_values, dim=1) &
                       - 1.0_real64) < 5.0e-14_real64), &
               'corrcoef supports dim=1 reduction')
    call check(all(abs(corrcoef(values, related_values, dim=2) &
                       - 1.0_real64) < 5.0e-14_real64), &
               'corrcoef supports dim=2 reduction')
  end subroutine test_multidimensional_statistics


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

    call check_close(real(variance(real(values, real32)), real64), &
                     2.0_real64, 1.0e-6_real64, 1.0e-6_real64, &
                     'variance supports real32')
    call check_close(real(variance(real(values, real128)), real64), &
                     2.0_real64, 1.0e-14_real64, 1.0e-14_real64, &
                     'variance supports real128')
    call check_close(real(covariance(real(values, real32), &
                                     real(values, real32)), real64), &
                     2.0_real64, 1.0e-6_real64, 1.0e-6_real64, &
                     'covariance supports real32')
    call check_close(real(covariance(real(values, real128), &
                                     real(values, real128)), real64), &
                     2.0_real64, 1.0e-14_real64, 1.0e-14_real64, &
                     'covariance supports real128')
    call check_close(real(corrcoef(real(values, real32), &
                                   real(values, real32)), real64), &
                     1.0_real64, 1.0e-6_real64, 1.0e-6_real64, &
                     'corrcoef supports real32')
    call check_close(real(corrcoef(real(values, real128), &
                                   real(values, real128)), real64), &
                     1.0_real64, 1.0e-14_real64, 1.0e-14_real64, &
                     'corrcoef supports real128')
  end subroutine test_supported_precisions

end program test_math
