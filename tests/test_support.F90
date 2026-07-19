module test_support
  use, intrinsic :: iso_fortran_env, only: error_unit, real64
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  implicit none
  private

  integer :: failure_count = 0

  public :: check
  public :: check_close
  public :: check_array_close
  public :: check_nan
  public :: finish_tests

contains

  subroutine check(condition, description)
    logical, intent(in) :: condition
    character(*), intent(in) :: description

    if (.not. condition) then
      failure_count = failure_count + 1
      write(error_unit, '(A)') 'FAIL: ' // trim(description)
    end if
  end subroutine check


  subroutine check_close(actual, expected, relative_tolerance, absolute_tolerance, description)
    real(real64), intent(in) :: actual
    real(real64), intent(in) :: expected
    real(real64), intent(in) :: relative_tolerance
    real(real64), intent(in) :: absolute_tolerance
    character(*), intent(in) :: description

    real(real64) :: difference
    real(real64) :: tolerance

    difference = abs(actual - expected)
    tolerance = max(absolute_tolerance, &
                    relative_tolerance * max(abs(actual), abs(expected)))

    call check(difference <= tolerance, description)

    if (difference > tolerance) then
      write(error_unit, '(A,1X,ES24.16)') '  actual:    ', actual
      write(error_unit, '(A,1X,ES24.16)') '  expected:  ', expected
      write(error_unit, '(A,1X,ES24.16)') '  difference:', difference
      write(error_unit, '(A,1X,ES24.16)') '  tolerance: ', tolerance
    end if
  end subroutine check_close


  subroutine check_array_close(actual, expected, relative_tolerance, absolute_tolerance, description)
    real(real64), intent(in) :: actual(:)
    real(real64), intent(in) :: expected(:)
    real(real64), intent(in) :: relative_tolerance
    real(real64), intent(in) :: absolute_tolerance
    character(*), intent(in) :: description

    integer :: index
    real(real64) :: difference
    real(real64) :: tolerance

    if (size(actual) /= size(expected)) then
      call check(.false., description)
      write(error_unit, '(A,I0)') '  actual size:   ', size(actual)
      write(error_unit, '(A,I0)') '  expected size: ', size(expected)
      return
    end if

    do index = 1, size(actual)
      difference = abs(actual(index) - expected(index))
      tolerance = max(absolute_tolerance, &
                      relative_tolerance * max(abs(actual(index)), abs(expected(index))))

      if (difference > tolerance) then
        call check(.false., description)
        write(error_unit, '(A,I0)') '  first mismatching element: ', index
        write(error_unit, '(A,1X,ES24.16)') '  actual:    ', actual(index)
        write(error_unit, '(A,1X,ES24.16)') '  expected:  ', expected(index)
        write(error_unit, '(A,1X,ES24.16)') '  difference:', difference
        write(error_unit, '(A,1X,ES24.16)') '  tolerance: ', tolerance
        return
      end if
    end do

    call check(.true., description)
  end subroutine check_array_close


  subroutine check_nan(value, description)
    real(real64), intent(in) :: value
    character(*), intent(in) :: description

    call check(ieee_is_nan(value), description)
  end subroutine check_nan


  subroutine finish_tests(suite_name)
    character(*), intent(in) :: suite_name

    if (failure_count > 0) then
      write(error_unit, '(A,I0)') trim(suite_name) // ' failures: ', failure_count
      error stop 1
    end if

    write(*, '(A)') trim(suite_name) // ': PASS'
  end subroutine finish_tests

end module test_support
