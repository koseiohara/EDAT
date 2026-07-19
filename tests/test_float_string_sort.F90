program test_float_string_sort
  use, intrinsic :: iso_fortran_env, only: real32, real64
  use EDAT_Float, only: isclose
  use EDAT_Sort, only: quick_sort
  use EDAT_String, only: to_lower, to_upper
  use test_support, only: check, finish_tests
  implicit none

  call test_isclose
  call test_case_conversion
  call test_sorting

  call finish_tests('test_float_string_sort')

contains

  subroutine test_isclose
    call check(isclose(1.0_real64, 1.0_real64 + 1.0e-14_real64), &
               'isclose accepts values within the default tolerance')
    call check(.not. isclose(1.0_real64, 1.1_real64), &
               'isclose rejects clearly different values')
    call check(isclose(0.0_real64, 1.0e-9_real64, abs_tol=1.0e-8_real64), &
               'isclose accepts an explicit absolute tolerance')
    call check(all(isclose([1.0_real64, 2.0_real64], &
                           [1.0_real64, 2.0_real64 + 1.0e-14_real64])), &
               'isclose applies elementally to arrays')
  end subroutine test_isclose


  subroutine test_case_conversion
    character(8) :: words(3)

    call check(to_upper('Ab c-09') == 'AB C-09', &
               'to_upper preserves spaces, digits, and punctuation')
    call check(to_lower('Ab C-09') == 'ab c-09', &
               'to_lower preserves spaces, digits, and punctuation')

    words = ['Alpha   ', 'BETA    ', 'gAmMa   ']
    call check(all(to_lower(words) == ['alpha   ', 'beta    ', 'gamma   ']), &
               'case conversion applies elementally to string arrays')
  end subroutine test_case_conversion


  subroutine test_sorting
    integer, parameter :: maximum_size = 65

    integer, allocatable :: integer_values(:)
    real(real32), allocatable :: real32_values(:)
    real(real64), allocatable :: real64_values(:)
    integer :: array_size
    integer :: index
    character(80) :: description

    do array_size = 0, maximum_size
      allocate(integer_values(array_size))
      allocate(real32_values(array_size))
      allocate(real64_values(array_size))

      do index = 1, array_size
        integer_values(index) = mod(37 * index + 11, 17) - 8
        real32_values(index) = &
          real(mod(19 * index + 3, 13) - 6, real32) / 3.0_real32
        real64_values(index) = &
          real(mod(23 * index + 5, 19) - 9, real64) / 7.0_real64
      end do

      call quick_sort(integer_values)
      call quick_sort(real32_values)
      call quick_sort(real64_values)

      if (array_size > 1) then
        write(description, '(A,I0)') 'integer sort, size=', array_size
        call check(all(integer_values(:array_size - 1) <= &
                       integer_values(2:)), description)

        write(description, '(A,I0)') 'real32 sort, size=', array_size
        call check(all(real32_values(:array_size - 1) <= &
                       real32_values(2:)), description)

        write(description, '(A,I0)') 'real64 sort, size=', array_size
        call check(all(real64_values(:array_size - 1) <= &
                       real64_values(2:)), description)
      end if

      deallocate(integer_values, real32_values, real64_values)
    end do
  end subroutine test_sorting

end program test_float_string_sort
