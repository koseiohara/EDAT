program test_pairwise_sum
  use, intrinsic :: iso_fortran_env, only: real32, real64, real128
  use EDAT_Math, only: sum_hp
  use test_support, only: check, finish_tests
  implicit none

  call test_rank1_sizes
  call test_rank2
  call test_rank3
  call test_rank4
  call test_rank5
  call test_rank6
  call test_rank7
  call test_rank8
  call test_rank9
  call test_rank10
  call test_core_geometries
  call test_dimensional_cancellation
  call test_empty_reduced_axes
  call test_empty_nonreduced_axes
  call test_rank10_other_kinds

  call finish_tests('test_pairwise_sum')

contains

  subroutine test_rank1_sizes
    real(real64), allocatable :: values(:)
    integer :: n
    character(80) :: description

    do n = 0, 150
      allocate(values(n))
      values(:) = make_real64_data(n)

      write(description, '(A,I0)') 'rank 1 full reduction, size=', n
      call check(sum_hp(values) == sum(values), description)

      write(description, '(A,I0)') 'rank 1 dim=1 reduction, size=', n
      call check(sum_hp(values, dim=1) == sum(values, dim=1), description)

      deallocate(values)
    end do
  end subroutine test_rank1_sizes


  subroutine test_rank2
    real(real64) :: values(3,4)
    real(real64), allocatable :: actual(:)
    real(real64), allocatable :: expected(:)
    integer :: dim
    character(80) :: description

    values(:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 2 full reduction')

    do dim = 1, 2
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 2 reduction, dim=', dim
      call check_real64_result(actual, expected, shape(actual), shape(expected), description)
    end do
  end subroutine test_rank2


  subroutine test_rank3
    real(real64) :: values(2,3,4)
    real(real64), allocatable :: actual(:,:)
    real(real64), allocatable :: expected(:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 3 full reduction')

    do dim = 1, 3
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 3 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank3


  subroutine test_rank4
    real(real64) :: values(2,3,2,4)
    real(real64), allocatable :: actual(:,:,:)
    real(real64), allocatable :: expected(:,:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 4 full reduction')

    do dim = 1, 4
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 4 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank4


  subroutine test_rank5
    real(real64) :: values(2,3,2,3,4)
    real(real64), allocatable :: actual(:,:,:,:)
    real(real64), allocatable :: expected(:,:,:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 5 full reduction')

    do dim = 1, 5
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 5 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank5


  subroutine test_rank6
    real(real64) :: values(2,3,2,3,2,4)
    real(real64), allocatable :: actual(:,:,:,:,:)
    real(real64), allocatable :: expected(:,:,:,:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:,:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 6 full reduction')

    do dim = 1, 6
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 6 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank6


  subroutine test_rank7
    real(real64) :: values(2,3,2,3,2,3,4)
    real(real64), allocatable :: actual(:,:,:,:,:,:)
    real(real64), allocatable :: expected(:,:,:,:,:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:,:,:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 7 full reduction')

    do dim = 1, 7
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 7 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank7


  subroutine test_rank8
    real(real64) :: values(2,3,2,3,2,3,2,4)
    real(real64), allocatable :: actual(:,:,:,:,:,:,:)
    real(real64), allocatable :: expected(:,:,:,:,:,:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:,:,:,:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 8 full reduction')

    do dim = 1, 8
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 8 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank8


  subroutine test_rank9
    real(real64) :: values(2,3,2,3,2,3,2,3,4)
    real(real64), allocatable :: actual(:,:,:,:,:,:,:,:)
    real(real64), allocatable :: expected(:,:,:,:,:,:,:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:,:,:,:,:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 9 full reduction')

    do dim = 1, 9
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 9 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank9


  subroutine test_rank10
    real(real64) :: values(2,3,2,3,2,3,2,3,2,4)
    real(real64), allocatable :: actual(:,:,:,:,:,:,:,:,:)
    real(real64), allocatable :: expected(:,:,:,:,:,:,:,:,:)
    integer :: dim
    character(80) :: description

    values(:,:,:,:,:,:,:,:,:,:) = reshape(make_real64_data(size(values)), shape(values))
    call check(sum_hp(values) == sum(values), 'rank 10 full reduction')

    do dim = 1, 10
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'rank 10 reduction, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
    end do
  end subroutine test_rank10


  subroutine test_core_geometries
    integer, parameter :: case_count = 6
    integer, parameter :: ns(case_count) = [1, 9, 5, 7, 12, 17]
    integer, parameter :: strides(case_count) = [2, 2, 3, 1, 4, 6]
    integer, parameter :: howmany_values(case_count) = [3, 3, 2, 6, 1, 5]

    real(real64), allocatable :: values(:,:,:)
    real(real64), allocatable :: actual(:,:)
    real(real64), allocatable :: expected(:,:)
    integer :: case_index
    integer :: position
    character(100) :: description

    do case_index = 1, case_count
      allocate(values(strides(case_index),ns(case_index),howmany_values(case_index)))
      values(:,:,:) = reshape([(real(position, real64), position=1,size(values))], shape(values))
      actual = sum_hp(values, dim=2)
      expected = sum(values, dim=2)
      write(description, '(A,I0,A,I0,A,I0,A)') 'core geometry (n=', ns(case_index), &
                                               ', stride=', strides(case_index), &
                                               ', howmany=', howmany_values(case_index), ')'
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
      deallocate(values, actual, expected)
    end do
  end subroutine test_core_geometries


  subroutine test_dimensional_cancellation
    integer, parameter :: case_count = 4
    integer, parameter :: dimensions(case_count) = [1, 2, 2, 3]
    integer, parameter :: geometries(3,case_count) = reshape([5, 2, 3, &
                                                             2, 5, 3, &
                                                             3, 5, 2, &
                                                             2, 3, 5], [3, case_count])
    real(real64), parameter :: cancellation(5) = [1.0e16_real64, -1.0e16_real64, 1.0_real64, &
                                                  2.0_real64, 3.0_real64]

    real(real64), allocatable :: values(:,:,:)
    real(real64), allocatable :: actual(:,:)
    real(real64), allocatable :: expected(:,:)
    integer :: case_index
    integer :: dim
    integer :: position
    character(100) :: description

    do case_index = 1, case_count
      dim = dimensions(case_index)
      allocate(values(geometries(1,case_index),geometries(2,case_index),geometries(3,case_index)))

      do position = 1, 5
        select case (dim)
        case (1)
          values(position,:,:) = cancellation(position)
        case (2)
          values(:,position,:) = cancellation(position)
        case (3)
          values(:,:,position) = cancellation(position)
        end select
      end do

      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0,A,3(I0,1X))') 'cancellation dim=', dim, ', shape=', shape(values)
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
      call check(all(actual == 6.0_real64), trim(description) // ' residual')
      deallocate(values, actual, expected)
    end do
  end subroutine test_dimensional_cancellation


  subroutine test_empty_reduced_axes
    integer, parameter :: case_count = 3
    integer, parameter :: dimensions(case_count) = [1, 2, 3]
    integer, parameter :: geometries(3,case_count) = reshape([0, 2, 3, &
                                                             2, 0, 3, &
                                                             2, 3, 0], [3, case_count])

    real(real64), allocatable :: values(:,:,:)
    real(real64), allocatable :: actual(:,:)
    real(real64), allocatable :: expected(:,:)
    integer :: case_index
    integer :: dim
    character(80) :: description

    do case_index = 1, case_count
      dim = dimensions(case_index)
      allocate(values(geometries(1,case_index),geometries(2,case_index),geometries(3,case_index)))
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0)') 'empty reduced axis, dim=', dim
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
      call check(all(actual == 0.0_real64), trim(description) // ' identity')
      deallocate(values, actual, expected)
    end do
  end subroutine test_empty_reduced_axes


  subroutine test_empty_nonreduced_axes
    integer, parameter :: case_count = 2
    integer, parameter :: dimensions(case_count) = [2, 2]
    integer, parameter :: geometries(3,case_count) = reshape([0, 3, 2, &
                                                             2, 3, 0], [3, case_count])

    real(real64), allocatable :: values(:,:,:)
    real(real64), allocatable :: actual(:,:)
    real(real64), allocatable :: expected(:,:)
    integer :: case_index
    integer :: dim
    character(100) :: description

    do case_index = 1, case_count
      dim = dimensions(case_index)
      allocate(values(geometries(1,case_index),geometries(2,case_index),geometries(3,case_index)))
      actual = sum_hp(values, dim=dim)
      expected = sum(values, dim=dim)
      write(description, '(A,I0,A,3(I0,1X))') 'empty non-reduced axis, dim=', dim, ', shape=', shape(values)
      call check(size(actual) == 0, trim(description) // ' zero size')
      call check_real64_result(reshape(actual, [size(actual)]), reshape(expected, [size(expected)]), &
                               shape(actual), shape(expected), description)
      deallocate(values, actual, expected)
    end do
  end subroutine test_empty_nonreduced_axes


  subroutine test_rank10_other_kinds
    real(real32) :: values32(2,1,2,1,2,1,2,1,2,3)
    real(real32), allocatable :: actual32(:,:,:,:,:,:,:,:,:)
    real(real32), allocatable :: expected32(:,:,:,:,:,:,:,:,:)
    real(real128) :: values128(2,1,2,1,2,1,2,1,2,3)
    real(real128), allocatable :: actual128(:,:,:,:,:,:,:,:,:)
    real(real128), allocatable :: expected128(:,:,:,:,:,:,:,:,:)

    values32(:,:,:,:,:,:,:,:,:,:) = reshape(real(make_real64_data(size(values32)), real32), shape(values32))
    call check(sum_hp(values32) == sum(values32), 'real32 rank 10 full reduction')
    actual32 = sum_hp(values32, dim=5)
    expected32 = sum(values32, dim=5)
    call check_real32_result(reshape(actual32, [size(actual32)]), reshape(expected32, [size(expected32)]), &
                             shape(actual32), shape(expected32), 'real32 rank 10 dim=5 reduction')

    values128(:,:,:,:,:,:,:,:,:,:) = reshape(real(make_real64_data(size(values128)), real128), shape(values128))
    call check(sum_hp(values128) == sum(values128), 'real128 rank 10 full reduction')
    actual128 = sum_hp(values128, dim=7)
    expected128 = sum(values128, dim=7)
    call check_real128_result(reshape(actual128, [size(actual128)]), reshape(expected128, [size(expected128)]), &
                              shape(actual128), shape(expected128), 'real128 rank 10 dim=7 reduction')
  end subroutine test_rank10_other_kinds


  subroutine check_real32_result(actual, expected, actual_shape, expected_shape, description)
    real(real32), intent(in) :: actual(:)
    real(real32), intent(in) :: expected(:)
    integer, intent(in) :: actual_shape(:)
    integer, intent(in) :: expected_shape(:)
    character(*), intent(in) :: description

    call check(all(actual_shape == expected_shape), trim(description) // ' shape')
    call check(all(actual == expected), trim(description) // ' flattened values')
  end subroutine check_real32_result


  subroutine check_real64_result(actual, expected, actual_shape, expected_shape, description)
    real(real64), intent(in) :: actual(:)
    real(real64), intent(in) :: expected(:)
    integer, intent(in) :: actual_shape(:)
    integer, intent(in) :: expected_shape(:)
    character(*), intent(in) :: description

    call check(all(actual_shape == expected_shape), trim(description) // ' shape')
    call check(all(actual == expected), trim(description) // ' flattened values')
  end subroutine check_real64_result


  subroutine check_real128_result(actual, expected, actual_shape, expected_shape, description)
    real(real128), intent(in) :: actual(:)
    real(real128), intent(in) :: expected(:)
    integer, intent(in) :: actual_shape(:)
    integer, intent(in) :: expected_shape(:)
    character(*), intent(in) :: description

    call check(all(actual_shape == expected_shape), trim(description) // ' shape')
    call check(all(actual == expected), trim(description) // ' flattened values')
  end subroutine check_real128_result


  pure function make_real64_data(n) result(values)
    integer, intent(in) :: n

    real(real64), allocatable :: values(:)
    integer :: i

    allocate(values(n))
    do i = 1, n
      values(i) = real(modulo(7*i, 17) - 8, real64)
    end do
  end function make_real64_data

end program test_pairwise_sum
