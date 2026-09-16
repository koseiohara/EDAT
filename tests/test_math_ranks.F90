#define CAT_INNER(a,b) a##b
#define CAT(a,b) CAT_INNER(a,b)
#ifndef EMIT_RANK
program test_math_ranks
    use, intrinsic :: iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    use :: EDAT_Math, only : corrcoef, covariance, mean, variance
    use :: test_support, only : check, check_array_close, check_close, finish_tests

    implicit none

    real(real64), parameter :: relative_tolerance = 5.0e-12_real64
    real(real64), parameter :: absolute_tolerance = 5.0e-12_real64

    call test_rank_2()
    call test_rank_3()
    call test_rank_4()
    call test_rank_5()
    call test_rank_6()
    call test_rank_7()
    call test_rank_8()
    call test_rank_9()
    call test_rank_10()
    call test_multidimensional_edges()

    call finish_tests('test_math_ranks')

contains

#define EMIT_RANK
#define RANK 2
#define DIMS 3,2
#define SECT :,:
#define RDECL :
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 3
#define DIMS 3,2,3
#define SECT :,:,:
#define RDECL :,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 4
#define DIMS 3,2,3,2
#define SECT :,:,:,:
#define RDECL :,:,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 5
#define DIMS 3,2,3,2,3
#define SECT :,:,:,:,:
#define RDECL :,:,:,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 6
#define DIMS 3,2,3,2,3,2
#define SECT :,:,:,:,:,:
#define RDECL :,:,:,:,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 7
#define DIMS 3,2,3,2,3,2,3
#define SECT :,:,:,:,:,:,:
#define RDECL :,:,:,:,:,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 8
#define DIMS 3,2,3,2,3,2,3,2
#define SECT :,:,:,:,:,:,:,:
#define RDECL :,:,:,:,:,:,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 9
#define DIMS 3,2,3,2,3,2,3,2,3
#define SECT :,:,:,:,:,:,:,:,:
#define RDECL :,:,:,:,:,:,:,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#define RANK 10
#define DIMS 3,2,3,2,3,2,3,2,3,2
#define SECT :,:,:,:,:,:,:,:,:,:
#define RDECL :,:,:,:,:,:,:,:,:
#include "test_math_ranks.F90"
#undef RANK
#undef DIMS
#undef SECT
#undef RDECL
#undef EMIT_RANK


    subroutine test_multidimensional_edges()
        real(real64) :: empty_x(0,2)
        real(real64) :: empty_y(0,2)
        real(real64) :: single_x(1,2)
        real(real64) :: single_y(1,2)
        real(real64), allocatable :: actual(:)
        real(real64) :: zeros(2)

        empty_x(:,:) = 0.0_real64
        empty_y(:,:) = 0.0_real64
        single_x(:,:) = reshape([1.25_real64, -0.75_real64], shape(single_x))
        single_y(:,:) = reshape([2.0_real64, 4.0_real64], shape(single_y))
        zeros(:) = 0.0_real64

        actual = mean(empty_x, dim=1)
        call check(all(shape(actual) == [2]), 'mean rank 2 empty shape')
        call check_array_close(actual, zeros, 0.0_real64, 0.0_real64, 'mean rank 2 n=0')
        actual = variance(empty_x, dim=1)
        call check(all(shape(actual) == [2]), 'variance rank 2 empty shape')
        call check_array_close(actual, zeros, 0.0_real64, 0.0_real64, 'variance rank 2 n=0')
        actual = covariance(empty_x, empty_y, dim=1)
        call check(all(shape(actual) == [2]), 'covariance rank 2 empty shape')
        call check_array_close(actual, zeros, 0.0_real64, 0.0_real64, 'covariance rank 2 n=0')
        actual = corrcoef(empty_x, empty_y, dim=1)
        call check(all(shape(actual) == [2]), 'corrcoef rank 2 empty shape')
        call check_array_close(actual, zeros, 0.0_real64, 0.0_real64, 'corrcoef rank 2 n=0')

        actual = mean(single_x, dim=1)
        call check(all(shape(actual) == [2]), 'mean rank 2 singleton shape')
        call check_array_close(actual, reshape(single_x, [2]), 0.0_real64, 0.0_real64, 'mean rank 2 n=1')
        actual = variance(single_x, dim=1, sample=.TRUE.)
        call check(all(ieee_is_nan(actual)), 'sample variance rank 2 n=1 is NaN')
        actual = covariance(single_x, single_y, dim=1, sample=.TRUE.)
        call check(all(ieee_is_nan(actual)), 'sample covariance rank 2 n=1 is NaN')
        actual = corrcoef(single_x, single_y, dim=1)
        call check(all(ieee_is_nan(actual)), 'corrcoef rank 2 n=1 is NaN')
    end subroutine test_multidimensional_edges


    subroutine compute_reference(x, y, dims, dim, sample, expected_mean, expected_variance, &
                                 expected_covariance, expected_corrcoef)
        real(real64), intent(in) :: x(:)
        real(real64), intent(in) :: y(:)
        integer, intent(in) :: dims(:)
        integer, intent(in) :: dim
        logical, intent(in) :: sample
        real(real64), allocatable, intent(out) :: expected_mean(:)
        real(real64), allocatable, intent(out) :: expected_variance(:)
        real(real64), allocatable, intent(out) :: expected_covariance(:)
        real(real64), allocatable, intent(out) :: expected_corrcoef(:)

        real(real64), allocatable :: samples_x(:,:)
        real(real64), allocatable :: samples_y(:,:)
        real(real64), allocatable :: centered_x(:,:)
        real(real64), allocatable :: centered_y(:,:)
        real(real64), allocatable :: mean_y(:)
        real(real64), allocatable :: variance_y(:)
        integer :: denominator
        integer :: input_index
        integer :: n
        integer :: output_index
        integer :: output_size
        integer :: sample_index
        integer :: stride

        if (dim == 0) then
            n = size(x)
            output_size = 1
            allocate(samples_x(n,1), samples_y(n,1))
            samples_x(:,1) = x(:)
            samples_y(:,1) = y(:)
        else
            n = dims(dim)
            output_size = size(x) / n
            stride = product(dims(1:dim-1))
            allocate(samples_x(n,output_size), samples_y(n,output_size))

            do input_index = 0, size(x) - 1
                sample_index = mod(input_index / stride, n) + 1
                output_index = mod(input_index, stride) + &
                               (input_index / (stride*n))*stride + 1
                samples_x(sample_index,output_index) = x(input_index + 1)
                samples_y(sample_index,output_index) = y(input_index + 1)
            enddo
        endif

        allocate(expected_mean(output_size), expected_variance(output_size))
        allocate(expected_covariance(output_size), expected_corrcoef(output_size))
        allocate(mean_y(output_size), variance_y(output_size))
        allocate(centered_x(n,output_size), centered_y(n,output_size))

        expected_mean(:) = sum(samples_x(:,:), dim=1) / real(n, real64)
        mean_y(:) = sum(samples_y(:,:), dim=1) / real(n, real64)
        centered_x(:,:) = samples_x(:,:) - spread(expected_mean(:), dim=1, ncopies=n)
        centered_y(:,:) = samples_y(:,:) - spread(mean_y(:), dim=1, ncopies=n)
        denominator = n
        if (sample) then
            denominator = n - 1
        endif
        expected_variance(:) = sum(centered_x(:,:)**2, dim=1) / real(denominator, real64)
        variance_y(:) = sum(centered_y(:,:)**2, dim=1) / real(denominator, real64)
        expected_covariance(:) = sum(centered_x(:,:)*centered_y(:,:), dim=1) / real(denominator, real64)
        expected_corrcoef(:) = expected_covariance(:) / sqrt(expected_variance(:)*variance_y(:))
    end subroutine compute_reference


    subroutine verify_full(actual_mean, actual_variance, actual_covariance, actual_corrcoef, &
                           expected_mean, expected_variance, expected_covariance, expected_corrcoef, rank)
        real(real64), intent(in) :: actual_mean
        real(real64), intent(in) :: actual_variance
        real(real64), intent(in) :: actual_covariance
        real(real64), intent(in) :: actual_corrcoef
        real(real64), intent(in) :: expected_mean
        real(real64), intent(in) :: expected_variance
        real(real64), intent(in) :: expected_covariance
        real(real64), intent(in) :: expected_corrcoef
        integer, intent(in) :: rank

        character(80) :: description

        write(description, '(A,I0,A)') 'mean rank ', rank, ' full'
        call check_close(actual_mean, expected_mean, relative_tolerance, absolute_tolerance, description)
        write(description, '(A,I0,A)') 'variance rank ', rank, ' full'
        call check_close(actual_variance, expected_variance, relative_tolerance, absolute_tolerance, description)
        write(description, '(A,I0,A)') 'covariance rank ', rank, ' full'
        call check_close(actual_covariance, expected_covariance, relative_tolerance, absolute_tolerance, description)
        write(description, '(A,I0,A)') 'corrcoef rank ', rank, ' full'
        call check_close(actual_corrcoef, expected_corrcoef, relative_tolerance, absolute_tolerance, description)
    end subroutine verify_full


    subroutine verify_array(actual, expected, actual_shape, full_shape, dim, statistic, rank)
        real(real64), intent(in) :: actual(:)
        real(real64), intent(in) :: expected(:)
        integer, intent(in) :: actual_shape(:)
        integer, intent(in) :: full_shape(:)
        integer, intent(in) :: dim
        character(*), intent(in) :: statistic
        integer, intent(in) :: rank

        integer, allocatable :: expected_shape(:)
        character(80) :: description
        integer :: i

        expected_shape = pack(full_shape, [(i /= dim, i=1,size(full_shape))])
        write(description, '(A,A,I0,A,I0)') trim(statistic), ' rank ', rank, ' dim=', dim
        call check(all(actual_shape == expected_shape), trim(description) // ' shape')
        call check_array_close(actual, expected, relative_tolerance, absolute_tolerance, description)
    end subroutine verify_array

end program test_math_ranks
#else


    subroutine CAT(test_rank_,RANK)()
        integer, parameter :: dims(RANK) = [DIMS]

        real(real64) :: x(DIMS)
        real(real64) :: y(DIMS)
        real(real64), allocatable :: actual_mean(RDECL)
        real(real64), allocatable :: actual_variance(RDECL)
        real(real64), allocatable :: actual_covariance(RDECL)
        real(real64), allocatable :: actual_corrcoef(RDECL)
        real(real64), allocatable :: expected_mean(:)
        real(real64), allocatable :: expected_variance(:)
        real(real64), allocatable :: expected_covariance(:)
        real(real64), allocatable :: expected_corrcoef(:)
        integer :: dim
        integer :: i

        x(SECT) = reshape([(sin(0.37_real64*real(i, real64)) + 0.017_real64*real(i, real64) + &
                          0.11_real64*real(mod(i, 7), real64), i=1,size(x))], shape(x))
        y(SECT) = 0.4_real64*x(SECT)**2 - 0.8_real64*x(SECT) + &
                  reshape([(cos(0.23_real64*real(i, real64)) + 0.003_real64*real(i, real64), &
                           i=1,size(y))], shape(y))

        call compute_reference(reshape(x, [size(x)]), reshape(y, [size(y)]), dims, 0, .FALSE., &
                               expected_mean, expected_variance, expected_covariance, expected_corrcoef)
        call verify_full(mean(x), variance(x), covariance(x, y), corrcoef(x, y), expected_mean(1), &
                         expected_variance(1), expected_covariance(1), expected_corrcoef(1), RANK)

        do dim = 1, RANK
            call compute_reference(reshape(x, [size(x)]), reshape(y, [size(y)]), dims, dim, .FALSE., &
                                   expected_mean, expected_variance, expected_covariance, expected_corrcoef)
            actual_mean = mean(x(SECT), dim=dim)
            actual_variance = variance(x(SECT), dim=dim)
            actual_covariance = covariance(x(SECT), y(SECT), dim=dim)
            actual_corrcoef = corrcoef(x(SECT), y(SECT), dim=dim)

            call verify_array(reshape(actual_mean, [size(actual_mean)]), expected_mean, shape(actual_mean), &
                              dims, dim, 'mean', RANK)
            call verify_array(reshape(actual_variance, [size(actual_variance)]), expected_variance, &
                              shape(actual_variance), dims, dim, 'variance', RANK)
            call verify_array(reshape(actual_covariance, [size(actual_covariance)]), expected_covariance, &
                              shape(actual_covariance), dims, dim, 'covariance', RANK)
            call verify_array(reshape(actual_corrcoef, [size(actual_corrcoef)]), expected_corrcoef, &
                              shape(actual_corrcoef), dims, dim, 'corrcoef', RANK)
        enddo

#if RANK == 4
        dim = 2
        call compute_reference(reshape(x, [size(x)]), reshape(y, [size(y)]), dims, dim, .TRUE., &
                               expected_mean, expected_variance, expected_covariance, expected_corrcoef)
        actual_variance = variance(x(SECT), dim=dim, sample=.TRUE.)
        actual_covariance = covariance(x(SECT), y(SECT), dim=dim, sample=.TRUE.)
        call verify_array(reshape(actual_variance, [size(actual_variance)]), expected_variance, &
                          shape(actual_variance), dims, dim, 'sample variance', RANK)
        call verify_array(reshape(actual_covariance, [size(actual_covariance)]), expected_covariance, &
                          shape(actual_covariance), dims, dim, 'sample covariance', RANK)

        call compute_reference(reshape(x, [size(x)]), reshape(y, [size(y)]), dims, 0, .TRUE., &
                               expected_mean, expected_variance, expected_covariance, expected_corrcoef)
        call check_close(variance(x, sample=.TRUE.), expected_variance(1), relative_tolerance, &
                         absolute_tolerance, 'sample variance rank 4 full')
        call check_close(covariance(x, y, sample=.TRUE.), expected_covariance(1), relative_tolerance, &
                         absolute_tolerance, 'sample covariance rank 4 full')
#endif
    end subroutine CAT(test_rank_,RANK)
#endif
