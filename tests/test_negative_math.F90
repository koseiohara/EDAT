program test_negative_math
    use, intrinsic :: iso_fortran_env, only : real64
    use :: EDAT_Math, only : corrcoef, covariance, mean, sum_hp, variance

    implicit none

    real(real64) :: arr1(2,2)
    real(real64) :: arr2(2,3)
    real(real64) :: arr_rank1(2)
    real(real64) :: arr_rank10(1,1,1,1,1,1,1,1,1,1)
    real(real64), allocatable :: output_rank1(:)
    real(real64), allocatable :: output_rank9(:,:,:,:,:,:,:,:,:)
    real(real64) :: output
    character(32) :: mode

    arr1(:,:) = 1.0_real64
    arr2(:,:) = 1.0_real64
    arr_rank1(:) = 1.0_real64
    arr_rank10(:,:,:,:,:,:,:,:,:,:) = 1.0_real64
    call get_command_argument(1, mode)

    select case (trim(mode))
    case ('covariance_shape')
        output = covariance(arr1, arr2)
    case ('corrcoef_shape')
        output = corrcoef(arr1, arr2)
    case ('sum_rank1_dim0')
        output = sum_hp(arr_rank1, dim=0)
    case ('sum_rank10_dim11')
        output_rank9 = sum_hp(arr_rank10, dim=11)
        output = sum(output_rank9)
    case ('mean_dim0')
        output_rank1 = mean(arr1, dim=0)
        output = sum(output_rank1)
    case ('mean_dim3')
        output_rank1 = mean(arr1, dim=3)
        output = sum(output_rank1)
    case ('variance_dim0')
        output_rank1 = variance(arr1, dim=0)
        output = sum(output_rank1)
    case ('variance_dim3')
        output_rank1 = variance(arr1, dim=3)
        output = sum(output_rank1)
    case ('covariance_dim0')
        output_rank1 = covariance(arr1, arr1, dim=0)
        output = sum(output_rank1)
    case ('covariance_dim3')
        output_rank1 = covariance(arr1, arr1, dim=3)
        output = sum(output_rank1)
    case ('corrcoef_dim0')
        output_rank1 = corrcoef(arr1, arr1, dim=0)
        output = sum(output_rank1)
    case ('corrcoef_dim3')
        output_rank1 = corrcoef(arr1, arr1, dim=3)
        output = sum(output_rank1)
    case default
        ERROR STOP 'unknown negative math test mode'
    end select

    write (*, '(ES24.16)') output
    ERROR STOP 'invalid math input was not rejected'
end program test_negative_math
