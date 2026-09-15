program test_negative_math
    use, intrinsic :: iso_fortran_env, only : real64
    use :: EDAT_Math, only : corrcoef, covariance

    implicit none

    real(real64) :: arr1(2,2)
    real(real64) :: arr2(2,3)
    real(real64) :: output
    character(32) :: mode

    arr1(:,:) = 1.0_real64
    arr2(:,:) = 1.0_real64
    call get_command_argument(1, mode)

    select case (trim(mode))
    case ('covariance_shape')
        output = covariance(arr1, arr2)
    case ('corrcoef_shape')
        output = corrcoef(arr1, arr2)
    case default
        ERROR STOP 'unknown negative math test mode'
    end select

    write (*, '(ES24.16)') output
    ERROR STOP 'shape mismatch was not rejected'
end program test_negative_math
