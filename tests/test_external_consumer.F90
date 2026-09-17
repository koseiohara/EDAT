program test_external_consumer
    use, intrinsic :: iso_fortran_env, only : real64
    use :: EDAT_Math, only : mean

    implicit none

    if (mean([1.0_real64, 3.0_real64]) /= 2.0_real64) then
        ERROR STOP 'test_external_consumer: mean result mismatch'
    endif
end program test_external_consumer
