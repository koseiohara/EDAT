

#ifdef DEBUG
function CORRCOEF_1(arr1, arr2, dim) result(output)
#else
pure function CORRCOEF_1(arr1, arr2, dim) result(output)
#endif
    real(RK), intent(in) :: arr1(:)
    real(RK), intent(in) :: arr2(:)
    integer , intent(in), optional :: dim

    real(RK)    :: output
    real(RK)    :: covar
    real(RK)    :: variance1
    real(RK)    :: variance2
    integer(ik) :: n

    if (size(arr2, kind=ik) /= size(arr1, kind=ik)) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    if (present(dim)) then
        if (dim > 1 .OR. dim <= 0) then
            ERROR STOP 'corrcoef: dim must be between 1 and 1'
        endif
    endif

    n = size(arr1, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (n == 1_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    covar     = covariance(arr1(:), arr2(:))
    variance1 = variance(arr1(:))
    variance2 = variance(arr2(:))
    output    = covar / sqrt(variance1*variance2)

end function CORRCOEF_1


#ifdef DEBUG
function CORRCOEF_DIM_2(arr1, arr2, dim) result(output)
#else
pure function CORRCOEF_DIM_2(arr1, arr2, dim) result(output)
#endif
    integer, parameter :: ndim = 2

    real(RK), intent(in) :: arr1(:,:)
    real(RK), intent(in) :: arr2(:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'corrcoef: dim must be between 1 and 2'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)
    n = ishape(dim)

    allocate(output(oshape(1)))

    if (n == 0_ik) then
        output(:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (n == 1_ik) then
        output(:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    output(:) = covariance(arr1(:,:), arr2(:,:), dim) / sqrt(variance(arr1(:,:), dim) * variance(arr2(:,:), dim))

end function CORRCOEF_DIM_2


#ifdef DEBUG
function CORRCOEF_FULL_2(arr1, arr2) result(output)
#else
pure function CORRCOEF_FULL_2(arr1, arr2) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:)
    real(RK), intent(in) :: arr2(:,:)

    real(RK)    :: output
    real(RK)    :: covar
    real(RK)    :: variance1
    real(RK)    :: variance2
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    n = size(arr1, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (n == 1_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    covar     = covariance(arr1, arr2(:,:))
    variance1 = variance(arr1(:,:))
    variance2 = variance(arr2(:,:))
    output    = covar / sqrt(variance1*variance2)

end function CORRCOEF_FULL_2


#ifdef DEBUG
function CORRCOEF_DIM_3(arr1, arr2, dim) result(output)
#else
pure function CORRCOEF_DIM_3(arr1, arr2, dim) result(output)
#endif
    integer, parameter :: ndim = 3

    real(RK), intent(in) :: arr1(:,:,:)
    real(RK), intent(in) :: arr2(:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'corrcoef: dim must be between 1 and 3'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)
    n = ishape(dim)

    allocate(output(oshape(1),oshape(2)))

    if (n == 0_ik) then
        output(:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (n == 1_ik) then
        output(:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    output(:,:) = covariance(arr1(:,:,:), arr2(:,:,:), dim) / sqrt(variance(arr1(:,:,:), dim) * variance(arr2(:,:,:), dim))

end function CORRCOEF_DIM_3


#ifdef DEBUG
function CORRCOEF_FULL_3(arr1, arr2) result(output)
#else
pure function CORRCOEF_FULL_3(arr1, arr2) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:)
    real(RK), intent(in) :: arr2(:,:,:)

    real(RK)    :: output
    real(RK)    :: covar
    real(RK)    :: variance1
    real(RK)    :: variance2
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    n = size(arr1, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (n == 1_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    covar     = covariance(arr1(:,:,:), arr2(:,:,:))
    variance1 = variance(arr1(:,:,:))
    variance2 = variance(arr2(:,:,:))
    output    = covar / sqrt(variance1*variance2)

end function CORRCOEF_FULL_3


#ifdef DEBUG
function CORRCOEF_DIM_4(arr1, arr2, dim) result(output)
#else
pure function CORRCOEF_DIM_4(arr1, arr2, dim) result(output)
#endif
    integer, parameter :: ndim = 4

    real(RK), intent(in) :: arr1(:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'corrcoef: dim must be between 1 and 4'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)
    n = ishape(dim)

    allocate(output(oshape(1),oshape(2),oshape(3)))

    if (n == 0_ik) then
        output(:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (n == 1_ik) then
        output(:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    output(:,:,:) = covariance(arr1(:,:,:,:), arr2(:,:,:,:), dim) / &
                  & sqrt(variance(arr1(:,:,:,:), dim) * variance(arr2(:,:,:,:), dim))

end function CORRCOEF_DIM_4


#ifdef DEBUG
function CORRCOEF_FULL_4(arr1, arr2) result(output)
#else
pure function CORRCOEF_FULL_4(arr1, arr2) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:)

    real(RK)    :: output
    real(RK)    :: covar
    real(RK)    :: variance1
    real(RK)    :: variance2
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    n = size(arr1, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (n == 1_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    covar     = covariance(arr1(:,:,:,:), arr2(:,:,:,:))
    variance1 = variance(arr1(:,:,:,:))
    variance2 = variance(arr2(:,:,:,:))
    output    = covar / sqrt(variance1*variance2)

end function CORRCOEF_FULL_4


#ifdef DEBUG
function CORRCOEF_DIM_5(arr1, arr2, dim) result(output)
#else
pure function CORRCOEF_DIM_5(arr1, arr2, dim) result(output)
#endif
    integer, parameter :: ndim = 5

    real(RK), intent(in) :: arr1(:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'corrcoef: dim must be between 1 and 5'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)
    n = ishape(dim)

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4)))

    if (n == 0_ik) then
        output(:,:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (n == 1_ik) then
        output(:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    output(:,:,:,:) = covariance(arr1(:,:,:,:,:), arr2(:,:,:,:,:), dim) &
                    & / sqrt(variance(arr1(:,:,:,:,:), dim) * variance(arr2(:,:,:,:,:), dim))

end function CORRCOEF_DIM_5


#ifdef DEBUG
function CORRCOEF_FULL_5(arr1, arr2) result(output)
#else
pure function CORRCOEF_FULL_5(arr1, arr2) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:)

    real(RK)    :: output
    real(RK)    :: covar
    real(RK)    :: variance1
    real(RK)    :: variance2
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    n = size(arr1, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (n == 1_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    covar     = covariance(arr1(:,:,:,:,:), arr2(:,:,:,:,:))
    variance1 = variance(arr1(:,:,:,:,:))
    variance2 = variance(arr2(:,:,:,:,:))
    output    = covar / sqrt(variance1*variance2)

end function CORRCOEF_FULL_5


#ifdef DEBUG
function CORRCOEF_DIM_6(arr1, arr2, dim) result(output)
#else
pure function CORRCOEF_DIM_6(arr1, arr2, dim) result(output)
#endif
    integer, parameter :: ndim = 6

    real(RK), intent(in) :: arr1(:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'corrcoef: dim must be between 1 and 6'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)
    n = ishape(dim)

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5)))

    if (n == 0_ik) then
        output(:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (n == 1_ik) then
        output(:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    output(:,:,:,:,:) = covariance(arr1(:,:,:,:,:,:), arr2(:,:,:,:,:,:), dim) &
                      & / sqrt(variance(arr1(:,:,:,:,:,:), dim) * variance(arr2(:,:,:,:,:,:), dim))

end function CORRCOEF_DIM_6


#ifdef DEBUG
function CORRCOEF_FULL_6(arr1, arr2) result(output)
#else
pure function CORRCOEF_FULL_6(arr1, arr2) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:)

    real(RK)    :: output
    real(RK)    :: covar
    real(RK)    :: variance1
    real(RK)    :: variance2
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    n = size(arr1, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (n == 1_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    covar     = covariance(arr1(:,:,:,:,:,:), arr2(:,:,:,:,:,:))
    variance1 = variance(arr1(:,:,:,:,:,:))
    variance2 = variance(arr2(:,:,:,:,:,:))
    output    = covar / sqrt(variance1*variance2)

end function CORRCOEF_FULL_6


#ifdef DEBUG
function CORRCOEF_DIM_7(arr1, arr2, dim) result(output)
#else
pure function CORRCOEF_DIM_7(arr1, arr2, dim) result(output)
#endif
    integer, parameter :: ndim = 7

    real(RK), intent(in) :: arr1(:,:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'corrcoef: dim must be between 1 and 7'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)
    n = ishape(dim)

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (n == 1_ik) then
        output(:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    output(:,:,:,:,:,:) = covariance(arr1(:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:), dim) &
                        & / sqrt(variance(arr1(:,:,:,:,:,:,:), dim) * variance(arr2(:,:,:,:,:,:,:), dim))

end function CORRCOEF_DIM_7


#ifdef DEBUG
function CORRCOEF_FULL_7(arr1, arr2) result(output)
#else
pure function CORRCOEF_FULL_7(arr1, arr2) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:,:)

    real(RK)    :: output
    real(RK)    :: covar
    real(RK)    :: variance1
    real(RK)    :: variance2
    integer(ik) :: n

    if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
        ERROR STOP 'corrcoef: array shapes must match'
    endif

    n = size(arr1, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (n == 1_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    covar     = covariance(arr1(:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:))
    variance1 = variance(arr1(:,:,:,:,:,:,:))
    variance2 = variance(arr2(:,:,:,:,:,:,:))
    output    = covar / sqrt(variance1*variance2)

end function CORRCOEF_FULL_7


! #ifdef DEBUG
! function CORRCOEF_DIM_8(arr1, arr2, dim) result(output)
! #else
! pure function CORRCOEF_DIM_8(arr1, arr2, dim) result(output)
! #endif
!     integer, parameter :: ndim = 8

!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim

!     real(RK), allocatable :: output(:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n

!     if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
!         ERROR STOP 'corrcoef: array shapes must match'
!     endif

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'corrcoef: dim must be between 1 and 8'
!     endif

!     ishape(1:ndim)     = shape(arr1, kind=ik)
!     oshape(1:dim-1)    = ishape(1:dim-1)
!     oshape(dim:ndim-1) = ishape(dim+1:ndim)
!     n = ishape(dim)

!     allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))

!     if (n == 0_ik) then
!         output(:,:,:,:,:,:,:) = real(0, kind=RK)
!         return
!     endif

!     if (size(output, kind=ik) == 0_ik) then
!         return
!     endif

!     if (n == 1_ik) then
!         output(:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     output(:,:,:,:,:,:,:) = covariance(arr1(:,:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:,:), dim) &
!                           & / sqrt(variance(arr1(:,:,:,:,:,:,:,:), dim) * variance(arr2(:,:,:,:,:,:,:,:), dim))

! end function CORRCOEF_DIM_8


! #ifdef DEBUG
! function CORRCOEF_FULL_8(arr1, arr2) result(output)
! #else
! pure function CORRCOEF_FULL_8(arr1, arr2) result(output)
! #endif
!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:)

!     real(RK)    :: output
!     real(RK)    :: covar
!     real(RK)    :: variance1
!     real(RK)    :: variance2
!     integer(ik) :: n

!     if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
!         ERROR STOP 'corrcoef: array shapes must match'
!     endif

!     n = size(arr1, kind=ik)

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     if (n == 1_ik) then
!         output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     covar     = covariance(arr1(:,:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:,:))
!     variance1 = variance(arr1(:,:,:,:,:,:,:,:))
!     variance2 = variance(arr2(:,:,:,:,:,:,:,:))
!     output    = covar / sqrt(variance1*variance2)

! end function CORRCOEF_FULL_8


! #ifdef DEBUG
! function CORRCOEF_DIM_9(arr1, arr2, dim) result(output)
! #else
! pure function CORRCOEF_DIM_9(arr1, arr2, dim) result(output)
! #endif
!     integer, parameter :: ndim = 9

!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim

!     real(RK), allocatable :: output(:,:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n

!     if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
!         ERROR STOP 'corrcoef: array shapes must match'
!     endif

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'corrcoef: dim must be between 1 and 9'
!     endif

!     ishape(1:ndim)     = shape(arr1, kind=ik)
!     oshape(1:dim-1)    = ishape(1:dim-1)
!     oshape(dim:ndim-1) = ishape(dim+1:ndim)
!     n = ishape(dim)

!     allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8)))

!     if (n == 0_ik) then
!         output(:,:,:,:,:,:,:,:) = real(0, kind=RK)
!         return
!     endif

!     if (size(output, kind=ik) == 0_ik) then
!         return
!     endif

!     if (n == 1_ik) then
!         output(:,:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     output(:,:,:,:,:,:,:,:) = covariance(arr1(:,:,:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:,:,:), dim) &
!                             & / sqrt(variance(arr1(:,:,:,:,:,:,:,:,:), dim) * variance(arr2(:,:,:,:,:,:,:,:,:), dim))

! end function CORRCOEF_DIM_9


! #ifdef DEBUG
! function CORRCOEF_FULL_9(arr1, arr2) result(output)
! #else
! pure function CORRCOEF_FULL_9(arr1, arr2) result(output)
! #endif
!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:)

!     real(RK)    :: output
!     real(RK)    :: covar
!     real(RK)    :: variance1
!     real(RK)    :: variance2
!     integer(ik) :: n

!     if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
!         ERROR STOP 'corrcoef: array shapes must match'
!     endif

!     n = size(arr1, kind=ik)

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     if (n == 1_ik) then
!         output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     covar     = covariance(arr1(:,:,:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:,:,:))
!     variance1 = variance(arr1(:,:,:,:,:,:,:,:,:))
!     variance2 = variance(arr2(:,:,:,:,:,:,:,:,:))
!     output    = covar / sqrt(variance1*variance2)

! end function CORRCOEF_FULL_9


! #ifdef DEBUG
! function CORRCOEF_DIM_10(arr1, arr2, dim) result(output)
! #else
! pure function CORRCOEF_DIM_10(arr1, arr2, dim) result(output)
! #endif
!     integer, parameter :: ndim = 10

!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim

!     real(RK), allocatable :: output(:,:,:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n

!     if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
!         ERROR STOP 'corrcoef: array shapes must match'
!     endif

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'corrcoef: dim must be between 1 and 10'
!     endif

!     ishape(1:ndim)     = shape(arr1, kind=ik)
!     oshape(1:dim-1)    = ishape(1:dim-1)
!     oshape(dim:ndim-1) = ishape(dim+1:ndim)
!     n = ishape(dim)

!     allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8),oshape(9)))

!     if (n == 0_ik) then
!         output(:,:,:,:,:,:,:,:,:) = real(0, kind=RK)
!         return
!     endif

!     if (size(output, kind=ik) == 0_ik) then
!         return
!     endif

!     if (n == 1_ik) then
!         output(:,:,:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     output(:,:,:,:,:,:,:,:,:) = covariance(arr1(:,:,:,:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:,:,:,:), dim) &
!                               & / sqrt(variance(arr1(:,:,:,:,:,:,:,:,:,:), dim) * variance(arr2(:,:,:,:,:,:,:,:,:,:), dim))

! end function CORRCOEF_DIM_10


! #ifdef DEBUG
! function CORRCOEF_FULL_10(arr1, arr2) result(output)
! #else
! pure function CORRCOEF_FULL_10(arr1, arr2) result(output)
! #endif
!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:,:)

!     real(RK)    :: output
!     real(RK)    :: covar
!     real(RK)    :: variance1
!     real(RK)    :: variance2
!     integer(ik) :: n

!     if (any(shape(arr2, kind=ik) /= shape(arr1, kind=ik))) then
!         ERROR STOP 'corrcoef: array shapes must match'
!     endif

!     n = size(arr1, kind=ik)

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     if (n == 1_ik) then
!         output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     covar     = covariance(arr1(:,:,:,:,:,:,:,:,:,:), arr2(:,:,:,:,:,:,:,:,:,:))
!     variance1 = variance(arr1(:,:,:,:,:,:,:,:,:,:))
!     variance2 = variance(arr2(:,:,:,:,:,:,:,:,:,:))
!     output    = covar / sqrt(variance1*variance2)

! end function CORRCOEF_FULL_10


