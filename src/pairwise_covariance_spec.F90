

#ifdef DEBUG
function COVARIANCE_1(arr1, arr2, sample) result(output)
#else
pure function COVARIANCE_1(arr1, arr2, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:)
    real(RK), intent(in) :: arr2(:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array1_mean
    real(RK)    :: array2_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    if (size(arr1, kind=ik) /= size(arr2, kind=ik)) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    n = size(arr1, kind=ik)
    sample_num = n

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (sample_num == 0_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    array1_mean = mean(arr1(1:n))
    array2_mean = mean(arr2(1:n))
    output = sum_hp((arr1(1:n) - array1_mean) * (arr2(1:n) - array2_mean)) / real(sample_num, kind=RK)

end function COVARIANCE_1


#ifdef DEBUG
function COVARIANCE_DIM_1(arr1, arr2, dim, sample) result(output)
#else
pure function COVARIANCE_DIM_1(arr1, arr2, dim, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:)
    real(RK), intent(in) :: arr2(:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK) :: output

    if (size(arr1, kind=ik) /= size(arr2, kind=ik)) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    if (dim /= 1) then
        ERROR STOP 'covariance: dim must be between 1 and 1'
    endif

    output = COVARIANCE_1(arr1(:), arr2(:), sample)

end function COVARIANCE_DIM_1


#ifdef DEBUG
function COVARIANCE_DIM_2(arr1, arr2, dim, sample) result(output)
#else
pure function COVARIANCE_DIM_2(arr1, arr2, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 2

    real(RK), intent(in) :: arr1(:,:)
    real(RK), intent(in) :: arr2(:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:)
    real(RK), allocatable :: array1_mean(:)
    real(RK), allocatable :: array2_mean(:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'covariance: dim must be between 1 and 2'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1)))

    if (n == 0_ik) then
        output(:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (sample_num == 0_ik) then
        output(:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array1_mean(oshape(1)))
    allocate(array2_mean(oshape(1)))

    array1_mean(:) = mean(arr1(:,:), dim)
    array2_mean(:) = mean(arr2(:,:), dim)
    output(:) = sum_hp((arr1(:,:) - spread(array1_mean(:), dim, n)) &
                   & * (arr2(:,:) - spread(array2_mean(:), dim, n)), dim) &
                   & / real(sample_num, kind=RK)

end function COVARIANCE_DIM_2


#ifdef DEBUG
function COVARIANCE_FULL_2(arr1, arr2, sample) result(output)
#else
pure function COVARIANCE_FULL_2(arr1, arr2, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:)
    real(RK), intent(in) :: arr2(:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array1_mean
    real(RK)    :: array2_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    n = size(arr1, kind=ik)
    sample_num = n

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (sample_num == 0_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    array1_mean = mean(arr1(:,:))
    array2_mean = mean(arr2(:,:))
    output = sum_hp((arr1(:,:) - array1_mean) * (arr2(:,:) - array2_mean)) / real(sample_num, kind=RK)

end function COVARIANCE_FULL_2


#ifdef DEBUG
function COVARIANCE_DIM_3(arr1, arr2, dim, sample) result(output)
#else
pure function COVARIANCE_DIM_3(arr1, arr2, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 3

    real(RK), intent(in) :: arr1(:,:,:)
    real(RK), intent(in) :: arr2(:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:)
    real(RK), allocatable :: array1_mean(:,:)
    real(RK), allocatable :: array2_mean(:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'covariance: dim must be between 1 and 3'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2)))

    if (n == 0_ik) then
        output(:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (sample_num == 0_ik) then
        output(:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array1_mean(oshape(1),oshape(2)))
    allocate(array2_mean(oshape(1),oshape(2)))

    array1_mean(:,:) = mean(arr1(:,:,:), dim)
    array2_mean(:,:) = mean(arr2(:,:,:), dim)
    output(:,:) = sum_hp((arr1(:,:,:) - spread(array1_mean(:,:), dim, n)) &
                     & * (arr2(:,:,:) - spread(array2_mean(:,:), dim, n)), dim) &
                     & / real(sample_num, kind=RK)

end function COVARIANCE_DIM_3


#ifdef DEBUG
function COVARIANCE_FULL_3(arr1, arr2, sample) result(output)
#else
pure function COVARIANCE_FULL_3(arr1, arr2, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:)
    real(RK), intent(in) :: arr2(:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array1_mean
    real(RK)    :: array2_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    n = size(arr1, kind=ik)
    sample_num = n

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (sample_num == 0_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    array1_mean = mean(arr1(:,:,:))
    array2_mean = mean(arr2(:,:,:))
    output = sum_hp((arr1(:,:,:) - array1_mean) * (arr2(:,:,:) - array2_mean)) / real(sample_num, kind=RK)

end function COVARIANCE_FULL_3


#ifdef DEBUG
function COVARIANCE_DIM_4(arr1, arr2, dim, sample) result(output)
#else
pure function COVARIANCE_DIM_4(arr1, arr2, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 4

    real(RK), intent(in) :: arr1(:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:)
    real(RK), allocatable :: array1_mean(:,:,:)
    real(RK), allocatable :: array2_mean(:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'covariance: dim must be between 1 and 4'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2),oshape(3)))

    if (n == 0_ik) then
        output(:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (sample_num == 0_ik) then
        output(:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array1_mean(oshape(1),oshape(2),oshape(3)))
    allocate(array2_mean(oshape(1),oshape(2),oshape(3)))

    array1_mean(:,:,:) = mean(arr1(:,:,:,:), dim)
    array2_mean(:,:,:) = mean(arr2(:,:,:,:), dim)
    output(:,:,:) = sum_hp((arr1(:,:,:,:) - spread(array1_mean(:,:,:), dim, n)) &
                       & * (arr2(:,:,:,:) - spread(array2_mean(:,:,:), dim, n)), dim) &
                       & / real(sample_num, kind=RK)

end function COVARIANCE_DIM_4


#ifdef DEBUG
function COVARIANCE_FULL_4(arr1, arr2, sample) result(output)
#else
pure function COVARIANCE_FULL_4(arr1, arr2, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK) :: output
    real(RK) :: array1_mean
    real(RK) :: array2_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    n = size(arr1, kind=ik)
    sample_num = n

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (sample_num == 0_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    array1_mean = mean(arr1(:,:,:,:))
    array2_mean = mean(arr2(:,:,:,:))
    output = sum_hp((arr1(:,:,:,:) - array1_mean) * (arr2(:,:,:,:) - array2_mean)) / real(sample_num, kind=RK)

end function COVARIANCE_FULL_4


#ifdef DEBUG
function COVARIANCE_DIM_5(arr1, arr2, dim, sample) result(output)
#else
pure function COVARIANCE_DIM_5(arr1, arr2, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 5

    real(RK), intent(in) :: arr1(:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:)
    real(RK), allocatable :: array1_mean(:,:,:,:)
    real(RK), allocatable :: array2_mean(:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'covariance: dim must be between 1 and 5'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4)))

    if (n == 0_ik) then
        output(:,:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (sample_num == 0_ik) then
        output(:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array1_mean(oshape(1),oshape(2),oshape(3),oshape(4)))
    allocate(array2_mean(oshape(1),oshape(2),oshape(3),oshape(4)))

    array1_mean(:,:,:,:) = mean(arr1(:,:,:,:,:), dim)
    array2_mean(:,:,:,:) = mean(arr2(:,:,:,:,:), dim)
    output(:,:,:,:) = sum_hp((arr1(:,:,:,:,:) - spread(array1_mean(:,:,:,:), dim, n)) &
                         & * (arr2(:,:,:,:,:) - spread(array2_mean(:,:,:,:), dim, n)), dim) &
                         & / real(sample_num, kind=RK)

end function COVARIANCE_DIM_5


#ifdef DEBUG
function COVARIANCE_FULL_5(arr1, arr2, sample) result(output)
#else
pure function COVARIANCE_FULL_5(arr1, arr2, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK) :: output
    real(RK) :: array1_mean
    real(RK) :: array2_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    n = size(arr1, kind=ik)
    sample_num = n

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (sample_num == 0_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    array1_mean = mean(arr1(:,:,:,:,:))
    array2_mean = mean(arr2(:,:,:,:,:))
    output = sum_hp((arr1(:,:,:,:,:) - array1_mean) * (arr2(:,:,:,:,:) - array2_mean)) / real(sample_num, kind=RK)

end function COVARIANCE_FULL_5


#ifdef DEBUG
function COVARIANCE_DIM_6(arr1, arr2, dim, sample) result(output)
#else
pure function COVARIANCE_DIM_6(arr1, arr2, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 6

    real(RK), intent(in) :: arr1(:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:,:)
    real(RK), allocatable :: array1_mean(:,:,:,:,:)
    real(RK), allocatable :: array2_mean(:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'covariance: dim must be between 1 and 6'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5)))

    if (n == 0_ik) then
        output(:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (sample_num == 0_ik) then
        output(:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array1_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5)))
    allocate(array2_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5)))

    array1_mean(:,:,:,:,:) = mean(arr1(:,:,:,:,:,:), dim)
    array2_mean(:,:,:,:,:) = mean(arr2(:,:,:,:,:,:), dim)
    output(:,:,:,:,:) = sum_hp((arr1(:,:,:,:,:,:) - spread(array1_mean(:,:,:,:,:), dim, n)) &
                           & * (arr2(:,:,:,:,:,:) - spread(array2_mean(:,:,:,:,:), dim, n)), dim) &
                           & / real(sample_num, kind=RK)

end function COVARIANCE_DIM_6


#ifdef DEBUG
function COVARIANCE_FULL_6(arr1, arr2, sample) result(output)
#else
pure function COVARIANCE_FULL_6(arr1, arr2, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK) :: output
    real(RK) :: array1_mean
    real(RK) :: array2_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    n = size(arr1, kind=ik)
    sample_num = n

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (sample_num == 0_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    array1_mean = mean(arr1(:,:,:,:,:,:))
    array2_mean = mean(arr2(:,:,:,:,:,:))
    output = sum_hp((arr1(:,:,:,:,:,:) - array1_mean) * (arr2(:,:,:,:,:,:) - array2_mean)) &
           & / real(sample_num, kind=RK)

end function COVARIANCE_FULL_6


#ifdef DEBUG
function COVARIANCE_DIM_7(arr1, arr2, dim, sample) result(output)
#else
pure function COVARIANCE_DIM_7(arr1, arr2, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 7

    real(RK), intent(in) :: arr1(:,:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:,:,:)
    real(RK), allocatable :: array1_mean(:,:,:,:,:,:)
    real(RK), allocatable :: array2_mean(:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'covariance: dim must be between 1 and 7'
    endif

    ishape(1:ndim)     = shape(arr1, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (sample_num == 0_ik) then
        output(:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array1_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6)))
    allocate(array2_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6)))

    array1_mean(:,:,:,:,:,:) = mean(arr1(:,:,:,:,:,:,:), dim)
    array2_mean(:,:,:,:,:,:) = mean(arr2(:,:,:,:,:,:,:), dim)
    output(:,:,:,:,:,:) = sum_hp((arr1(:,:,:,:,:,:,:) - spread(array1_mean(:,:,:,:,:,:), dim, n)) &
                             & * (arr2(:,:,:,:,:,:,:) - spread(array2_mean(:,:,:,:,:,:), dim, n)), dim) &
                             & / real(sample_num, kind=RK)

end function COVARIANCE_DIM_7


#ifdef DEBUG
function COVARIANCE_FULL_7(arr1, arr2, sample) result(output)
#else
pure function COVARIANCE_FULL_7(arr1, arr2, sample) result(output)
#endif
    real(RK), intent(in) :: arr1(:,:,:,:,:,:,:)
    real(RK), intent(in) :: arr2(:,:,:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK) :: output
    real(RK) :: array1_mean
    real(RK) :: array2_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    if (any(shape(arr1) /= shape(arr2))) then
        ERROR STOP 'covariance: array shapes must match'
    endif

    n = size(arr1, kind=ik)
    sample_num = n

    if (present(sample)) then
        if (sample) then
            sample_num = n - 1_ik
        endif
    endif

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    if (sample_num == 0_ik) then
        output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    array1_mean = mean(arr1(:,:,:,:,:,:,:))
    array2_mean = mean(arr2(:,:,:,:,:,:,:))
    output = sum_hp((arr1(:,:,:,:,:,:,:) - array1_mean) * (arr2(:,:,:,:,:,:,:) - array2_mean)) &
           & / real(sample_num, kind=RK)

end function COVARIANCE_FULL_7


! #ifdef DEBUG
! function COVARIANCE_DIM_8(arr1, arr2, dim, sample) result(output)
! #else
! pure function COVARIANCE_DIM_8(arr1, arr2, dim, sample) result(output)
! #endif
!     integer, parameter :: ndim = 8

!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim
!     logical , intent(in), optional :: sample

!     real(RK), allocatable :: output(:,:,:,:,:,:,:)
!     real(RK), allocatable :: array1_mean(:,:,:,:,:,:,:)
!     real(RK), allocatable :: array2_mean(:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n
!     integer(ik) :: sample_num

!     if (any(shape(arr1) /= shape(arr2))) then
!         ERROR STOP 'covariance: array shapes must match'
!     endif

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'covariance: dim must be between 1 and 8'
!     endif

!     ishape(1:ndim)     = shape(arr1, kind=ik)
!     oshape(1:dim-1)    = ishape(1:dim-1)
!     oshape(dim:ndim-1) = ishape(dim+1:ndim)

!     n = ishape(dim)
!     sample_num = n

!     allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))

!     if (n == 0_ik) then
!         output(:,:,:,:,:,:,:) = real(0, kind=RK)
!         return
!     endif

!     if (size(output, kind=ik) == 0_ik) then
!         return
!     endif

!     if (present(sample)) then
!         if (sample) then
!             sample_num = n - 1_ik
!         endif
!     endif

!     if (sample_num == 0_ik) then
!         output(:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     allocate(array1_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))
!     allocate(array2_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))

!     array1_mean(:,:,:,:,:,:,:) = mean(arr1(:,:,:,:,:,:,:,:), dim)
!     array2_mean(:,:,:,:,:,:,:) = mean(arr2(:,:,:,:,:,:,:,:), dim)
!     output(:,:,:,:,:,:,:) = sum_hp((arr1(:,:,:,:,:,:,:,:) - spread(array1_mean(:,:,:,:,:,:,:), dim, n)) &
!                                & * (arr2(:,:,:,:,:,:,:,:) - spread(array2_mean(:,:,:,:,:,:,:), dim, n)), dim) &
!                                & / real(sample_num, kind=RK)

! end function COVARIANCE_DIM_8


! #ifdef DEBUG
! function COVARIANCE_FULL_8(arr1, arr2, sample) result(output)
! #else
! pure function COVARIANCE_FULL_8(arr1, arr2, sample) result(output)
! #endif
!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:)
!     logical , intent(in), optional :: sample

!     real(RK) :: output
!     real(RK) :: array1_mean
!     real(RK) :: array2_mean
!     integer(ik) :: n
!     integer(ik) :: sample_num

!     if (any(shape(arr1) /= shape(arr2))) then
!         ERROR STOP 'covariance: array shapes must match'
!     endif

!     n = size(arr1, kind=ik)
!     sample_num = n

!     if (present(sample)) then
!         if (sample) then
!             sample_num = n - 1_ik
!         endif
!     endif

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     if (sample_num == 0_ik) then
!         output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     array1_mean = mean(arr1(:,:,:,:,:,:,:,:))
!     array2_mean = mean(arr2(:,:,:,:,:,:,:,:))
!     output = sum_hp((arr1(:,:,:,:,:,:,:,:) - array1_mean) * (arr2(:,:,:,:,:,:,:,:) - array2_mean)) &
!            & / real(sample_num, kind=RK)

! end function COVARIANCE_FULL_8


! #ifdef DEBUG
! function COVARIANCE_DIM_9(arr1, arr2, dim, sample) result(output)
! #else
! pure function COVARIANCE_DIM_9(arr1, arr2, dim, sample) result(output)
! #endif
!     integer, parameter :: ndim = 9

!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim
!     logical , intent(in), optional :: sample

!     real(RK), allocatable :: output(:,:,:,:,:,:,:,:)
!     real(RK), allocatable :: array1_mean(:,:,:,:,:,:,:,:)
!     real(RK), allocatable :: array2_mean(:,:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n
!     integer(ik) :: sample_num

!     if (any(shape(arr1) /= shape(arr2))) then
!         ERROR STOP 'covariance: array shapes must match'
!     endif

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'covariance: dim must be between 1 and 9'
!     endif

!     ishape(1:ndim)     = shape(arr1, kind=ik)
!     oshape(1:dim-1)    = ishape(1:dim-1)
!     oshape(dim:ndim-1) = ishape(dim+1:ndim)

!     n = ishape(dim)
!     sample_num = n

!     allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8)))

!     if (n == 0_ik) then
!         output(:,:,:,:,:,:,:,:) = real(0, kind=RK)
!         return
!     endif

!     if (size(output, kind=ik) == 0_ik) then
!         return
!     endif

!     if (present(sample)) then
!         if (sample) then
!             sample_num = n - 1_ik
!         endif
!     endif

!     if (sample_num == 0_ik) then
!         output(:,:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     allocate(array1_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8)))
!     allocate(array2_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8)))

!     array1_mean(:,:,:,:,:,:,:,:) = mean(arr1(:,:,:,:,:,:,:,:,:), dim)
!     array2_mean(:,:,:,:,:,:,:,:) = mean(arr2(:,:,:,:,:,:,:,:,:), dim)
!     output(:,:,:,:,:,:,:,:) = sum_hp((arr1(:,:,:,:,:,:,:,:,:) - spread(array1_mean(:,:,:,:,:,:,:,:), dim, n)) &
!                                  & * (arr2(:,:,:,:,:,:,:,:,:) - spread(array2_mean(:,:,:,:,:,:,:,:), dim, n)), dim) &
!                                  & / real(sample_num, kind=RK)

! end function COVARIANCE_DIM_9


! #ifdef DEBUG
! function COVARIANCE_FULL_9(arr1, arr2, sample) result(output)
! #else
! pure function COVARIANCE_FULL_9(arr1, arr2, sample) result(output)
! #endif
!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:)
!     logical , intent(in), optional :: sample

!     real(RK) :: output
!     real(RK) :: array1_mean
!     real(RK) :: array2_mean
!     integer(ik) :: n
!     integer(ik) :: sample_num

!     if (any(shape(arr1) /= shape(arr2))) then
!         ERROR STOP 'covariance: array shapes must match'
!     endif

!     n = size(arr1, kind=ik)
!     sample_num = n

!     if (present(sample)) then
!         if (sample) then
!             sample_num = n - 1_ik
!         endif
!     endif

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     if (sample_num == 0_ik) then
!         output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     array1_mean = mean(arr1(:,:,:,:,:,:,:,:,:))
!     array2_mean = mean(arr2(:,:,:,:,:,:,:,:,:))
!     output = sum_hp((arr1(:,:,:,:,:,:,:,:,:) - array1_mean) * (arr2(:,:,:,:,:,:,:,:,:) - array2_mean)) &
!            & / real(sample_num, kind=RK)

! end function COVARIANCE_FULL_9


! #ifdef DEBUG
! function COVARIANCE_DIM_10(arr1, arr2, dim, sample) result(output)
! #else
! pure function COVARIANCE_DIM_10(arr1, arr2, dim, sample) result(output)
! #endif
!     integer, parameter :: ndim = 10

!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim
!     logical , intent(in), optional :: sample

!     real(RK), allocatable :: output(:,:,:,:,:,:,:,:,:)
!     real(RK), allocatable :: array1_mean(:,:,:,:,:,:,:,:,:)
!     real(RK), allocatable :: array2_mean(:,:,:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n
!     integer(ik) :: sample_num

!     if (any(shape(arr1) /= shape(arr2))) then
!         ERROR STOP 'covariance: array shapes must match'
!     endif

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'covariance: dim must be between 1 and 10'
!     endif

!     ishape(1:ndim)     = shape(arr1, kind=ik)
!     oshape(1:dim-1)    = ishape(1:dim-1)
!     oshape(dim:ndim-1) = ishape(dim+1:ndim)

!     n = ishape(dim)
!     sample_num = n

!     allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8),oshape(9)))

!     if (n == 0_ik) then
!         output(:,:,:,:,:,:,:,:,:) = real(0, kind=RK)
!         return
!     endif

!     if (size(output, kind=ik) == 0_ik) then
!         return
!     endif

!     if (present(sample)) then
!         if (sample) then
!             sample_num = n - 1_ik
!         endif
!     endif

!     if (sample_num == 0_ik) then
!         output(:,:,:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     allocate(array1_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8),oshape(9)))
!     allocate(array2_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8),oshape(9)))

!     array1_mean(:,:,:,:,:,:,:,:,:) = mean(arr1(:,:,:,:,:,:,:,:,:,:), dim)
!     array2_mean(:,:,:,:,:,:,:,:,:) = mean(arr2(:,:,:,:,:,:,:,:,:,:), dim)
!     output(:,:,:,:,:,:,:,:,:) = sum_hp((arr1(:,:,:,:,:,:,:,:,:,:) - spread(array1_mean(:,:,:,:,:,:,:,:,:), dim, n)) &
!                                    & * (arr2(:,:,:,:,:,:,:,:,:,:) - spread(array2_mean(:,:,:,:,:,:,:,:,:), dim, n)), dim) &
!                                    & / real(sample_num, kind=RK)

! end function COVARIANCE_DIM_10


! #ifdef DEBUG
! function COVARIANCE_FULL_10(arr1, arr2, sample) result(output)
! #else
! pure function COVARIANCE_FULL_10(arr1, arr2, sample) result(output)
! #endif
!     real(RK), intent(in) :: arr1(:,:,:,:,:,:,:,:,:,:)
!     real(RK), intent(in) :: arr2(:,:,:,:,:,:,:,:,:,:)
!     logical , intent(in), optional :: sample

!     real(RK) :: output
!     real(RK) :: array1_mean
!     real(RK) :: array2_mean
!     integer(ik) :: n
!     integer(ik) :: sample_num

!     if (any(shape(arr1) /= shape(arr2))) then
!         ERROR STOP 'covariance: array shapes must match'
!     endif

!     n = size(arr1, kind=ik)
!     sample_num = n

!     if (present(sample)) then
!         if (sample) then
!             sample_num = n - 1_ik
!         endif
!     endif

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     if (sample_num == 0_ik) then
!         output = ieee_value(real(0, kind=RK), ieee_quiet_nan)
!         return
!     endif

!     array1_mean = mean(arr1(:,:,:,:,:,:,:,:,:,:))
!     array2_mean = mean(arr2(:,:,:,:,:,:,:,:,:,:))
!     output = sum_hp((arr1(:,:,:,:,:,:,:,:,:,:) - array1_mean) * (arr2(:,:,:,:,:,:,:,:,:,:) - array2_mean)) &
!            & / real(sample_num, kind=RK)

! end function COVARIANCE_FULL_10



