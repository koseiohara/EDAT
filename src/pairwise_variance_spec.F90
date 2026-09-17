

#ifdef DEBUG
function VARIANCE_1(arr, sample) result(output)
#else
pure function VARIANCE_1(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:))
    output = sum_hp((arr(:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_1


#ifdef DEBUG
function VARIANCE_DIM_1(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_1(arr, dim, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK) :: output

    if (dim /= 1) then
        ERROR STOP 'variance: dim must be between 1 and 1'
    endif

    output = VARIANCE_1(arr(:), sample)

end function VARIANCE_DIM_1


#ifdef DEBUG
function VARIANCE_DIM_2(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_2(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 2

    real(RK), intent(in) :: arr(:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:)
    real(RK), allocatable :: array_mean(:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 2'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    allocate(array_mean(oshape(1)))

    array_mean(:) = mean(arr(:,:), dim)
    output(:) = sum_hp((arr(:,:) - spread(array_mean(:), dim, n))**2, dim)
    output(:) = output(:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_2


#ifdef DEBUG
function VARIANCE_FULL_2(arr, sample) result(output)
#else
pure function VARIANCE_FULL_2(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:))
    output = sum_hp((arr(:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_2


#ifdef DEBUG
function VARIANCE_DIM_3(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_3(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 3

    real(RK), intent(in) :: arr(:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:)
    real(RK), allocatable :: array_mean(:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 3'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    allocate(array_mean(oshape(1),oshape(2)))

    array_mean(:,:) = mean(arr(:,:,:), dim)
    output(:,:) = sum_hp((arr(:,:,:) - spread(array_mean(:,:), dim, n))**2, dim)
    output(:,:) = output(:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_3


#ifdef DEBUG
function VARIANCE_FULL_3(arr, sample) result(output)
#else
pure function VARIANCE_FULL_3(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:))
    output = sum_hp((arr(:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_3


#ifdef DEBUG
function VARIANCE_DIM_4(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_4(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 4

    real(RK), intent(in) :: arr(:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:)
    real(RK), allocatable :: array_mean(:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 4'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    allocate(array_mean(oshape(1),oshape(2),oshape(3)))

    array_mean(:,:,:) = mean(arr(:,:,:,:), dim)
    output(:,:,:) = sum_hp((arr(:,:,:,:) - spread(array_mean(:,:,:), dim, n))**2, dim)
    output(:,:,:) = output(:,:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_4


#ifdef DEBUG
function VARIANCE_FULL_4(arr, sample) result(output)
#else
pure function VARIANCE_FULL_4(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:,:))
    output = sum_hp((arr(:,:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_4


#ifdef DEBUG
function VARIANCE_DIM_5(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_5(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 5

    real(RK), intent(in) :: arr(:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:)
    real(RK), allocatable :: array_mean(:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 5'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    allocate(array_mean(oshape(1),oshape(2),oshape(3),oshape(4)))

    array_mean(:,:,:,:) = mean(arr(:,:,:,:,:), dim)
    output(:,:,:,:) = sum_hp((arr(:,:,:,:,:) - spread(array_mean(:,:,:,:), dim, n))**2, dim)
    output(:,:,:,:) = output(:,:,:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_5


#ifdef DEBUG
function VARIANCE_FULL_5(arr, sample) result(output)
#else
pure function VARIANCE_FULL_5(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:,:,:))
    output = sum_hp((arr(:,:,:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_5


#ifdef DEBUG
function VARIANCE_DIM_6(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_6(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 6

    real(RK), intent(in) :: arr(:,:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:,:)
    real(RK), allocatable :: array_mean(:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 6'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    allocate(array_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5)))

    array_mean(:,:,:,:,:) = mean(arr(:,:,:,:,:,:), dim)
    output(:,:,:,:,:) = sum_hp((arr(:,:,:,:,:,:) - spread(array_mean(:,:,:,:,:), dim, n))**2, dim)
    output(:,:,:,:,:) = output(:,:,:,:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_6


#ifdef DEBUG
function VARIANCE_FULL_6(arr, sample) result(output)
#else
pure function VARIANCE_FULL_6(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:,:,:,:))
    output = sum_hp((arr(:,:,:,:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_6


#ifdef DEBUG
function VARIANCE_DIM_7(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_7(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 7

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:,:,:)
    real(RK), allocatable :: array_mean(:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 7'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    allocate(array_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6)))

    array_mean(:,:,:,:,:,:) = mean(arr(:,:,:,:,:,:,:), dim)
    output(:,:,:,:,:,:) = sum_hp((arr(:,:,:,:,:,:,:) - spread(array_mean(:,:,:,:,:,:), dim, n))**2, dim)
    output(:,:,:,:,:,:) = output(:,:,:,:,:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_7


#ifdef DEBUG
function VARIANCE_FULL_7(arr, sample) result(output)
#else
pure function VARIANCE_FULL_7(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:,:,:,:,:))
    output = sum_hp((arr(:,:,:,:,:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_7


#ifdef DEBUG
function VARIANCE_DIM_8(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_8(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 8

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:,:,:,:)
    real(RK), allocatable :: array_mean(:,:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 8'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:,:) = real(0, kind=RK)
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
        output(:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))

    array_mean(:,:,:,:,:,:,:) = mean(arr(:,:,:,:,:,:,:,:), dim)
    output(:,:,:,:,:,:,:) = sum_hp((arr(:,:,:,:,:,:,:,:) - spread(array_mean(:,:,:,:,:,:,:), dim, n))**2, dim)
    output(:,:,:,:,:,:,:) = output(:,:,:,:,:,:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_8


#ifdef DEBUG
function VARIANCE_FULL_8(arr, sample) result(output)
#else
pure function VARIANCE_FULL_8(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:,:,:,:,:,:))
    output = sum_hp((arr(:,:,:,:,:,:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_8


#ifdef DEBUG
function VARIANCE_DIM_9(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_9(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 9

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:,:,:,:,:)
    real(RK), allocatable :: array_mean(:,:,:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 9'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:,:,:) = real(0, kind=RK)
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
        output(:,:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8)))

    array_mean(:,:,:,:,:,:,:,:) = mean(arr(:,:,:,:,:,:,:,:,:), dim)
    output(:,:,:,:,:,:,:,:) = sum_hp((arr(:,:,:,:,:,:,:,:,:) - spread(array_mean(:,:,:,:,:,:,:,:), dim, n))**2, dim)
    output(:,:,:,:,:,:,:,:) = output(:,:,:,:,:,:,:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_9


#ifdef DEBUG
function VARIANCE_FULL_9(arr, sample) result(output)
#else
pure function VARIANCE_FULL_9(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:,:,:,:,:,:,:))
    output = sum_hp((arr(:,:,:,:,:,:,:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_9


#ifdef DEBUG
function VARIANCE_DIM_10(arr, dim, sample) result(output)
#else
pure function VARIANCE_DIM_10(arr, dim, sample) result(output)
#endif
    integer, parameter :: ndim = 10

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:,:)
    integer , intent(in) :: dim
    logical , intent(in), optional :: sample

    real(RK), allocatable :: output(:,:,:,:,:,:,:,:,:)
    real(RK), allocatable :: array_mean(:,:,:,:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: sample_num

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'variance: dim must be between 1 and 10'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    n = ishape(dim)
    sample_num = n

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8),oshape(9)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:,:,:,:) = real(0, kind=RK)
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
        output(:,:,:,:,:,:,:,:,:) = ieee_value(real(0, kind=RK), ieee_quiet_nan)
        return
    endif

    allocate(array_mean(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8),oshape(9)))

    array_mean(:,:,:,:,:,:,:,:,:) = mean(arr(:,:,:,:,:,:,:,:,:,:), dim)
    output(:,:,:,:,:,:,:,:,:) = sum_hp((arr(:,:,:,:,:,:,:,:,:,:) - spread(array_mean(:,:,:,:,:,:,:,:,:), dim, n))**2, dim)
    output(:,:,:,:,:,:,:,:,:) = output(:,:,:,:,:,:,:,:,:) / real(sample_num, kind=RK)

end function VARIANCE_DIM_10


#ifdef DEBUG
function VARIANCE_FULL_10(arr, sample) result(output)
#else
pure function VARIANCE_FULL_10(arr, sample) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:,:)
    logical , intent(in), optional :: sample

    real(RK)    :: output
    real(RK)    :: array_mean
    integer(ik) :: n
    integer(ik) :: sample_num

    n = size(arr, kind=ik)
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

    array_mean = mean(arr(:,:,:,:,:,:,:,:,:,:))
    output = sum_hp((arr(:,:,:,:,:,:,:,:,:,:) - array_mean)**2) / real(sample_num, kind=RK)

end function VARIANCE_FULL_10


