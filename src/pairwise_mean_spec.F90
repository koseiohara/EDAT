

#ifdef DEBUG
function MEAN_1(arr, dim) result(output)
#else
pure function MEAN_1(arr, dim) result(output)
#endif
    real(RK), intent(in) :: arr(:)
    integer , intent(in), optional :: dim

    real(RK)    :: output
    integer(ik) :: n

    if (present(dim)) then
        if (dim > 1 .OR. dim <= 0) then
            ERROR STOP 'mean: dim must be between 1 and 1'
        endif
    endif

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_1


#ifdef DEBUG
function MEAN_DIM_2(arr, dim) result(output)
#else
pure function MEAN_DIM_2(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 2

    real(RK), intent(in) :: arr(:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'mean: dim must be between 1 and 2'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    output(:) = sum_hp(arr, dim)
    output(:) = output(:) / real(n, kind=RK)

end function MEAN_DIM_2


#ifdef DEBUG
function MEAN_FULL_2(arr) result(output)
#else
pure function MEAN_FULL_2(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:)

    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_FULL_2


#ifdef DEBUG
function MEAN_DIM_3(arr, dim) result(output)
#else
pure function MEAN_DIM_3(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 3

    real(RK), intent(in) :: arr(:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'mean: dim must be between 1 and 3'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    output(:,:) = sum_hp(arr, dim)
    output(:,:) = output(:,:) / real(n, kind=RK)

end function MEAN_DIM_3


#ifdef DEBUG
function MEAN_FULL_3(arr) result(output)
#else
pure function MEAN_FULL_3(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:)

    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_FULL_3


#ifdef DEBUG
function MEAN_DIM_4(arr, dim) result(output)
#else
pure function MEAN_DIM_4(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 4

    real(RK), intent(in) :: arr(:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'mean: dim must be between 1 and 4'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    output(:,:,:) = sum_hp(arr, dim)
    output(:,:,:) = output(:,:,:) / real(n, kind=RK)

end function MEAN_DIM_4


#ifdef DEBUG
function MEAN_FULL_4(arr) result(output)
#else
pure function MEAN_FULL_4(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:)

    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_FULL_4


#ifdef DEBUG
function MEAN_DIM_5(arr, dim) result(output)
#else
pure function MEAN_DIM_5(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 5

    real(RK), intent(in) :: arr(:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'mean: dim must be between 1 and 5'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    output(:,:,:,:) = sum_hp(arr, dim)
    output(:,:,:,:) = output(:,:,:,:) / real(n, kind=RK)

end function MEAN_DIM_5


#ifdef DEBUG
function MEAN_FULL_5(arr) result(output)
#else
pure function MEAN_FULL_5(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:)

    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_FULL_5


#ifdef DEBUG
function MEAN_DIM_6(arr, dim) result(output)
#else
pure function MEAN_DIM_6(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 6

    real(RK), intent(in) :: arr(:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'mean: dim must be between 1 and 6'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    output(:,:,:,:,:) = sum_hp(arr, dim)
    output(:,:,:,:,:) = output(:,:,:,:,:) / real(n, kind=RK)

end function MEAN_DIM_6


#ifdef DEBUG
function MEAN_FULL_6(arr) result(output)
#else
pure function MEAN_FULL_6(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:)

    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_FULL_6


#ifdef DEBUG
function MEAN_DIM_7(arr, dim) result(output)
#else
pure function MEAN_DIM_7(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 7

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'mean: dim must be between 1 and 7'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
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

    output(:,:,:,:,:,:) = sum_hp(arr, dim)
    output(:,:,:,:,:,:) = output(:,:,:,:,:,:) / real(n, kind=RK)

end function MEAN_DIM_7


#ifdef DEBUG
function MEAN_FULL_7(arr) result(output)
#else
pure function MEAN_FULL_7(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:)

    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_FULL_7


#ifdef DEBUG
function MEAN_DIM_8(arr, dim) result(output)
#else
pure function MEAN_DIM_8(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 8

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: output(:,:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP 'mean: dim must be between 1 and 8'
    endif

    ishape(1:ndim)     = shape(arr, kind=ik)
    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)
    n = ishape(dim)

    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    if (size(output, kind=ik) == 0_ik) then
        return
    endif

    output(:,:,:,:,:,:,:) = sum_hp(arr, dim)
    output(:,:,:,:,:,:,:) = output(:,:,:,:,:,:,:) / real(n, kind=RK)

end function MEAN_DIM_8


#ifdef DEBUG
function MEAN_FULL_8(arr) result(output)
#else
pure function MEAN_FULL_8(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:)

    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    output = sum_hp(arr) / real(n, kind=RK)

end function MEAN_FULL_8


! #ifdef DEBUG
! function MEAN_DIM_9(arr, dim) result(output)
! #else
! pure function MEAN_DIM_9(arr, dim) result(output)
! #endif
!     integer, parameter :: ndim = 9

!     real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim

!     real(RK), allocatable :: output(:,:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'mean: dim must be between 1 and 9'
!     endif

!     ishape(1:ndim)     = shape(arr, kind=ik)
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

!     output(:,:,:,:,:,:,:,:) = sum_hp(arr, dim)
!     output(:,:,:,:,:,:,:,:) = output(:,:,:,:,:,:,:,:) / real(n, kind=RK)

! end function MEAN_DIM_9


! #ifdef DEBUG
! function MEAN_FULL_9(arr) result(output)
! #else
! pure function MEAN_FULL_9(arr) result(output)
! #endif
!     real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:)

!     real(RK)    :: output
!     integer(ik) :: n

!     n = size(arr, kind=ik)

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     output = sum_hp(arr) / real(n, kind=RK)

! end function MEAN_FULL_9


! #ifdef DEBUG
! function MEAN_DIM_10(arr, dim) result(output)
! #else
! pure function MEAN_DIM_10(arr, dim) result(output)
! #endif
!     integer, parameter :: ndim = 10

!     real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:,:)
!     integer , intent(in) :: dim

!     real(RK), allocatable :: output(:,:,:,:,:,:,:,:,:)
!     integer(ik) :: ishape(ndim)
!     integer(ik) :: oshape(ndim-1)
!     integer(ik) :: n

!     if (dim > ndim .OR. dim <= 0) then
!         ERROR STOP 'mean: dim must be between 1 and 10'
!     endif

!     ishape(1:ndim)     = shape(arr, kind=ik)
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

!     output(:,:,:,:,:,:,:,:,:) = sum_hp(arr, dim)
!     output(:,:,:,:,:,:,:,:,:) = output(:,:,:,:,:,:,:,:,:) / real(n, kind=RK)

! end function MEAN_DIM_10


! #ifdef DEBUG
! function MEAN_FULL_10(arr) result(output)
! #else
! pure function MEAN_FULL_10(arr) result(output)
! #endif
!     real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:,:)

!     real(RK)    :: output
!     integer(ik) :: n

!     n = size(arr, kind=ik)

!     if (n == 0_ik) then
!         output = real(0, kind=RK)
!         return
!     endif

!     output = sum_hp(arr) / real(n, kind=RK)

! end function MEAN_FULL_10


