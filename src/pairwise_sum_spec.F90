

#ifdef DEBUG
function SUM_HP_1(arr, dim) result(output)
#else
pure function SUM_HP_1(arr, dim) result(output)
#endif
    real(RK), intent(in) :: arr(:)
    integer , intent(in) , optional :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK) :: oarr(1)
    real(RK) :: output
    integer(ik) :: n

    if (present(dim)) then
        if (dim > 1 .OR. dim <= 0) then
            ERROR STOP
        endif
    endif

    n = size(arr, kind=ik)

    if (n <= 3_ik) then
        if (n == 0) then
            output = real(0, kind=RK)
        else if (n == 1) then
            output = arr(1)
        else if (n == 2) then
            output = arr(1) + arr(2)
        else if (n == 3) then
            output = arr(1) + arr(2) + arr(3)
        endif

        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = arr(1:n)

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_1


#ifdef DEBUG
function SUM_HP_DIM_2(arr, dim) result(output)
#else
pure function SUM_HP_DIM_2(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 2

    real(RK), intent(in) :: arr(:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: output(:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany

    allocate(arr_cpy(isize))
    allocate(output(oshape(1)))

    if (n == 0_ik) then
        output(:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & output(:)         )  !! OUT

end function SUM_HP_DIM_2


#ifdef DEBUG
function SUM_HP_FULL_2(arr) result(output)
#else
pure function SUM_HP_FULL_2(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_2


#ifdef DEBUG
function SUM_HP_DIM_3(arr, dim) result(output)
#else
pure function SUM_HP_DIM_3(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 3

    real(RK), intent(in) :: arr(:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2)))

    if (n == 0_ik) then
        output(:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_3


#ifdef DEBUG
function SUM_HP_FULL_3(arr) result(output)
#else
pure function SUM_HP_FULL_3(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_3


#ifdef DEBUG
function SUM_HP_DIM_4(arr, dim) result(output)
#else
pure function SUM_HP_DIM_4(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 4

    real(RK), intent(in) :: arr(:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2),oshape(3)))

    if (n == 0_ik) then
        output(:,:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_4


#ifdef DEBUG
function SUM_HP_FULL_4(arr) result(output)
#else
pure function SUM_HP_FULL_4(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_4


#ifdef DEBUG
function SUM_HP_DIM_5(arr, dim) result(output)
#else
pure function SUM_HP_DIM_5(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 5

    real(RK), intent(in) :: arr(:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4)))

    if (n == 0_ik) then
        output(:,:,:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:,:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_5


#ifdef DEBUG
function SUM_HP_FULL_5(arr) result(output)
#else
pure function SUM_HP_FULL_5(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_5


#ifdef DEBUG
function SUM_HP_DIM_6(arr, dim) result(output)
#else
pure function SUM_HP_DIM_6(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 6

    real(RK), intent(in) :: arr(:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5)))

    if (n == 0_ik) then
        output(:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:,:,:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_6


#ifdef DEBUG
function SUM_HP_FULL_6(arr) result(output)
#else
pure function SUM_HP_FULL_6(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_6


#ifdef DEBUG
function SUM_HP_DIM_7(arr, dim) result(output)
#else
pure function SUM_HP_DIM_7(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 7

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:,:,:,:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_7


#ifdef DEBUG
function SUM_HP_FULL_7(arr) result(output)
#else
pure function SUM_HP_FULL_7(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_7


#ifdef DEBUG
function SUM_HP_DIM_8(arr, dim) result(output)
#else
pure function SUM_HP_DIM_8(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 8

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:,:,:,:,:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_8


#ifdef DEBUG
function SUM_HP_FULL_8(arr) result(output)
#else
pure function SUM_HP_FULL_8(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_8


#ifdef DEBUG
function SUM_HP_DIM_9(arr, dim) result(output)
#else
pure function SUM_HP_DIM_9(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 9

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:,:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:,:,:,:,:,:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_9


#ifdef DEBUG
function SUM_HP_FULL_9(arr) result(output)
#else
pure function SUM_HP_FULL_9(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_9


#ifdef DEBUG
function SUM_HP_DIM_10(arr, dim) result(output)
#else
pure function SUM_HP_DIM_10(arr, dim) result(output)
#endif
    integer, parameter :: ndim = 10

    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:,:)
    integer , intent(in) :: dim

    real(RK), allocatable :: arr_cpy(:)
    real(RK), allocatable :: oarr(:)
    real(RK), allocatable :: output(:,:,:,:,:,:,:,:,:)
    integer(ik) :: ishape(ndim)
    integer(ik) :: oshape(ndim-1)
    integer(ik) :: n
    integer(ik) :: howmany
    integer(ik) :: stride
    integer(ik) :: isize
    integer(ik) :: osize
    integer     :: i

    if (dim > ndim .OR. dim <= 0) then
        ERROR STOP
    endif

    ishape(1:ndim) = shape(arr, kind=ik)

    oshape(1:dim-1)    = ishape(1:dim-1)
    oshape(dim:ndim-1) = ishape(dim+1:ndim)

    stride = 1_ik
    do i = 1, dim-1
        stride = stride * ishape(i)
    enddo

    n = ishape(dim)

    howmany = 1
    do i = dim+1, ndim
        howmany = howmany * ishape(i)
    enddo

    isize = stride * n * howmany
    osize = stride * howmany

    allocate(arr_cpy(isize))
    allocate(oarr(osize))
    allocate(output(oshape(1),oshape(2),oshape(3),oshape(4),oshape(5),oshape(6),oshape(7),oshape(8),oshape(9)))

    if (n == 0_ik) then
        output(:,:,:,:,:,:,:,:,:) = real(0, kind=RK)
        return
    endif

    arr_cpy(1:isize) = reshape(arr, shape=[isize])

    call CORE(n               , &  !! IN
            & howmany         , &  !! IN
            & stride          , &  !! IN
            & arr_cpy(1:isize), &  !! IN
            & oarr(:)           )  !! OUT

    output(:,:,:,:,:,:,:,:,:) = reshape(oarr(:), shape=oshape)

end function SUM_HP_DIM_10


#ifdef DEBUG
function SUM_HP_FULL_10(arr) result(output)
#else
pure function SUM_HP_FULL_10(arr) result(output)
#endif
    real(RK), intent(in) :: arr(:,:,:,:,:,:,:,:,:,:)

    real(RK), allocatable :: arr_cpy(:)
    real(RK)    :: oarr(1)
    real(RK)    :: output
    integer(ik) :: n

    n = size(arr, kind=ik)

    if (n == 0_ik) then
        output = real(0, kind=RK)
        return
    endif

    allocate(arr_cpy(n))

    arr_cpy(1:n) = reshape(arr, shape=[n])

    call CORE(n           , &  !! IN
            & 1_ik        , &  !! IN
            & 1_ik        , &  !! IN
            & arr_cpy(1:n), &  !! IN
            & oarr(1:1)     )  !! OUT

    output = oarr(1)

end function SUM_HP_FULL_10



