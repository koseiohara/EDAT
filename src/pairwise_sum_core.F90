

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
subroutine CORE(n, howmany, stride, iarr, oarr)
#else
pure subroutine CORE(n, howmany, stride, iarr, oarr)
#endif

#ifdef DEBUG
    use, intrinsic :: iso_fortran_env, only : ounit=>output_unit
#endif

    integer(ik), intent(in) :: n
    integer(ik), intent(in) :: howmany
    integer(ik), intent(in) :: stride
    real(RK)   , intent(inout), contiguous :: iarr(:)
    real(RK)   , intent(out)  , contiguous :: oarr(:)

    real(RK), allocatable :: work_arr(:)
    integer(ik) :: n_resized
    integer(ik) :: nwork
    integer(ik) :: work_n
    integer(ik) :: half_n
    integer(ik) :: i
    integer(ik) :: j
    integer(ik) :: k
    integer(ik) :: skip_iarr
    integer(ik) :: skip_work
    integer(ik) :: ioff
    integer(ik) :: idx1
    integer(ik) :: idx2

    n_resized = largest_power_of_2(n)

    call RESIZE(n        , &  !! IN
              & n_resized, &  !! IN
              & stride   , &  !! IN
              & howmany  , &  !! IN
              & iarr(:)    )  !! INOUT


    work_n = n_resized
    half_n = shiftr(work_n, 1)
    nwork  = half_n * stride


    if (work_n > 2) then
        allocate(work_arr(nwork))
    endif

    do i = 0, howmany-1
        work_n = n_resized
        do
            if (work_n > 2) then
                work_n = shiftr(work_n, 1)
#ifdef DEBUG
                write(ounit,'(A,I0)') 'i = ', i
#endif
                skip_iarr = i * n_resized * stride

                idx1 = 1
                idx2 = 1 + stride
                do j = 1, work_n
#ifdef DEBUG
                    write(ounit,'(A,I0)') 'j = ', j
#endif
                    ioff = (j - 1) * stride
                    do k = 1, stride
#ifdef DEBUG
                        write(ounit,'(A,I0,A,I0,A,I0,A)') 'work_arr(', ioff+k, &
                                                        & ') = iarr(', skip_iarr+idx1, &
                                                        & ') + iarr(', skip_iarr+idx2, ')'
#endif
                        work_arr(ioff+k) = iarr(skip_iarr+idx1) + iarr(skip_iarr+idx2)

                        idx1 = idx1 + 1
                        idx2 = idx1 + stride
                    enddo
                    idx1 = idx2
                    idx2 = idx1 + stride
                enddo

                work_n = shiftr(work_n, 1)
#ifdef DEBUG
                write(ounit,'(A,I0)') 'i = ', i
#endif
                skip_iarr = i * n_resized * stride

                idx1 = 1
                idx2 = 1 + stride
                do j = 1, work_n
#ifdef DEBUG
                    write(ounit,'(A,I0)') 'j = ', j
#endif
                    ioff = skip_iarr + (j - 1) * stride
                    do k = 1, stride
#ifdef DEBUG
                        write(ounit,'(A,I0,A,I0,A,I0,A)') 'iarr(', ioff+k, &
                                                        & ') = work_arr(', idx1,&
                                                        & ') + work_arr(', idx2, ')'
#endif
                        iarr(ioff+k) = work_arr(idx1) + work_arr(idx2)

                        idx1 = idx1 + 1
                        idx2 = idx1 + stride
                    enddo
                    idx1 = idx2
                    idx2 = idx1 + stride
                enddo

                cycle
            else
                exit
            endif
        enddo
    enddo

    if (work_n == 2) then
        do i = 0, howmany-1
            skip_iarr = i * n_resized * stride
            skip_work = i * stride
            do j = 1, stride
#ifdef DEBUG
            write(ounit,'(A,I0,A,I0,A,I0,A)') 'oarr(', skip_work+j, &
                                            & ') = iarr(', skip_iarr+j, &
                                            & ') + iarr(', skip_iarr+j+stride, ')'
#endif
                oarr(skip_work+j) = iarr(skip_iarr+j) + iarr(skip_iarr+j+stride)
            enddo
        enddo
    else
        do i = 0, howmany-1
            skip_iarr = i * n_resized * stride
            skip_work = i * stride
            do j = 1, stride
#ifdef DEBUG
            write(ounit,'(A,I0,A,I0,A,I0,A)') 'oarr(', skip_work+j, ') = iarr(', skip_iarr+j, ')'
#endif
                oarr(skip_work+j) = iarr(skip_iarr+j)
            enddo
        enddo
    endif

    if (allocated(work_arr)) then
        deallocate(work_arr)
    endif

end subroutine CORE


pure subroutine RESIZE(n, n_resized, stride, howmany, arr)
    integer(ik), intent(in) :: n
    integer(ik), intent(in) :: n_resized
    integer(ik), intent(in) :: stride
    integer(ik), intent(in) :: howmany
    real(RK)   , intent(inout), contiguous :: arr(:)

    integer(ik) :: remainder_n
    integer(ik) :: half_n
    integer(ik) :: resized_group_n
    integer(ik) :: old_offset
    integer(ik) :: new_offset
    integer(ik) :: source_offset
    integer(ik) :: target_offset
    integer(ik) :: dist
    integer(ik) :: position
    integer(ik) :: position_remainder
    integer(ik) :: position_step
    integer(ik) :: position_step_remainder
    integer(ik) :: group
    integer(ik) :: tail
    integer(ik) :: i

    if (n == n_resized) then
        return
    endif

    remainder_n     = n - n_resized
    half_n          = shiftr(n_resized, 1)
    resized_group_n = n_resized * stride

    do group = 0_ik, howmany-1_ik
        old_offset = group * n * stride
        new_offset = group * resized_group_n

        if (new_offset /= old_offset) then
            do i = 1_ik, resized_group_n
                arr(new_offset+i) = arr(old_offset+i)
            enddo
        endif

        if (remainder_n+remainder_n-1_ik <= huge(0_ik) / half_n) then
            do tail = 1_ik, remainder_n
                dist = 1_ik + (tail+tail-1_ik) * half_n / remainder_n
                source_offset = old_offset + (n_resized+tail-1_ik) * stride
                target_offset = new_offset + (dist-1_ik) * stride

                arr(target_offset+1_ik:target_offset+stride) = arr(target_offset+1_ik:target_offset+stride) + &
                                                             & arr(source_offset+1_ik:source_offset+stride)
            enddo
        else
            position                = half_n / remainder_n
            position_remainder      = modulo(half_n, remainder_n)
            position_step           = n_resized / remainder_n
            position_step_remainder = modulo(n_resized, remainder_n)

            do tail = 1_ik, remainder_n
                dist = 1_ik + position
                source_offset = old_offset + (n_resized+tail-1_ik) * stride
                target_offset = new_offset + (dist-1_ik) * stride

                arr(target_offset+1_ik:target_offset+stride) = arr(target_offset+1_ik:target_offset+stride) + &
                                                             & arr(source_offset+1_ik:source_offset+stride)

                if (tail < remainder_n) then
                    position           = position + position_step
                    position_remainder = position_remainder + position_step_remainder

                    if (position_remainder >= remainder_n) then
                        position_remainder = position_remainder - remainder_n
                        position = position + 1_ik
                    endif
                endif
            enddo
        endif
    enddo

end subroutine RESIZE


pure function largest_power_of_2(n) result(output)
    integer(ik), intent(in) :: n
    integer(ik) :: output
    integer(ik) :: work

    work   = n
    output = 1_ik
    do
        if (work <= 1_ik) then
            exit
        endif
        work = shiftr(work, 1)
        output = output + output
    enddo

end function largest_power_of_2


