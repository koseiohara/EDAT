
subroutine PROC(n, howmany, stride, iarr, oarr)
    use, intrinsic :: iso_fortran_env, only : ik=>int64
#ifdef DEBUG
    use, intrinsic :: iso_fortran_env, only : ounit=>output_unit
#endif

    integer(ik), intent(in) :: n
    integer(ik), intent(in) :: howmany
    integer(ik), intent(in) :: stride
    real(RK)   , intent(inout), contiguous :: iarr(:)
    real(RK)   , intent(out)  , contiguous :: oarr(:)

    real(RK), allocatable :: work_arr(:)
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

    work_n = n
    half_n = shiftr(work_n, 1)
    nwork  = half_n * howmany * stride

    allocate(work_arr(nwork))

    do
        if (work_n > 2) then
            work_n = shiftr(work_n, 1)
            do i = 0, howmany-1
#ifdef DEBUG
                write(ounit,'(A,I0)') 'i = ', i
#endif
                skip_iarr = i *      n * stride
                skip_work = i * half_n * stride

                idx1 = 1
                idx2 = 1 + stride
                do j = 1, work_n
#ifdef DEBUG
                    write(ounit,'(A,I0)') 'j = ', j
#endif
                    ioff = skip_work + (j - 1) * stride
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
            enddo

            work_n = shiftr(work_n, 1)
            do i = 0, howmany-1
#ifdef DEBUG
                write(ounit,'(A,I0)') 'i = ', i
#endif
                skip_iarr = i *      n * stride
                skip_work = i * half_n * stride

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
                                                        & ') = work_arr(', skip_work+idx1,&
                                                        & ') + work_arr(', skip_work+idx2, ')'
#endif
                        iarr(ioff+k) = work_arr(skip_work+idx1) + work_arr(skip_work+idx2)

                        idx1 = idx1 + 1
                        idx2 = idx1 + stride
                    enddo
                    idx1 = idx2
                    idx2 = idx1 + stride
                enddo
            enddo

            cycle
        else
            exit
        endif
    enddo

    if (work_n == 2) then
        do i = 0, howmany-1
            skip_iarr = i * n * stride
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
            skip_iarr = i * n * stride
            skip_work = i * stride
            do j = 1, stride
#ifdef DEBUG
            write(ounit,'(A,I0,A,I0,A,I0,A)') 'oarr(', skip_work+j, ') = iarr(', skip_iarr+j, ')'
#endif
                oarr(skip_work+j) = iarr(skip_iarr+j)
            enddo
        enddo
    endif

    deallocate(work_arr)

end subroutine PROC
