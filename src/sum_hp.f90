

module sum_hp

    use, intrinsic :: iso_fortran_env, only : ik=>int64

    implicit none

    private
    public :: sum_hp

    interface sum_hp
        module procedure sum_hp_sp
        module procedure sum_hp_dp
        module procedure sum_hp_qp
    end interface sum_hp

    interface sum_hp_core
        module procedure sum_hp_core_sp
        module procedure sum_hp_core_dp
        module procedure sum_hp_core_qp
    end interface sum_hp

    contains


    pure function largest_power_of_2(n) result(output)
        integer(ik), intent(in) :: n
        integer(ik) :: output
        integer(ik) :: work

        work   = n
        output = 1_ik
        do
            if (work > 1_ik) then
                exit
            endif
            work = shiftr(work, 1)
            output = output + output
        enddo

    end function largest_power_of_2


    pure function sum_hp_sp(arr) result(output)
        use, intrinsic :: iso_fortran_env, only : rk=>real32
        real(rk), intent(in) :: arr(:)
        real(rk) :: output

        real(rk), allocatable :: workspace(:)
        integer(ik) :: n

        n = size(arr)
        if (n < 2_ik) then
            if (n == 0_ik) then
                output = 0._rk
            else if (n == 1_ik) then
                output = arr(1)
            else
                output = arr(1) + arr(2)
            endif

            return
        endif

        allocate(workspace(n))

        call sum_hp_core(n             , &  !! IN
                       & workspace(1:n), &  !! IN
                       & output          )  !! IN

    end function sum_hp_sp


    pure subroutine sum_hp_core_sp(n, arr, output)
        use, intrinsic :: iso_fortran_env, only : rk=>real32
        integer(ik), intent(in) :: n
        real(rk)   , intent(out), contiguous :: arr(:)
        real(rk)   , intent(out)             :: output

        real(rk), allocatable :: work_arr(:)
        integer(ik) :: len
        integer(ik) :: newlen
        integer(ik) :: half
        integer(ik) :: i
        integer(ik) :: i2
        integer(ik) :: k
        integer(ik) :: dist

        len = largest_power_of_2(n)
        k   = n - len

        half = shiftr(len, 1)
        allocate(work_arr(half))

        do i = 1_ik, k
            dist = 1_ik + (i+i-1) * half / k
            arr(dist) = arr(dist) + arr(len+1)
        enddo

        do
            if (len < 16_ik) then
                exit
            endif

            newlen = shiftr(len, 1)

#ifdef VECTOR
            work_arr(1:newlen) = arr(1:len-1:2) + arr(2:len:2)
#else
            do i = 1_ik, newlen, 8_ik
                i2 = i + i
                work_arr(i     ) = arr(i2- 1_ik) + arr(i2      )
                work_arr(i+1_ik) = arr(i2+ 1_ik) + arr(i2+ 2_ik)
                work_arr(i+2_ik) = arr(i2+ 3_ik) + arr(i2+ 4_ik)
                work_arr(i+3_ik) = arr(i2+ 5_ik) + arr(i2+ 6_ik)
                work_arr(i+4_ik) = arr(i2+ 7_ik) + arr(i2+ 8_ik)
                work_arr(i+5_ik) = arr(i2+ 9_ik) + arr(i2+10_ik)
                work_arr(i+6_ik) = arr(i2+11_ik) + arr(i2+12_ik)
                work_arr(i+7_ik) = arr(i2+13_ik) + arr(i2+14_ik)
            enddo
#endif

            len = shiftr(newlen, 1)

#ifdef VECTOR
            arr(1:len) = work_arr(1:newlen-1:2) + work_arr(2:newlen:2)
#else
            do i = 1_ik, len, 4_ik
                i2 = i + i
                arr(i     ) = work_arr(i2-1_ik) + work_arr(i2     )
                arr(i+1_ik) = work_arr(i2+1_ik) + work_arr(i2+2_ik)
                arr(i+2_ik) = work_arr(i2+3_ik) + work_arr(i2+4_ik)
                arr(i+3_ik) = work_arr(i2+5_ik) + work_arr(i2+6_ik)
            enddo
#endif
        enddo

        do
            if (len <= 2_ik) then
                if (len == 1_ik) then
                    output = arr(1)
                else
                    output = arr(1) + arr(2)
                endif
                deallocate(work_arr)
                return
            endif

            newlen = shiftr(len, 1)

#ifdef VECTOR
            work_arr(1:newlen) = arr(1:len-1:2) + arr(2:len:2)
#else
            do i = 1_ik, newlen
                i2 = i + i
                work_arr(i) = arr(i2-1_ik) + arr(i2)
            enddo
#endif

            len = shiftr(newlen, 1)

#ifdef VECTOR
            arr(1:len) = work_arr(1:newlen-1:2) + work_arr(2:newlen:2)
#else
            do i = 1_ik, len
                i2 = i + i
                arr(i) = work_arr(i2-1_ik) + arr(i2)
            enddo
#endif
        enddo

    end subroutine sum_hp_core_sp

end module sum_hp


