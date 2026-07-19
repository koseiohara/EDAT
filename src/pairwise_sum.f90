

module pairwise_sum

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
    end interface sum_hp_core

    contains


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


    pure function sum_hp_sp(arr) result(output)
        use, intrinsic :: iso_fortran_env, only : rk=>real32
        real(rk), intent(in) :: arr(:)
        real(rk) :: output

        real(rk), allocatable :: workspace(:)
        integer(ik) :: n

        n = size(arr, kind=ik)
        if (n <= 3_ik) then
            if (n == 0_ik) then
                output = 0._rk
            else if (n == 1_ik) then
                output = arr(1)
            else if (n == 2_ik) then
                output = arr(1) + arr(2)
            else
                output = arr(1) + arr(2) + arr(3)
            endif

            return
        endif

        allocate(workspace(n))
        workspace(1:n) = arr(1:n)

        call sum_hp_core(n             , &  !! IN
                       & workspace(1:n), &  !! IN
                       & output          )  !! IN

    end function sum_hp_sp


    pure function sum_hp_dp(arr) result(output)
        use, intrinsic :: iso_fortran_env, only : rk=>real64
        real(rk), intent(in) :: arr(:)
        real(rk) :: output

        real(rk), allocatable :: workspace(:)
        integer(ik) :: n

        n = size(arr, kind=ik)
        if (n <= 3_ik) then
            if (n == 0_ik) then
                output = 0._rk
            else if (n == 1_ik) then
                output = arr(1)
            else if (n == 2_ik) then
                output = arr(1) + arr(2)
            else
                output = arr(1) + arr(2) + arr(3)
            endif

            return
        endif

        allocate(workspace(n))
        workspace(1:n) = arr(1:n)

        call sum_hp_core(n             , &  !! IN
                       & workspace(1:n), &  !! IN
                       & output          )  !! IN

    end function sum_hp_dp


    pure function sum_hp_qp(arr) result(output)
        use, intrinsic :: iso_fortran_env, only : rk=>real128
        real(rk), intent(in) :: arr(:)
        real(rk) :: output

        real(rk), allocatable :: workspace(:)
        integer(ik) :: n

        n = size(arr, kind=ik)
        if (n <= 3_ik) then
            if (n == 0_ik) then
                output = 0._rk
            else if (n == 1_ik) then
                output = arr(1)
            else if (n == 2_ik) then
                output = arr(1) + arr(2)
            else
                output = arr(1) + arr(2) + arr(3)
            endif

            return
        endif

        allocate(workspace(n))
        workspace(1:n) = arr(1:n)

        call sum_hp_core(n             , &  !! IN
                       & workspace(1:n), &  !! IN
                       & output          )  !! IN

    end function sum_hp_qp


    pure subroutine sum_hp_core_sp(n, arr, output)
        use, intrinsic :: iso_fortran_env, only : rk=>real32
        integer(ik), intent(in) :: n
        real(rk)   , intent(inout), contiguous :: arr(:)
        real(rk)   , intent(out)               :: output

        real(rk), allocatable :: work_arr(:)
        integer(ik) :: len
        integer(ik) :: newlen
        integer(ik) :: half
        integer(ik) :: i
        integer(ik) :: k
        integer(ik) :: dist

        integer(ik) :: pos
        integer(ik) :: rem
        integer(ik) :: step
        integer(ik) :: step_rem

        len = largest_power_of_2(n)
        k   = n - len

        half = shiftr(len, 1)
        allocate(work_arr(half))

        if (k+k-1_ik <= huge(0_ik) / half) then
            do i = 1_ik, k
                dist = 1_ik + (i+i-1_ik) * half / k
                arr(dist) = arr(dist) + arr(len+i)
            enddo
        else
            pos      = half / k
            rem      = modulo(half, k)
            step     = len / k
            step_rem = modulo(len, k)

            do i = 1_ik, k
                dist = 1_ik + pos
                arr(dist) = arr(dist) + arr(len+i)
                if (i < k) then
                    pos = pos + step
                    rem = rem + step_rem
                    if (rem >= k) then
                        rem = rem - k
                        pos = pos + 1_ik
                    endif
                endif
            enddo
        endif

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
            work_arr(1_ik:newlen) = arr(1_ik:len-1_ik:2_ik) + arr(2_ik:len:2_ik)

            len = shiftr(newlen, 1)
            arr(1_ik:len) = work_arr(1_ik:newlen-1_ik:2_ik) + work_arr(2_ik:newlen:2_ik)
        enddo

    end subroutine sum_hp_core_sp


    pure subroutine sum_hp_core_dp(n, arr, output)
        use, intrinsic :: iso_fortran_env, only : rk=>real64
        integer(ik), intent(in) :: n
        real(rk)   , intent(inout), contiguous :: arr(:)
        real(rk)   , intent(out)               :: output

        real(rk), allocatable :: work_arr(:)
        integer(ik) :: len
        integer(ik) :: newlen
        integer(ik) :: half
        integer(ik) :: i
        integer(ik) :: k
        integer(ik) :: dist

        integer(ik) :: pos
        integer(ik) :: rem
        integer(ik) :: step
        integer(ik) :: step_rem

        len = largest_power_of_2(n)
        k   = n - len

        half = shiftr(len, 1)
        allocate(work_arr(half))

        if (k+k-1_ik <= huge(0_ik) / half) then
            do i = 1_ik, k
                dist = 1_ik + (i+i-1_ik) * half / k
                arr(dist) = arr(dist) + arr(len+i)
            enddo
        else
            pos      = half / k
            rem      = modulo(half, k)
            step     = len / k
            step_rem = modulo(len, k)

            do i = 1_ik, k
                dist = 1_ik + pos
                arr(dist) = arr(dist) + arr(len+i)
                if (i < k) then
                    pos = pos + step
                    rem = rem + step_rem
                    if (rem >= k) then
                        rem = rem - k
                        pos = pos + 1_ik
                    endif
                endif
            enddo
        endif

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
            work_arr(1_ik:newlen) = arr(1_ik:len-1_ik:2_ik) + arr(2_ik:len:2_ik)

            len = shiftr(newlen, 1)
            arr(1_ik:len) = work_arr(1_ik:newlen-1_ik:2_ik) + work_arr(2_ik:newlen:2_ik)
        enddo

    end subroutine sum_hp_core_dp


    pure subroutine sum_hp_core_qp(n, arr, output)
        use, intrinsic :: iso_fortran_env, only : rk=>real128
        integer(ik), intent(in) :: n
        real(rk)   , intent(inout), contiguous :: arr(:)
        real(rk)   , intent(out)               :: output

        real(rk), allocatable :: work_arr(:)
        integer(ik) :: len
        integer(ik) :: newlen
        integer(ik) :: half
        integer(ik) :: i
        integer(ik) :: k
        integer(ik) :: dist

        integer(ik) :: pos
        integer(ik) :: rem
        integer(ik) :: step
        integer(ik) :: step_rem

        len = largest_power_of_2(n)
        k   = n - len

        half = shiftr(len, 1)
        allocate(work_arr(half))

        if (k+k-1_ik <= huge(0_ik) / half) then
            do i = 1_ik, k
                dist = 1_ik + (i+i-1_ik) * half / k
                arr(dist) = arr(dist) + arr(len+i)
            enddo
        else
            pos      = half / k
            rem      = modulo(half, k)
            step     = len / k
            step_rem = modulo(len, k)

            do i = 1_ik, k
                dist = 1_ik + pos
                arr(dist) = arr(dist) + arr(len+i)
                if (i < k) then
                    pos = pos + step
                    rem = rem + step_rem
                    if (rem >= k) then
                        rem = rem - k
                        pos = pos + 1_ik
                    endif
                endif
            enddo
        endif

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
            work_arr(1_ik:newlen) = arr(1_ik:len-1_ik:2_ik) + arr(2_ik:len:2_ik)

            len = shiftr(newlen, 1)
            arr(1_ik:len) = work_arr(1_ik:newlen-1_ik:2_ik) + work_arr(2_ik:newlen:2_ik)
        enddo

    end subroutine sum_hp_core_qp

end module pairwise_sum


