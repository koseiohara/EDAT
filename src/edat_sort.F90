module EDAT_Sort
    use iso_c_binding

    implicit none

    private
    public :: quick_sort

    interface quick_sort
        module procedure &
            & quick_sort_i4, &
            & quick_sort_sp, &
            & quick_sort_dp
    end interface quick_sort

    interface
        pure subroutine c_quick_sort_i4(n, array) bind(C)
            use iso_c_binding
            integer(C_LONG), intent(in)    :: n
            integer(C_INT) , intent(inout) :: array(n)
        end subroutine c_quick_sort_i4
    end interface


    interface
        pure subroutine c_quick_sort_sp(n, array) bind(C)
            use iso_c_binding
            integer(C_LONG), intent(in)    :: n
            real(C_FLOAT)  , intent(inout) :: array(n)
        end subroutine c_quick_sort_sp
    end interface


    interface
        pure subroutine c_quick_sort_dp(n, array) bind(C)
            use iso_c_binding
            integer(C_LONG), intent(in)    :: n
            real(C_DOUBLE) , intent(inout) :: array(n)
        end subroutine c_quick_sort_dp
    end interface


    !interface
    !    subroutine c_quick_sort_qp(n, array) bind(C)
    !        use iso_c_binding
    !        integer(c_int), intent(in)         :: n
    !        real(c_long_double), intent(inout) :: array(n)
    !    end subroutine c_quick_sort_qp
    !end interface


    contains


    pure subroutine quick_sort_i4(array)
        use, intrinsic :: iso_fortran_env, only : lik=>int32
        integer(lik), intent(inout) :: array(:)
        integer(C_LONG) :: n
        integer(C_INT), allocatable :: work_arr(:)

        n = size(array, kind=C_LONG)
        allocate(work_arr(n))

        work_arr(1:n) = int(array(1:n), kind=C_INT)

        call c_quick_sort_i4(n, work_arr(1:n))
        array(1:n) = int(work_arr(1:n), kind=lik)

    end subroutine quick_sort_i4


    pure subroutine quick_sort_sp(array)
        use, intrinsic :: iso_fortran_env, only : lrk=>real32
        use, intrinsic :: iso_c_binding  , only : crk=>C_FLOAT
        real(lrk), intent(inout) :: array(:)
        integer(C_LONG) :: n
        real(crk), allocatable :: work_arr(:)

        n = size(array, kind=C_LONG)
        allocate(work_arr(n))

        work_arr(1:n) = real(array(1:n), kind=crk)

        call c_quick_sort_sp(n, work_arr(1:n))
        array(1:n) = real(work_arr(1:n), kind=lrk)

    end subroutine quick_sort_sp


    pure subroutine quick_sort_dp(array)
        use, intrinsic :: iso_fortran_env, only : lrk=>real64
        use, intrinsic :: iso_c_binding  , only : crk=>C_DOUBLE
        real(lrk), intent(inout) :: array(:)
        integer(C_LONG) :: n
        real(crk), allocatable :: work_arr(:)

        n = size(array, kind=C_LONG)
        allocate(work_arr(n))

        work_arr(1:n) = real(array(1:n), kind=crk)

        call c_quick_sort_dp(n, work_arr(1:n))
        array(1:n) = real(work_arr(1:n), kind=lrk)

    end subroutine quick_sort_dp


    !subroutine quick_sort_qp(n, array)
    !    integer , parameter :: lrk = c_long_double
    !    integer , intent(in)    :: n
    !    real(lrk), intent(inout) :: array(n)

    !    call c_quick_sort_qp(n, array(1:n))

    !end subroutine quick_sort_qp


end module EDAT_Sort

