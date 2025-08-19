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
            integer(c_int), intent(in)    :: n
            integer(c_int), intent(inout) :: array(n)
        end subroutine c_quick_sort_i4
    end interface


    interface
        pure subroutine c_quick_sort_sp(n, array) bind(C)
            use iso_c_binding
            integer(c_int), intent(in)    :: n
            real(c_float) , intent(inout) :: array(n)
        end subroutine c_quick_sort_sp
    end interface


    interface
        pure subroutine c_quick_sort_dp(n, array) bind(C)
            use iso_c_binding
            integer(c_int), intent(in)    :: n
            real(c_double), intent(inout) :: array(n)
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


    pure subroutine quick_sort_i4(n, array)
        integer, intent(in)    :: n
        integer, intent(inout) :: array(n)

        call c_quick_sort_i4(n, array(1:n))

    end subroutine quick_sort_i4


    pure subroutine quick_sort_sp(n, array)
        integer , parameter :: lrk = c_float
        integer , intent(in)    :: n
        real(lrk), intent(inout) :: array(n)

        call c_quick_sort_sp(n, array(1:n))

    end subroutine quick_sort_sp


    pure subroutine quick_sort_dp(n, array)
        integer , parameter :: lrk = c_double
        integer , intent(in)    :: n
        real(lrk), intent(inout) :: array(n)

        call c_quick_sort_dp(n, array(1:n))

    end subroutine quick_sort_dp


    !subroutine quick_sort_qp(n, array)
    !    integer , parameter :: lrk = c_long_double
    !    integer , intent(in)    :: n
    !    real(lrk), intent(inout) :: array(n)

    !    call c_quick_sort_qp(n, array(1:n))

    !end subroutine quick_sort_qp


end module EDAT_Sort

