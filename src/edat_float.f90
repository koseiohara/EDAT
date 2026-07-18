

module edat_float

    implicit none

    private
    public :: isclose

    interface isclose
        module procedure isclose_sp
        module procedure isclose_dp
        module procedure isclose_qp
    end interface isclose

    contains

    pure elemental function isclose_sp(a, b, rel_tol, abs_tol) result(output)
        use, intrinsic :: iso_fortran_env, only : rk=>real32
        real(rk), parameter :: rel_default = 1.E-4_rk
        real(rk), parameter :: abs_default = 0._rk

        real(rk), intent(in) :: a
        real(rk), intent(in) :: b
        real(rk), intent(in), optional :: rel_tol
        real(rk), intent(in), optional :: abs_tol

        real(rk) :: diff_abs
        real(rk) :: limit
        real(rk) :: work_rel_tol
        real(rk) :: work_abs_tol

        logical :: output

        if (present(rel_tol)) then
            work_rel_tol = rel_tol
        else
            work_rel_tol = rel_default
        endif

        if (present(abs_tol)) then
            work_abs_tol = abs_tol
        else
            work_abs_tol = abs_default
        endif

        diff_abs = abs(a - b)
        limit    = max(work_rel_tol * max(abs(a), abs(b)), work_abs_tol)

        output = (diff_abs <= limit)

    end function isclose_sp


    pure elemental function isclose_dp(a, b, rel_tol, abs_tol) result(output)
        use, intrinsic :: iso_fortran_env, only : rk=>real64
        real(rk), parameter :: rel_default = 1.E-13_rk
        real(rk), parameter :: abs_default = 0._rk

        real(rk), intent(in) :: a
        real(rk), intent(in) :: b
        real(rk), intent(in), optional :: rel_tol
        real(rk), intent(in), optional :: abs_tol

        real(rk) :: diff_abs
        real(rk) :: limit
        real(rk) :: work_rel_tol
        real(rk) :: work_abs_tol

        logical :: output

        if (present(rel_tol)) then
            work_rel_tol = rel_tol
        else
            work_rel_tol = rel_default
        endif

        if (present(abs_tol)) then
            work_abs_tol = abs_tol
        else
            work_abs_tol = abs_default
        endif

        diff_abs = abs(a - b)
        limit    = max(work_rel_tol * max(abs(a), abs(b)), work_abs_tol)

        output = (diff_abs <= limit)

    end function isclose_dp


    pure elemental function isclose_qp(a, b, rel_tol, abs_tol) result(output)
        use, intrinsic :: iso_fortran_env, only : rk=>real128
        real(rk), parameter :: rel_default = 1.E-31_rk
        real(rk), parameter :: abs_default = 0._rk

        real(rk), intent(in) :: a
        real(rk), intent(in) :: b
        real(rk), intent(in), optional :: rel_tol
        real(rk), intent(in), optional :: abs_tol

        real(rk) :: diff_abs
        real(rk) :: limit
        real(rk) :: work_rel_tol
        real(rk) :: work_abs_tol

        logical :: output

        if (present(rel_tol)) then
            work_rel_tol = rel_tol
        else
            work_rel_tol = rel_default
        endif

        if (present(abs_tol)) then
            work_abs_tol = abs_tol
        else
            work_abs_tol = abs_default
        endif

        diff_abs = abs(a - b)
        limit    = max(work_rel_tol * max(abs(a), abs(b)), work_abs_tol)

        output = (diff_abs <= limit)

    end function isclose_qp

end module edat_float



