

module c_sum_hp_interface

    implicit none

    interface
        pure function c_sum_hp_sp(n, arr) result(output) bind(C)
            use, intrinsic :: iso_c_binding
            integer(C_LONG), intent(in) :: n
            real(C_FLOAT)  , intent(in) :: arr(n)
            real(C_FLOAT) :: output
        end function c_sum_hp_sp
    end interface

    ! interface
    !     pure function c_sum_hp_dp(n, arr) result(output) bind(C)
    !         use, intrinsic :: iso_c_binding
    !         integer(C_LONG), intent(in) :: n
    !         real(C_DOUBLE) , intent(in) :: arr(n)
    !         real(C_DOUBLE) :: output
    !     end function c_sum_hp_dp
    ! end interface

end module c_sum_hp_interface


