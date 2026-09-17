program test_math_multidim
    use, intrinsic :: iso_fortran_env, only : real64
    use :: EDAT_Math, only : sum_hp
    use :: test_support, only : check, finish_tests

    implicit none

    call test_rank_2
    call test_rank_3
    call test_rank_4
    call test_rank_5
    call test_rank_6
    call test_rank_7
    call test_rank_8
    call test_rank_9
    call test_rank_10
    call test_zero_extents

    call finish_tests('test_math_multidim')

    contains


    subroutine test_rank_2
        real(real64) :: arr(2,3)
        integer :: i

        arr(1:2,1:3) = reshape([(real(i, kind=real64), i = 1, 6)], [2,3])

        call check(abs(sum_hp(arr(1:2,1:3)) - sum(arr(1:2,1:3))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 2 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3), 1)) == &
                 & shape(sum(arr(1:2,1:3), 1))), &  !! IN
                 & 'sum_hp rank 2 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3), 1) - &
                 & sum(arr(1:2,1:3), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 2 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3), 2)) == &
                 & shape(sum(arr(1:2,1:3), 2))), &  !! IN
                 & 'sum_hp rank 2 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3), 2) - &
                 & sum(arr(1:2,1:3), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 2 dim 2 values match sum'  )  !! IN
    end subroutine test_rank_2


    subroutine test_rank_3
        real(real64) :: arr(2,3,4)
        integer :: i

        arr(1:2,1:3,1:4) = reshape([(real(i, kind=real64), i = 1, 24)], [2,3,4])

        call check(abs(sum_hp(arr(1:2,1:3,1:4)) - sum(arr(1:2,1:3,1:4))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 3 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4), 1))), &  !! IN
                 & 'sum_hp rank 3 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4), 1) - &
                 & sum(arr(1:2,1:3,1:4), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4), 2))), &  !! IN
                 & 'sum_hp rank 3 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4), 2) - &
                 & sum(arr(1:2,1:3,1:4), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4), 3))), &  !! IN
                 & 'sum_hp rank 3 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4), 3) - &
                 & sum(arr(1:2,1:3,1:4), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 dim 3 values match sum'  )  !! IN
    end subroutine test_rank_3


    subroutine test_rank_4
        real(real64) :: arr(2,3,4,2)
        integer :: i

        arr(1:2,1:3,1:4,1:2) = reshape([(real(i, kind=real64), i = 1, 48)], [2,3,4,2])

        call check(abs(sum_hp(arr(1:2,1:3,1:4,1:2)) - sum(arr(1:2,1:3,1:4,1:2))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 4 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2), 1))), &  !! IN
                 & 'sum_hp rank 4 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2), 1) - &
                 & sum(arr(1:2,1:3,1:4,1:2), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 4 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2), 2))), &  !! IN
                 & 'sum_hp rank 4 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2), 2) - &
                 & sum(arr(1:2,1:3,1:4,1:2), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 4 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2), 3))), &  !! IN
                 & 'sum_hp rank 4 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2), 3) - &
                 & sum(arr(1:2,1:3,1:4,1:2), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 4 dim 3 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2), 4)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2), 4))), &  !! IN
                 & 'sum_hp rank 4 dim 4 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2), 4) - &
                 & sum(arr(1:2,1:3,1:4,1:2), 4)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 4 dim 4 values match sum'  )  !! IN
    end subroutine test_rank_4


    subroutine test_rank_5
        real(real64) :: arr(2,3,4,2,3)
        integer :: i

        arr(1:2,1:3,1:4,1:2,1:3) = reshape([(real(i, kind=real64), i = 1, 144)], [2,3,4,2,3])

        call check(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3)) - sum(arr(1:2,1:3,1:4,1:2,1:3))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 5 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3), 1))), &  !! IN
                 & 'sum_hp rank 5 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 1) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 5 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3), 2))), &  !! IN
                 & 'sum_hp rank 5 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 2) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 5 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3), 3))), &  !! IN
                 & 'sum_hp rank 5 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 3) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 5 dim 3 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 4)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3), 4))), &  !! IN
                 & 'sum_hp rank 5 dim 4 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 4) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3), 4)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 5 dim 4 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 5)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3), 5))), &  !! IN
                 & 'sum_hp rank 5 dim 5 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3), 5) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3), 5)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 5 dim 5 values match sum'  )  !! IN
    end subroutine test_rank_5


    subroutine test_rank_6
        real(real64) :: arr(2,3,4,2,3,4)
        integer :: i

        arr(1:2,1:3,1:4,1:2,1:3,1:4) = reshape([(real(i, kind=real64), i = 1, 576)], [2,3,4,2,3,4])

        call check(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4)) - sum(arr(1:2,1:3,1:4,1:2,1:3,1:4))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 6 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 1))), &  !! IN
                 & 'sum_hp rank 6 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 1) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 6 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 2))), &  !! IN
                 & 'sum_hp rank 6 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 2) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 6 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 3))), &  !! IN
                 & 'sum_hp rank 6 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 3) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 6 dim 3 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 4)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 4))), &  !! IN
                 & 'sum_hp rank 6 dim 4 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 4) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 4)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 6 dim 4 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 5)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 5))), &  !! IN
                 & 'sum_hp rank 6 dim 5 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 5) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 5)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 6 dim 5 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 6)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 6))), &  !! IN
                 & 'sum_hp rank 6 dim 6 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4), 6) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4), 6)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 6 dim 6 values match sum'  )  !! IN
    end subroutine test_rank_6


    subroutine test_rank_7
        real(real64) :: arr(2,3,4,2,3,4,2)
        integer :: i

        arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2) = reshape([(real(i, kind=real64), i = 1, 1152)], [2,3,4,2,3,4,2])

        call check(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2)) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 7 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1))), &  !! IN
                 & 'sum_hp rank 7 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 7 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2))), &  !! IN
                 & 'sum_hp rank 7 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 7 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3))), &  !! IN
                 & 'sum_hp rank 7 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 7 dim 3 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4))), &  !! IN
                 & 'sum_hp rank 7 dim 4 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 7 dim 4 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5))), &  !! IN
                 & 'sum_hp rank 7 dim 5 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 7 dim 5 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6))), &  !! IN
                 & 'sum_hp rank 7 dim 6 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 7 dim 6 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7))), &  !! IN
                 & 'sum_hp rank 7 dim 7 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 7 dim 7 values match sum'  )  !! IN
    end subroutine test_rank_7


    subroutine test_rank_8
        real(real64) :: arr(2,3,4,2,3,4,2,3)
        integer :: i

        arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3) = reshape([(real(i, kind=real64), i = 1, 3456)], [2,3,4,2,3,4,2,3])

        call check(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3)) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 8 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 1))), &  !! IN
                 & 'sum_hp rank 8 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 1) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 2))), &  !! IN
                 & 'sum_hp rank 8 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 2) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 3))), &  !! IN
                 & 'sum_hp rank 8 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 3) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 3 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 4)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 4))), &  !! IN
                 & 'sum_hp rank 8 dim 4 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 4) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 4)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 4 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 5)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 5))), &  !! IN
                 & 'sum_hp rank 8 dim 5 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 5) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 5)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 5 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 6)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 6))), &  !! IN
                 & 'sum_hp rank 8 dim 6 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 6) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 6)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 6 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 7)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 7))), &  !! IN
                 & 'sum_hp rank 8 dim 7 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 7) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 7)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 7 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 8)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 8))), &  !! IN
                 & 'sum_hp rank 8 dim 8 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 8) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3), 8)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 8 dim 8 values match sum'  )  !! IN
    end subroutine test_rank_8


    subroutine test_rank_9
        real(real64) :: arr(2,3,4,2,3,4,2,3,4)
        integer :: i

        arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4) = reshape([(real(i, kind=real64), i = 1, 13824)], [2,3,4,2,3,4,2,3,4])

        call check(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4)) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 9 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 1))), &  !! IN
                 & 'sum_hp rank 9 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 1) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 2))), &  !! IN
                 & 'sum_hp rank 9 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 2) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 3))), &  !! IN
                 & 'sum_hp rank 9 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 3) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 3 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 4)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 4))), &  !! IN
                 & 'sum_hp rank 9 dim 4 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 4) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 4)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 4 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 5)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 5))), &  !! IN
                 & 'sum_hp rank 9 dim 5 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 5) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 5)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 5 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 6)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 6))), &  !! IN
                 & 'sum_hp rank 9 dim 6 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 6) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 6)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 6 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 7)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 7))), &  !! IN
                 & 'sum_hp rank 9 dim 7 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 7) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 7)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 7 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 8)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 8))), &  !! IN
                 & 'sum_hp rank 9 dim 8 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 8) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 8)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 8 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 9)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 9))), &  !! IN
                 & 'sum_hp rank 9 dim 9 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 9) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4), 9)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 9 dim 9 values match sum'  )  !! IN
    end subroutine test_rank_9


    subroutine test_rank_10
        real(real64) :: arr(2,3,4,2,3,4,2,3,4,2)
        integer :: i

        arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2) = reshape([(real(i, kind=real64), i = 1, 27648)], [2,3,4,2,3,4,2,3,4,2])

        call check(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2)) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 10 full matches sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1))), &  !! IN
                 & 'sum_hp rank 10 dim 1 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 1 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2))), &  !! IN
                 & 'sum_hp rank 10 dim 2 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 2 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3))), &  !! IN
                 & 'sum_hp rank 10 dim 3 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 3 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4))), &  !! IN
                 & 'sum_hp rank 10 dim 4 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 4)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 4 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5))), &  !! IN
                 & 'sum_hp rank 10 dim 5 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 5)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 5 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6))), &  !! IN
                 & 'sum_hp rank 10 dim 6 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 6)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 6 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7))), &  !! IN
                 & 'sum_hp rank 10 dim 7 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 7)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 7 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 8)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 8))), &  !! IN
                 & 'sum_hp rank 10 dim 8 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 8) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 8)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 8 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 9)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 9))), &  !! IN
                 & 'sum_hp rank 10 dim 9 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 9) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 9)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 9 values match sum'  )  !! IN
        call check(all(shape(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 10)) == &
                 & shape(sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 10))), &  !! IN
                 & 'sum_hp rank 10 dim 10 shape matches sum'  )  !! IN
        call check(all(abs(sum_hp(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 10) - &
                 & sum(arr(1:2,1:3,1:4,1:2,1:3,1:4,1:2,1:3,1:4,1:2), 10)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 10 dim 10 values match sum'  )  !! IN
    end subroutine test_rank_10


    subroutine test_zero_extents
        real(real64), allocatable :: arr2(:,:)
        real(real64), allocatable :: arr3(:,:,:)

        allocate(arr2(0,3))
        call check(abs(sum_hp(arr2(1:0,1:3)) - sum(arr2(1:0,1:3))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 2 empty full matches sum'                             )  !! IN
        call check(all(shape(sum_hp(arr2(1:0,1:3), 1)) == shape(sum(arr2(1:0,1:3), 1))), &  !! IN
                 & 'sum_hp rank 2 zero reduction extent shape'                              )  !! IN
        call check(all(abs(sum_hp(arr2(1:0,1:3), 1) - sum(arr2(1:0,1:3), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 2 zero reduction extent values'                                  )  !! IN
        call check(all(shape(sum_hp(arr2(1:0,1:3), 2)) == shape(sum(arr2(1:0,1:3), 2))), &  !! IN
                 & 'sum_hp rank 2 zero output extent shape'                               )  !! IN
        call check(all(abs(sum_hp(arr2(1:0,1:3), 2) - sum(arr2(1:0,1:3), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 2 zero output extent values'                                  )  !! IN
        deallocate(arr2)

        allocate(arr3(2,0,4))
        call check(abs(sum_hp(arr3(1:2,1:0,1:4)) - sum(arr3(1:2,1:0,1:4))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 3 empty full matches sum'                                         )  !! IN
        call check(all(abs(sum_hp(arr3(1:2,1:0,1:4), 2) - sum(arr3(1:2,1:0,1:4), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 zero reduction extent values'                                          )  !! IN
        call check(all(abs(sum_hp(arr3(1:2,1:0,1:4), 1) - sum(arr3(1:2,1:0,1:4), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 zero output extent dim 1'                                               )  !! IN
        call check(all(abs(sum_hp(arr3(1:2,1:0,1:4), 3) - sum(arr3(1:2,1:0,1:4), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 zero output extent dim 3'                                               )  !! IN
        deallocate(arr3)

        allocate(arr3(0,0,0))
        call check(abs(sum_hp(arr3(1:0,1:0,1:0)) - sum(arr3(1:0,1:0,1:0))) <= 0.0_real64, &  !! IN
                 & 'sum_hp rank 3 all zero extents full matches sum'                                  )  !! IN
        call check(all(abs(sum_hp(arr3(1:0,1:0,1:0), 1) - sum(arr3(1:0,1:0,1:0), 1)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 all zero extents dim 1'                                                   )  !! IN
        call check(all(abs(sum_hp(arr3(1:0,1:0,1:0), 2) - sum(arr3(1:0,1:0,1:0), 2)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 all zero extents dim 2'                                                   )  !! IN
        call check(all(abs(sum_hp(arr3(1:0,1:0,1:0), 3) - sum(arr3(1:0,1:0,1:0), 3)) <= 0.0_real64), &  !! IN
                 & 'sum_hp rank 3 all zero extents dim 3'                                                   )  !! IN
        deallocate(arr3)
    end subroutine test_zero_extents
end program test_math_multidim
