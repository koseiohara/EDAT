program test_extended
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64, real128
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf
    use :: EDAT_Math, only : mean, variance, covariance, corrcoef
    use :: EDAT_Float, only : isclose
    use :: EDAT_Sort, only : quick_sort
    use :: EDAT_String, only : to_lower, to_upper
    use :: EDAT_Met, only : potential_temperature, meridionalIntegral, verticalIntegral, &
                                            zonalDerivative, meridionalDerivative, verticalDerivative
    use :: EDAT_BinIO, only : finfo, endian_converter
    use :: test_support, only : check, check_close, check_array_close, check_nan, finish_tests

    implicit none

    call test_math_edges
    call test_all_math_kinds
    call test_isclose_edges
    call test_string_edges
    call test_sort_permutation
    call test_derivative_status_and_surface
    call test_integral_status_and_boundaries
    call test_met_all_kinds
    call test_binio_all_ranks_and_kinds
    call test_endian_bytes
    call finish_tests('test_extended')
    contains

    subroutine test_math_edges
        real(real64), allocatable :: empty(:)
        real(real64)              :: one(1)
        real(real64)              :: constant(4)
        real(real64)              :: x(4)
        real(real64)              :: y(4)

        allocate(empty(0))
        one(1:size(one,1)) = 2.0_real64
        constant(1:size(constant,1)) = 3.0_real64
        x(1:size(x,1)) = [-2.0_real64, -1.0_real64, 1.0_real64, 2.0_real64]
        y(1:size(y,1)) = -4.0_real64*x(1:size(x,1)) + 7.0_real64
        call check_close(variance(empty(1:size(empty,1)))            , &  !! IN
                       & 0.0_real64                 , &  !! IN
                       & 0.0_real64                 , &  !! IN
                       & 0.0_real64                 , &  !! IN
                       & 'empty population variance'  )  !! IN
        call check_close(variance(empty(1:size(empty,1)), sample = .TRUE.), &  !! IN
                       & 0.0_real64                    , &  !! IN
                       & 0.0_real64                    , &  !! IN
                       & 0.0_real64                    , &  !! IN
                       & 'empty sample variance'         )  !! IN
        call check_close(covariance(empty(1:size(empty,1)), empty(1:size(empty,1)))     , &  !! IN
                       & 0.0_real64                   , &  !! IN
                       & 0.0_real64                   , &  !! IN
                       & 0.0_real64                   , &  !! IN
                       & 'empty population covariance'  )  !! IN
        call check_close(covariance(empty(1:size(empty,1)), empty(1:size(empty,1)), sample = .TRUE.), &  !! IN
                       & 0.0_real64                             , &  !! IN
                       & 0.0_real64                             , &  !! IN
                       & 0.0_real64                             , &  !! IN
                       & 'empty sample covariance'                )  !! IN
        call check_close(corrcoef(empty(1:size(empty,1)), empty(1:size(empty,1))), &  !! IN
                       & 0.0_real64            , &  !! IN
                       & 0.0_real64            , &  !! IN
                       & 0.0_real64            , &  !! IN
                       & 'empty correlation'     )  !! IN
        call check_nan(variance(one(1:size(one,1)), sample = .TRUE.)        , &  !! IN
                     & 'one-element sample variance is NaN'  )  !! IN
        call check_nan(covariance(one(1:size(one,1)), one(1:size(one,1)), sample = .TRUE.)   , &  !! IN
                     & 'one-element sample covariance is NaN'  )  !! IN
        call check_nan(corrcoef(one(1:size(one,1)), one(1:size(one,1)))              , &  !! IN
                     & 'one-element correlation is NaN'  )  !! IN
        call check_nan(corrcoef(constant(1:size(constant,1)), constant(1:size(constant,1)))       , &  !! IN
                     & 'constant-array correlation is NaN'  )  !! IN
        call check_close(corrcoef(x(1:size(x,1)), y(1:size(y,1)))                , &  !! IN
                       & -1.0_real64                   , &  !! IN
                       & 1.0e-14_real64                , &  !! IN
                       & 1.0e-14_real64                , &  !! IN
                       & 'perfect negative correlation'  )  !! IN
        call check_close(covariance(x(1:size(x,1)), y(1:size(y,1)), sample = .TRUE.), &  !! IN
                       & -40.0_real64/3.0_real64        , &  !! IN
                       & 1.0e-14_real64                 , &  !! IN
                       & 1.0e-14_real64                 , &  !! IN
                       & 'sample covariance'              )  !! IN
        deallocate(empty)
    end subroutine test_math_edges


    subroutine test_all_math_kinds
        real(real32)  :: x4(3)
        real(real32)  :: y4(3)
        real(real64)  :: x8(3)
        real(real64)  :: y8(3)
        real(real128) :: x16(3)
        real(real128) :: y16(3)

        x4(1:size(x4,1)) = [1._real32,2._real32,3._real32]
        y4(1:size(y4,1)) = 2._real32*x4(1:size(x4,1))
        x8(1:size(x8,1)) = real(x4(1:size(x4,1)),real64)
        y8(1:size(y8,1)) = real(y4(1:size(y4,1)),real64)
        x16(1:size(x16,1)) = real(x4(1:size(x4,1)),real128)
        y16(1:size(y16,1)) = real(y4(1:size(y4,1)),real128)
        call check(abs(real(mean(x4(1:size(x4,1))),real64)-2._real64) < 1.e-6_real64, &  !! IN
                 & 'mean real32'                                      )  !! IN
        call check(abs(real(variance(x4(1:size(x4,1))),real64)-2._real64/3._real64) < 1.e-6_real64, &  !! IN
                 & 'variance real32'                                                )  !! IN
        call check(abs(real(covariance(x4(1:size(x4,1)),y4(1:size(y4,1))), &
            & real64)-4._real64/3._real64) < 2.e-6_real64, &  !! IN
                 & 'covariance real32'                                                   )  !! IN
        call check(abs(real(corrcoef(x4(1:size(x4,1)),y4(1:size(y4,1))),real64)-1._real64) < 1.e-6_real64, &  !! IN
                 & 'corrcoef real32'                                         )  !! IN
        call check_close(mean(x8(1:size(x8,1)))     , &  !! IN
                       & 2._real64    , &  !! IN
                       & 0._real64    , &  !! IN
                       & 0._real64    , &  !! IN
                       & 'mean real64'  )  !! IN
        call check(abs(real(mean(x16(1:size(x16,1))),real64)-2._real64) < 1.e-14_real64, &  !! IN
                 & 'mean real128'                                       )  !! IN
        call check(abs(real(variance(x16(1:size(x16,1))),real64)-2._real64/3._real64) < 1.e-14_real64, &  !! IN
                 & 'variance real128'                                                 )  !! IN
        call check(abs(real(covariance(x16(1:size(x16,1)),y16(1:size(y16,1))), &
            & real64)-4._real64/3._real64) < 1.e-14_real64, &  !! IN
                 & 'covariance real128'                                                     )  !! IN
        call check(abs(real(corrcoef(x16(1:size(x16,1)),y16(1:size(y16,1))),real64)-1._real64) < 1.e-14_real64, &  !! IN
                 & 'corrcoef real128'                                           )  !! IN
    end subroutine test_all_math_kinds


    subroutine test_isclose_edges
        real(real64) :: nanv
        real(real64) :: infv

        nanv = ieee_value(0._real64,ieee_quiet_nan)
        infv = ieee_value(0._real64,ieee_positive_inf)
        call check(isclose(0._real64,0._real64), &  !! IN
                 & 'isclose zero equals zero'    )  !! IN
        call check(.NOT.isclose(0._real64,1.e-15_real64), &  !! IN
                 & 'isclose zero requires abs_tol'        )  !! IN
        call check(isclose(0._real64,1.e-15_real64,abs_tol = 1.e-15_real64), &  !! IN
                 & 'isclose exact absolute boundary'                       )  !! IN
        call check(.NOT.isclose(0._real64,1.0001e-15_real64,abs_tol = 1.e-15_real64), &  !! IN
                 & 'isclose above absolute boundary'                                )  !! IN
        call check(isclose(100._real64,101._real64,rel_tol = 0.01_real64), &  !! IN
                 & 'isclose exact relative boundary'                     )  !! IN
        call check(.NOT.isclose(nanv,nanv), &  !! IN
                 & 'isclose NaN is false'   )  !! IN
        call check(.NOT.isclose(infv,infv)             , &  !! IN
                 & 'current infinity behavior is false'  )  !! IN
        call check(isclose(1._real32,1.00001_real32), &  !! IN
                 & 'isclose real32 generic'           )  !! IN
        call check(isclose(1._real128,1._real128+1.e-32_real128), &  !! IN
                 & 'isclose real128 generic'                      )  !! IN
    end subroutine test_isclose_edges


    subroutine test_string_edges
        character(0) :: z
        character(0) :: zo
        character(6) :: s

        z = ''
        zo = to_upper(z)
        call check(len(zo) == 0               , &  !! IN
                 & 'empty string conversion'  )  !! IN
        call check(to_upper('a') == 'A'    , &  !! IN
                 & 'length-one uppercase'  )  !! IN
        call check(to_lower('Z') == 'z'    , &  !! IN
                 & 'length-one lowercase'  )  !! IN
        s = 'Ab  '
        call check(to_upper(s) == 'AB    '      , &  !! IN
                 & 'trailing blanks preserved'  )  !! IN
        call check(to_lower(' A-B ') == ' a-b '                 , &  !! IN
                 & 'internal blanks and punctuation preserved'  )  !! IN
    end subroutine test_string_edges


    subroutine test_sort_permutation
        integer(int32) :: a(8)
        integer(int32) :: ae(8)
        integer(int32) :: amin(3)
        real(real32)   :: b(8)
        real(real32)   :: be(8)
        real(real32)   :: bz(4)
        real(real64)   :: c(8)
        real(real64)   :: ce(8)
        real(real64)   :: section_source(8)

        a(1:size(a,1)) = [3,-1,3,0,2,-5,2,9]
        ae(1:size(ae,1)) = [-5,-1,0,2,2,3,3,9]
        call quick_sort(a(1:size(a,1)))
        call check(all(a(1:size(a,1)) == ae(1:size(ae,1)))                       , &  !! IN
                 & 'integer sort preserves multiset'  )  !! IN
        amin(1:size(amin,1)) = [huge(0_int32),-huge(0_int32)-1_int32,0_int32]
        call quick_sort(amin(1:size(amin,1)))
        call check(amin(1) < amin(2) .AND. amin(2) < amin(3), &  !! IN
                 & 'integer extrema sort'               )  !! IN
        b(1:size(b,1)) = [3._real32,-1._real32,3._real32,0._real32,2._real32,-5._real32,2._real32,9._real32]
        be(1:size(be,1)) = [-5._real32,-1._real32,0._real32,2._real32,2._real32,3._real32,3._real32,9._real32]
        call quick_sort(b(1:size(b,1)))
        call check(all(b(1:size(b,1)) == be(1:size(be,1)))                      , &  !! IN
                 & 'real32 sort preserves multiset'  )  !! IN
        bz(1:size(bz,1)) = [0._real32,-0._real32,huge(0._real32),-huge(0._real32)]
        call quick_sort(bz(1:size(bz,1)))
        call check(all(bz(1:3) <= bz(2:4))                    , &  !! IN
                 & 'real32 extreme and signed-zero ordering'  )  !! IN
        c(1:size(c,1)) = real([8,7,6,5,4,3,2,1],real64)
        ce(1:size(ce,1)) = real([1,2,3,4,5,6,7,8],real64)
        call quick_sort(c(1:size(c,1)))
        call check(all(c(1:size(c,1)) == ce(1:size(ce,1)))                 , &  !! IN
                 & 'real64 reverse order sort'  )  !! IN
        section_source(1:size(section_source,1)) = real([8,1,7,2,6,3,5,4],real64)
        call quick_sort(section_source(1:8:2))
        call check(all(section_source(1:8:2) == real([5,6,7,8],real64)), &  !! IN
                 & 'noncontiguous section copied back'                 )  !! IN
    end subroutine test_sort_permutation


    subroutine test_derivative_status_and_surface
        real(real64) :: lon2(2)
        real(real64) :: lon3(3)
        real(real64) :: lon5(5)
        real(real64) :: lat1(1)
        real(real64) :: lat3(3)
        real(real64) :: lev1(1)
        real(real64) :: lev4(4)
        real(real64) :: in2(2,1,1)
        real(real64) :: out2(2,1,1)
        real(real64) :: in3(3,1,1)
        real(real64) :: out3(3,1,1)
        real(real64) :: in5(5,1,1)
        real(real64) :: out5(5,1,1)
        real(real64) :: out5_reversed(5,1,1)
        real(real64) :: minput(1,3,1)
        real(real64) :: moutput(1,3,1)
        real(real64) :: monein(1,1,1)
        real(real64) :: moneout(1,1,1)
        real(real64) :: vinput(2,1,4)
        real(real64) :: voutput(2,1,4)
        real(real64) :: ps(2,1)
        real(real64) :: vonein(2,1,1)
        real(real64) :: voneout(2,1,1)
        real(real64) :: psone(2,1)
        integer      :: status
        integer      :: k

        lon2(1:size(lon2,1)) = [0._real64,1._real64]
        in2(:,1,1) = lon2(1:size(lon2,1))
        call zonalDerivative(lon2(1:size(lon2,1))         , &  !! IN
                           & in2(1:size(in2,1),1:size(in2,2),1:size(in2,3))          , &  !! IN
                           & out2(1:size(out2,1),1:size(out2,2),1:size(out2,3))         , &  !! OUT
                           & status = status  )  !! OUT
        call check(status == -3                                    , &  !! IN
                 & 'periodic zonal derivative needs three points'  )  !! IN
        call zonalDerivative(lon2(1:size(lon2,1))            , &  !! IN
                           & in2(1:size(in2,1),1:size(in2,2),1:size(in2,3))             , &  !! IN
                           & out2(1:size(out2,1),1:size(out2,2),1:size(out2,3))            , &  !! OUT
                           & periodic = .FALSE., &  !! IN
                           & status = status     )  !! OUT
        call check(status == 1 .AND. all(abs(out2(1:size(out2,1),1:size(out2,2),1:size(out2, &
            & 3))-1._real64) < 1.e-14_real64), &  !! IN
                 & 'nonperiodic two-point derivative'                    )  !! IN
        lon3(1:size(lon3,1)) = [0._real64,1._real64,1._real64]
        in3(:,1,1) = lon3(1:size(lon3,1))
        call zonalDerivative(lon3(1:size(lon3,1))            , &  !! IN
                           & in3(1:size(in3,1),1:size(in3,2),1:size(in3,3))             , &  !! IN
                           & out3(1:size(out3,1),1:size(out3,2),1:size(out3,3))            , &  !! OUT
                           & periodic = .FALSE., &  !! IN
                           & status = status     )  !! OUT
        call check(status == -2                         , &  !! IN
                 & 'zonal duplicate coordinate status'  )  !! IN
        lon5(1:size(lon5,1)) = [-1._real64,-0.45_real64,0.1_real64,0.8_real64,1.7_real64]
        in5(:,1,1) = 3._real64*lon5(1:size(lon5,1))**2-2._real64*lon5(1:size(lon5,1))+4._real64
        call zonalDerivative(lon5(1:size(lon5,1))            , &  !! IN
                           & in5(1:size(in5,1),1:size(in5,2),1:size(in5,3))             , &  !! IN
                           & out5(1:size(out5,1),1:size(out5,2),1:size(out5,3))            , &  !! OUT
                           & periodic = .FALSE., &  !! IN
                           & status = status     )  !! OUT
        call check(status == 1                              , &  !! IN
                 & 'ascending longitude derivative status'  )  !! IN
        call zonalDerivative(lon5(5:1:-1)    , &  !! IN
                           & in5(5:1:-1,:,:) , &  !! IN
                           & out5_reversed(1:size(out5_reversed,1),1:size(out5_reversed,2), &
                               & 1:size(out5_reversed,3))   , &  !! OUT
                           & periodic = .FALSE., &  !! IN
                           & status = status     )  !! OUT
        call check(status == 1                               , &  !! IN
                 & 'descending longitude derivative status'  )  !! IN
        call check_array_close(reshape(out5_reversed(1:size(out5_reversed,1), &
            & 1:size(out5_reversed,2),1:size(out5_reversed,3)),[size(out5_reversed)])                       , &  !! IN
                             & reshape(out5(5:1:-1,:,:),[size(out5)])                             , &  !! IN
                             & 1.e-13_real64                                                      , &  !! IN
                             & 1.e-13_real64                                                      , &  !! IN
                             & 'descending longitude derivative matches reversed ascending result'  )  !! IN
        lat1(1:size(lat1,1)) = [0._real64]
        monein(1:size(monein,1),1:size(monein,2),1:size(monein,3)) = 1._real64
        moneout(1:size(moneout,1),1:size(moneout,2),1:size(moneout,3)) = 0._real64
        call meridionalDerivative(lat1(1:size(lat1,1))   , &  !! IN
                                & monein(1:size(monein,1),1:size(monein,2),1:size(monein,3)) , &  !! IN
                                & moneout(1:size(moneout,1),1:size(moneout,2),1:size(moneout,3)), &  !! OUT
                                & status   )  !! OUT
        call check(status == -3                   , &  !! IN
                 & 'meridional one-point status'  )  !! IN
        lat3(1:size(lat3,1)) = [1._real64,0._real64,-1._real64]
        minput(1,:,1) = 2._real64*lat3(1:size(lat3,1))+3._real64
        call meridionalDerivative(lat3(1:size(lat3,1))   , &  !! IN
                                & minput(1:size(minput,1),1:size(minput,2),1:size(minput,3)) , &  !! IN
                                & moutput(1:size(moutput,1),1:size(moutput,2),1:size(moutput,3)), &  !! OUT
                                & status   )  !! OUT
        call check(status == 1 .AND. all(abs(moutput(1:size(moutput,1),1:size(moutput,2), &
            & 1:size(moutput,3))-2._real64) < 1.e-14_real64), &  !! IN
                 & 'descending latitude derivative'                         )  !! IN
        lev1(1:size(lev1,1)) = [10000._real64]
        vonein(:,1,1) = [1._real64,2._real64]
        voneout(1:size(voneout,1),1:size(voneout,2),1:size(voneout,3)) = 0._real64
        psone(:,1) = 90000._real64
        call verticalDerivative(lev1(1:size(lev1,1))         , &  !! IN
                              & vonein(1:size(vonein,1),1:size(vonein,2),1:size(vonein,3))       , &  !! IN
                              & psone(1:size(psone,1),1:size(psone,2))        , &  !! IN
                              & voneout(1:size(voneout,1),1:size(voneout,2),1:size(voneout,3))      , &  !! OUT
                              & status = status  )  !! OUT
        call check(status == -3                 , &  !! IN
                 & 'vertical one-level status'  )  !! IN
        lev4(1:size(lev4,1)) = [10000._real64,30000._real64,60000._real64,90000._real64]
        do k = 1,4
        vinput(:,:,k) = 2._real64*lev4(k)+1._real64
        enddo
        ps(:,1) = [75000._real64,5000._real64]
        call verticalDerivative(lev4(1:size(lev4,1))                , &  !! IN
                              & vinput(1:size(vinput,1),1:size(vinput,2),1:size(vinput,3))              , &  !! IN
                              & ps(1:size(ps,1),1:size(ps,2))                  , &  !! IN
                              & voutput(1:size(voutput,1),1:size(voutput,2),1:size(voutput,3))             , &  !! OUT
                              & undef = -12345._real64, &  !! IN
                              & status = status         )  !! OUT
        call check(status == 1                , &  !! IN
                 & 'vertical surface status'  )  !! IN
        call check(all(abs(voutput(1,1,1:3)-2._real64) < 1.e-12_real64), &  !! IN
                 & 'vertical above-ground derivative'                  )  !! IN
        call check(voutput(1,1,4) == -12345._real64, &  !! IN
                 & 'vertical underground undef'    )  !! IN
        call check(all(voutput(2,1,:) == -12345._real64), &  !! IN
                 & 'all levels underground'             )  !! IN
    end subroutine test_derivative_status_and_surface


    subroutine test_integral_status_and_boundaries
        real(real64) :: lat4(4)
        real(real64) :: f(1,4,1)
        real(real64) :: out(1,1)
        real(real64) :: out_ascending(1,1)
        real(real64) :: out_descending(1,1)
        real(real64) :: vs
        real(real64) :: vn
        real(real64) :: expected
        real(real64) :: lev4(4)
        real(real64) :: vf(1,1,4)
        real(real64) :: ps(1,1)
        real(real64) :: vo(1,1)
        real(real64) :: vo_ascending(1,1)
        real(real64) :: vo_descending(1,1)
        real(real64) :: latone(1)
        real(real64) :: fone(1,1,1)
        real(real64) :: outone(1,1)
        real(real64) :: latdup(3)
        real(real64) :: fdup(1,3,1)
        real(real64) :: levone(1)
        integer      :: status

        lat4(1:size(lat4,1)) = [-1._real64,-0.2_real64,0.4_real64,1._real64]
        f(1:size(f,1),1:size(f,2),1:size(f,3)) = 1._real64
        call meridionalIntegral(lat4(1:size(lat4,1))        , &  !! IN
                              & f(1:size(f,1),1:size(f,2),1:size(f,3))           , &  !! IN
                              & -0.55_real64, &  !! IN
                              & 0.7_real64  , &  !! IN
                              & out(1:size(out,1),1:size(out,2))         , &  !! out(1:size(out,1),1:size(out,2))
                              & status      , &  !! out(1:size(out,1),1:size(out,2))
                              & vs          , &  !! out(1:size(out,1),1:size(out,2))
                              & vn            )  !! out(1:size(out,1),1:size(out,2))
        expected = 0.5_real64*(cos(lat4(2))+cos(lat4(3)))*(lat4(3)-lat4(2))
        call check(status == 1                         , &  !! IN
                 & 'nearest-boundary integral status'  )  !! IN
        call check_close(vs                         , &  !! IN
                       & lat4(2)                    , &  !! IN
                       & 0._real64                  , &  !! IN
                       & 0._real64                  , &  !! IN
                       & 'valid south nearest point'  )  !! IN
        call check_close(vn                         , &  !! IN
                       & lat4(3)                    , &  !! IN
                       & 0._real64                  , &  !! IN
                       & 0._real64                  , &  !! IN
                       & 'valid north nearest point'  )  !! IN
        call check_close(out(1,1)                         , &  !! IN
                       & expected                         , &  !! IN
                       & 1.e-14_real64                    , &  !! IN
                       & 1.e-14_real64                    , &  !! IN
                       & 'nearest-boundary integral value'  )  !! IN
        call meridionalIntegral(lat4(1:size(lat4,1))        , &  !! IN
                              & f(1:size(f,1),1:size(f,2),1:size(f,3))           , &  !! IN
                              & 0.7_real64  , &  !! IN
                              & -0.55_real64, &  !! IN
                              & out(1:size(out,1),1:size(out,2))         , &  !! out(1:size(out,1),1:size(out,2))
                              & status        )  !! out(1:size(out,1),1:size(out,2))
        call check_close(out(1,1)                                                , &  !! IN
                       & expected                                                , &  !! IN
                       & 1.e-14_real64                                           , &  !! IN
                       & 1.e-14_real64                                           , &  !! IN
                       & 'reversed integral boundaries keep positive orientation'  )  !! IN
        call meridionalIntegral(lat4(1:size(lat4,1))        , &  !! IN
                              & f(1:size(f,1),1:size(f,2),1:size(f,3))           , &  !! IN
                              & -0.19_real64, &  !! IN
                              & -0.21_real64, &  !! IN
                              & out(1:size(out,1),1:size(out,2))         , &  !! out(1:size(out,1),1:size(out,2))
                              & status        )  !! out(1:size(out,1),1:size(out,2))
        call check(status == 1 .AND. out(1,1) == 0._real64 , &  !! IN
                 & 'boundaries mapping to same point'  )  !! IN
        call meridionalIntegral(lat4(1:size(lat4,1))         , &  !! IN
                              & f(1:size(f,1),1:size(f,2),1:size(f,3))            , &  !! IN
                              & lat4(1)      , &  !! IN
                              & lat4(4)      , &  !! IN
                              & out_ascending(1:size(out_ascending,1),1:size(out_ascending,2)), &
                                  & &  !! out(1:size(out,1),1:size(out,2))
                              & status         )  !! out(1:size(out,1),1:size(out,2))
        call check(status == 1                                      , &  !! IN
                 & 'ascending latitude integral comparison status'  )  !! IN
        call meridionalIntegral(lat4([4,3,2,1]) , &  !! IN
                              & f(:,[4,3,2,1],:), &  !! IN
                              & -1._real64      , &  !! IN
                              & 1._real64       , &  !! IN
                              & out_descending(1:size(out_descending,1),1:size(out_descending, &
                                  & 2))  , &  !! out(1:size(out,1),1:size(out,2))
                              & status            )  !! out(1:size(out,1),1:size(out,2))
        call check(status == 1                                       , &  !! IN
                 & 'descending latitude integral comparison status'  )  !! IN
        call check_array_close(reshape(out_descending(1:size(out_descending,1), &
            & 1:size(out_descending,2)),[size(out_descending)])         , &  !! IN
                             & reshape(out_ascending(1:size(out_ascending,1), &
                                 & 1:size(out_ascending,2)),[size(out_ascending)])           , &  !! IN
                             & 1.e-14_real64                                          , &  !! IN
                             & 1.e-14_real64                                          , &  !! IN
                             & 'descending latitude integral matches ascending result'  )  !! IN
        latone(1:size(latone,1)) = 0._real64
        fone(1:size(fone,1),1:size(fone,2),1:size(fone,3)) = 1._real64
        outone(1:size(outone,1),1:size(outone,2)) = 0._real64
        call meridionalIntegral(latone(1:size(latone,1))   , &  !! IN
                              & fone(1:size(fone,1),1:size(fone,2),1:size(fone,3))     , &  !! IN
                              & 0._real64, &  !! IN
                              & 0._real64, &  !! IN
                              & outone(1:size(outone,1),1:size(outone,2))   , &  !! out(1:size(out,1),1:size(out,2))
                              & status     )  !! out(1:size(out,1),1:size(out,2))
        call check(status == -3                            , &  !! IN
                 & 'meridional integral one-point status'  )  !! IN
        latdup(1:size(latdup,1)) = [0._real64,0._real64,1._real64]
        fdup(1:size(fdup,1),1:size(fdup,2),1:size(fdup,3)) = 1._real64
        outone(1:size(outone,1),1:size(outone,2)) = 0._real64
        call meridionalIntegral(latdup(1:size(latdup,1))   , &  !! IN
                              & fdup(1:size(fdup,1),1:size(fdup,2),1:size(fdup,3))     , &  !! IN
                              & 0._real64, &  !! IN
                              & 1._real64, &  !! IN
                              & outone(1:size(outone,1),1:size(outone,2))   , &  !! out(1:size(out,1),1:size(out,2))
                              & status     )  !! out(1:size(out,1),1:size(out,2))
        call check(status == -2                            , &  !! IN
                 & 'meridional integral duplicate status'  )  !! IN
        lev4(1:size(lev4,1)) = [10000._real64,30000._real64,60000._real64,90000._real64]
        vf(1:size(vf,1),1:size(vf,2),1:size(vf,3)) = 1._real64
        ps(1:size(ps,1),1:size(ps,2)) = 5000._real64
        call verticalIntegral(lev4(1:size(lev4,1))  , &  !! IN
                            & vf(1:size(vf,1),1:size(vf,2),1:size(vf,3))    , &  !! IN
                            & ps(1:size(ps,1),1:size(ps,2))    , &  !! IN
                            & vo(1:size(vo,1),1:size(vo,2))    , &  !! out(1:size(out,1),1:size(out,2))
                            & status  )  !! out(1:size(out,1),1:size(out,2))
        call check(status == 1 .AND. vo(1,1) == 0._real64          , &  !! IN
                 & 'vertical integral all levels underground'  )  !! IN
        ps(1:size(ps,1),1:size(ps,2)) = 120000._real64
        call verticalIntegral(lev4(1:size(lev4,1))  , &  !! IN
                            & vf(1:size(vf,1),1:size(vf,2),1:size(vf,3))    , &  !! IN
                            & ps(1:size(ps,1),1:size(ps,2))    , &  !! IN
                            & vo(1:size(vo,1),1:size(vo,2))    , &  !! out(1:size(out,1),1:size(out,2))
                            & status  )  !! out(1:size(out,1),1:size(out,2))
        call check_close(vo(1,1)                                             , &  !! IN
                       & 85000._real64                                       , &  !! IN
                       & 0._real64                                           , &  !! IN
                       & 1.e-10_real64                                       , &  !! IN
                       & 'surface below lowest level truncates at last level'  )  !! IN
        ps(1:size(ps,1),1:size(ps,2)) = 90000._real64
        call verticalIntegral(lev4(1:size(lev4,1))        , &  !! IN
                            & vf(1:size(vf,1),1:size(vf,2),1:size(vf,3))          , &  !! IN
                            & ps(1:size(ps,1),1:size(ps,2))          , &  !! IN
                            & vo_ascending(1:size(vo_ascending,1),1:size(vo_ascending,2)), &
                                & &  !! out(1:size(out,1),1:size(out,2))
                            & status        )  !! out(1:size(out,1),1:size(out,2))
        call check(status == 1                                      , &  !! IN
                 & 'ascending pressure integral comparison status'  )  !! IN
        call verticalIntegral(lev4([4,3,2,1])   , &  !! IN
                            & vf(:,:, [4,3,2,1]), &  !! IN
                            & ps(1:size(ps,1),1:size(ps,2))                , &  !! IN
                            & vo_descending(1:size(vo_descending,1),1:size(vo_descending, &
                                & 2))     , &  !! out(1:size(out,1),1:size(out,2))
                            & status              )  !! out(1:size(out,1),1:size(out,2))
        call check(status == 1                                       , &  !! IN
                 & 'descending pressure integral comparison status'  )  !! IN
        call check_array_close(reshape(vo_descending(1:size(vo_descending,1), &
            & 1:size(vo_descending,2)),[size(vo_descending)])           , &  !! IN
                             & reshape(vo_ascending(1:size(vo_ascending,1),1:size(vo_ascending, &
                                 & 2)),[size(vo_ascending)])             , &  !! IN
                             & 1.e-14_real64                                          , &  !! IN
                             & 1.e-10_real64                                          , &  !! IN
                             & 'descending pressure integral matches ascending result'  )  !! IN
        levone(1:size(levone,1)) = 10000._real64
        fone(1:size(fone,1),1:size(fone,2),1:size(fone,3)) = 1._real64
        ps(1:size(ps,1),1:size(ps,2)) = 100000._real64
        outone(1:size(outone,1),1:size(outone,2)) = 0._real64
        call verticalIntegral(levone(1:size(levone,1)), &  !! IN
                            & fone(1:size(fone,1),1:size(fone,2),1:size(fone,3))  , &  !! IN
                            & ps(1:size(ps,1),1:size(ps,2))    , &  !! IN
                            & outone(1:size(outone,1),1:size(outone,2)), &  !! out(1:size(out,1),1:size(out,2))
                            & status  )  !! out(1:size(out,1),1:size(out,2))
        call check(status == -3                          , &  !! IN
                 & 'vertical integral one-level status'  )  !! IN
    end subroutine test_integral_status_and_boundaries


    subroutine test_met_all_kinds
        real(real32)  :: t4(1)
        real(real32)  :: p4(1)
        real(real32)  :: lat4(2)
        real(real32)  :: f4(1,2,1)
        real(real32)  :: o4(1,1)
        real(real128) :: t16(1)
        real(real128) :: p16(1)
        real(real128) :: lat16(2)
        real(real128) :: f16(1,2,1)
        real(real128) :: o16(1,1)
        integer       :: status

        t4(1:size(t4,1)) = 300._real32
        p4(1:size(p4,1)) = 100000._real32
        call check(abs(real(potential_temperature(t4(1),p4(1)),real64)-300._real64) < 1.e-5_real64, &  !! IN
                 & 'potential temperature real32'                                                 )  !! IN
        t16(1:size(t16,1)) = 300._real128
        p16(1:size(p16,1)) = 100000._real128
        call check(abs(real(potential_temperature(t16(1),p16(1)),real64)-300._real64) < 1.e-14_real64, &  !! IN
                 & 'potential temperature real128'                                                   )  !! IN
        lat4(1:size(lat4,1)) = [0._real32,0.5_real32]
        f4(1:size(f4,1),1:size(f4,2),1:size(f4,3)) = 1._real32
        call meridionalIntegral(lat4(1:size(lat4,1))   , &  !! IN
                              & f4(1:size(f4,1),1:size(f4,2),1:size(f4,3))     , &  !! IN
                              & lat4(1), &  !! IN
                              & lat4(2), &  !! IN
                              & o4(1:size(o4,1),1:size(o4,2))     , &  !! OUT
                              & status   )  !! OUT
        call check(status == 1                           , &  !! IN
                 & 'meridional integral real32 generic'  )  !! IN
        lat16(1:size(lat16,1)) = [0._real128,0.5_real128]
        f16(1:size(f16,1),1:size(f16,2),1:size(f16,3)) = 1._real128
        call meridionalIntegral(lat16(1:size(lat16,1))   , &  !! IN
                              & f16(1:size(f16,1),1:size(f16,2),1:size(f16,3))     , &  !! IN
                              & lat16(1), &  !! IN
                              & lat16(2), &  !! IN
                              & o16(1:size(o16,1),1:size(o16,2))     , &  !! OUT
                              & status    )  !! OUT
        call check(status == 1                            , &  !! IN
                 & 'meridional integral real128 generic'  )  !! IN
    end subroutine test_met_all_kinds


    subroutine test_binio_all_ranks_and_kinds
        type(finfo)             :: fi
        character(*), parameter :: fn = 'test_extended_binio.tmp'
        real(real32)            :: s0
        real(real32)            :: r0
        real(real32)            :: a3(2,2,2)
        real(real32)            :: b3(2,2,2)
        real(real32)            :: a4(2,1,2,2)
        real(real32)            :: b4(2,1,2,2)
        real(real32)            :: a5(1,2,1,2,2)
        real(real32)            :: b5(1,2,1,2,2)
        real(real128)           :: q1(3)
        real(real128)           :: qr1(3)
        integer(int64)          :: rec
        integer                 :: i

        s0 = 1.25_real32
        fi = finfo(file = fn,action = 'WrItE',record = 1_int64,recl = 4_int64,recstep = 1_int64)
        call fi % fwrite(s0)
        call fi % fclose()
        fi = finfo(file = fn,action = 'READ',record = 1_int64,recl = 4_int64,recstep = 1_int64)
        call fi % fread(r0)
        call fi % fclose()
        call check(r0 == s0        , &  !! IN
                 & 'scalar binio'  )  !! IN
        a3(1:size(a3,1),1:size(a3,2),1:size(a3,3)) = reshape([(real(i,real32),i = 1,size(a3))],shape(a3))
        call io_rank3(a3(1:size(a3,1),1:size(a3,2),1:size(a3,3)), &  !! IN
                    & b3(1:size(b3,1),1:size(b3,2),1:size(b3,3)), &  !! OUT
                    & fn  )  !! IN
        call check(all(a3(1:size(a3,1),1:size(a3,2),1:size(a3,3)) == b3(1:size(b3,1),1:size(b3,2),1:size(b3,3)))  , &  !! IN
                 & 'rank3 binio'  )  !! IN
        a4(1:size(a4,1),1:size(a4,2),1:size(a4,3),1:size(a4,4)) = reshape([(real(i,real32),i = 1,size(a4))],shape(a4))
        call io_rank4(a4(1:size(a4,1),1:size(a4,2),1:size(a4,3),1:size(a4,4)), &  !! IN
                    & b4(1:size(b4,1),1:size(b4,2),1:size(b4,3),1:size(b4,4)), &  !! OUT
                    & fn  )  !! IN
        call check(all(a4(1:size(a4,1),1:size(a4,2),1:size(a4,3),1:size(a4,4)) == b4(1:size(b4, &
            & 1),1:size(b4,2),1:size(b4,3),1:size(b4,4)))  , &  !! IN
                 & 'rank4 binio'  )  !! IN
        a5(1:size(a5,1),1:size(a5,2),1:size(a5,3),1:size(a5,4),1:size(a5,5)) = reshape([(real(i, &
            & real32),i = 1,size(a5))],shape(a5))
        call io_rank5(a5(1:size(a5,1),1:size(a5,2),1:size(a5,3),1:size(a5,4),1:size(a5,5)), &  !! IN
                    & b5(1:size(b5,1),1:size(b5,2),1:size(b5,3),1:size(b5,4),1:size(b5,5)), &  !! OUT
                    & fn  )  !! IN
        call check(all(a5(1:size(a5,1),1:size(a5,2),1:size(a5,3),1:size(a5,4),1:size(a5, &
            & 5)) == b5(1:size(b5,1),1:size(b5,2),1:size(b5,3),1:size(b5,4),1:size(b5,5)))  , &  !! IN
                 & 'rank5 binio'  )  !! IN
        q1(1:size(q1,1)) = [1._real128/3._real128,2._real128/3._real128,1._real128]
        fi = finfo(file = fn,action = 'write',record = 1_int64,recl = 12_int64,recstep = 1_int64)
        call fi % fwrite(q1(1:size(q1,1)))
        call fi % fclose()
        fi = finfo(file = fn,action = 'read',record = 1_int64,recl = 12_int64,recstep = 1_int64)
        call fi % fread(qr1(1:size(qr1,1)))
        call fi % fclose()
        call check(all(qr1(1:size(qr1,1)) == real(real(q1(1:size(q1,1)),real32),real128)), &  !! IN
                 & 'real128 binio through real32 payload'   )  !! IN
        fi = finfo(file = fn,action = 'write',record = 1_int64,recl = 4_int64,recstep = 2_int64)
        call fi % fwrite(1._real32)
        call fi % fwrite(3._real32)
        call fi % reset_record(increment = -2_int64)
        call fi % get_record(rec)
        call check(rec == 3_int64            , &  !! IN
                 & 'reset_record increment'  )  !! IN
        call fi % reset_record(increment = 2_int64 , &  !! IN
                             & newrecord = 99_int64  )  !! IN
        call fi % get_record(rec)
        call check(rec == 5_int64                             , &  !! IN
                 & 'increment takes priority over newrecord'  )  !! IN
        call fi % fclose()
        call remove_file(fn)
    end subroutine test_binio_all_ranks_and_kinds


    subroutine io_rank3(a, b, fn)
        real(real32),intent(in)  :: a(:,:,:)
        real(real32),intent(out) :: b(:,:,:)
        character(*),intent(in)  :: fn

        type(finfo)              :: fi

        fi = finfo(file = fn,action = 'write',record = 1_int64,recl = int(size(a)*4,int64),recstep = 1_int64)
        call fi % fwrite(a(1:size(a,1),1:size(a,2),1:size(a,3)))
        call fi % fclose()
        fi = finfo(file = fn,action = 'read',record = 1_int64,recl = int(size(a)*4,int64),recstep = 1_int64)
        call fi % fread(b(1:size(b,1),1:size(b,2),1:size(b,3)))
        call fi % fclose()
    end subroutine io_rank3


    subroutine io_rank4(a, b, fn)
        real(real32),intent(in)  :: a(:,:,:,:)
        real(real32),intent(out) :: b(:,:,:,:)
        character(*),intent(in)  :: fn

        type(finfo)              :: fi

        fi = finfo(file = fn,action = 'write',record = 1_int64,recl = int(size(a)*4,int64),recstep = 1_int64)
        call fi % fwrite(a(1:size(a,1),1:size(a,2),1:size(a,3),1:size(a,4)))
        call fi % fclose()
        fi = finfo(file = fn,action = 'read',record = 1_int64,recl = int(size(a)*4,int64),recstep = 1_int64)
        call fi % fread(b(1:size(b,1),1:size(b,2),1:size(b,3),1:size(b,4)))
        call fi % fclose()
    end subroutine io_rank4


    subroutine io_rank5(a, b, fn)
        real(real32),intent(in)  :: a(:,:,:,:,:)
        real(real32),intent(out) :: b(:,:,:,:,:)
        character(*),intent(in)  :: fn

        type(finfo)              :: fi

        fi = finfo(file = fn,action = 'write',record = 1_int64,recl = int(size(a)*4,int64),recstep = 1_int64)
        call fi % fwrite(a(1:size(a,1),1:size(a,2),1:size(a,3),1:size(a,4),1:size(a,5)))
        call fi % fclose()
        fi = finfo(file = fn,action = 'read',record = 1_int64,recl = int(size(a)*4,int64),recstep = 1_int64)
        call fi % fread(b(1:size(b,1),1:size(b,2),1:size(b,3),1:size(b,4),1:size(b,5)))
        call fi % fclose()
    end subroutine io_rank5


    subroutine test_endian_bytes
        real(real32)  :: x
        integer(int8) :: before(storage_size(x)/8)
        integer(int8) :: after(storage_size(x)/8)

        x = 1._real32
        before(1:size(before,1)) = transfer(x,before(1:size(before,1)))
        call endian_converter(x)
        after(1:size(after,1)) = transfer(x,after(1:size(after,1)))
        call check(all(after(1:size(after,1)) == before(size(before):1:-1)), &  !! IN
                 & 'endian converter reverses bytes'      )  !! IN
    end subroutine test_endian_bytes


    subroutine remove_file(fn)
        character(*),intent(in) :: fn

        integer                 :: u
        logical                 :: exists

        inquire(file = fn,exist = exists)
        if(exists) then
        open(newunit = u,file = fn,status = 'old')
        close(u,status = 'delete')
        endif
    end subroutine remove_file
end program test_extended
