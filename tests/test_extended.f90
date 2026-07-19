program test_extended
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real32, real64, real128
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf
  use EDAT_Math, only: mean, variance, covariance, corrcoef
  use EDAT_Float, only: isclose
  use EDAT_Sort, only: quick_sort
  use EDAT_String, only: to_lower, to_upper
  use EDAT_Met, only: potential_temperature, meridionalIntegral, verticalIntegral, &
                      zonalDerivative, meridionalDerivative, verticalDerivative
  use EDAT_BinIO, only: finfo, endian_converter
  use test_support, only: check, check_close, check_array_close, check_nan, finish_tests
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
    real(real64) :: one(1), constant(4), x(4), y(4)
    allocate(empty(0))
    one = 2.0_real64
    constant = 3.0_real64
    x = [-2.0_real64, -1.0_real64, 1.0_real64, 2.0_real64]
    y = -4.0_real64*x + 7.0_real64

    call check_close(variance(empty), 0.0_real64, 0.0_real64, 0.0_real64, 'empty population variance')
    call check_close(variance(empty, sample=.true.), 0.0_real64, 0.0_real64, 0.0_real64, 'empty sample variance')
    call check_close(covariance(empty, empty), 0.0_real64, 0.0_real64, 0.0_real64, 'empty population covariance')
    call check_close(covariance(empty, empty, sample=.true.), 0.0_real64, 0.0_real64, 0.0_real64, 'empty sample covariance')
    call check_close(corrcoef(empty, empty), 0.0_real64, 0.0_real64, 0.0_real64, 'empty correlation')
    call check_nan(variance(one, sample=.true.), 'one-element sample variance is NaN')
    call check_nan(covariance(one, one, sample=.true.), 'one-element sample covariance is NaN')
    call check_nan(corrcoef(one, one), 'one-element correlation is NaN')
    call check_nan(corrcoef(constant, constant), 'constant-array correlation is NaN')
    call check_close(corrcoef(x, y), -1.0_real64, 1.0e-14_real64, 1.0e-14_real64, 'perfect negative correlation')
    call check_close(covariance(x, y, sample=.true.), -40.0_real64/3.0_real64, 1.0e-14_real64, 1.0e-14_real64, 'sample covariance')
    call check_nan(corrcoef(x, y(1:3)), 'correlation size mismatch is NaN')
    deallocate(empty)
  end subroutine

  subroutine test_all_math_kinds
    real(real32) :: x4(3), y4(3)
    real(real64) :: x8(3), y8(3)
    real(real128) :: x16(3), y16(3)
    x4=[1._real32,2._real32,3._real32]; y4=2._real32*x4
    x8=real(x4,real64); y8=real(y4,real64)
    x16=real(x4,real128); y16=real(y4,real128)
    call check(abs(real(mean(x4),real64)-2._real64)<1.e-6_real64, 'mean real32')
    call check(abs(real(variance(x4),real64)-2._real64/3._real64)<1.e-6_real64, 'variance real32')
    call check(abs(real(covariance(x4,y4),real64)-4._real64/3._real64)<2.e-6_real64, 'covariance real32')
    call check(abs(real(corrcoef(x4,y4),real64)-1._real64)<1.e-6_real64, 'corrcoef real32')
    call check_close(mean(x8),2._real64,0._real64,0._real64,'mean real64')
    call check(abs(real(mean(x16),real64)-2._real64)<1.e-14_real64, 'mean real128')
    call check(abs(real(variance(x16),real64)-2._real64/3._real64)<1.e-14_real64, 'variance real128')
    call check(abs(real(covariance(x16,y16),real64)-4._real64/3._real64)<1.e-14_real64, 'covariance real128')
    call check(abs(real(corrcoef(x16,y16),real64)-1._real64)<1.e-14_real64, 'corrcoef real128')
  end subroutine

  subroutine test_isclose_edges
    real(real64) :: nanv, infv
    nanv=ieee_value(0._real64,ieee_quiet_nan)
    infv=ieee_value(0._real64,ieee_positive_inf)
    call check(isclose(0._real64,0._real64),'isclose zero equals zero')
    call check(.not.isclose(0._real64,1.e-15_real64),'isclose zero requires abs_tol')
    call check(isclose(0._real64,1.e-15_real64,abs_tol=1.e-15_real64),'isclose exact absolute boundary')
    call check(.not.isclose(0._real64,1.0001e-15_real64,abs_tol=1.e-15_real64),'isclose above absolute boundary')
    call check(isclose(100._real64,101._real64,rel_tol=0.01_real64),'isclose exact relative boundary')
    call check(.not.isclose(nanv,nanv),'isclose NaN is false')
    call check(.not.isclose(infv,infv),'current infinity behavior is false')
    call check(isclose(1._real32,1.00001_real32),'isclose real32 generic')
    call check(isclose(1._real128,1._real128+1.e-32_real128),'isclose real128 generic')
  end subroutine

  subroutine test_string_edges
    character(0) :: z, zo
    character(6) :: s
    z=''; zo=to_upper(z)
    call check(len(zo)==0,'empty string conversion')
    call check(to_upper('a')=='A','length-one uppercase')
    call check(to_lower('Z')=='z','length-one lowercase')
    s='Ab  '
    call check(to_upper(s)=='AB    ','trailing blanks preserved')
    call check(to_lower(' A-B ')==' a-b ','internal blanks and punctuation preserved')
  end subroutine

  subroutine test_sort_permutation
    integer(int32) :: a(8), ae(8), amin(3)
    real(real32) :: b(8), be(8), bz(4)
    real(real64) :: c(8), ce(8), section_source(8)
    a=[3,-1,3,0,2,-5,2,9]; ae=[-5,-1,0,2,2,3,3,9]
    call quick_sort(a); call check(all(a==ae),'integer sort preserves multiset')
    amin=[huge(0_int32),-huge(0_int32)-1_int32,0_int32]
    call quick_sort(amin); call check(amin(1)<amin(2).and.amin(2)<amin(3),'integer extrema sort')
    b=[3._real32,-1._real32,3._real32,0._real32,2._real32,-5._real32,2._real32,9._real32]
    be=[-5._real32,-1._real32,0._real32,2._real32,2._real32,3._real32,3._real32,9._real32]
    call quick_sort(b); call check(all(b==be),'real32 sort preserves multiset')
    bz=[0._real32,-0._real32,huge(0._real32),-huge(0._real32)]
    call quick_sort(bz); call check(all(bz(1:3)<=bz(2:4)),'real32 extreme and signed-zero ordering')
    c=real([8,7,6,5,4,3,2,1],real64); ce=real([1,2,3,4,5,6,7,8],real64)
    call quick_sort(c); call check(all(c==ce),'real64 reverse order sort')
    section_source=real([8,1,7,2,6,3,5,4],real64)
    call quick_sort(section_source(1:8:2))
    call check(all(section_source(1:8:2)==real([5,6,7,8],real64)),'noncontiguous section copied back')
  end subroutine

  subroutine test_derivative_status_and_surface
    real(real64) :: lon2(2), lon3(3), lon5(5), lat1(1), lat3(3), lev1(1), lev4(4)
    real(real64) :: in2(2,1,1), out2(2,1,1), in3(3,1,1), out3(3,1,1)
    real(real64) :: in5(5,1,1), out5(5,1,1), out5_reversed(5,1,1)
    real(real64) :: minput(1,3,1), moutput(1,3,1), monein(1,1,1), moneout(1,1,1)
    real(real64) :: vinput(2,1,4), voutput(2,1,4), ps(2,1), vonein(2,1,1), voneout(2,1,1), psone(2,1)
    integer :: status, k
    lon2=[0._real64,1._real64]; in2(:,1,1)=lon2
    call zonalDerivative(lon2,in2,out2,status=status)
    call check(status==-3,'periodic zonal derivative needs three points')
    call zonalDerivative(lon2,in2,out2,periodic=.false.,status=status)
    call check(status==1.and.all(abs(out2-1._real64)<1.e-14_real64),'nonperiodic two-point derivative')
    lon3=[0._real64,1._real64,1._real64]; in3(:,1,1)=lon3
    call zonalDerivative(lon3,in3,out3,periodic=.false.,status=status)
    call check(status==-2,'zonal duplicate coordinate status')
    lon5=[-1._real64,-0.45_real64,0.1_real64,0.8_real64,1.7_real64]
    in5(:,1,1)=3._real64*lon5**2-2._real64*lon5+4._real64
    call zonalDerivative(lon5,in5,out5,periodic=.false.,status=status)
    call check(status==1,'ascending longitude derivative status')
    call zonalDerivative(lon5(5:1:-1),in5(5:1:-1,:,:),out5_reversed,periodic=.false.,status=status)
    call check(status==1,'descending longitude derivative status')
    call check_array_close(reshape(out5_reversed,[size(out5_reversed)]), &
      reshape(out5(5:1:-1,:,:),[size(out5)]),1.e-13_real64,1.e-13_real64, &
      'descending longitude derivative matches reversed ascending result')
    lat1=[0._real64]
    monein=1._real64; moneout=0._real64
    call meridionalDerivative(lat1,monein,moneout,status)
    call check(status==-3,'meridional one-point status')
    lat3=[1._real64,0._real64,-1._real64]
    minput(1,:,1)=2._real64*lat3+3._real64
    call meridionalDerivative(lat3,minput,moutput,status)
    call check(status==1.and.all(abs(moutput-2._real64)<1.e-14_real64),'descending latitude derivative')
    lev1=[10000._real64]
    vonein(:,1,1)=[1._real64,2._real64]; voneout=0._real64; psone(:,1)=90000._real64
    call verticalDerivative(lev1,vonein,psone,voneout,status=status)
    call check(status==-3,'vertical one-level status')
    lev4=[10000._real64,30000._real64,60000._real64,90000._real64]
    do k=1,4; vinput(:,:,k)=2._real64*lev4(k)+1._real64; end do
    ps(:,1)=[75000._real64,5000._real64]
    call verticalDerivative(lev4,vinput,ps,voutput,undef=-12345._real64,status=status)
    call check(status==1,'vertical surface status')
    call check(all(abs(voutput(1,1,1:3)-2._real64)<1.e-12_real64),'vertical above-ground derivative')
    call check(voutput(1,1,4)==-12345._real64,'vertical underground undef')
    call check(all(voutput(2,1,:)==-12345._real64),'all levels underground')
  end subroutine

  subroutine test_integral_status_and_boundaries
    real(real64) :: lat4(4), f(1,4,1), out(1,1), out_ascending(1,1), out_descending(1,1), vs, vn, expected
    real(real64) :: lev4(4), vf(1,1,4), ps(1,1), vo(1,1), vo_ascending(1,1), vo_descending(1,1), latone(1), fone(1,1,1), outone(1,1), latdup(3), fdup(1,3,1), levone(1)
    integer :: status
    lat4=[-1._real64,-0.2_real64,0.4_real64,1._real64]; f=1._real64
    call meridionalIntegral(lat4,f,-0.55_real64,0.7_real64,out,status,vs,vn)
    expected=0.5_real64*(cos(lat4(2))+cos(lat4(3)))*(lat4(3)-lat4(2))
    call check(status==1,'nearest-boundary integral status')
    call check_close(vs,lat4(2),0._real64,0._real64,'valid south nearest point')
    call check_close(vn,lat4(3),0._real64,0._real64,'valid north nearest point')
    call check_close(out(1,1),expected,1.e-14_real64,1.e-14_real64,'nearest-boundary integral value')
    call meridionalIntegral(lat4,f,0.7_real64,-0.55_real64,out,status)
    call check_close(out(1,1),expected,1.e-14_real64,1.e-14_real64,'reversed integral boundaries keep positive orientation')
    call meridionalIntegral(lat4,f,-0.19_real64,-0.21_real64,out,status)
    call check(status==1.and.out(1,1)==0._real64,'boundaries mapping to same point')
    call meridionalIntegral(lat4,f,lat4(1),lat4(4),out_ascending,status)
    call check(status==1,'ascending latitude integral comparison status')
    call meridionalIntegral(lat4([4,3,2,1]),f(:,[4,3,2,1],:),-1._real64,1._real64,out_descending,status)
    call check(status==1,'descending latitude integral comparison status')
    call check_array_close(reshape(out_descending,[size(out_descending)]), &
      reshape(out_ascending,[size(out_ascending)]),1.e-14_real64,1.e-14_real64, &
      'descending latitude integral matches ascending result')
    latone=0._real64; fone=1._real64; outone=0._real64
    call meridionalIntegral(latone,fone,0._real64,0._real64,outone,status)
    call check(status==-3,'meridional integral one-point status')
    latdup=[0._real64,0._real64,1._real64]; fdup=1._real64; outone=0._real64
    call meridionalIntegral(latdup,fdup,0._real64,1._real64,outone,status)
    call check(status==-2,'meridional integral duplicate status')
    lev4=[10000._real64,30000._real64,60000._real64,90000._real64]
    vf=1._real64; ps=5000._real64
    call verticalIntegral(lev4,vf,ps,vo,status)
    call check(status==1.and.vo(1,1)==0._real64,'vertical integral all levels underground')
    ps=120000._real64; call verticalIntegral(lev4,vf,ps,vo,status)
    call check_close(vo(1,1),85000._real64,0._real64,1.e-10_real64,'surface below lowest level truncates at last level')
    ps=90000._real64
    call verticalIntegral(lev4,vf,ps,vo_ascending,status)
    call check(status==1,'ascending pressure integral comparison status')
    call verticalIntegral(lev4([4,3,2,1]),vf(:,:, [4,3,2,1]),ps,vo_descending,status)
    call check(status==1,'descending pressure integral comparison status')
    call check_array_close(reshape(vo_descending,[size(vo_descending)]), &
      reshape(vo_ascending,[size(vo_ascending)]),1.e-14_real64,1.e-10_real64, &
      'descending pressure integral matches ascending result')
    levone=10000._real64; fone=1._real64; ps=100000._real64; outone=0._real64
    call verticalIntegral(levone,fone,ps,outone,status)
    call check(status==-3,'vertical integral one-level status')
  end subroutine

  subroutine test_met_all_kinds
    real(real32) :: t4(1),p4(1),lat4(2),f4(1,2,1),o4(1,1)
    real(real128) :: t16(1),p16(1),lat16(2),f16(1,2,1),o16(1,1)
    integer :: status
    t4=300._real32;p4=100000._real32
    call check(abs(real(potential_temperature(t4(1),p4(1)),real64)-300._real64)<1.e-5_real64,'potential temperature real32')
    t16=300._real128;p16=100000._real128
    call check(abs(real(potential_temperature(t16(1),p16(1)),real64)-300._real64)<1.e-14_real64,'potential temperature real128')
    lat4=[0._real32,0.5_real32];f4=1._real32
    call meridionalIntegral(lat4,f4,lat4(1),lat4(2),o4,status)
    call check(status==1,'meridional integral real32 generic')
    lat16=[0._real128,0.5_real128];f16=1._real128
    call meridionalIntegral(lat16,f16,lat16(1),lat16(2),o16,status)
    call check(status==1,'meridional integral real128 generic')
  end subroutine

  subroutine test_binio_all_ranks_and_kinds
    type(finfo) :: fi
    character(*), parameter :: fn='test_extended_binio.tmp'
    real(real32) :: s0, r0, a3(2,2,2), b3(2,2,2), a4(2,1,2,2), b4(2,1,2,2), a5(1,2,1,2,2), b5(1,2,1,2,2)
    real(real128) :: q1(3), qr1(3)
    integer(int64) :: rec
    integer :: i
    s0=1.25_real32
    fi=finfo(file=fn,action='WrItE',record=1_int64,recl=4_int64,recstep=1_int64)
    call fi%fwrite(s0); call fi%fclose()
    fi=finfo(file=fn,action='READ',record=1_int64,recl=4_int64,recstep=1_int64)
    call fi%fread(r0); call fi%fclose(); call check(r0==s0,'scalar binio')
    a3=reshape([(real(i,real32),i=1,size(a3))],shape(a3)); call io_rank3(a3,b3,fn); call check(all(a3==b3),'rank3 binio')
    a4=reshape([(real(i,real32),i=1,size(a4))],shape(a4)); call io_rank4(a4,b4,fn); call check(all(a4==b4),'rank4 binio')
    a5=reshape([(real(i,real32),i=1,size(a5))],shape(a5)); call io_rank5(a5,b5,fn); call check(all(a5==b5),'rank5 binio')
    q1=[1._real128/3._real128,2._real128/3._real128,1._real128]
    fi=finfo(file=fn,action='write',record=1_int64,recl=12_int64,recstep=1_int64); call fi%fwrite(q1); call fi%fclose()
    fi=finfo(file=fn,action='read',record=1_int64,recl=12_int64,recstep=1_int64); call fi%fread(qr1); call fi%fclose()
    call check(all(qr1==real(real(q1,real32),real128)),'real128 binio through real32 payload')
    fi=finfo(file=fn,action='write',record=1_int64,recl=4_int64,recstep=2_int64)
    call fi%fwrite(1._real32); call fi%fwrite(3._real32); call fi%reset_record(increment=-2_int64); call fi%get_record(rec)
    call check(rec==3_int64,'reset_record increment')
    call fi%reset_record(increment=2_int64,newrecord=99_int64); call fi%get_record(rec)
    call check(rec==5_int64,'increment takes priority over newrecord')
    call fi%fclose()
    call remove_file(fn)
  end subroutine

  subroutine io_rank3(a,b,fn)
    real(real32),intent(in)::a(:,:,:); real(real32),intent(out)::b(:,:,:); character(*),intent(in)::fn; type(finfo)::fi
    fi=finfo(file=fn,action='write',record=1_int64,recl=int(size(a)*4,int64),recstep=1_int64); call fi%fwrite(a); call fi%fclose()
    fi=finfo(file=fn,action='read',record=1_int64,recl=int(size(a)*4,int64),recstep=1_int64); call fi%fread(b); call fi%fclose()
  end subroutine
  subroutine io_rank4(a,b,fn)
    real(real32),intent(in)::a(:,:,:,:); real(real32),intent(out)::b(:,:,:,:); character(*),intent(in)::fn; type(finfo)::fi
    fi=finfo(file=fn,action='write',record=1_int64,recl=int(size(a)*4,int64),recstep=1_int64); call fi%fwrite(a); call fi%fclose()
    fi=finfo(file=fn,action='read',record=1_int64,recl=int(size(a)*4,int64),recstep=1_int64); call fi%fread(b); call fi%fclose()
  end subroutine
  subroutine io_rank5(a,b,fn)
    real(real32),intent(in)::a(:,:,:,:,:); real(real32),intent(out)::b(:,:,:,:,:); character(*),intent(in)::fn; type(finfo)::fi
    fi=finfo(file=fn,action='write',record=1_int64,recl=int(size(a)*4,int64),recstep=1_int64); call fi%fwrite(a); call fi%fclose()
    fi=finfo(file=fn,action='read',record=1_int64,recl=int(size(a)*4,int64),recstep=1_int64); call fi%fread(b); call fi%fclose()
  end subroutine

  subroutine test_endian_bytes
    real(real32) :: x
    integer(int8) :: before(storage_size(x)/8), after(storage_size(x)/8)
    x=1._real32; before=transfer(x,before); call endian_converter(x); after=transfer(x,after)
    call check(all(after==before(size(before):1:-1)),'endian converter reverses bytes')
  end subroutine

  subroutine remove_file(fn)
    character(*),intent(in)::fn; integer::u; logical::exists
    inquire(file=fn,exist=exists); if(exists) then; open(newunit=u,file=fn,status='old'); close(u,status='delete'); end if
  end subroutine

end program test_extended
