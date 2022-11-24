module localParamKernel

  ! 2022.11.24 Nobuaki Fuji
  
  implicit none
  

  integer :: nktype_real

  ! file name

  character(200) :: infofile, gridfile, kerfile, kertotalfile
  character(120) :: tmpchar
  integer :: i,icheck,jt,it,k,j
  character(40) :: datex,timex
  character(120) :: list
  real(kind(0d0)) :: tmparray(1:9) ! for model parameters

  real(kind(0d0)) :: thetasgcs,phisgcs
  real(kind(0d0)) :: thetargcs,phirgcs
  real(kind(0d0)) :: distan,azim,bazim

  ! ignoring scheme
  integer :: iitype,itype
  integer(2), allocatable :: iflagForRapidity(:),iflagForRapidityOld(:), iflagForRapidityNext(:)
  integer(2), allocatable :: iflagForRapidity0(:),modiflag(:)
  integer :: searchiteration,iiphi
  
  ! filter
  integer :: ift
  
  ! the output
  real(kind(0e0)), allocatable :: totalker(:,:,:,:,:)
  integer :: kc,idum
  real(kind(0e0)) :: fdum


  ! number of components ! attention! these definitions are also necessary in calculateKenel !!
  integer, parameter :: num_tsgtPSV = 20
  integer, parameter :: num_rsgtPSV = 10
  integer, parameter :: num_synnPSV = 10
  integer, parameter :: num_tsgtSH = 10
  integer, parameter :: num_rsgtSH = 5
  integer, parameter :: num_synnSH = 5
  integer, parameter :: num_h3 = 6
  integer, parameter :: num_h4 = 6


  ! for MPI
  integer :: nproc, my_rank, ierr
  
end module localParamKernel

subroutine local_MPI_INIT
  use localParamKernel
  implicit none
  
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,my_rank,ierr)
end subroutine local_MPI_INIT

subroutine local_MPI_BCAST_1
  use localParamKernel
  use parametersKernel
  use tmpSGTs
  use angles
  use kernels
  use rotate
  implicit none

  call MPI_BCAST(iCompute,     1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(SGTinfo,    120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(parentDir,  120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(eventName,  120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(stationName,120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(phase,      120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(compo,      120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(paramWRT,   120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(synnfile,   120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(INFO_TSGT,  120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(INFO_RSGT,  120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rsampletxt, 120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(modelcard,  120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(Poutputdir, 120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(psvmodel,   120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(modelname,  120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(DSMconfFile,120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(evla,      1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(evlo,      1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(evdepth,   1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(stla,      1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(stlo,      1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(mt,        6,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(twin,      4,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ipdistance,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(c_red_reci,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ibwfilt,      1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(npButterworth,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ifastFFT,     1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(fmin,         1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(fmax,         1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(fclp,1+nfilter,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(fchp,1+nfilter,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(freqid,120*(1+nfilter),MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(dph,       1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ph1,       1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(dth,       1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(thw,       1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(start,     1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(end,       1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(iWindowStart, 1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(iWindowEnd,   1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(samplingHz,  1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(calculrapide,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(nntype,       1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(iPSVSH,       1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(iCompute,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)


  ! minici should be for each node
  ! NF: I cannot make this simpler for the moment Nov. 2022
  if(my_rank.ne.0) then
     allocate(minici(1:nntype,0:nfilter))
     allocate(idecidetype(1:nntype))
  endif  
  call MPI_BCAST(idecidetype,nntype,MPI_INTEGER,0,MPI_COMM_WORLD,ierr) 
 
  

  ! Component to handle
  
  mtype=0
  if(compo.eq.'Z') then
     mtype=mtype+10
  elseif(compo.eq.'R') then
     mtype=mtype+20
  elseif(compo.eq.'T') then
     mtype=mtype+30
  else
     compo='X'
     mtype=99
  endif

  ! exporting DSM parameters
  call MPI_BCAST(re,  1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ratc,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ratl,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(omegai,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(tlen,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rmin_,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rmax_,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rdelta_,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  return

end subroutine local_MPI_BCAST_1


subroutine local_MPI_BCAST_2
  use localParamKernel
  use parametersKernel
  use tmpSGTs
  use angles
  use kernels
  use rotate
  implicit none
  


  if((iCompute.eq.0).or.(iCompute.eq.1)) then
     if(my_rank.eq.0) then
        r_n =  int((rmax_-rmin_)/rdelta_)+1
     endif
     call MPI_BCAST(r_n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
     allocate(r_(1:r_n))
     if(my_rank.eq.0) then
        do i = 1, r_n
           r_(i) = rmin_ + dble(i-1)*rdelta_
        enddo
     endif
     call MPI_BCAST(r_,r_n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  

     if(my_rank.eq.0) then
        if((rmin.eq.0.d0).and.(rmax.eq.0.d0).and.(rdelta.eq.0.d0)) then
           rmin=rmin_
           rmax=rmax_
           rdelta=rdelta_
        endif
     endif
     
  endif

  !! NF here iCompute=2 option should be treated: r(:) should be constructed from the whole list of points

  
  call MPI_BCAST(rmin,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rmax,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rdelta,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  
  if(my_rank.eq.0) then
     nr =  int((rmax-rmin)/rdelta)+1
  endif
  call MPI_BCAST(nr,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  allocate(r(1:nr))
  if(my_rank.eq.0) then
     do i = 1, nr
        r(i) = rmin + dble(i-1)*rdelta
     enddo
  endif
  call MPI_BCAST(r,nr,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
        
  ! check r(:) and r_(:) because we don't interpolate for depth
  ! NF wants to limit the number of r(:) and avoid Specfem3D connection: regSEM should be better for Earth problems, i.e. DSM's interest
  
  do ir=1,nr
     icheck=0
     do i=1,r_n
        if(r_(i).eq.r(ir)) then
           icheck=1
        endif
     enddo
     if(icheck.eq.0) then
        print *, r(ir), "is not in the catalogue, sorry"
        stop
     endif
  enddo
  
  
     

  call MPI_BCAST(r0min,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(r0max,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(r0delta,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  if(my_rank.eq.0) then
     r0_n =  int((r0max-r0min)/r0delta)+1
  endif
  call MPI_BCAST(r0_n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)  
  allocate(r0D(1:r0_n))
  if(my_rank.eq.0) then
     do i = 1, r0_n
        r0D(i) = r0min + dble(i-1)*r0delta
     enddo
  endif
  call MPI_BCAST(r0D,r0_n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)   
  ir0 = r0_n

  
  call MPI_BCAST(thetamin,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(thetamax,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(thetadelta,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  if(iCompute.eq.1) then
     call MPI_BCAST(qlat_min,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
     call MPI_BCAST(qlat_max,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
     call MPI_BCAST(qlat_delta,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
     call MPI_BCAST(qlon_min,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
     call MPI_BCAST(qlon_max,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
     call MPI_BCAST(qlon_delta,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  endif
  
  if(my_rank.eq.0) then
     theta_n = int((thetamax-thetamin)/thetadelta)+1
  endif
  call MPI_BCAST(theta_n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  allocate(thetaD(1:theta_n))
  if(my_rank.eq.0) then   
     do i = 1,theta_n
        thetaD(i) = (thetamin + dble(i-1)*thetadelta)
     enddo
  endif  
  call MPI_BCAST(thetaD,theta_n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  return
end subroutine local_MPI_BCAST_2

subroutine local_MPI_BCAST_DSM_params
  use localParamKernel
  use parametersKernel
  use tmpSGTs
  use angles
  use kernels
  use rotate
  implicit none

  
  if(my_rank.eq.0) then
     psvmodel = tmppsvfile
     open(20, file = psvmodel, status = 'old', action='read', position='rewind')
     read(20,*) nzone
     close(20)
  endif
  call MPI_BCAST(nzone,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  allocate(vrminD(1:nzone))
  allocate(vrmaxD(1:nzone))
  allocate(rrhoD(1:4,1:nzone))
  allocate(vpvD(1:4,1:nzone))
  allocate(vphD(1:4,1:nzone))
  allocate(vsvD(1:4,1:nzone))
  allocate(vshD(1:4,1:nzone))
  allocate(etaD(1:4,1:nzone))
  allocate(qmuD(1:nzone))
  allocate(qkappaD(1:nzone))
  if(my_rank.eq.0) then
     psvmodel = tmppsvfile
     open(20, file = psvmodel, status = 'old', action='read', position='rewind')
     read(20,*) nzone
     do i = 1, nzone
        read (20, *) vrminD(i), vrmaxD(i), rrhoD(1,i), rrhoD(2,i), rrhoD(3,i), rrhoD(4,i), vpvD(1,i), vpvD(2,i), vpvD(3,i), vpvD(4,i), vphD(1,i), vphD(2,i), vphD(3,i), vphD(4,i), vsvD(1,i), vsvD(2,i), vsvD(3,i), vsvD(4,i), vshD(1,i), vshD(2,i), vshD(3,i), vshD(4,i), etaD(1,i), etaD(2,i), etaD(3,i), etaD(4,i), qmuD(i), qkappaD(i)
     enddo
     close(20,status='delete')
  endif
  call MPI_BCAST(vrminD,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vrmaxD,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rrhoD,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vpvD,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vphD,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vsvD,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vshD,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(etaD,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(qmuD,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(qkappaD,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  

  rEarth=vrmaxD(nzone)

  return
  
end subroutine local_MPI_BCAST_DSM_params


subroutine local_allocate_values
  use localParamKernel
  use parametersKernel
  use tmpSGTs
  use angles
  use kernels
  use rotate
  implicit none

  
  ! Record the date and time at the beginning of the job
  if(my_rank.eq.0) then
     list = trim(parentDir)//"/log/calLog"//"."// &
          trim(stationName)//"."//trim(eventName)//"."//trim(compo)//"."//trim(paramWRT)//".log"
     open(1,file =list, status = 'unknown', form = 'formatted')
     call date_and_time(datex,timex)
     write(1,'(/a,a4,a,a2,a,a2,a,a2,a,a2,a,a4)') &
          '    Starting date and time:                     ', &
          datex(1:4),'-',datex(5:6),'-',datex(7:8),'.  ', &
          timex(1:2),':',timex(3:4),':',timex(5:8)   
     close (1)
  endif
  !-----------------------------------------------------------------------   
  ! lsmoothfinder for FFT
  np0=fmax
  call lsmoothfinder(tlen,np0,samplingHz,lsmooth)
  i=1
  do while (i<lsmooth)
     i = i*2
  enddo
  lsmooth = i
  i = 0
  np1 = 1
  do while (np1<np0)
     np1 = np1*2
  enddo  
  np1 = np1*lsmooth    
  
  ! redefinition of samplingHz
  samplingHz = dble(2*np1)/tlen
  dtn = 1.d0/samplingHz
  iWindowStart = int(start*samplingHz)
  iWindowEnd   = int(end*samplingHz)

  

  ! allocate SGTs, synthetics in frequency
  allocate(omega(fmin:fmax))
  do i = fmin, fmax
     omega(i) = 2.d0*pi*dble(i)/tlen
  enddo

  allocate(tmph01(fmin:fmax))
  allocate(tmph02(fmin:fmax))
  allocate(tmph03(fmin:fmax))
  allocate(tmph04(fmin:fmax))
  allocate(tmph05(fmin:fmax))
  allocate(tmph06(fmin:fmax))
  allocate(tmph07(fmin:fmax))
  allocate(tmph08(fmin:fmax))
  allocate(tmph09(fmin:fmax))
  allocate(tmph10(fmin:fmax))

  allocate(tmph11(fmin:fmax))
  allocate(tmph12(fmin:fmax))
  allocate(tmph13(fmin:fmax))
  allocate(tmph14(fmin:fmax))
  allocate(tmph15(fmin:fmax))
  allocate(tmph16(fmin:fmax))
  allocate(tmph17(fmin:fmax))
  allocate(tmph18(fmin:fmax))
  allocate(tmph19(fmin:fmax))
  allocate(tmph20(fmin:fmax))

  allocate(tmph21(fmin:fmax))
  allocate(tmph22(fmin:fmax))
  allocate(tmph23(fmin:fmax))
  allocate(tmph24(fmin:fmax))
  allocate(tmph25(fmin:fmax))
  allocate(tmph26(fmin:fmax))


  ! allocate vectors in time domain
  allocate(t(iWindowStart:iWindowEnd))
  allocate(u(iWindowStart:iWindowEnd))
  allocate(u0(0:nfilter,iWindowStart:iWindowEnd))
  allocate(v(iWindowStart:iWindowEnd))
  allocate(v0(0:nfilter,iWindowStart:iWindowEnd))
  allocate(hu(iWindowStart:iWindowEnd))
  allocate(hu0(0:nfilter,iWindowStart:iWindowEnd))
  allocate(fwin(0:nfilter,iWindowStart:iWindowEnd))  
  allocate(nt1(0:nfilter))
  allocate(nt2(0:nfilter))
  allocate(denomv(0:nfilter))
  allocate(denomu(0:nfilter))
  
  do i = iWindowStart,iWindowEnd
     t(i) = dble(i)*dtn
  enddo

  
  ! extract the reference model parameters on the radial grid points
  allocate(rhom(1:nr))
  allocate(vpm(1:nr))
  allocate(vsm(1:nr))
  allocate(qmm(1:nr))
  allocate(qkp(1:nr))
  do ir = 1,nr
     call calstg_for_card(r(ir),nzone,vrminD,vrmaxD,rrhoD,vpvD,vphD,vsvD,vshD,etaD,qmuD,qkappaD,tmparray)
     rhom(ir) = tmparray(2)
     vpm(ir)  = 5.d-1*tmparray(3)+5.d-1*tmparray(7)
     vsm(ir)  = 5.d-1*tmparray(4)+5.d-1*tmparray(8)
     qmm(ir)  = tmparray(6)
     qkp(ir)  = tmparray(5)
     if(qmm(ir).le.0.d0) qmm(ir)  = 1.d5
     if(qkp(ir).le.0.d0) qkp(ir)  = 1.d5
  enddo



  call find_cmb(rcmb,nzone,vrminD,vrmaxD,vsvD,vshD)

  ! source check
  
  icheck=0
  do ir0 = 1,r0_n
     if(rEarth-evdepth.eq.r0D(ir0)) then
        icheck =1
     endif
  enddo
  if(icheck.eq.0) then
     print *, "depth",evdepth, "is not in the catalogue",Poutputdir,",sorry"
     stop
  endif
  
  sdep=evdepth
  rs=rEarth-evdepth
  rr=rEarth-rdep
  
  ! Convert the event latitude (0 to +/- pi/2) to co-latitude (0 to pi)
  !  and psuedo-longitude (0 to +/- pi) to longitude (0 to 2*pi).
  
  slat = evla
  slon = evlo
  rlat  = stla
  rlon  = stlo
  
  !thetasgcs0=(90.d0-slat)*pi/180.d0
  !phisgcs0=slon*pi/180.d0
  !if(slon.lt.0.d0) phisgcs0=(360.d0+slon)*pi/180.d0
  
  thetargcs=(90.d0-rlat)*pi/180.d0
  phirgcs=rlon*pi/180.d0
  if(rlon.lt.0.d0) phirgcs=(360.d0+rlon)*pi/180.d0
  
  thetasgcs=(90.d0-slat)*pi/180.d0
  phisgcs=slon*pi/180.d0
  if(slon.lt.0.d0) phisgcs=(360.d0+slon)*pi/180.d0


  slat=90.d0-thetasgcs*180.d0/pi
  slon=phisgcs*180.d0/pi

  ! Calculate the station epicentral distance and azimuth
  call azimth(0,slat,slon,rlat,rlon,distan,azim,bazim)
  call rotmat(thetasgcs,phisgcs,thetargcs,phirgcs,azim)

  
  

  if(my_rank.eq.0) then
     list = trim(parentDir)//"/log/calLog"//"."// &
          trim(stationName)//"."//trim(eventName)//"."//trim(compo)//"."//trim(paramWRT)//".log"
     open(1,file =list, status = 'old', access='append',form = 'formatted')
     
     write(1,'(a,4(1x,f8.4))') '    Source and distance: ', &
     	  slat,slon,sdep,distan
     write(1,'(a,4(1x,f8.4))') '    Receiver location:            ', &
     	  rlat,rlon
     write(1,'(a,2(1x,f8.4))') '    azimuth and back-azimuth:     ', &
     	  azim,bazim
     close(1)


  endif


  ! Of course MinLengthFor0 can be chosen but nMinLengthFor0=2 works well so I fixed this value
  MinLengthFor0 = dph*2.d0
  nMinLengthFor0=int(MinLengthFor0/dph)
  ! see the explication above

  
  return
end subroutine local_allocate_values


subroutine local_allocate_catalogue
  ! Nobuaki Fuji 2022.11.24
  ! This subroutine allocates sgt catalogues and writes grid files (iCompute=0,1 for time being)
  use localParamKernel
  use parametersKernel
  use tmpSGTs
  use angles
  use kernels
  use rotate
  implicit none


  if((iCompute.eq.0).or.(iCompute.eq.1)) then
     if(my_rank.eq.0) then
        
        if(trim(paramWRT).eq.'vRSGT') then
           nktype_real=num_h3-1
        elseif(trim(paramWRT).eq.'vTSGT') then
           nktype_real=num_h4-1
        elseif((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'allV').or.(trim(paramWRT).eq.'serious')) then
           nktype_real=nkvtype
        else
           nktype_real=nktype
        endif
        
        
        ift=nfilter
        
        
        gridfile = trim(parentDir)//trim(stationName)//"."//trim(eventName)//"."//&
             trim(phase)//"."//trim(compo)//"."//trim(paramWRT)//trim(".grid")
        open(1,file=gridfile,status='unknown',form='unformatted',access='sequential')
        write(1) nr, nphi, ntheta, nktype_real
        write(1) real(r)
        write(1) real(phitheta) 
        write(1) real(thetaphi)
        
        ! in iCompute = 1 mode, phitheta denotes the real latitude and thetaphi denotes the real longitude
        close(1)
        
        
     endif
  endif

  ! allocation for catalogues
  allocate(tsgtomega(1:num_tsgtPSV,fmin:fmax,1:theta_n))
  allocate(rsgtomega(1:num_rsgtPSV,fmin:fmax,1:theta_n))
  allocate(synnomega(1:num_synnPSV,fmin:fmax,1:theta_n))
  
  ! allocation for sgtF (interpolated functions)
  allocate(tsgtF(1:num_tsgtPSV,fmin:fmax))
  allocate(rsgtF(1:num_rsgtPSV,fmin:fmax))
  allocate(synnF(1:num_synnPSV,fmin:fmax))
  allocate(h3(1:num_h3,fmin:fmax))
  allocate(h4(1:num_h4,fmin:fmax))
  allocate(u_freq(fmin:fmax))

  ! Now computing reference synthetic (in my_rank = 0)
  tsgtF=cmplx(0.d0)
  rsgtF=cmplx(0.d0)
  synnF=cmplx(0.d0)
  
  synnomega=cmplx(0.d0)

  tsgtomega=cmplx(0.d0)
  !print *, "coucou  avant rsgt=0"
  rsgtomega=cmplx(0.d0)
  !print *, "coucou avant u0=0"
  h3=cmplx(0.d0)
  h4=cmplx(0.d0)
  u=0.d0
  u0=0.d0

  return
end subroutine local_allocate_catalogue
