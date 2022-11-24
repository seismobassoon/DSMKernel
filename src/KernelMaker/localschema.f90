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

  !! NF here iCompute=2 option should be treated: r_(:) should be constructed from the whole list of points

  
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
