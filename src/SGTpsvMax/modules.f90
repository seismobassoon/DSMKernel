module parameters  


!-------------------------<< input matrix >>----------------------------------
  !include 'mpif.h'  
  character(120) :: outputDir, psvmodel, modelname,DSMconfFile 
  character(120) :: list,list1
  character(40) :: datex,timex
  real(kind(0d0)), parameter :: pi=3.1415926535897932d0
  real(kind(0d0)) :: re,ratc,ratl
  integer :: maxlmax,deltalwindow
  real(kind(0d0)) :: maxMemoryInGigabyte
  integer, allocatable :: iFrequencyArray(:), lChunk(:,:)
  integer :: iFrequencyChunk,nFrequencyChunk
  integer :: iAngularOrderChunk,nAngularOrderChunk

  real(kind(0d0)) :: tlen 
  real(kind(0d0)) :: rmin_, rmax_, rdelta_ 
  real(kind(0d0)) :: r0min, r0max, r0delta  !!! JUST FOR ONE DEPTH FOR THIS MOMENT !!
  real(kind(0d0)) :: thetamin, thetamax, thetadelta
  real(kind(0d0)), allocatable :: r_(:),r0(:),theta(:),theta_radian(:)
  real(kind(0d0)), allocatable :: rrsta(:,:)
  real(kind(0d0)), allocatable :: lambda(:),qkp(:) !! only for stack points and only used for fluid regions

  logical, allocatable :: log_solid_liquid(:) ! true for solid and false for liquid
   
  integer, allocatable :: iista(:,:)
  integer :: r_n,r0_n,ciista, ir_,ir0,imt,icomp,itheta, theta_n
  integer :: intir0  
  character(120) :: coutfile
  integer :: imin, imax
  integer :: rsgtswitch, tsgtswitch, synnswitch, psgtswitch


  
  ! ---------------------------<< variables >>---------------------------
  ! variable for the trial function
  integer:: nnlayer,nlay
  integer, allocatable :: nlayer(:)
  integer:: nslay,nllay
  integer:: inlayer,jnlayer,jnslay,jnllay
  integer:: l,m
  real(kind(0d0)),allocatable:: ra(:)
  ! variable for the structure
  integer:: nzone,isl,ill,nsl,nll
  integer,allocatable:: iphase(:)
  integer::ndc,vnp
  real(kind(0d0)):: rmin,rmax
  real(kind(0d0)),allocatable:: vrmin(:),vrmax(:),rrho(:,:),vpv(:,:),vph(:,:)
  real(kind(0d0)),allocatable:: vsv(:,:),vsh(:,:),eta(:,:),qmu(:),qkappa(:)
  real(kind(0d0)),allocatable:: vra(:),rho(:),kappa(:) 
  real(kind(0d0)),allocatable:: ecKx(:) !3*Kx=3A-4N
  real(kind(0d0)),allocatable:: ecKy(:) !3*Ky=3F+2N
  real(kind(0d0)),allocatable:: ecKz(:) !3*Kz=2F+C
  real(kind(0d0)),allocatable:: mu(:),ecL(:),ecN(:),rhoinv(:),kappainv(:)
  complex(kind(0d0)),allocatable:: coef1(:),coef2(:),coef(:)
  ! variable for the periodic range
  
  real(kind(0d0)):: omega,omegai
  ! variable for the source
  integer:: spn,ns
  real(kind(0d0)):: mt(3,3),spo
  real(kind(0d0)):: ecC0,ecF0,ecL0
  complex(kind(0d0)):: ya(4),yb(4),yc(4),yd(4)

  ! variable for the matrix elements
  complex(kind(0d0)),allocatable:: a0(:,:),a1(:,:),a2(:,:), a(:,:), c(:,:), ctmp(:,:)
  real(kind(0d0)), allocatable :: t(:)
  real(kind(0d0)), allocatable :: h1x(:), h1y(:), h1z(:), h2L(:), h2N(:), h3ax(:), h3ay(:), h3az(:), h4aL(:), h4aN(:), h5ax(:), h5ay(:), h5az(:), h6aL(:), h6aN(:), h3x(:), h3y(:), h3z(:), h4L(:), h4N(:), h5x(:), h5y(:), h5z(:), h6L(:), h6N(:), h7x(:), h7y(:), h7z(:), h8L(:), h8N(:), h3mx(:,:), h3my(:,:), h3mz(:,:), h5mx(:,:), h5my(:,:), h5mz(:,:), h4m1L(:,:), h4m1N(:,:), h4m2L(:,:), h4m2N(:,:), h6m1L(:,:), h6m1N(:,:), h6m2L(:,:), h6m2N(:,:)
  real(kind(0d0)),allocatable:: p1(:),p2(:),p3(:)
  complex(kind(0d0)),allocatable:: g0(:)
  complex(kind(0d0)),allocatable:: d0(:)
  complex(kind(0d0)):: g0tmp(2),g0dertmp(2) ! forward
  ! variable for the stack point
  integer,allocatable:: isp(:),issp(:),ilsp(:),jssp(:),jsp(:), ksp(:),lsp(:)
  integer::isdr,jsdr,ildr,cista,cksta
  ! variables for the output stack point
  integer,allocatable:: istazone(:)
  integer,allocatable:: ksta(:)   ! output stack point for g
  integer,allocatable:: jsta(:)   ! output stack point for d
  
 
  ! variables for the gridding
  integer,allocatable:: jjdr(:),kkdr(:)
  integer:: jdr,kdr
  real(kind(0d0)),allocatable:: vmin(:),gridpar(:),dzpar(:)
  ! variables for l cut off
  integer:: kc,lsuf,sufzone,ismall,llog
  real(kind(0d0)):: maxamp
  ! variables for the numerical integration
  complex(kind(0d0)):: anum(4,4,10),bnum(4,4,10)
 
  ! other variables
  integer:: i,j,nn,ier,itmp,jtmp,mtmp,kkdr0,nn0,ig2
  integer:: ll(12),lli(12),llj(12)
  real(kind(0d0)):: eps,l2,lsq
  real(kind(0d0)),allocatable:: work(:)
  complex(kind(0d0)), allocatable ::z(:), w(:),cwork(:)
  
  !-----------------------------------------------------------------------
  !complex(kind(0d0)), allocatable :: dvec(:,:,:,:),dvecdt(:,:,:,:),dvecdp(:,:,:,:)
  complex(kind(0d0)), allocatable :: dvec0(:,:,:),dvecdt0(:,:,:),dvecdp0(:,:,:)
  complex(kind(0d0)), allocatable :: tsgt(:,:,:,:),rsgt(:,:,:),synn(:,:) ,psgt(:,:,:,:)
  complex(kind(0e0)), allocatable :: tsgtsngl(:,:), rsgtsngl(:,:),synnsngl(:,:),psgtsngl(:,:)
  real(kind(0d0)), allocatable :: plm(:,:,:)
  complex(kind(0d0)) :: rdvec(1:3,-2:2)
  complex(kind(0d0))::u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3)



  data eps /-1.d0/

  ! for Pinv : num_tsgt = 4, num_rsgt = 1
  
  !integer, parameter :: num_tsgt = 4
  !integer, parameter :: num_rsgt = 1
  integer, parameter :: num_tsgt = 20
  integer, parameter :: num_rsgt = 10
  integer, parameter :: num_synn = 10



  
  !--------------------------------------------------------------------------
  ! for MPI

  integer :: nproc,my_rank,ierr
    
  !--------------------------------------------------------------------------
  

  
end module parameters



subroutine bcast_allocate_1
  use mpi
  use parameters
  implicit none
  character(200) :: tmpfile
  
  if(my_rank.eq.0) then
     tmpfile='argvModeUsed'
     print *, allocated(qkappa)
     call pinputDatabaseFileMAX(DSMconfFile,outputDir,psvmodel,modelname,tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta,thetamin,thetamax,thetadelta,imin,imax,rsgtswitch,tsgtswitch,synnswitch,psgtswitch,re,ratc,ratl,omegai,maxlmax,deltalwindow,maxMemoryInGigabyte,tmpfile)

     print *, maxMemoryInGigabyte, tmpfile, DSMconfFile
     print *, "no error is observed 09032023"
     if(allocated(qkappa))print *, "wow, qkappa is allocated  before timefilenamegenerator  in ", my_rank, allocated(qkappa)
     print *, allocated(qkappa)
     print *, "no error is observed 09032023-2"


     
     !call readDSMconfFile(DSMconfFile,re,ratc,ratl,omegai,maxlmax)
     tmpfile='tmpworkingfile_for_psvmodel'
     call tmpfileNameGenerator(tmpfile,tmpfile)
     call readpsvmodel(psvmodel,tmpfile)
     psvmodel=tmpfile
 
     open(20, file = psvmodel, status = 'old', action='read', position='rewind')
     read(20,*) nzone
     close(20)
 
  endif

  if(allocated(qkappa))print *, "wow, qkappa is allocated after pinput in ", my_rank
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  ! exporting DSM parameters
  call MPI_BCAST(re,  1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ratc,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(ratl,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(omegai,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(maxlmax,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  
  call MPI_BCAST(outputDir,120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(psvmodel,120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(modelname,120,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(tlen,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(imin,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(imax,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rsgtswitch,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(tsgtswitch,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(synnswitch,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(psgtswitch,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(nzone,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  call MPI_BCAST(rmin_,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rmax_,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rdelta_,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(thetamin,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(thetamax,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(thetadelta,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(r0min,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(r0max,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(r0delta,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  

  allocate(nlayer(1:nzone))
  allocate(iphase(1:nzone))
  allocate(vrmin(1:nzone))
  allocate(vrmax(1:nzone))
  allocate(rrho(1:4,1:nzone))
  allocate(vpv(1:4,1:nzone))
  allocate(vph(1:4,1:nzone))
  allocate(vsv(1:4,1:nzone))
  allocate(vsh(1:4,1:nzone))
  allocate(eta(1:4,1:nzone))
  allocate(qmu(1:nzone))
  allocate(qkappa(1:nzone))
  
  allocate(coef1(1:nzone))
  allocate(coef2(1:nzone))
  allocate(coef(1:nzone))
  allocate(jjdr(1:nzone))
  allocate(kkdr(1:nzone))
  allocate(vmin(1:nzone))
  allocate(gridpar(1:nzone))
  allocate(dzpar(1:nzone))
  allocate(isp(1:nzone))
  allocate(issp(1:nzone))
  allocate(ilsp(1:nzone))
  allocate(jssp(1:nzone))
  allocate(jsp(1:nzone)) 
  allocate(ksp(1:nzone))
  allocate(lsp(1:nzone))

  if(my_rank.eq.0) then
     open(20, file = psvmodel, status = 'old', action='read', position='rewind')
     read(20,*) nzone
     
     do i = 1, nzone
        read (20, *) vrmin(i), vrmax(i), rrho(1,i), rrho(2,i), rrho(3,i), rrho(4,i), vpv(1,i), vpv(2,i), vpv(3,i), vpv(4,i), vph(1,i), vph(2,i), vph(3,i), vph(4,i), vsv(1,i), vsv(2,i), vsv(3,i), vsv(4,i), vsh(1,i), vsh(2,i), vsh(3,i), vsh(4,i), eta(1,i), eta(2,i), eta(3,i), eta(4,i), qmu(i), qkappa(i)
     enddo
     
     close(20,status='delete')

  endif
   
  call MPI_BCAST(vrmin,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vrmax,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(rrho,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vpv,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vph,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vsv,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(vsh,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(eta,4*nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(qmu,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(qkappa,nzone,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  
  rmin = vrmin(1)
  rmax = vrmax(nzone)
  omegai = - dlog(omegai) / tlen

  ! depths for stocking the Green function
  
  r_n =  int((rmax_-rmin_)/rdelta_)+1

  allocate(r_(1:r_n))
  allocate(log_solid_liquid(1:r_n))
  allocate(rrsta(1:3,1:r_n))
  allocate(iista(1:3,1:r_n))
  allocate(istazone(1:r_n))
  allocate(jsta(1:r_n))
  allocate(ksta(1:r_n))
  allocate(lambda(1:r_n))
  allocate(qkp(1:r_n))

  do i = 1, r_n
     r_(i) = rmin_ + dble(i-1)*rdelta_
  enddo

  lambda=0.d0
  qkp=0.d0
  ! find lambda
  do ir_ = 1,r_n
     call calstg_for_stackpoints(r_(ir_),nzone,vrmin,vrmax,rrho,vpv,vph,vsv,vsh,eta,qmu,qkappa,lambda(ir_),qkp(ir_))
  enddo

  theta_n = int((thetamax-thetamin)/thetadelta)+1

  allocate(theta(1:theta_n))
  allocate(theta_radian(1:theta_n))
  allocate(dvec0(1:3,-2:2,1:theta_n))
  allocate(dvecdt0(1:3,-2:2,1:theta_n))
  allocate(dvecdp0(1:3,-2:2,1:theta_n))
  allocate(plm(1:3,0:3,1:theta_n))
  
  do i = 1,theta_n
     theta(i) = (thetamin + dble(i-1)*thetadelta)
     theta_radian(i) = theta(i)/180.d0*pi
  enddo
  
  ! source depths
  ! for this moment we don't put any necessary allocation   
   
  r0_n =  int((r0max-r0min)/r0delta)+1

  allocate(r0(1:r0_n))

  
  do i = 1, r0_n
     r0(i) = r0min + dble(i-1)*r0delta
  enddo   
  
  ir0 = r0_n

  intir0=int(r0(ir0)*1.d3)

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
end subroutine bcast_allocate_1


subroutine preparation_2
  use mpi
  use parameters
  implicit none
  
  ! --- computing the required parameters ---
  ! counting of the nsl and nll
  call calnl( nzone,vsv,iphase,nsl,nll )
  ndc = nzone - 1
  
  ir0 = 1
  
  if ( (r0(ir0).lt.rmin) .or. (r0(ir0).gt.rmax) ) pause 'Location of the source is improper.'
  
  ! calcul du nombre et la location des points de grilles
  call calgrid( nzone,vrmin,vrmax,vpv,vsv,rmin,rmax,imax,1,tlen,vmin,gridpar,dzpar )
  
  call calra_psv(nnlayer,inlayer,jnlayer,jnslay,jnllay,gridpar,dzpar,nzone,vrmin,vrmax,iphase,rmin,rmax,nslay,nllay,nlayer,re )
  
  allocate(ra(1:nnlayer+nzone+1))
  call calra2_psv(nnlayer,gridpar,dzpar,nzone,vrmin,vrmax,rmin,rmax,nlayer,ra,re,r_n,r_,rrsta,iista,log_solid_liquid,r0(ir0),cista,iphase,istazone,ciista)
  
end subroutine preparation_2

subroutine allocation_preparation_3
  use mpi
  use parameters
  implicit none
  
  ! SSGT and TSGT allocation
  
  allocate(tsgt(1:num_tsgt,1:r_n,1:theta_n,1:r0_n))
  allocate(rsgt(1:num_rsgt,1:r_n,1:theta_n))
  allocate(synn(1:num_synn,1:theta_n))
  if(psgtswitch.eq.1) allocate(psgt(1:num_rsgt,1:r_n,1:r_n,1:theta_n))


  allocate(tsgtsngl(1:num_tsgt,1:theta_n))
  allocate(rsgtsngl(1:num_rsgt,1:theta_n))
  allocate(synnsngl(1:num_synn,1:theta_n)) 
  !if(psgtswitch.eq.1) allocate(psgtsngl(1:num_rsgt,1:r_n,1:r_n,1:theta_n))
  ! psgt option is not yet ready

  nlay = nnlayer
  allocate(vra(1:nlay+2*nzone+1))
  allocate(rho(1:nlay+2*nzone+1))
  allocate(kappa(1:nlay+2*nzone+1))
  allocate(ecKx(1:nlay+2*nzone+1)) !3*Kx=3A-4N
  allocate(ecKy(1:nlay+2*nzone+1)) !3*Ky=3F+2N
  allocate(ecKz(1:nlay+2*nzone+1)) !3*Kz=2F+C
  allocate(mu(1:nlay+2*nzone+1))
  allocate(ecL(1:nlay+2*nzone+1))
  allocate(ecN(1:nlay+2*nzone+1))
  allocate(rhoinv(1:nlay+2*nzone+1))
  allocate(kappainv(nlay+2*nzone+1))
  allocate(a0(1:4,1:2*(2*(nslay+1)+(nllay+1)+2*nzone)))
  allocate(a1(1:4,1:2*(2*(nslay+1)+(nllay+1)+2*nzone)))
  allocate(a2(1:4,1:2*(2*(nslay+1)+(nllay+1)+2*nzone))) 
  allocate(a(1:4,1:2*(nslay+1)+(nllay+1))) 
  allocate(c(1:2,1:(nslay+1)+(nllay+1)))
  allocate(ctmp(1:2,1:(nslay+1)+(nllay+1)))
  allocate(t(1:8*nslay))
  allocate(h1x(1:8*nslay))
  allocate(h1y(1:8*nslay))
  allocate(h1z(1:8*nslay))
  allocate(h2L(1:8*nslay))
  allocate(h2N(1:8*nslay))
  allocate(h3ax(1:8*nslay))
  allocate(h3ay(1:8*nslay))
  allocate(h3az(1:8*nslay))
  allocate(h4aL(1:8*nslay))
  allocate(h4aN(1:8*nslay))
  allocate(h5ax(1:8*nslay))
  allocate(h5ay(1:8*nslay))
  allocate(h5az(1:8*nslay))
  allocate(h6aL(1:8*nslay))
  allocate(h6aN(1:8*nslay))
  allocate(h3x(1:8*nslay))
  allocate(h3y(1:8*nslay))
  allocate(h3z(1:8*nslay))
  allocate(h4L(1:8*nslay))
  allocate(h4N(1:8*nslay))
  allocate(h5x(1:8*nslay))
  allocate(h5y(1:8*nslay))
  allocate(h5z(1:8*nslay))
  allocate(h6L(1:8*nslay))
  allocate(h6N(1:8*nslay))
  allocate(h7x(1:8*nslay))
  allocate(h7y(1:8*nslay))
  allocate(h7z(1:8*nslay))
  allocate(h8L(1:8*nslay))
  allocate(h8N(1:8*nslay))
  allocate(h3mx(-2:1,1:2*(nslay+nzone)))
  allocate(h3my(-2:1,1:2*(nslay+nzone)))
  allocate(h3mz(-2:1,1:2*(nslay+nzone)))
  allocate(h5mx(-1:2,1:2*(nslay+nzone)))
  allocate(h5my(-1:2,1:2*(nslay+nzone)))
  allocate(h5mz(-1:2,1:2*(nslay+nzone)))
  allocate(h4m1L(-1:2,1:2*(nslay+nzone)))
  allocate(h4m1N(-1:2,1:2*(nslay+nzone)))
  allocate(h4m2L(-2:1,1:2*(nslay+nzone)))
  allocate(h4m2N(-2:1,1:2*(nslay+nzone)))
  allocate(h6m1L(-1:2,1:2*(nslay+nzone)))
  allocate(h6m1N(-1:2,1:2*(nslay+nzone)))
  allocate(h6m2L(-2:1,1:2*(nslay+nzone)))
  allocate(h6m2N(-2:1,1:2*(nslay+nzone)))
  allocate(p1(1:8*nllay))
  allocate(p2(1:8*nllay))
  allocate(p3(1:8*nllay))
  allocate(g0(1:2*(nslay+1)+(nllay+1)+nzone))
  allocate(d0(1:(nslay+1)+(nllay+1)+nzone))
  allocate(work(1:8*nslay))
  allocate(z(1:2*(nslay+1)+(nllay+1)))
  allocate(w(1:2*(nslay+1)+(nllay+1)))
  allocate(cwork(1:4*(16*nslay+4*nllay)))
  

  ! computing the stack points
  call calsp(nzone,ndc,nsl,nll,iphase,nlayer,nslay,nllay,isp,jsp,ksp,issp,ilsp,lsp,jssp,isdr,jsdr,ildr,jdr,kdr )

  ! computing the source location
  ir0 =1
  call calspo(nlay,nzone,ndc,vrmax,iphase,inlayer,r0(ir0),rmin,rmax,ra,isp,spo,spn )
  ! ******************* Computing the matrix elements *******************
  ! data initialization
  a = 0.d0
  t = 0.d0
  h1x = 0.d0
  h1y = 0.d0
  h1z = 0.d0
  h2L = 0.d0
  h2N = 0.d0
  h3ax = 0.d0
  h3ay = 0.d0
  h3az = 0.d0
  h4aL = 0.d0
  h4aN = 0.d0
  h5ax = 0.d0
  h5ay = 0.d0
  h5az = 0.d0
  h6aL = 0.d0
  h6aN = 0.d0
  h3x = 0.d0
  h3y = 0.d0
  h3z = 0.d0
  h4L = 0.d0
  h4N = 0.d0
  h5x = 0.d0
  h5y = 0.d0
  h5z = 0.d0
  h6L = 0.d0
  h6N = 0.d0
  h7x = 0.d0
  h7y = 0.d0
  h7z = 0.d0
  h8L = 0.d0
  h8N = 0.d0
  h3mx = 0.d0
  h3my = 0.d0
  h3mz = 0.d0
  h5mx = 0.d0
  h5my = 0.d0
  h5mz = 0.d0
  h4m1L = 0.d0
  h4m1N = 0.d0
  h4m2L = 0.d0
  h4m2N = 0.d0
  h6m1L = 0.d0
  h6m1N = 0.d0
  h6m2L = 0.d0
  h6m2N = 0.d0
  p1 = 0.d0
  p2 = 0.d0
  p3 = 0.d0

  ! computing the structure grid points
  

  call calstg(nlay,nzone,nzone,iphase,rrho,vpv,vph,vsv,vsh,eta,nlayer,ra,rmax,vnp,vra,rho,kappa,ecKx,ecKy,ecKz,mu,ecL,ecN,r0(ir0),spn,ecC0,ecF0,ecL0 )
  call calinv( vnp,rho,kappa,rhoinv,kappainv )
  isl = 0
  ill = 0

  do i=1,ndc+1
     if ( iphase(i).eq.1 ) then
        isl = isl + 1
        itmp = isdr+issp(isl)
        call calmatc( nlayer(i),vnp,vra,rho ,2,0,0,ra(isp(i)), t(itmp) )
        call caltl( nlayer(i),vnp,vra,rho,ra(isp(i)),work(itmp) )
        call calt( nlayer(i), t(itmp),work(itmp), t(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecKx,0,0,0,ra(isp(i)),h1x(itmp) )
        call calhl( nlayer(i),vnp,vra,ecKx,ra(isp(i)),work(itmp) )
        call calt( nlayer(i), h1x(itmp),work(itmp),h1x(itmp))
        call calmatc( nlayer(i),vnp,vra,ecKy,0,0,0,ra(isp(i)),h1y(itmp) )
        call calhl( nlayer(i),vnp,vra,ecKy,ra(isp(i)),work(itmp) )
        call calt( nlayer(i), h1y(itmp),work(itmp),h1y(itmp))
        call calmatc( nlayer(i),vnp,vra,ecKz,0,0,0,ra(isp(i)),h1z(itmp) )
        call calhl( nlayer(i),vnp,vra,ecKz,ra(isp(i)),work(itmp) )
        call calt( nlayer(i), h1z(itmp),work(itmp),h1z(itmp))
        call calmatc( nlayer(i),vnp,vra,ecL ,0,0,0,ra(isp(i)),h2L(itmp) )
        call calhl( nlayer(i),vnp,vra,ecL,ra(isp(i)),work(itmp) )
        call calt( nlayer(i), h2L(itmp),work(itmp),h2L(itmp))
        call calmatc( nlayer(i),vnp,vra,ecN ,0,0,0,ra(isp(i)),h2N(itmp) )
        call calhl( nlayer(i),vnp,vra,ecN,ra(isp(i)),work(itmp) )
        call calt( nlayer(i), h2N(itmp),work(itmp),h2N(itmp))
        call calmatc( nlayer(i),vnp,vra,ecKx,1,0,1,ra(isp(i)),h5ax(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecKy,1,0,1,ra(isp(i)),h5ay(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecKz,1,0,1,ra(isp(i)),h5az(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecL,1,0,1,ra(isp(i)),h6aL(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecN,1,0,1,ra(isp(i)),h6aN(itmp) )
        call mtrnp( nlayer(i),h5ax(itmp),h3ax(itmp) )
        call mtrnp( nlayer(i),h5ay(itmp),h3ay(itmp) )
        call mtrnp( nlayer(i),h5az(itmp),h3az(itmp) )
        call mtrnp( nlayer(i),h6aL(itmp),h4aL(itmp) )
        call mtrnp( nlayer(i),h6aN(itmp),h4aN(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecKx,2,1,1,ra(isp(i)),h7x(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecKy,2,1,1,ra(isp(i)), h7y(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecKz,2,1,1,ra(isp(i)), h7z(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecL ,2,1,1,ra(isp(i)), h8L(itmp) )
        call calmatc( nlayer(i),vnp,vra,ecN ,2,1,1,ra(isp(i)), h8N(itmp) )
     else
        ill = ill + 1
        itmp = ildr+ilsp(ill)
        call calmatc( nlayer(i),vnp,vra,rhoinv,2,1,1,ra(isp(i)),p1(itmp) )
        call calmatc( nlayer(i),vnp,vra,rhoinv,0,0,0,ra(isp(i)),p2(itmp) )
        call calhl( nlayer(i),vnp,vra,rhoinv,ra(isp(i)),work(itmp) )
        call calt( nlayer(i),p2(itmp),work(itmp),p2(itmp) )
        call calmatc( nlayer(i),vnp,vra,kappainv,2,0,0,ra(isp(i)),p3(itmp) )
        call caltl( nlayer(i),vnp,vra,kappainv,ra(isp(i)),work(itmp) )
        call calt( nlayer(i),p3(itmp),work(itmp),p3(itmp) )
     endif
  enddo
  
  ! Computing the modified operator of the 1st derivative
  call caltstg(nlay,nzone,nzone,rrho,vpv,vph,vsv,vsh,eta,nlayer,ra,rmax,vra,kappa,ecKx,ecKy,ecKz,mu,ecL,ecN )
  isl = 0
  do i=1,ndc+1
     if ( iphase(i).eq.1 ) then
        isl = isl + 1
        itmp = isdr+issp(isl)
        jtmp = isp(i)+i-1
        call calh5( nlayer(i),vra(jtmp),ecKx(jtmp),ra(isp(i)),work(itmp) )
        call submat( nlayer(i),h5ax(itmp),work(itmp),h5x(itmp) )
        call calh5( nlayer(i),vra(jtmp),ecKy(jtmp),ra(isp(i)),work(itmp) )
        call submat( nlayer(i),h5ay(itmp),work(itmp),h5y(itmp) )
        call calh5( nlayer(i),vra(jtmp),ecKz(jtmp),ra(isp(i)),work(itmp) )
        call submat( nlayer(i),h5az(itmp),work(itmp),h5z(itmp) )
        call calh5( nlayer(i),vra(jtmp),ecL(jtmp),ra(isp(i)),work(itmp) )
        call submat( nlayer(i),h6aL(itmp),work(itmp),h6L(itmp) )
        call calh5( nlayer(i),vra(jtmp),ecN(jtmp),ra(isp(i)),work(itmp) )
        call submat( nlayer(i),h6aN(itmp),work(itmp),h6N(itmp) )
        call mtrnp( nlayer(i),h5x(itmp),h3x(itmp) )
        call mtrnp( nlayer(i),h5y(itmp),h3y(itmp) )
        call mtrnp( nlayer(i),h5z(itmp),h3z(itmp) )
        call mtrnp( nlayer(i),h6L(itmp),h4L(itmp) )
        call mtrnp( nlayer(i),h6N(itmp),h4N(itmp) )
        itmp = jsdr+jssp(isl)
        call calhm1( nlayer(i),vra(jtmp),ecKx(jtmp),ra(isp(i)),h5mx(-1,itmp) )
        call calhm1( nlayer(i),vra(jtmp),ecKy(jtmp),ra(isp(i)),h5my(-1,itmp) )
        call calhm1( nlayer(i),vra(jtmp),ecKz(jtmp),ra(isp(i)),h5mz(-1,itmp) )
        call calhm1( nlayer(i),vra(jtmp),ecL(jtmp),ra(isp(i)),h6m1L(-1,itmp) )
        call calhm1( nlayer(i),vra(jtmp),ecN(jtmp),ra(isp(i)),h6m1N(-1,itmp) )
        call calhm2( nlayer(i),vra(jtmp),ecL(jtmp),ra(isp(i)),h6m2L(-2,itmp) )
        call calhm2( nlayer(i),vra(jtmp),ecN(jtmp),ra(isp(i)),h6m2N(-2,itmp) )
        call mtrnp2( nlayer(i),1,2,h5mx(-1,itmp),h3mx(-2,itmp) )
        call mtrnp2( nlayer(i),1,2,h5my(-1,itmp),h3my(-2,itmp) )
        call mtrnp2( nlayer(i),1,2,h5mz(-1,itmp),h3mz(-2,itmp) )
        call mtrnp2( nlayer(i),1,2,h6m1L(-1,itmp),h4m2L(-2,itmp) )
        call mtrnp2( nlayer(i),1,2,h6m1N(-1,itmp),h4m2N(-2,itmp) )
        call mtrnp2( nlayer(i),2,1,h6m2L(-2,itmp),h4m1L(-1,itmp) )
        call mtrnp2( nlayer(i),2,1,h6m2N(-2,itmp),h4m1N(-1,itmp) )
     endif
  enddo

    
  ! Record the date and time at the beginning of the job
  if(my_rank.eq.0) then
     write(list, '(I7,".",I7)') imin,imax
     do j = 1,15
        if(list(j:j).eq.' ')list(j:j) = '0'
     enddo
     list = trim(outputDir)//"/log/calLog"//"."//trim(modelname)//"."//trim(list)
     
     
     open(1,file =list, status = 'unknown', form = 'formatted')
     call date_and_time(datex,timex)
     write(1,'(/a,a4,a,a2,a,a2,a,a2,a,a2,a,a4)') &
          '    Starting date and time:                     ', &
          datex(1:4),'-',datex(5:6),'-',datex(7:8),'.  ', &
          timex(1:2),':',timex(3:4),':',timex(5:8)   
     close (1)
  endif

    llog = 0

  

  write(list1, '(I7,".",I7,".",I7)') imin,imax,my_rank
  do j = 1,22
     if(list1(j:j).eq.' ')list1(j:j) = '0'
  enddo
  list1 = trim(outputDir)//"/log/list"//"."//trim(modelname)//"."//trim(list1)
  
  open(24, file = list1, status = 'unknown', form = 'formatted')
  write(24,*) 
  close(24)
     
  if(my_rank.eq.0) then
    
     open(1,file =list, status = 'old', access = 'append', form = 'formatted')
     call date_and_time(datex,timex)
     write(1,'(/a,a4,a,a2,a,a2,a,a2,a,a2,a,a4)') &
          '    here we go!:                     ', &
          datex(1:4),'-',datex(5:6),'-',datex(7:8),'.  ', &
          timex(1:2),':',timex(3:4),':',timex(5:8)   
     close (1)
  endif

end subroutine allocation_preparation_3
