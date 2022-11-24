program KernelMaker


  !
  !   
  !  KernelMaker
  !                                      by FUJI Nobuaki 2012
  !
  !       many subroutines from Li Zhao's code for normal mode
  !   
  !     0.3.1    video mode             Nobuaki Fuji 2014.3.
  !
  !     1.0.0    for new file format (compatible with SGTpsv/sh >1.0.0)
  !              


  !! NF did not at all take care of iCompute = 2 (future bridge to regSEM) 2022.11.22

  use localParamKernel
  use parametersKernel
  use tmpSGTs
  use angles
  use kernels
  use rotate
  use mpi
  
  implicit none
  include '../../etc/config_calcul/constants.h'


  call local_MPI_INIT

  if(my_rank.eq.0) call pinputKernel ! input parameters

  call local_MPI_BCAST_1
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  call local_MPI_BCAST_2 ! varies with iCompute modes
  call local_MPI_BCAST_DSM_params
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)  
  call local_allocate_values ! entre autres

  ! Creating horizontal grid and computing sines and cosines
  if(iCompute.eq.0) call greatCircleBasedGridding
  if(iCompute.eq.1) call gridMakingGeograhic

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  call local_allocate_catalogue ! with grid file writing

  call referenceSyntheticComputation ! filering, tapering, hilbert transform etc. if paramWRT=test, it stops here
  ! it's followed by time print
  ! for MarsMTInversion mode (iCompute = -??) it's better to use another subroutine

  call preparation_kernel_allocation

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  
  ntot=nphi*ntheta*nr
  k=0  

  do ir=1,nr
     ! if-line for parallelisation
     !print *, ir, r(ir), nr, my_rank, nr-my_rank-ir,2*nproc,mod(nr+my_rank+1-ir,2*nproc)
     if((ir.ne.0).and.((mod(nr-my_rank-ir,2*nproc).eq.0).or.(mod(nr+my_rank+1-ir,2*nproc).eq.0))) then
        rx=r(ir)
        call localMetacomputeKernel
     endif ! if-line for parallelisation
  enddo! ir-loop termine
  
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)

  ! write final reordered kernel output

  deallocate(ker)
  
  call compileKernelOutput
  call finalLogMessage ! just myrank=0
  call MPI_FINALIZE(ierr)
  stop
end program KernelMaker
  
