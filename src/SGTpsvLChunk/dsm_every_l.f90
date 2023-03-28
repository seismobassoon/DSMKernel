! NF changed dcsymbdl.f90 (dcsbdlv0_bandwidth_1 and dcsbdlv0_bandwidth_3)


subroutine whoDoesWhatDSM
  use mpi
  use parameters
  implicit none
  real(kind(0d0)) :: omegaLocal,frequencyLocal
  real(kind(0d0)) :: reasonableLwidthInReal
  integer :: jobsOfnThetaLengthMinusOne, jobsOfnThetaLength
  integer :: reasonableLwidth

  allocate(lmaxPredefined(imin:imax))
  
  nFrequencyChunk = 0
  do i=imin,imax
     !if((i.ne.0).and.((mod(imax-my_rank-i,2*nproc).eq.0).or.(mod(imax+my_rank+1-i,2*nproc).eq.0))) then
     frequencyLocal = dble(i)/tlen
     omegaLocal = 2.d0 * pi * dble(i) / tlen

     ! NF this is ok for the Earth (5 km around)
     lmaxPredefined(i)=20000+int(frequencyLocal*5.d3)
     
     if((i.ne.0).and.(mod(i-my_rank,nproc).eq.0)) then ! forget about the l-max problem, since we work for max l every omega
        nFrequencyChunk = nFrequencyChunk + 1
     endif
  enddo
  
  allocate (iFrequencyArray(1:nFrequencyChunk))
  iFrequencyChunk = 0 
  do i=imin,imax
     !if((i.ne.0).and.((mod(imax-my_rank-i,2*nproc).eq.0).or.(mod(imax+my_rank+1-i,2*nproc).eq.0))) then
     
     if((i.ne.0).and.(mod(i-my_rank,nproc).eq.0)) then ! forget about the l-max problem, since we work for max l every omega
        iFrequencyChunk = iFrequencyChunk + 1
        iFrequencyArray(iFrequencyChunk) = i
     endif
  enddo

  ! the total of Phi can take the 60% of memory (just testing)
  
  reasonableLwidthInReal = 6.d-1 * maxMemoryInGigabyte * 1.d9 / 16.d0 / 57.d0 / dble(theta_n)

  reasonableLwidth = int(reasonableLwidthInReal)

  ! Here the maxlmax changes !!
  
  maxlmax=maxval(lmaxPredefined)

  ! Here the maxlmax changes !!!
  
  nAngularOrderChunk = (maxlmax-1)/reasonableLwidth
  if(mod(maxlmax-1,nAngularOrderChunk).ne.0) nAngularOrderChunk = nAngularOrderChunk + 1
  allocate(lChunk(1:2,1:nAngularOrderChunk))
  
  do iAngularOrderChunk = 1,nAngularOrderChunk-1
     lChunk(1,iAngularOrderChunk) = (iAngularOrderChunk-1)*reasonableLwidth
     lChunk(2,iAngularOrderChunk) = iAngularOrderChunk*reasonableLwidth-1
  enddo
  
  lChunk(1,nAngularOrderChunk) = (nAngularOrderChunk-1)*reasonableLwidth
  lChunk(2,nAngularOrderChunk) = maxlmax
  
  !print *, "theta_n=", theta_n
  !print *, "my_rank =", my_rank
  !print *, lChunk(1,:)
  !print *, lChunk(2,:)

  if(imin.eq.0) then
     lmaxPredefined(0) = 0
  endif


  ! Here I organise itheta arrays for each rank in order to precompute plm for reasonableLwidth

  nThetaLength = theta_n/nproc + 1
  jobsOfnThetaLengthMinusOne = nThetaLength*nproc - theta_n
  jobsOfnThetaLength = nproc - jobsOfnThetaLengthMinusOne
  if(my_rank.lt.jobsOfnThetaLength) then
     iThetaMinLocal = (my_rank-1) * nThetaLength +1
     iThetaMaxLocal = my_rank * nThetaLength
  else
     iThetaMinLocal = jobsOfnThetaLength*nThetaLength + (my_rank-jobsOfnThetaLength) * (nThetaLength-1) +1
     iThetaMaxLocal = jobsOfnThetaLength*nThetaLength + (my_rank-jobsOfnThetaLength+1) * (nThetaLength-1) 
  endif
  
 
  !print *, "itheta=",iThetaMinLocal,iThetaMaxLocal
  
  

  
  !print *, "lChunk starting points:", lChunk(1,:)
  !print *, "lChunk ending points:", lChunk(2,:)
  !print *, 'lmaxPredefined', lmaxPredefined(:)
  !stop
  return

  
end subroutine whoDoesWhatDSM


subroutine computePLMforlChunkLocal
  use mpi
  use parameters
  implicit none
  real(kind(0d0)) :: plmtmp(1:3,0:3),x
  real(kind(0d0)) :: plmLocal(0:3,lChunk(1,iAngularOrderChunk):lChunk(2,iAngularOrderChunk),iThetaMinLocal:iThetaMaxLocal)
  
  
  ! if it is the first lChunk (starting with l=0) then we start without no pb
  ! but if iAngularOrderChun ne 1 then we need to get the last result from the global plm
  ! 
  ! first we compute plm and bvec for all the l in the lChunk

  plmLocal = 0.d0

 
  
  if(iAngularOrderChunk.eq.1) then
     
     allocate(plmLocalLast(1:2,0:3,iThetaMinLocal:iThetaMaxLocal))
     
     do itheta=iThetaMinLocal,iThetaMaxLocal
        
        x = dcos(theta_radian(itheta))
        
        plmtmp = 0.d0
        do l=0,4
           do m = 0, min0(l,3)
              call calplm_l_small(l,m,x,plmtmp(1:3,0:3))
           enddo
           plmLocal(0:3,l,itheta)=plmtmp(1,0:3)
        enddo
        do l=5, lChunk(2,1)
           do m = 0, 3
              call calplm_l_big(l,m,x,plmtmp(1:3,0:3))
           enddo
           plmLocal(0:3,l,itheta)=plmtmp(1,0:3)
        enddo
        plmLocalLast(1:2,0:3,itheta)=plmtmp(1:2,0:3)
     enddo
     
     allocate(plmGlobal(0:3,lChunk(1,iAngularOrderChunk):lChunk(2,iAngularOrderChunk),1:theta_n))
     
  else
     
     deallocate(plmGlobalTranspose)
     
     do itheta=iThetaMinLocal,iThetaMaxLocal
        
        x = dcos(theta_radian(itheta))
        
        plmtmp(1:2,0:3)=plmLocalLast(1:2,0:3,itheta)
        plmtmp(3,0:3) = 0.d0
        
        !!! plmGlobal should be deallocated after this
        
        do l= lChunk(1,iAngularOrderChunk), lChunk(2,iAngularOrderChunk)
           do m = 0, 3
              call calplm_l_big(l,m,x,plmtmp(1:3,0:3))
           enddo
           plmLocal(0:3,l,itheta)=plmtmp(1,0:3)
        enddo
        plmLocalLast(1:2,0:3,itheta)=plmtmp(1:2,0:3)
     enddo

     allocate(plmGlobal(0:3,lChunk(1,iAngularOrderChunk):lChunk(2,iAngularOrderChunk),1:theta_n))
     
  endif


  print *, "I'm ok for", my_rank, plmLocal(1:3,100,ithetaMinLocal+1)
  call MPI_ALLGATHER(plmLocal,4*(lChunk(2,iAngularOrderChunk)-lChunk(1,iAngularOrderChunk)+1)*(iThetaMaxLocal-iThetaMinLocal+1), &
       MPI_DOUBLE_PRECISION, &
       plmGlobal, &
       4*(lChunk(2,iAngularOrderChunk)-lChunk(1,iAngularOrderChunk)+1)*(iThetaMaxLocal-iThetaMinLocal+1), &
       MPI_DOUBLE_PRECISION, MPI_COMM_WORLD,ierr)
  print *, "I'm ok for---", my_rank,ithetaMinLocal,ithetaMaxLocal
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  
  allocate(plmGlobalTranspose(0:3,1:theta_n,lChunk(1,iAngularOrderChunk):lChunk(2,iAngularOrderChunk)))
  do itheta=1,theta_n
     do l = lChunk(1,iAngularOrderChunk), lChunk(2,iAngularOrderChunk)
        plmGlobalTranspose(0:3,itheta,l) = plmGlobal(0:3,l,itheta)
     enddo
  enddo
  deallocate(plmGlobal)
  
  ! This should be very much MPI-ed!!
  ! First compute plm with each processor and communicate them each other and construct bvec

  return
  
end subroutine computePLMforlChunkLocal

subroutine computeDVECforlChunkLocal
  use mpi
  use parameters
  implicit none
  
  dvec0=dcmplx(0.d0)
  dvecdt0=dcmplx(0.d0)
  dvecdp0=dcmplx(0.d0)
  print *, "my_rank, l at computeDVECforlChunkLocal", my_rank, l

  call caldvecphi0_withoutplm(l,theta_radian(1),plmGlobalTranspose(0:3,1,l),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withoutplm(l,theta_radian(theta_n),plmGlobalTranspose(0:3,theta_n,l),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta=pi
  do itheta=2,theta_n-1
     call caldvecphi0_withoutplm_sinnonzero(l,theta_radian(theta_n),plmGlobalTranspose(0:3,theta_n,l),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n))
  enddo
  return

  
end subroutine computeDVECforlChunkLocal


subroutine dsm_write_files

  use mpi
  use parameters
  implicit none

  open(24,file =list1, status = 'old',access='append', form = 'formatted')
  write(24,*) i, dble(i)/tlen, llog-1     
  close(24)
  do ir_ = 1,r_n
     ! do ir0 = 1,r0_n
     ir0 = 1
     write(coutfile, '(I7,".",I7,".",I7,".TSGT_PSV_",i3.3)') intir0,int(r_(ir_)*1.d3),i,iAngularOrderChunk
     do j = 1,29
        if (coutfile(j:j).eq.' ')coutfile(j:j) = '0'
     enddo
     
     coutfile = trim(modelname)//"."//coutfile
     coutfile = trim(outputDir)//"/TSGT/"//coutfile
     open(1,file=coutfile,status='unknown',form='unformatted', &
          access = 'direct', recl=2*num_tsgt*kind(0e0)*theta_n)
     tsgtsngl(1:num_tsgt,1:theta_n) = tsgt(1:num_tsgt,ir_,1:theta_n,ir0)
     write(1,rec=1)tsgtsngl(1:num_tsgt,1:theta_n)
     close(1)                     
        !end
     write(coutfile, '(I7,".",I7,".RSGT_PSV_",i3.3)') int(r_(ir_)*1.d3),i,iAngularOrderChunk
     do j = 1,21
        if (coutfile(j:j).eq.' ')coutfile(j:j) = '0'
     enddo
     coutfile = trim(modelname)//"."//coutfile
     coutfile = trim(outputDir)//"/RSGT/"//coutfile
     open(1,file=coutfile,status='unknown',form='unformatted', &
          access = 'direct', recl=2*num_rsgt*kind(0e0)*theta_n)
     rsgtsngl(1:num_rsgt,1:theta_n) = rsgt(1:num_rsgt,ir_,1:theta_n)
     write(1,rec=1)rsgtsngl(1:num_rsgt,1:theta_n)
     close(1)                    
     
  enddo
  
  !do ir0 = 1, r0_n
  ir0 =1
  
  
  write(coutfile, '(I7,".",I7,".SYNN_PSV_",i3.3) ') intir0,i,iAngularOrderChunk
  do j = 1,21
     if (coutfile(j:j).eq.' ')coutfile(j:j) = '0'
  enddo
  coutfile = trim(modelname)//"."//coutfile
  coutfile = trim(outputDir)//"/RSGT/"//coutfile
  open(1,file=coutfile,status='unknown',form='unformatted', &
       access = 'direct', recl=2*num_synn*kind(0e0)*theta_n)
  synnsngl(1:num_synn,1:theta_n) = synn(1:num_synn,1:theta_n)
  write(1,rec=1)synnsngl(1:num_synn,1:theta_n)
  close(1)
  return
  !enddo        
end subroutine dsm_write_files


