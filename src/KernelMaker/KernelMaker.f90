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

  if((iCompute.eq.0).or.(iCompute.eq.1)) call referenceSyntheticComputation ! if paramWRT=test, it stops here


  

  
  
  call MPI_BCAST(u0,(iWindowEnd-iWindowStart+1)*(nfilter+1),MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(u,(iWindowEnd-iWindowStart+1),MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  call MPI_BCAST(v0,(iWindowEnd-iWindowStart+1)*(nfilter+1),MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(v,(iWindowEnd-iWindowStart+1),MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  
  call MPI_BCAST(hu0,(iWindowEnd-iWindowStart+1)*(nfilter+1),MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(hu,(iWindowEnd-iWindowStart+1),MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  
  call fwinDeterminator

  ! extracting "the phase"

  u0(0:nfilter,iWindowStart:iWindowEnd)=u0(0:nfilter,iWindowStart:iWindowEnd)*fwin(0:nfilter,iWindowStart:iWindowEnd)
  v0(0:nfilter,iWindowStart:iWindowEnd)=v0(0:nfilter,iWindowStart:iWindowEnd)*fwin(0:nfilter,iWindowStart:iWindowEnd)
  hu0(0:nfilter,iWindowStart:iWindowEnd)=hu0(0:nfilter,iWindowStart:iWindowEnd)*fwin(0:nfilter,iWindowStart:iWindowEnd)
 
  ! Calculate the denominator in the kernel expressions
  !print *, "coucou avant coefCal"
  call coeffCalculator
  ! for radial and azimuthal anisotropy
  if((sym.ge.0.d0).and.(sym.le.360.d0)) then
     csym=dcos(sym*pi/180.d0)
     ssym=dsin(sym*pi/180.d0)
  endif
 
  !print *, "coucou coeffCal"

  if(my_rank.eq.0) then   
     list = trim(parentDir)//"/log/calLog"//"."// &
           trim(stationName)//"."//trim(eventName)//"."//trim(compo)//"."//trim(paramWRT)//".log"         
     open(1,file =list, status = 'old',access='append', form = 'formatted')
     call date_and_time(datex,timex)
     write(1,'(/a,a4,a,a2,a,a2,a,a2,a,a2,a,a4)') &
          '    kernel calculation started:                     ', &
          datex(1:4),'-',datex(5:6),'-',datex(7:8),'.  ', &
          timex(1:2),':',timex(3:4),':',timex(5:8)   
     write(1,*) 'start kernel calculations Mtype= ',mtype
     close (1)
     
  endif

  ! for video mode, number of snapshots will be decided here
  call MPI_BCAST(timeincrementV,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  number_of_snapshots = 1
  if((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'allV').or.&
       (trim(paramWRT).eq.'vRSGT').or.(trim(paramWRT).eq.'vTSGT')) then
     jtstep_timeincrementV=1
     if(timeincrementV.ne.0.d0) jtstep_timeincrementV=int(timeincrementV*samplingHz) 
     if(timeincrementV.eq.0.d0) jtstep_timeincrementV=1
     !print *, jtstep_timeincrementV, timeincrementV, samplingHz
     number_of_snapshots = (iWindowEnd-iWindowStart+1)/jtstep_timeincrementV+1
  endif

  ! for the real partials we do it differently
  

  
  
  ! Now loop over all grid ponts to compute the kernels
  ! for the parallelisation, I devide nr into nproc

  allocate(tmpker(0:nktype,0:nfilter))

  if((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'allV')) then
     allocate(tmpvideoker(0:nkvtype,0:nfilter,1:number_of_snapshots))
     allocate(videoker(1:nphi,0:nkvtype,0:nfilter,1:number_of_snapshots)) ! be careful of the difference between ker and videoker (along theta)
     tmpvideoker=0.d0
     videoker=0.e0
  endif
     
  if(trim(paramWRT).eq.'vTSGT') then
     allocate(tmpvideoker(1:num_h4,0:nfilter,1:number_of_snapshots))
     allocate(videoker(1:nphi,1:num_h4,0:nfilter,1:number_of_snapshots))
     tmpvideoker=0.d0
     videoker=0.e0
  endif

  if(trim(paramWRT).eq.'vRSGT') then
     allocate(tmpvideoker(1:num_h3,0:nfilter,1:number_of_snapshots))
     allocate(videoker(1:nphi,1:num_h3,0:nfilter,1:number_of_snapshots))
     tmpvideoker=0.d0
     videoker=0.e0
  endif
  
  allocate(ker(1:nphi,1:ntheta,0:nktype,0:nfilter))

  tmpker = 0.d0
  ker = 0.e0
  allocate(du(iWindowStart:iWindowEnd))
  allocate(duf(0:nfilter,iWindowStart:iWindowEnd)) 
  allocate(duq(iWindowStart:iWindowEnd))
  allocate(duqf(0:nfilter,iWindowStart:iWindowEnd))
  ntot=nphi*ntheta*nr
  k=0  


  do ir=1,nr
     ! if-line for parallelisation
     !print *, ir, r(ir), nr, my_rank, nr-my_rank-ir,2*nproc,mod(nr+my_rank+1-ir,2*nproc)
     if((ir.ne.0).and.((mod(nr-my_rank-ir,2*nproc).eq.0).or.(mod(nr+my_rank+1-ir,2*nproc).eq.0))) then
        rx=r(ir)
        
        synnomega=cmplx(0.d0)
        tsgtomega=cmplx(0.d0)
        rsgtomega=cmplx(0.d0)
        h3=cmplx(0.d0)
        h4=cmplx(0.d0)
    
        ! SSGT reading
        if(iPSVSH.ne.1) call rdsgtomega(rx,0.d0,num_rsgtPSV,num_rsgtPSV,20)
        if((iPSVSH.ne.2).and.(rx.gt.rcmb)) call rdsgtomega(rx,0.d0,num_rsgtSH,num_rsgtPSV,10)
        ! TSGT reading
        if(iPSVSH.ne.1) call rdsgtomega(rs,rx,num_tsgtPSV,num_tsgtPSV,200)
        if((iPSVSH.ne.2).and.(rx.gt.rcmb)) call rdsgtomega(rs,rx,num_tsgtSH,num_tsgtPSV,100)
       
        

        ! Video mode will calculate for every point

        if((trim(paramWRT).eq.'allV').or.(trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'serious').or. &
           (trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'vRSGT').or.(trim(paramWRT).eq.'vTSGT')) then
           calculrapide=0.d0
        endif 

        !print *, "calculrapide=", calculrapide, paramWRT

        if((calculrapide.eq.0.d0).and.((trim(paramWRT).ne.'betaV').and.(trim(paramWRT).ne.'allV').and.(trim(paramWRT).ne.'alphaV')&
             .and.(trim(paramWRT).ne.'vRSGT').and.(trim(paramWRT).ne.'vTSGT'))) then ! calculate for every point
           do ith = 1,ntheta
              do ip=1,nphi
                 k=k+1
                 xlat=90.d0-theta(ip,ith)
                 xlon=phi(ip,ith)
                 
                 call calculateKernel
                 ker(ip,ith,:,:)=tmpker(:,:)
                 
              enddo
              
           enddo
        elseif((trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'allV').or.(trim(paramWRT).eq.'alphaV')) then
           
            do ith = 1,ntheta
               videoker=0.e0
              do ip=1,nphi
                 k=k+1
                 xlat=90.d0-theta(ip,ith)
                 xlon=phi(ip,ith)
                 tmpvideoker=0.e0
                 call calculateKernel
                 
                 videoker(ip,:,:,:)=tmpvideoker(:,:,:)
              enddo
            
             
              do jt=1,number_of_snapshots
                 write(tmpchar,'(I7,".",I7,".",I7)') ir,ith,jt
                 do j=1,23
                    if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
                 enddo
                 
                 kerfile=trim(parentDir)//"/tmpvideoparts/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)&
                      //"."//"timemarching"
                 open(1,file=kerfile,status='unknown',form='unformatted',access='direct',recl=kind(0e0)*nphi*(1+nkvtype)*(1+nfilter))
                 write(1,rec=1) videoker(1:nphi,0:nkvtype,0:nfilter,jt)
                 close(1)            

              enddo
              
           enddo


        elseif(trim(paramWRT).eq.'vRSGT') then


           do ith=1,ntheta
              videoker=0.e0
              do ip=1,nphi
                 k=k+1
                 xlat=90.d0-theta(ip,ith)
                 xlon=phi(ip,ith)
                 tmpvideoker=0.e0
                 call calculateRSGT
                 videoker(ip,:,:,:)=tmpvideoker(:,:,:)*1.d30
              enddo
              !print *,videoker(nphi/2,1,:,:)



               do jt=1,number_of_snapshots
                 write(tmpchar,'(I7,".",I7,".",I7)') ir,ith,jt
                 do j=1,23
                    if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
                 enddo
                 
                 kerfile=trim(parentDir)//"/tmpvideoparts/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)&
                      //"."//"timemarching"
                 
                 open(1,file=kerfile,status='unknown',form='unformatted',access='direct',recl=kind(0e0)*nphi*(num_h3)*(1+nfilter))
                 write(1,rec=1) videoker(1:nphi,1:num_h3,0:nfilter,jt)
                 close(1)           
   

              enddo


    

           enddo



        elseif(trim(paramWRT).eq.'vTSGT') then

           
           
           do ith=1,ntheta
              videoker=0.e0
              do ip=1,nphi
                 k=k+1
                 xlat=90.d0-theta(ip,ith)
                 xlon=phi(ip,ith)
                 tmpvideoker=0.e0
                 call calculateTSGT
                 videoker(ip,:,:,:)=tmpvideoker(:,:,:)*1.d30
              enddo




              
              do jt=1,number_of_snapshots
                 write(tmpchar,'(I7,".",I7,".",I7)') ir,ith,jt
                 do j=1,23
                    if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
                 enddo
                 
                 kerfile=trim(parentDir)//"/tmpvideoparts/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)&
                      //"."//"timemarching"
                 
                 open(1,file=kerfile,status='unknown',form='unformatted',access='direct',recl=kind(0e0)*nphi*(num_h4)*(1+nfilter))
                 write(1,rec=1) videoker(1:nphi,1:num_h4,0:nfilter,jt)
                 close(1)           
                 
                 
              enddo

           enddo


        else ! we ignore the value smaller than calculrapide*max

           ! First obtain the values on the great circle
           ith=ntheta/2+1
           do ip=1,nphi
              k=k+1
              xlat=90.d0-theta(ip,ith)
              xlon=phi(ip,ith)
              call calculateKernel
              ker(ip,ith,:,:)=tmpker(:,:)
           enddo
           
           ith=ntheta/2+1 ! for making criteria for grid searching
           
           minici = 0.d0
           
           ! step 1
           do ift=0,nfilter
              do iitype = 1,nntype
                 itype = idecidetype(iitype)
                 minici(iitype,ift)=calculrapide*maxval(abs(ker(1:nphi,ith,itype,ift)))
              enddo
           enddo
           
           
           ! step 2 and 3
           
           ith=ntheta/2+1 
           iflagForRapidity(1:nphi) = 1
           do ift=0,nfilter 
              do ip = 1,nphi
                 do iitype = 1,nntype
                    itype = idecidetype(iitype)
                    if(abs(ker(ip,ith,itype,ift)).gt.minici(iitype,ift)) then
                       iflagForRapidity(ip) = iflagForRapidity(ip)*0
                    endif
                 enddo
              enddo
           enddo
           
           
           
           ! step 4
           
           modiflag(1:nphi-nMinLengthFor0+1)=1
           do ip=1,nphi-nMinLengthFor0+1
              do iiphi=ip,ip+nMinLengthFor0-1
                 modiflag(ip) = modiflag(ip)*iflagForRapidity(iiphi)
              enddo
              ! iflagForRapidity(ip:ip+nMinLengthFor0-1)=tmpiflagForRapidity
           enddo
           
           iflagForRapidity(1:nphi)=1
           do ip=1,nphi-nMinLengthFor0+1
              if(modiflag(ip).eq.0) then
                 iflagForRapidity(ip:ip+nMinlengthFor0-1)=modiflag(ip)    
              endif
           enddo
           
           
           iflagForRapidity0(1:nphi)=iflagForRapidity(1:nphi)
           
           ! step 5 for +side 
           do ith=ntheta/2+2,ntheta
              iflagForRapidityOld(1:nphi) = iflagForRapidity(1:nphi)
              do searchiteration = 1, int(nphi/nMinLengthFor0)+1
                 
                 if(minval(iflagForRapidity(1:nphi)).eq.1) exit
                 !print *,    searchiteration, int(nphi/nMinlengthFor0)+1
                 do ip = 1,nphi
                     if(iflagForRapidity(ip).eq.1) then
                       !ker(ip,ith,2,0)=1.d0       
                       !cycle ! don't calculate if 1
                    else
                       k=k+1
                       xlat=90.d0-theta(ip,ith)
                       xlon=phi(ip,ith)
                       call calculateKernel
                       ker(ip,ith,:,:)=tmpker(:,:)
                    endif
                 enddo
                 
                 
                 
                 ! step 2 and 3 in step5
                 iflagForRapidity(1:nphi) = 1
                 do ift=0,nfilter 
                    do ip = 1,nphi
                       do iitype = 1,nntype
                          itype = idecidetype(iitype)
                          if(abs(ker(ip,ith,itype,ift)).gt.minici(iitype,ift)) then
                             iflagForRapidity(ip) = iflagForRapidity(ip)*0
                          endif
                       enddo
                    enddo
                 enddo
                 
                 ! step 4 in step5
                 
                 
                 
                 modiflag(1:nphi-nMinLengthFor0+1)=1
                 do ip=1,nphi-nMinLengthFor0+1
                    do iiphi=ip,ip+nMinLengthFor0-1
                       modiflag(ip) = modiflag(ip)*iflagForRapidity(iiphi)
                    enddo
                    ! iflagForRapidity(ip:ip+nMinLengthFor0-1)=tmpiflagForRapidity
                 enddo
                 
                 
                 
                 iflagForRapidity(1:nphi)=1
                 do ip=1,nphi-nMinLengthFor0+1
                    if(modiflag(ip).eq.0) then
                       iflagForRapidity(ip:ip+nMinlengthFor0-1)=modiflag(ip)    
                    endif
                 enddo
                 
                 
                 
                 
                 iflagForRapidityNext(1:nphi) = iflagForRapidity(1:nphi)                   
                 
                 do ip=1,nphi
                    if((iflagForRapidityOld(ip)-iflagForRapidity(ip)).eq.1) then
                       iflagForRapidity(ip) = 0
                    else
                       iflagForRapidity(ip) = 1
                    endif
                    
                    iflagForRapidityOld(ip)=iflagForRapidityOld(ip)*iflagForRapidityNext(ip)
                 enddo
                 
              enddo
              iflagForRapidity(1:nphi) = iflagForRapidityNext(1:nphi)
              
           enddo
           


           
           ! step 5 for -side 
           iflagForRapidity(1:nphi)=0
           iflagForRapidity(1:nphi)=iflagForRapidity0(1:nphi)
           
           do ith=ntheta/2,1,-1
              iflagForRapidityOld(1:nphi) = iflagForRapidity(1:nphi)
              do searchiteration = 1, int(nphi/nMinLengthFor0)+1
                 
                 if(minval(iflagForRapidity(1:nphi)).eq.1) exit
                 !print *,    searchiteration, int(nphi/nMinlengthFor0)+1
                 do ip = 1,nphi
                     if(iflagForRapidity(ip).eq.1) then
                       !ker(ip,ith,2,0)=1.d0       
                       !cycle ! don't calculate if 1
                    else
                       k=k+1
                       xlat=90.d0-theta(ip,ith)
                       xlon=phi(ip,ith)
                       call calculateKernel
                       ker(ip,ith,:,:)=tmpker(:,:)
                    endif
                 enddo
                 
                 
                 
                 ! step 2 and 3 in step5
                 iflagForRapidity(1:nphi) = 1
                 do ift=0,nfilter 
                    do ip = 1,nphi
                       do iitype = 1,nntype
                          itype = idecidetype(iitype)
                          if(abs(ker(ip,ith,itype,ift)).gt.minici(iitype,ift)) then
                             iflagForRapidity(ip) = iflagForRapidity(ip)*0
                          endif
                       enddo
                    enddo
                 enddo
                 
                 ! step 4 in step5
                 
                 
                 
                 modiflag(1:nphi-nMinLengthFor0+1)=1
                 do ip=1,nphi-nMinLengthFor0+1
                    do iiphi=ip,ip+nMinLengthFor0-1
                       modiflag(ip) = modiflag(ip)*iflagForRapidity(iiphi)
                    enddo
                    ! iflagForRapidity(ip:ip+nMinLengthFor0-1)=tmpiflagForRapidity
                 enddo
                 
                 
                 
                 iflagForRapidity(1:nphi)=1
                 do ip=1,nphi-nMinLengthFor0+1
                    if(modiflag(ip).eq.0) then
                       iflagForRapidity(ip:ip+nMinlengthFor0-1)=modiflag(ip)    
                    endif
                 enddo
                 
                 
                 
                 
                 iflagForRapidityNext(1:nphi) = iflagForRapidity(1:nphi)                   
                 
                 do ip=1,nphi
                    if((iflagForRapidityOld(ip)-iflagForRapidity(ip)).eq.1) then
                       iflagForRapidity(ip) = 0
                    else
                       iflagForRapidity(ip) = 1
                    endif
                    
                    iflagForRapidityOld(ip)=iflagForRapidityOld(ip)*iflagForRapidityNext(ip)
                 enddo
                 
              enddo
              iflagForRapidity(1:nphi) = iflagForRapidityNext(1:nphi)
              
           enddo


        endif ! for the fast kernel calculation method
   

        ! write kernels for each depth
        
        write(tmpchar,'(I7)') int(rx*1.d3)
        do j=1,7
           if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
        enddo
        kerfile = trim(parentDir)//"/tmp/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)
        
        open(1,file=kerfile,status='unknown',form='unformatted', &
             access = 'direct', recl=kind(0e0)*nphi*ntheta*(nktype+1)*(nfilter+1))    
        write(1,rec=1) ker(1:nphi,1:ntheta,0:nktype,0:nfilter)
        close(1)
        
        
     endif ! if-line for parallelisation
  enddo! ir-loop termine
  
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)

  ! write final reordered kernel output



  deallocate(ker)
  
  if(trim(paramWRT).eq.'vRSGT') then
     allocate(totalker(nr,nphi,ntheta,1:num_h3,0:nfilter))
  elseif(trim(paramWRT).eq.'vTSGT') then
     allocate(totalker(nr,nphi,ntheta,1:num_h4,0:nfilter))
  elseif((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'allV')) then
     allocate(totalker(nr,nphi,ntheta,0:nkvtype,0:nfilter))
  else
     allocate(totalker(nr,nphi,ntheta,0:nktype,0:nfilter))
  endif

  if(my_rank.eq.0) then   
  


     if((trim(paramWRT).ne.'vRSGT').and.(trim(paramWRT).ne.'vTSGT').and.(trim(paramWRT).ne.'alphaV').and.&
          (trim(paramWRT).ne.'betaV').and.(trim(paramWRT).ne.'allV')) then
        totalker = 0.e0
        do ir=1,nr
           write(tmpchar,'(I7)') int(r(ir)*1.d3)
           do j=1,7
              if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
           enddo
           
           kerfile = trim(parentDir)//"/tmp/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)
           
           open(1,file=kerfile,status='unknown',form='unformatted', &
                access = 'direct', recl=kind(0e0)*nphi*ntheta*(nktype+1)*(nfilter+1))    
           read(1,rec=1) totalker(ir,1:nphi,1:ntheta,0:nktype,0:nfilter)
           close(1) 
        enddo
 

        do ift = 0,nfilter
           kertotalfile = trim(parentDir)//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(freqid(ift))//trim(".kernel")
           open(1,file=kertotalfile,status='unknown',form='unformatted',access='sequential')
           if(compo.eq.'Z') then
              kc=1
           elseif(compo.eq.'R') then
              kc=2
           elseif(compo.eq.'T') then
              kc=3
           else
              kc=-1
           endif
           idum=0
           fdum=0.e0
           
           write(1) totalker(:,:,:,:,ift)
           close(1) 
        enddo
     endif

  endif


  if(trim(paramWRT).eq.'vRSGT') then
     
     do jt=1,number_of_snapshots

        if(((mod(jt-my_rank-ir,2*nproc).eq.0).or.(mod(jt+my_rank+1-ir,2*nproc).eq.0))) then
           
           totalker=0.e0
           do ir=1,nr
              do ith=1,ntheta
                       

                 write(tmpchar,'(I7,".",I7,".",I7)') ir,ith,jt
                 do j=1,23
                    if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
                 enddo
                 kerfile=trim(parentDir)//"/tmpvideoparts/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)&
                      //"."//"timemarching"
                 open(1,file=kerfile,status='unknown',form='unformatted',access='direct',recl=kind(0e0)*nphi*(num_h3)*(1+nfilter))
                 read(1,rec=1) videoker(1:nphi,1:num_h3,0:nfilter,1)
                 close(1) 
                 totalker(ir,1:nphi,ith,1:num_h3,0:nfilter)=videoker(1:nphi,1:num_h3,0:nfilter,1)
              enddo
           enddo

                 
           do ift = 0,nfilter
              write(tmpchar,'(I7)') jt
              do j=1,7
                 if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
              enddo
              kertotalfile = trim(parentDir)//"/tmpvideo/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"." &
                   //trim(freqid(ift))//"."//trim(tmpchar)//trim(".video")
              open(1,file=kertotalfile,status='unknown',form='unformatted',access='sequential')
              if(compo.eq.'Z') then
                 kc=1
              elseif(compo.eq.'R') then
                 kc=2
              elseif(compo.eq.'T') then
                 kc=3
              else
                 kc=-1
              endif
              idum=0
              fdum=0.e0
              
              write(1) totalker(1:nr,1:nphi,1:ntheta,1:num_h3,ift)
              close(1) 
              
           enddo
         
        endif

     enddo
  endif


  if(trim(paramWRT).eq.'vTSGT') then
     
     do jt=1,number_of_snapshots
        if(((mod(jt-my_rank-ir,2*nproc).eq.0).or.(mod(jt+my_rank+1-ir,2*nproc).eq.0))) then
           totalker=0.e0
           do ir=1,nr
              do ith=1,ntheta
                 write(tmpchar,'(I7,".",I7,".",I7)') ir,ith,jt
                 do j=1,23
                    if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
                 enddo
                 
                 kerfile=trim(parentDir)//"/tmpvideoparts/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)&
                      //"."//"timemarching"
                 open(1,file=kerfile,status='unknown',form='unformatted',access='direct',recl=kind(0e0)*nphi*(num_h3)*(1+nfilter))
                 read(1,rec=1) videoker(1:nphi,1:num_h4,0:nfilter,1)
                 close(1) 
                 totalker(ir,1:nphi,ith,1:num_h4,0:nfilter)=videoker(1:nphi,1:num_h4,0:nfilter,1)
              enddo
           enddo

                 
           do ift = 0,nfilter
              write(tmpchar,'(I7)') jt
              do j=1,7
                 if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
              enddo
              kertotalfile = trim(parentDir)//"/tmpvideo/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"." &
                   //trim(freqid(ift))//"."//trim(tmpchar)//trim(".video")
              open(1,file=kertotalfile,status='unknown',form='unformatted',access='sequential')
              if(compo.eq.'Z') then
                 kc=1
              elseif(compo.eq.'R') then
                 kc=2
              elseif(compo.eq.'T') then
                 kc=3
              else
                 kc=-1
              endif
              idum=0
              fdum=0.e0
              
              write(1) totalker(1:nr,1:nphi,1:ntheta,1:num_h4,ift)
              close(1) 
              
           enddo
           
        endif
     enddo
  endif
     
  if((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'allV')) then
           
     do jt=1,number_of_snapshots
        if(((mod(jt-my_rank-ir,2*nproc).eq.0).or.(mod(jt+my_rank+1-ir,2*nproc).eq.0))) then
           totalker=0.e0
           do ir=1,nr
              do ith=1,ntheta
                 
                 write(tmpchar,'(I7,".",I7,".",I7)') ir,ith,jt
                 do j=1,23
                    if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
                 enddo
                 
                 kerfile=trim(parentDir)//"/tmpvideoparts/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"."//trim(tmpchar)&
                      //"."//"timemarching"
                 open(1,file=kerfile,status='old',form='unformatted',access='direct',recl=kind(0e0)*nphi*(1+nkvtype)*(1+nfilter))
                 read(1,rec=1) videoker(1:nphi,0:nkvtype,0:nfilter,1)
                 close(1)
                 totalker(ir,1:nphi,ith,0:nkvtype,0:nfilter)=videoker(1:nphi,0:nkvtype,0:nfilter,1)

                 ! something that we can try :
                 ! open(1,file=kerfile,status='old',form='unformatted',access='direct',recl=kind(0e0)*nphi*(1+nkvtype)*(1+nfilter))
                 ! read(1,rec=jt) totalker(ir,1:nphi,ith,0:nkvtype,0:nfilter)
                
              enddo
           enddo
           
           
           do ift = 0,nfilter
              write(tmpchar,'(I7)') jt
              do j=1,7
                 if(tmpchar(j:j).eq.' ') tmpchar(j:j) = '0'
              enddo
              kertotalfile = trim(parentDir)//"/tmpvideo/"//trim(stationName)//"."//trim(eventName)//"."//trim(phase)//"."//trim(compo)//"." &
                   //trim(freqid(ift))//"."//trim(tmpchar)//trim(".video")
              open(1,file=kertotalfile,status='unknown',form='unformatted',access='sequential')
              if(compo.eq.'Z') then
                 kc=1
              elseif(compo.eq.'R') then
                 kc=2
              elseif(compo.eq.'T') then
                 kc=3
              else
                 kc=-1
              endif
              idum=0
              fdum=0.e0
              
              write(1) totalker(1:nr,1:nphi,1:ntheta,0:nkvtype,ift)
              close(1) 
              
           enddo
        endif

     enddo
                 
  endif

  deallocate(totalker)
  
  
  if(my_rank.eq.0) then
     
     if(trim(paramWRT).eq.'vRSGT') then
        nktype_real=num_h3-1
     elseif(trim(paramWRT).eq.'vTSGT') then
        nktype_real=num_h4-1
     elseif((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.(trim(paramWRT).eq.'allV')) then
        nktype_real=nkvtype
     else
        nktype_real=nktype
     endif
     
     
     ift=nfilter

     infofile = trim(parentDir)//trim(stationName)//"."//trim(eventName)//"."//&
                trim(phase)//"."//trim(compo)//trim(".info")
     open(1,file=infofile,status='unknown',form='unformatted',access='sequential')
     write(1) nr,nphi,ntheta,nktype_real,nfilter,iWindowEnd-iWindowStart+1,number_of_snapshots
     write(1) real(t(nt1(ift))),real(t(nt2(ift)))
     write(1) (real(mt(i)),i=1,6)
     write(1) real(r0D)
     write(1) real(u)
     close(1)
     
    

     list = trim(parentDir)//"/log/calLog"//"."// &
          trim(stationName)//"."//trim(eventName)//"."//trim(compo)//"."//trim(paramWRT)//".log"   
     open(1,file =list, status = 'old',access='append', form = 'formatted')
     call date_and_time(datex,timex)
     write(1,'(/a,a4,a,a2,a,a2,a,a2,a,a2,a,a4)') &
          '    Finishing date and time:                     ', &
          datex(1:4),'-',datex(5:6),'-',datex(7:8),'.  ', &
          timex(1:2),':',timex(3:4),':',timex(5:8)   
     close (1)
     
  endif

  call MPI_FINALIZE(ierr)
  stop

end program KernelMaker
  
