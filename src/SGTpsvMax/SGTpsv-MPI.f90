program  SGTpsv


  !-----------------------------------------------------------------------
  !      SGTpsv
  !
  !  
  !   
  !
  !    calculation de la fonction de Green pour l'inversion des ondes PSV
  !       
  !                                               1994-2005 TAKEUCHI Nozomu, Phil Cummins
  !                                               2004.10.KAWAI Kenji 
  !
  !                                               2009.6. FUJI Nobuaki (f90)
  !                                               2010.10. FUJI Nobuaki (MPI, rewriting)
  !                                               2016.07. FUJI Nobuaki (outer core)
  !                                               2017.08. FUJI Nobuaki (outer core fixed)
  !                                               2020.10. FUJI Nobuaki (RSGT for depth: PSGT)
  !                                               
  !
  !      0.2.0 plm calculating with one frequency by one frequency
  !                 
  !-----------------------------------------------------------------------
  use mpi
  use parameters
  implicit none
  real(kind(0d0)) :: start_time, end_time

  
  character(200) :: tmpfile
  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,my_rank,ierr)

  if(my_rank.eq.0) then
     
     call pinputDatabaseFile(DSMconfFile,outputDir,psvmodel,modelname,tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta,thetamin,thetamax,thetadelta,imin,imax,rsgtswitch,tsgtswitch,synnswitch,psgtswitch,"argvModeUsed")
     call readDSMconfFile(DSMconfFile,re,ratc,ratl,omegai,maxlmax)
     tmpfile='tmpworkingfile_for_psvmodel'
     call tmpfileNameGenerator(tmpfile,tmpfile)
     call readpsvmodel(psvmodel,tmpfile)
     psvmodel=tmpfile
     open(20, file = psvmodel, status = 'old', action='read', position='rewind')
     read(20,*) nzone
     close(20)

     
     open(20, file = psvmodel, status = 'old', action='read', position='rewind')
     read(20,*) nzone
     do i = 1, nzone
        read (20, *) vrmin(i), vrmax(i), rrho(1,i), rrho(2,i), rrho(3,i), rrho(4,i), vpv(1,i), vpv(2,i), vpv(3,i), vpv(4,i), vph(1,i), vph(2,i), vph(3,i), vph(4,i), vsv(1,i), vsv(2,i), vsv(3,i), vsv(4,i), vsh(1,i), vsh(2,i), vsh(3,i), vsh(4,i), eta(1,i), eta(2,i), eta(3,i), eta(4,i), qmu(i), qkappa(i)
     enddo
     close(20,status='delete')     
  endif

  call bcast_allocate_preparation_1
  call preparation_2
  call allocation_preparation_3

  !******************** plm reading                 *********************


  call cpu_time(start_time)
  allocate(dvec0(1:3,0:2,1:theta_n,0:maxlmax))
  
  allocate(dvecdt0(1:3,0:2,1:theta_n,0:maxlmax))

  
  allocate(dvecdp0(1:3,0:2,1:theta_n,0:maxlmax))

  
  do l=0,maxlmax    ! l-loop start


     do itheta = 1,theta_n
        
        call caldvecphi0_withplm(l,(theta(itheta)/180.d0*pi),plm(1:3,0:3,itheta,l),dvec0(1:3,-2:2,itheta,l),dvecdt0(1:3,-2:2,itheta,l),dvecdp0(1:3,-2:2,itheta,l))
        
     enddo
  enddo

  call cpu_time(end_time)

  print *, end_time-start_time
  stop
           
  !******************** Computing the displacement *********************

 

  

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)

  
  if(my_rank.eq.0) then   
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
 
end program SGTpsv
