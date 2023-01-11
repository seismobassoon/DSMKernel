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

  call MPI_BARRIER(MPI_COMM_WORLD,ierr)


  do i=imin,imax            ! omega-loop start

     ir0 = 1 ! we compute only for one source depth!

     tsgt = dcmplx(0.d0)
     rsgt = dcmplx(0.d0)
     synn = dcmplx(0.d0)
     
     tsgtsngl=dcmplx(0.e0)
     rsgtsngl=dcmplx(0.e0)
     synnsngl=dcmplx(0.e0)
     
     omega = 2.d0 * pi * dble(i) / tlen

     !if((i.ne.0).and.((mod(imax-my_rank-i,2*nproc).eq.0).or.(mod(imax+my_rank+1-i,2*nproc).eq.0))) then
     
     if((i.ne.0).and.(mod(my_rank,nproc).eq.0)) then ! forget about the l-max problem, since we work for max l every omega
        
        call calcoef( nzone,omega,qmu,qkappa,coef1,coef2,coef )
        plm = 0.d0
        mtmp = isp(spn) + int(spo)
        if ( spo.eq.int(spo) ) mtmp = mtmp - 1
        call calabnum( omega,omegai,rmax, rrho(1,spn),vpv(1,spn),vph(1,spn),vsv(1,spn),vsh(1,spn),eta(1,spn),ra(mtmp),r0(ir0),coef1(spn),coef2(spn),anum(1,1,1),bnum(1,1,1) )
        ! computing the matrix elements independent of l
        isl = 0
        ill = 0
        do j=1,ndc+1
           if ( iphase(j).eq.1 ) then
              isl = isl + 1
              itmp = isdr+issp(isl)
              jtmp = jdr+jsp(j)
              mtmp = kdr+ksp(j)
              call cala0( nlayer(j),omega,omegai,t(itmp),h1x(itmp), h1y(itmp),h1z(itmp), h2L(itmp), h2N(itmp), h3ax(itmp),h3ay(itmp),h3az(itmp), h4aL(itmp),h4aN(itmp), h5ax(itmp),h5ay(itmp),h5az(itmp), h6aL(itmp),h6aN(itmp), h7x(itmp),h7y(itmp),h7z(itmp), h8L(itmp), h8N(itmp), coef1(j),coef2(j),cwork(jtmp) )
              call overlapa( nlayer(j),cwork(jtmp),a0(1,mtmp))
              call cala1( nlayer(j), h1x(itmp),h1y(itmp),h1z(itmp), h2L(itmp),h2N(itmp), h3x(itmp), h3y(itmp), h3z(itmp),  h4L(itmp), h4N(itmp), h5x(itmp), h5y(itmp), h5z(itmp), h6L(itmp), h6N(itmp),coef1(j),coef2(j),cwork(jtmp) )
              call overlapa( nlayer(j),cwork(jtmp),a1(1,mtmp))
              call cala2( nlayer(j),h1x(itmp), h1y(itmp),h1z(itmp),h2L(itmp),h2N(itmp),coef1(j),coef2(j),cwork(jtmp) )
              call overlapa( nlayer(j), cwork(jtmp),a2(1,mtmp))
              jtmp = jsdr+jssp(isl)
              call calhml( nlayer(j),coef1(j),coef2(j),h3mx(-2,jtmp),h3my(-2,jtmp),h3mz(-2,jtmp), h5mx(-1,jtmp),h5my(-1,jtmp),h5mz(-1,jtmp),h4m1L(-1,jtmp),h4m1N(-1,jtmp), h4m2L(-2,jtmp),h4m2N(-2,jtmp), h6m1L(-1,jtmp),h6m1N(-1,jtmp), h6m2L(-2,jtmp),h6m2N(-2,jtmp), a1(1,mtmp) )
           else
              ill = ill + 1
              itmp = ildr+ilsp(ill)
              jtmp = jdr+jsp(j)
              mtmp = kdr+ksp(j)
              call calb0( nlayer(j),omega,omegai, p1(itmp),p3(itmp),coef(j),cwork(jtmp) )
              call overlapb( nlayer(j), cwork(jtmp),a0(1,mtmp))
              call calb2( nlayer(j),omega,omegai, p2(itmp),coef(j),cwork(jtmp) )
              call overlapb( nlayer(j), cwork(jtmp),a2(1,mtmp))
           endif
        enddo

        kc = 1
        ismall = 0
        maxamp = -1.d0
        llog = maxlmax


        !l=0
        call dsm_l_0 ! done 11/01/2023

        !l=1
        call dsm_l_1
        
        !l=2

        call dsm_l_2
        
        !l=3

        call dsm_l_3
        
        !l=4

        call dsm_l_4
        
        !l>4
        
        do l=5,maxlmax    ! l-loop start

           call dsm_l_big
        
        enddo               ! l-loop end        
        !print *, list1
        
        open(24,file =list1, status = 'old',access='append', form = 'formatted')
        write(24,*) i, dble(i)/tlen, llog-1     
        close(24)

    
     
        do ir_ = 1,r_n
           ! do ir0 = 1,r0_n
           ir0 = 1
           write(coutfile, '(I7,".",I7,".",I7,".TSGT_PSV")') intir0,int(r_(ir_)*1.d3),i
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
           write(coutfile, '(I7,".",I7,".RSGT_PSV")') int(r_(ir_)*1.d3),i
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
        
        
        write(coutfile, '(I7,".",I7,".SYNN_PSV") ') intir0,i
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
   
        !enddo        
     endif    
  enddo

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
