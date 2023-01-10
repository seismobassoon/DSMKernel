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
        do l=0,maxlmax    ! l-loop start
           call caldvecphi0_withplm(l,(theta(1)/180.d0*pi),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
           call caldvecphi0_withplm(l,(theta(theta_n)/180.d0*pi),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2itheta_n)) ! itheta = theta_n can have theta = pi
           ! NF: theta_n should be bigger than 3, which should be the case for MAX use
           do itheta = 2,theta_n-1      
              call caldvecphi0_withplm_without_if_clause(l,(theta(itheta)/180.d0*pi),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
           enddo


           if( ismall.gt.20 ) then
              if(llog.gt.l) llog = l
              exit
           endif
           l2 = dble(l)*dble(l+1)
           lsq = dsqrt( l2 )

        
           rdvec = dcmplx(0.d0)
           call caldveczero(l,rdvec(1:3,-2:2))
           !rdvec=dconjg(rdvec) 


           ! computing the coefficient matrix elements
           ! --- renewing  mdr
           if ( mod(l,50).eq.0 )  then
              call calmdr( omega,l,nzone,vrmin,vrmax,vmin,dzpar,rmax,sufzone )
              call calspdr(nzone,nzone,iphase,nlayer,jjdr,kkdr )
              do ir_=1,r_n
                 ksta(ir_) = kkdr(istazone(ir_))+2*iista(1,ir_) - 1
              enddo
              cksta = kkdr(istazone(cista))+2*iista(1,cista) - 1
              nn = kkdr(nzone) + 2 * nlayer(nzone) + 1
           endif
     
           !     computing the matrix elements
           call cala( nzone,ndc,iphase,nlayer,kkdr,kdr,ksp,l2,lsq,nn,a0,a1,a2,a )
           ! computing the boundary condition elements
           call calbc( nzone,ndc,vrmax,iphase,kkdr,a )
           
           jtmp = kkdr(spn) + 2 * int(spo)
           mtmp = isp(spn) + int(spo)
           if ( spo.eq.int(spo) ) then
              jtmp = jtmp - 2
              mtmp = mtmp - 1
           endif
  
           call calya( anum(1,1,1),bnum(1,1,1),l2,ra(mtmp),r0(ir0),ya,yb,yc,yd )

           do m=-2,2        ! m-loop start

              if ( iabs(m).le.iabs(l) ) then
                 ig2 = 0
                 if ( l.eq.0 ) then ! l-branch for calu (l=0) 
                    !  rearranging the matrix elements 
                    do imt = 1,6
                       
                       call setmt(imt,mt)
                       g0 = dcmplx(0.d0)
                       call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
                       call rea2( nn,a,g0,c,d0,nzone,iphase,kkdr,spn,kkdr0,nn0,r_n,r_n,istazone,iista,jsta )
                  
                       itmp=1
                       if ( rmin.eq.0.d0 ) itmp=2
                 
                       ns = kkdr0 + ( nint(spo) - 1 )
                       call dcsymbdl0( c(1,itmp),1,nn0-itmp+1,1,eps,z(itmp),w(itmp),ll,lli,llj,ier)
                       if((abs(m).eq.0).and.((imt.eq.1).or.(imt.eq.2).or.(imt.eq.3))) then
                          call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
                       elseif((abs(m).eq.1).and.((imt.eq.4).or.(imt.eq.5))) then
                          call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
                       elseif((abs(m).eq.2).and.((imt.eq.2).or.(imt.eq.3).or.(imt.eq.6))) then
                          call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
                       endif

                       
                       
                       if(synnswitch.eq.1) then

                          
                          do itheta = 1, theta_n
                             u = dcmplx(0.d0)
                             call calup0(d0(nn0),dvec0(1:3,m,itheta),u(1:3))
                             call utosynn(imt,u(1:3),synn(1:num_synn,itheta))
                          enddo
                       endif

                       
                       do ir_=1,r_n
                          g0tmp = dcmplx(0.d0)
                          g0dertmp = dcmplx(0.d0)
                          
                          call interpolate( 1,0,r_(ir_), rrsta(1,ir_),d0(jsta(ir_)),g0tmp(1))
                          call interpolate( 1,1,r_(ir_), rrsta(1,ir_),d0(jsta(ir_)),g0dertmp(1))
                          
                          ! NF introduces liquid terms (this should be done more efficiently)                      
                          
                          if(iphase(istazone(ir_)).eq.2) then
                          !if(0.eq.1) then
                             ! NF will write like \omega*g0tmp(1)/\lambda
                             
                             do itheta = 1, theta_n
                                u = dcmplx(0.d0)
                                udr = dcmplx(0.d0)
                                udt = dcmplx(0.d0)
                                udp = dcmplx(0.d0)
                                uder = dcmplx(0.d0)
                            
                                call calupfluid(g0tmp(1),dcmplx(omega,-omegai),lambda(ir_),qkp(ir_),dvec0(1,m,itheta),uder)

                                ! Here in the liquid, u(1) is Q = lambda u_{k,k}/omega
                                ! that said, u_{r,r}=u_{t,t}=u_{p,p}=omega/(3 lambda)*Q
                                ! also, u_i=-1/(\rho \omega) * \partial_i Q
                                !if(my_rank.eq.0) print *, "fluid strain", uder(1,1)
                                
                                
                                
                                !call call calup0(g0tmp(1),dvec0(1:3,m,itheta),u(1:3))
                                !call calup0(g0dertmp(1),dvec0(1:3,m,itheta),udr(1:3))            
                                !call calup0(g0tmp(1),dvecdt0(1:3,m,itheta),udt(1:3))
                                !call calup0(g0tmp(1),dvecdp0(1:3,m,itheta),udp(1:3))
                                !call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)


                                call udertotsgt(imt,uder(1:3,1:3),tsgt(1:num_tsgt,ir_,itheta,ir0))
                             enddo




                          else
                             
                             do itheta = 1, theta_n
                                u = dcmplx(0.d0)
                                udr = dcmplx(0.d0)
                                udt = dcmplx(0.d0)
                                udp = dcmplx(0.d0)
                                uder = dcmplx(0.d0)
                                call calup0(g0tmp(1),dvec0(1:3,m,itheta),u(1:3))
                                call calup0(g0dertmp(1),dvec0(1:3,m,itheta),udr(1:3))            
                                call calup0(g0tmp(1),dvecdt0(1:3,m,itheta),udt(1:3))
                                call calup0(g0tmp(1),dvecdp0(1:3,m,itheta),udp(1:3))
                                call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                call udertotsgt(imt,uder(1:3,1:3),tsgt(1:num_tsgt,ir_,itheta,ir0))
                                

                                ! if(my_rank.eq.0) print *, "solid strain", uder(1,1)
                             enddo
                             
                             
                          endif
                       enddo
                    enddo ! imt-loop
            
                    if((m.eq.0).and.(rsgtswitch.eq.1)) then  ! back propagated for icomp = 1
                       do icomp = 1,1
                          g0 = dcmplx(0.d0)
                          g0(nn-1) = -conjg(rdvec(icomp,m))
                          call rea2_back( nn,a,g0,c,d0,nzone,iphase,kkdr,spn,kkdr0,nn0,r_n,r_n,istazone,iista,jsta )
                          itmp = 1
                          if ( rmin.eq.0.d0 ) itmp=2
                          ns = kkdr0 + ( nint(spo) - 1 )
                          !NF touches 
                          !call mydcsbdlv1(c(1,itmp),d0(itmp),nn0-itmp+1,z(itmp))
                          call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
                          do ir_=1,r_n
                             g0tmp = dcmplx(0.d0)
                             g0dertmp = dcmplx(0.d0)
                             call interpolate( 1,0,r_(ir_), rrsta(1,ir_),d0(jsta(ir_)),g0tmp(1))
                             call interpolate( 1,1,r_(ir_), rrsta(1,ir_),d0(jsta(ir_)),g0dertmp(1))

                             if(iphase(istazone(ir_)).eq.2) then
                             !if(0.eq.1) then
                                ! NF for fluid
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0) 
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   
                                   
                                   call calupfluid(g0tmp(1),dcmplx(omega,-omegai),lambda(ir_),qkp(ir_),dvec0(1,m,itheta),uder)                                   
                                   !call calup0(g0tmp(1),dvec0(1:3,m,itheta),u(1:3))
                                   !call calup0(g0dertmp(1),dvec0(1:3,m,itheta),udr(1:3))            
                                   !call calup0(g0tmp(1),dvecdt0(1:3,m,itheta),udt(1:3))
                                   !call calup0(g0tmp(1),dvecdp0(1:3,m,itheta),udp(1:3))
                                   !call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   
                                   
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))

 
                                   
                                enddo
                                
                             else
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0) 
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   call calup0(g0tmp(1),dvec0(1:3,m,itheta),u(1:3))
                                   call calup0(g0dertmp(1),dvec0(1:3,m,itheta),udr(1:3))            
                                   call calup0(g0tmp(1),dvecdt0(1:3,m,itheta),udt(1:3))
                                   call calup0(g0tmp(1),dvecdp0(1:3,m,itheta),udp(1:3))
                                   call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
                                enddo
                             endif
                          enddo
                       enddo
                    endif
                    
                    if((abs(m).eq.1).and.(rsgtswitch.eq.1)) then  ! back propagated for icomp = 2,3
                       do icomp = 2,3
                          g0 = dcmplx(0.d0)
                          ! NF touches g0(nn-1) -> g0(nn) 
                          g0(nn) = conjg(rdvec(icomp,m))/dcmplx(lsq)
                          ! NF touches
                          !call calg_force(l,m,lsq,icomp,rmax,g0(nn-1))
                          call rea2_back( nn,a,g0,c,d0,nzone,iphase,kkdr,spn,kkdr0,nn0,r_n,r_n,istazone,iista,jsta )
                          itmp = 1
                          if ( rmin.eq.0.d0 ) itmp=2
                          ns = kkdr0 + ( nint(spo) - 1 )
                          ! NF touches
                          call mydcsbdlv1(c(1,itmp),d0(itmp),nn0-itmp+1,z(itmp))
                          !call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
                          do ir_=1,r_n
                             g0tmp = dcmplx(0.d0)
                             g0dertmp = dcmplx(0.d0)
                             call interpolate( 1,0,r_(ir_), rrsta(1,ir_),d0(jsta(ir_)),g0tmp(1))
                             call interpolate( 1,1,r_(ir_), rrsta(1,ir_),d0(jsta(ir_)),g0dertmp(1))

                             if(iphase(istazone(ir_)).eq.2) then
                             !if(0.eq.1) then
                                ! NF for fluid
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0) 
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   call calupfluid(g0tmp(1),dcmplx(omega,-omegai),lambda(ir_),qkp(ir_),dvec0(1,m,itheta),uder)   
                                   !call calup0(g0tmp(1),dvec0(1:3,m,itheta),u(1:3))
                                   !call calup0(g0dertmp(1),dvec0(1:3,m,itheta),udr(1:3))            
                                   !call calup0(g0tmp(1),dvecdt0(1:3,m,itheta),udt(1:3))
                                   !call calup0(g0tmp(1),dvecdp0(1:3,m,itheta),udp(1:3))
                                   !call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
                                enddo
                                
                                
                             else
                                
                                
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0) 
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   call calup0(g0tmp(1),dvec0(1:3,m,itheta),u(1:3))
                                   call calup0(g0dertmp(1),dvec0(1:3,m,itheta),udr(1:3))            
                                   call calup0(g0tmp(1),dvecdt0(1:3,m,itheta),udt(1:3))
                                   call calup0(g0tmp(1),dvecdp0(1:3,m,itheta),udp(1:3))
                                   call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
                                enddo
                             endif
                             
                          enddo
                       enddo
                    endif



             
                 else ! for l!=0
                    do imt = 1,6
                       call setmt(imt,mt)
                       g0 = dcmplx(0.d0)
                       call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
                       !print *, l,m,imt,g0(jtmp:jtmp+3)
                       ! computing forward propagating component (l!=0)                              
                       itmp=1
                       if ( rmin.eq.0.d0 ) itmp=3
                       ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
                       if ( ( m.eq.-2 ).or.( m.eq.-l ) ) then
                          if(ig2.eq.0) then
                             call dcsymbdl0( a(1,itmp),3,nn-itmp+1,6,eps,z(itmp),w(itmp),ll,lli,llj,ier )
                             ig2 = 1
                          endif
                       endif
                       if((abs(m).eq.0).and.((imt.eq.1).or.(imt.eq.2).or.(imt.eq.3))) then
                          call dcsbdlv0( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
                       elseif ((abs(m).eq.1).and.((imt.eq.4).or.(imt.eq.5))) then
                          call dcsbdlv0( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
                       elseif((abs(m).eq.2).and.((imt.eq.2).or.(imt.eq.3).or.(imt.eq.6))) then
                          call dcsbdlv0( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
                       endif
                 
                       ! computing ampratio
                       ! print *, cksta,maxamp
                       
                       if((imt.eq.1).and.(ir0.eq.r0_n)) then
                          call calamp(g0(ksta(r_n)-1),l,lsuf,maxamp,ismall,ratl)
                          !print *, l,maxamp
                       endif
                       !print *, l,m,g0(nn-1), g0(nn)

                       
                       if(synnswitch.eq.1) then
                          do itheta = 1, theta_n
                             u = dcmplx(0.d0)
                             call calu(g0(nn-1),lsq,dvec0(1:3,m,itheta),u(1:3))
                             call utosynn(imt,u(1:3),synn(1:num_synn,itheta))
                          enddo
                       endif

                       do ir_=1,r_n ! stack point
                          g0tmp = dcmplx(0.d0)
                          g0dertmp = dcmplx(0.d0)
                          call interpolate( 2,0,r_(ir_),rrsta(1,ir_),g0(ksta(ir_)-1),g0tmp(1:2))
                          call interpolate( 2,1,r_(ir_),rrsta(1,ir_),g0(ksta(ir_)-1),g0dertmp(1:2) )
                          
                          
                          if(iphase(istazone(ir_)).eq.2) then
                          !if(0.eq.1) then
                             ! NF for fluid
                             
                             do itheta = 1, theta_n
                                u = dcmplx(0.d0)
                                udr = dcmplx(0.d0)
                                udt = dcmplx(0.d0)
                                udp = dcmplx(0.d0)
                                uder = dcmplx(0.d0)
                                call calupfluid(g0tmp(1),dcmplx(omega,-omegai),lambda(ir_),qkp(ir_),dvec0(1,m,itheta),uder)
                                !call calup(g0tmp(1),g0tmp(2),lsq,dvec0(1:3,m,itheta),u(1:3))
                                !call calup(g0dertmp(1),g0dertmp(2),lsq,dvec0(1:3,m,itheta),udr(1:3))
                                !call calup(g0tmp(1),g0tmp(2),lsq,dvecdt0(1:3,m,itheta),udt(1:3))
                                !call calup(g0tmp(1),g0tmp(2),lsq,dvecdp0(1:3,m,itheta),udp(1:3))
                                !call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)  
                                call udertotsgt(imt,uder(1:3,1:3),tsgt(1:num_tsgt,ir_,itheta,ir0))                                
                             enddo
                             
                          else
                             
                             do itheta = 1, theta_n
                                u = dcmplx(0.d0)
                                udr = dcmplx(0.d0)
                                udt = dcmplx(0.d0)
                                udp = dcmplx(0.d0)
                                uder = dcmplx(0.d0)
                                call calup(g0tmp(1),g0tmp(2),lsq,dvec0(1:3,m,itheta),u(1:3))
                                call calup(g0dertmp(1),g0dertmp(2),lsq,dvec0(1:3,m,itheta),udr(1:3))
                                call calup(g0tmp(1),g0tmp(2),lsq,dvecdt0(1:3,m,itheta),udt(1:3))
                                call calup(g0tmp(1),g0tmp(2),lsq,dvecdp0(1:3,m,itheta),udp(1:3))
                                call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi) 
                                call udertotsgt(imt,uder(1:3,1:3),tsgt(1:num_tsgt,ir_,itheta,ir0))
                                                                
                             enddo                             
                          endif



                       enddo   ! stack point
                    enddo ! mt-loop

                    
                    
                    if((m.eq.0).and.(rsgtswitch.eq.1)) then ! back propagated for icomp = 1
                       do icomp = 1,1
                          g0 = dcmplx(0.d0)
                          g0(nn-1) = -conjg(rdvec(icomp,m))
                          itmp=1
                          if ( rmin.eq.0.d0 ) itmp=3
                          ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
                          
                          call dcsbdlv0( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
                          do ir_=1,r_n
                             g0tmp = dcmplx(0.d0)
                             g0dertmp = dcmplx(0.d0)
                             call interpolate( 2,0,r_(ir_),rrsta(1,ir_),g0(ksta(ir_)-1),g0tmp(1:2))
                             call interpolate( 2,1,r_(ir_),rrsta(1,ir_),g0(ksta(ir_)-1),g0dertmp(1:2) )


                             if(iphase(istazone(ir_)).eq.2) then
                             !if(0.eq.1) then
                                ! NF for fluid
                                
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0)
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   call calupfluid(g0tmp(1),dcmplx(omega,-omegai),lambda(ir_),qkp(ir_),dvec0(1,m,itheta),uder)
                                   !call calup(g0tmp(1),g0tmp(2),lsq,dvec0(1:3,m,itheta),u(1:3))
                                   !call calup(g0dertmp(1),g0dertmp(2),lsq,dvec0(1:3,m,itheta),udr(1:3))
                                   !call calup(g0tmp(1),g0tmp(2),lsq,dvecdt0(1:3,m,itheta),udt(1:3))
                                   !call calup(g0tmp(1),g0tmp(2),lsq,dvecdp0(1:3,m,itheta),udp(1:3))
                                   !call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
                                enddo
                                
                                
                             else
                                
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0)
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   call calup(g0tmp(1),g0tmp(2),lsq,dvec0(1:3,m,itheta),u(1:3))
                                   call calup(g0dertmp(1),g0dertmp(2),lsq,dvec0(1:3,m,itheta),udr(1:3))
                                   call calup(g0tmp(1),g0tmp(2),lsq,dvecdt0(1:3,m,itheta),udt(1:3))
                                   call calup(g0tmp(1),g0tmp(2),lsq,dvecdp0(1:3,m,itheta),udp(1:3))
                                   call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
                                enddo
                             endif
                          enddo
                       enddo
                    endif

                    if((abs(m).eq.1).and.(rsgtswitch.eq.1)) then ! back propagated for icomp = 2,3
                       do icomp = 2,3
                          g0 = dcmplx(0.d0)
                          ! NF touches g0(nn-1) ->g0(nn)
                          g0(nn) = conjg(rdvec(icomp,m))/dcmplx(lsq)
                          itmp=1
                          if ( rmin.eq.0.d0 ) itmp=3
                          ns = kkdr(spn) + 2 * ( nint(spo) - 1 )

                          !if((ig2.eq.0).and.(m==-1)) then
                          !      call dcsymbdl0(a(1,itmp),3,nn-itmp+1,6,eps,z(itmp),w(itmp),ll,lli,llj,ier )
                          !      ig2 = 1
                          !!else
                          !NF touches
                          call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
                            !call dcsbdlv0( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
                          !endif
                          do ir_=1,r_n
                             g0tmp = dcmplx(0.d0)
                             g0dertmp = dcmplx(0.d0)
                             call interpolate( 2,0,r_(ir_),rrsta(1,ir_),g0(ksta(ir_)-1),g0tmp(1:2))
                             call interpolate( 2,1,r_(ir_),rrsta(1,ir_),g0(ksta(ir_)-1),g0dertmp(1:2) )

                             
                             if(iphase(istazone(ir_)).eq.2) then
                             !if(0.eq.1) then
                                ! NF for fluid
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0)
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   call calupfluid(g0tmp(1),dcmplx(omega,-omegai),lambda(ir_),qkp(ir_),dvec0(1,m,itheta),uder)
                                   !call calup(g0tmp(1),g0tmp(2),lsq,dvec0(1:3,m,itheta),u(1:3))
                                   !call calup(g0dertmp(1),g0dertmp(2),lsq,dvec0(1:3,m,itheta),udr(1:3))
                                   !call calup(g0tmp(1),g0tmp(2),lsq,dvecdt0(1:3,m,itheta),udt(1:3))
                                   !call calup(g0tmp(1),g0tmp(2),lsq,dvecdp0(1:3,m,itheta),udp(1:3))
                                   !call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
                                enddo
                             else
                                
                                do itheta = 1, theta_n
                                   u = dcmplx(0.d0)
                                   udr = dcmplx(0.d0)
                                   udt = dcmplx(0.d0)
                                   udp = dcmplx(0.d0)
                                   uder = dcmplx(0.d0)
                                   call calup(g0tmp(1),g0tmp(2),lsq,dvec0(1:3,m,itheta),u(1:3))
                                   call calup(g0dertmp(1),g0dertmp(2),lsq,dvec0(1:3,m,itheta),udr(1:3))
                                   call calup(g0tmp(1),g0tmp(2),lsq,dvecdt0(1:3,m,itheta),udt(1:3))
                                   call calup(g0tmp(1),g0tmp(2),lsq,dvecdp0(1:3,m,itheta),udp(1:3))
                                   call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta(itheta)/180.d0*pi)
                                   call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
                                enddo
                             endif

                          enddo
                       enddo
                    endif
                 endif   ! l-branch for calu

                 !print *, "sum up RSGT", i
                 !print *, rsgt(:,1,1)
                 !print *, "sum up TSGT"
                 !print *, tsgt(:,1,1,1)

                 
              endif
           enddo            ! m-loop end
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
           !enddo
           
           if(rsgtswitch.eq.1) then
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
           endif
        enddo
        
        !do ir0 = 1, r0_n
        ir0 =1
        
        if(synnswitch.eq.1) then
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
        endif
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
