! NF changed dcsymbdl.f90 (dcsbdlv0_bandwidth_1 and dcsbdlv0_bandwidth_3)


subroutine dsm_l_0
  use mpi
  use parameters
  implicit none

  ! l = 0
  l=0
  call caldvecphi0_withplm(l,theta_radian(1),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withplm(l,theta_radian(theta_n),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta = pi
  ! NF: theta_n should be bigger than 3, which should be the case for MAX use
  do itheta = 2,theta_n-1      
     call caldvecphi0_withplm_without_if_clause_l_0(l,theta_radian(itheta),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
  enddo
  
  l2 = 0.d0
  lsq = 0.d0
  
  rdvec = dcmplx(0.d0)
  call caldveczero_l_0(l,rdvec(1:3,-2:2))
  !rdvec=dconjg(rdvec) 
  
  
  ! computing the coefficient matrix elements
  ! --- renewing  mdr

  call calmdr( omega,l,nzone,vrmin,vrmax,vmin,dzpar,rmax,sufzone )
  call calspdr(nzone,nzone,iphase,nlayer,jjdr,kkdr )
  do ir_=1,r_n
     ksta(ir_) = kkdr(istazone(ir_))+2*iista(1,ir_) - 1
  enddo
  cksta = kkdr(istazone(cista))+2*iista(1,cista) - 1
  nn = kkdr(nzone) + 2 * nlayer(nzone) + 1
     
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

  m=0 ! l=0; m=0
  ig2 = 0
  
  !if ( l.eq.0 ) then ! l-branch for calu (l=0) 
  !  rearranging the matrix elements 

  
  do imt = 1,3 ! for m = 0
        
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     call rea2( nn,a,g0,c,d0,nzone,iphase,kkdr,spn,kkdr0,nn0,r_n,r_n,istazone,iista,jsta )
     
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=2
     
     ns = kkdr0 + ( nint(spo) - 1 )
     call dcsymbdl0( c(1,itmp),1,nn0-itmp+1,1,eps,z(itmp),w(itmp),ll,lli,llj,ier)
     call dcsbdlv0_bandwidth_1( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
     ! look at this for m = 0
     !if((abs(m).eq.0).and.((imt.eq.1).or.(imt.eq.2).or.(imt.eq.3))) then
     !   call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
     !elseif((abs(m).eq.1).and.((imt.eq.4).or.(imt.eq.5))) then
     !   call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
     !elseif((abs(m).eq.2).and.((imt.eq.2).or.(imt.eq.3).or.(imt.eq.6))) then
     !   call dcsbdlv0( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_synthetic
              
     call convolution_with_Phi_TSGT
                 
  enddo ! imt-loop
           
  !if((m.eq.0).and.(rsgtswitch.eq.1)) then  ! back propagated for icomp = 1; m = 0
  ! back propagation for icomp = 1 and m = 0
   
          
  icomp = 1
  g0 = dcmplx(0.d0)
  g0(nn-1) = -conjg(rdvec(icomp,m))
  call rea2_back( nn,a,g0,c,d0,nzone,iphase,kkdr,spn,kkdr0,nn0,r_n,r_n,istazone,iista,jsta )
  itmp = 1
  if ( rmin.eq.0.d0 ) itmp=2
  ns = kkdr0 + ( nint(spo) - 1 )

  call dcsbdlv0_bandwidth_1( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
  
  call convolution_with_Phi_RSGT
  
  return
  
end subroutine dsm_l_0


subroutine dsm_l_1
  use mpi
  use parameters
  implicit none

  l = 1
  
  call caldvecphi0_withplm(l,theta_radian(1),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withplm(l,theta_radian(theta_n),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta = pi
  ! NF: theta_n should be bigger than 3, which should be the case for MAX use
  do itheta = 2,theta_n-1      
     call caldvecphi0_withplm_without_if_clause_l_1(l,theta_radian(itheta),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
  enddo
  
  !l2 = dble(l)*dble(l+1)
  l2 = 2.d0
  lsq = dsqrt( l2 )
  
  
  rdvec = dcmplx(0.d0)
  call caldveczero_l_non_zero(l,rdvec(1:3,-2:2))
  !rdvec=dconjg(rdvec) 
  
     
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


  ! m = -1; l = 1 

  m = -1
  ig2=0

  ! imt = 4  just because we need to perform cholensky seriously
  imt = 4
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
  
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  
  !if((abs(m).eq.0).and.((imt.eq.1).or.(imt.eq.2).or.(imt.eq.3))) then
  !   call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  !elseif ((abs(m).eq.1).and.((imt.eq.4).or.(imt.eq.5))) then
  !   call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  !elseif((abs(m).eq.2).and.((imt.eq.2).or.(imt.eq.3).or.(imt.eq.6))) then
     !   call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  !endif
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT

  ! imt = 5
  imt = 5
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsymbdl0( a(1,itmp),3,nn-itmp+1,6,eps,z(itmp),w(itmp),ll,lli,llj,ier )
  ig2 = 1  
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )

  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT

  ! back propagation for m = -1
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo
  
  m = 0
  
  do imt = 1,3
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

     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )     
     !if((abs(m).eq.0).and.((imt.eq.1).or.(imt.eq.2).or.(imt.eq.3))) then
     !  
     !if((abs(m).eq.0).and.((imt.eq.1).or.(imt.eq.2).or.(imt.eq.3))) then
     !   call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !elseif ((abs(m).eq.1).and.((imt.eq.4).or.(imt.eq.5))) then
     !   call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !elseif((abs(m).eq.2).and.((imt.eq.2).or.(imt.eq.3).or.(imt.eq.6))) then
     !   call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif     
     call convolution_with_Phi_synthetic
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
  
  icomp =1
  g0 = dcmplx(0.d0)
  g0(nn-1) = -conjg(rdvec(icomp,m))
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_RSGT
  

  ! m = 1
  m = 1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     call convolution_with_Phi_synthetic    
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo
 
  return
  
end subroutine dsm_l_1


subroutine dsm_l_2
  use mpi
  use parameters
  implicit none

  l = 2

  ! this is more or less the same for l_3, l_4 but caldvec needs to be different so the first Phi computation part is slightly different
  ! from each other (calmdr is omitted and we will see whether it is ok)
  
  call caldvecphi0_withplm(l,theta_radian(1),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withplm(l,theta_radian(theta_n),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta = pi
  ! NF: theta_n should be bigger than 3, which should be the case for MAX use
  do itheta = 2,theta_n-1      
     call caldvecphi0_withplm_without_if_clause_l_2(l,theta_radian(itheta),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
  enddo
  
  l2 = dble(l)*dble(l+1)
  lsq = dsqrt( l2 )
  
  
  rdvec = dcmplx(0.d0)
  call caldveczero_l_nonzero(l,rdvec(1:3,-2:2))
  !rdvec=dconjg(rdvec) 
  
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

  ! m = -2
  m = -2

  ig2 = 0
  
  imt = 6 ! to start cholesky
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsymbdl0( a(1,itmp),3,nn-itmp+1,6,eps,z(itmp),w(itmp),ll,lli,llj,ier )
  ig2 = 1
  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
   
     
  ! m = -1
  m = -1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 0

  m = 0
  do imt = 1,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )     
        
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
 
  icomp = 1
  g0 = dcmplx(0.d0)
  g0(nn-1) = -conjg(rdvec(icomp,m))
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_RSGT


  ! m = 1

  m = 1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 2
  
  m = 2
  
  imt = 6
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
  
  return
end subroutine dsm_l_2


subroutine dsm_l_3
  use mpi
  use parameters
  implicit none

  l = 3

  ! this is more or less the same for l_3, l_4 but caldvec needs to be different so the first Phi computation part is slightly different
  ! from each other (calmdr is omitted and we will see whether it is ok)
  
  call caldvecphi0_withplm(l,theta_radian(1),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withplm(l,theta_radian(theta_n),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta = pi
  ! NF: theta_n should be bigger than 3, which should be the case for MAX use
  do itheta = 2,theta_n-1      
     call caldvecphi0_withplm_without_if_clause_l_3(l,theta_radian(itheta),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
  enddo
  
  l2 = dble(l)*dble(l+1)
  lsq = dsqrt( l2 )
  
  
  rdvec = dcmplx(0.d0)
  call caldveczero_l_nonzero(l,rdvec(1:3,-2:2))
  !rdvec=dconjg(rdvec) 
  
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

  ! m = -2
  m = -2

  ig2 = 0
  
  imt = 6 ! to start cholesky
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsymbdl0( a(1,itmp),3,nn-itmp+1,6,eps,z(itmp),w(itmp),ll,lli,llj,ier )
  ig2 = 1
  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
   
     
  ! m = -1
  m = -1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 0

  m = 0
  do imt = 1,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )     
        
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
 
  icomp = 1
  g0 = dcmplx(0.d0)
  g0(nn-1) = -conjg(rdvec(icomp,m))
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_RSGT


  ! m = 1

  m = 1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 2
  
  m = 2
  
  imt = 6
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
  
  return
end subroutine dsm_l_3

subroutine dsm_l_4
  use mpi
  use parameters
  implicit none

  l = 4

  ! this is more or less the same for l_3, l_4 but caldvec needs to be different so the first Phi computation part is slightly different
  ! from each other (calmdr is omitted and we will see whether it is ok)
  
  call caldvecphi0_withplm(l,theta_radian(1),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withplm(l,theta_radian(theta_n),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta = pi
  ! NF: theta_n should be bigger than 3, which should be the case for MAX use
  do itheta = 2,theta_n-1      
     call caldvecphi0_withplm_without_if_clause_l_4(l,theta_radian(itheta),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
  enddo
  
  l2 = dble(l)*dble(l+1)
  lsq = dsqrt( l2 )
  
  
  rdvec = dcmplx(0.d0)
  call caldveczero_l_nonzero(l,rdvec(1:3,-2:2))
  !rdvec=dconjg(rdvec) 
  
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

  ! m = -2
  m = -2

  ig2 = 0
  
  imt = 6 ! to start cholesky
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsymbdl0( a(1,itmp),3,nn-itmp+1,6,eps,z(itmp),w(itmp),ll,lli,llj,ier )
  ig2 = 1
  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
   
     
  ! m = -1
  m = -1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 0

  m = 0
  do imt = 1,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )     
        
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
 
  icomp = 1
  g0 = dcmplx(0.d0)
  g0(nn-1) = -conjg(rdvec(icomp,m))
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_RSGT


  ! m = 1

  m = 1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 2
  
  m = 2
  
  imt = 6
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
  
  return
end subroutine dsm_l_4

subroutine dsm_l_big
  use mpi
  use parameters
  implicit none

  

  ! this is more or less the same for l_3, l_4 but caldvec needs to be different so the first Phi computation part is slightly different
  ! from each other (calmdr is omitted and we will see whether it is ok)
  
  call caldvecphi0_withplm(l,theta_radian(1),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withplm(l,theta_radian(theta_n),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta = pi
  ! NF: theta_n should be bigger than 3, which should be the case for MAX use
  do itheta = 2,theta_n-1      
     call caldvecphi0_withplm_without_if_clause_l_big(l,theta_radian(itheta),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
  enddo
  
  l2 = dble(l)*dble(l+1)
  lsq = dsqrt( l2 )
  
  
  rdvec = dcmplx(0.d0)
  call caldveczero_l_nonzero(l,rdvec(1:3,-2:2))
  !rdvec=dconjg(rdvec) 
  
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

  ! m = -2
  m = -2

  ig2 = 0
  
  imt = 6 ! to start cholesky
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsymbdl0( a(1,itmp),3,nn-itmp+1,6,eps,z(itmp),w(itmp),ll,lli,llj,ier )
  ig2 = 1
  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
   
     
  ! m = -1
  m = -1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 0

  m = 0
  do imt = 1,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )     
        
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
 
  icomp = 1
  g0 = dcmplx(0.d0)
  g0(nn-1) = -conjg(rdvec(icomp,m))
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
  
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_RSGT


  ! m = 1

  m = 1

  do imt = 4,5
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
   
      
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
       
     call convolution_with_Phi_synthetic
     
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
     
  
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
     !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     !endif
     call convolution_with_Phi_RSGT
  enddo

  ! m = 2
  
  m = 2
  
  imt = 6
  call setmt(imt,mt)
  g0 = dcmplx(0.d0)
  call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
  !print *, l,m,imt,g0(jtmp:jtmp+3)
  ! computing forward propagating component (l!=0)                              
  itmp=1
  if ( rmin.eq.0.d0 ) itmp=3
  ns = kkdr(spn) + 2 * ( nint(spo) - 1 )  
   
  call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
  call convolution_with_Phi_synthetic
  call convolution_with_Phi_TSGT
 

  do imt = 2,3
     call setmt(imt,mt)
     g0 = dcmplx(0.d0)
     call calg( l,m,coef1(spn),coef2(spn),lsq,ecC0,ecF0,ecL0,ya,yb,yc,yd,ra(mtmp),r0(ir0),mt,g0(jtmp) ) 
     !print *, l,m,imt,g0(jtmp:jtmp+3)
     ! computing forward propagating component (l!=0)                              
     itmp=1
     if ( rmin.eq.0.d0 ) itmp=3
     ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
     
     call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
     
     call convolution_with_Phi_synthetic  
     call convolution_with_Phi_TSGT
  enddo ! mt-loop
  
  
  return
end subroutine dsm_l_big

subroutine convolution_with_Phi_synthetic
  use mpi
  use parameters
  implicit none

  ! this subroutines will take the c^T Phi for synthetics
  do itheta = 1, theta_n
     u = dcmplx(0.d0)
     call calup0(d0(nn0),dvec0(1:3,m,itheta),u(1:3))
     call utosynn(imt,u(1:3),synn(1:num_synn,itheta))
  enddo

  return
end subroutine convolution_with_Phi_synthetic

subroutine convolution_with_Phi_TSGT
  use mpi
  use parameters
  implicit none

  ! this subroutines will take the c^T Phi for TSGT
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
            call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta))
            call udertotsgt(imt,uder(1:3,1:3),tsgt(1:num_tsgt,ir_,itheta,ir0))
                       
            
            ! if(my_rank.eq.0) print *, "solid strain", uder(1,1)
         enddo
         
         
      endif
   enddo
           
  

  return
end subroutine convolution_with_Phi_TSGT

subroutine convolution_with_Phi_RSGT
  use mpi
  use parameters
  implicit none

  ! this subroutines will take the c^T Phi for RSGT



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
            call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta))
            call udertorsgt(icomp,uder(1:3,1:3),rsgt(1:num_rsgt,ir_,itheta))
         enddo
      endif
   enddo
   return
end subroutine convolution_with_Phi_RSGT



!!!! DON'T TOUCH BELOW!!!
subroutine dsm_l_all
  use mpi
  use parameters
  implicit none

  call caldvecphi0_withplm(l,theta_radian(1),plm(1:3,0:3,1),dvec0(1:3,-2:2,1),dvecdt0(1:3,-2:2,1),dvecdp0(1:3,-2:2,1)) ! itheta = 1 can have theta = 0 
  call caldvecphi0_withplm(l,theta_radian(theta_n),plm(1:3,0:3,theta_n),dvec0(1:3,-2:2,theta_n),dvecdt0(1:3,-2:2,theta_n),dvecdp0(1:3,-2:2,theta_n)) ! itheta = theta_n can have theta = pi
  ! NF: theta_n should be bigger than 3, which should be the case for MAX use
  do itheta = 2,theta_n-1      
     call caldvecphi0_withplm_without_if_clause(l,theta_radian(itheta),plm(1:3,0:3,itheta),dvec0(1:3,-2:2,itheta),dvecdt0(1:3,-2:2,itheta),dvecdp0(1:3,-2:2,itheta))
  enddo
  
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
                 call dcsbdlv0_bandwidth_1( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
              elseif((abs(m).eq.1).and.((imt.eq.4).or.(imt.eq.5))) then
                 call dcsbdlv0_bandwidth_1( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
              elseif((abs(m).eq.2).and.((imt.eq.2).or.(imt.eq.3).or.(imt.eq.6))) then
                 call dcsbdlv0_bandwidth_1( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
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
                       call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta))
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
                 call dcsbdlv0_bandwidth_1( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
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
                          call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta))
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
                 
                 !call mydcsbdlv1(c(1,itmp),d0(itmp),nn0-itmp+1,z(itmp))
                 call dcsbdlv0_bandwidth_1( c(1,itmp),d0(itmp),1,nn0-itmp+1,eps,z(itmp),ier )
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
                          call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta))
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
                 call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
              elseif ((abs(m).eq.1).and.((imt.eq.4).or.(imt.eq.5))) then
                 call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
              elseif((abs(m).eq.2).and.((imt.eq.2).or.(imt.eq.3).or.(imt.eq.6))) then
                 call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
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
                       call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta)) 
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
                 
                 call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
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
                          call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta))
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
                 !call mydcsbdlv3(a(1,itmp),g0(itmp),nn-itmp+1,z(itmp))
                 call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
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
                          call locallyCartesianDerivatives(u(1:3),udr(1:3),udt(1:3),udp(1:3),uder(1:3,1:3),r_(ir_),theta_radian(itheta))
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
  
end subroutine dsm_l_all
