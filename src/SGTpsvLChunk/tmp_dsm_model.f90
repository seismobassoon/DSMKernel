subroutine dsm_l_all
  use mpi
  use paramters
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
              
              call convolution_with_Phi_synthetic
              
              call convolution_with_Phi_TSGT
              
            
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

                 call convolution_with_Phi_RSGT
                 
                
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
                 call convolution_with_Phi_RSGT
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
              
              
              call convolution_with_Phi_synthetic
            
              call convolution_with_Phi_TSGT
           enddo ! mt-loop
           
           
           
           if((m.eq.0).and.(rsgtswitch.eq.1)) then ! back propagated for icomp = 1
              do icomp = 1,1
                 g0 = dcmplx(0.d0)
                 g0(nn-1) = -conjg(rdvec(icomp,m))
                 itmp=1
                 if ( rmin.eq.0.d0 ) itmp=3
                 ns = kkdr(spn) + 2 * ( nint(spo) - 1 )
                 
                 call dcsbdlv0_bandwidth_3( a(1,itmp),g0(itmp),3,nn-itmp+1,eps,z(itmp),ier )
                 call convolution_with_Phi_RSGT
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
                 call convolution_with_Phi_RSGT
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
