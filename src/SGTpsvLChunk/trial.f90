subroutine calplm_l_small( l,m,x,plm )
  implicit none
  integer :: l,m,i
  real(kind(0d0)) :: x,plm(1:3),pmm,somx2,fact

  !if ((m.lt.0).or.(m.gt.l).or.(dabs(x).gt.1.d0)) pause 'bad arguments'
  if ( l.eq.m ) then
     pmm = 1.d0
     if ( m.gt.0 ) then
        somx2 = dsqrt( (1.d0-x)*(1.d0+x) )
        fact = 1.d0
        do i=1,m
           pmm = -pmm * fact * somx2
           fact = fact + 2.d0
        enddo
     endif
     plm(3) = 0.d0
     plm(2) = 0.d0
     plm(1) = pmm
  else
     plm(3) = plm(2)
     plm(2) = plm(1)
     if ( l.eq.m+1 ) then
        plm(1) = x * dble(2*m+1) * plm(2)
     else
        plm(1) = (x*dble(2*l-1) * plm(2)-dble(l+m-1) * plm(3) )/dble(l-m)
     endif
  endif

end subroutine calplm_l_small


subroutine calplm_l_big( l,m,x,plm )
  implicit none
  integer :: l,m,i
  real(kind(0d0)) :: x,plm(1:3),pmm,somx2,fact

  ! this subroutine will no more check anything where l >= 5 so l > m always
  ! this is for m = 0
  
  plm(3) = plm(2)
  plm(2) = plm(1)
  plm(1) = (x*dble(2*l-1) * plm(2)-dble(l+m-1) * plm(3) )/dble(l-m)
  return
end subroutine calplm_l_big

subroutine caldvecphi0_withoutplm( l,theta,plmlocal,bvec,bvecdt,bvecdp)
  
  implicit none
  real(kind(0d0)), parameter ::  pi=3.1415926535897932d0
  real(kind(0d0)), parameter :: quarterpiinverse = 0.07957747154d0
  integer  :: l,i,j
  real(kind(0d0)) :: theta,x,plmlocal(0:3),fact,coef
  complex(kind(0d0)) :: bvec(1:3,-2:2)
  complex(kind(0d0)) :: bvecdt(1:3,-2:2),bvecdp(1:3,-2:2)
  real(kind(0d0)) :: plmdt,xl2,yinverse
  real(kind(0d0)) :: rtxl2,coeff,rtxl22,sign1,sign2

  
  if((theta.eq.0.d0).or.(theta.eq.pi)) then
     sign1 = 1.d0
     sign2 = 1.d0
     if(theta.eq.pi) then
        sign1 = dble((-1)**l)
        sign2 = dble((-1)**(l+1)) 
     endif
     
     
     bvec = dcmplx(0.d0)
     bvecdt = dcmplx(0.d0)
     bvecdp = dcmplx(0.d0)
     
     xl2 = dble(l) * dble(l+1)
     rtxl2 = sqrt(xl2)
     
     coeff = dsqrt(dble(2*l+1)/(4.d0*pi))
     bvec(1,0) = dcmplx(coeff,0.d0) * sign1     
     
     return
  endif
  

  x = dcos(theta)
  yinverse = 1.d0/dsin(theta)
  xl2 = dble(l) * dble(l+1)

  

  !m=0
    
  coef = dsqrt( dble(2*l+1)*quarterpiinverse )
  plmdt = plmlocal(1)
     
  bvec(1,0)  = coef * plmlocal(0) 
  bvec(2,0) = coef * plmdt
  bvec(3,0) = dcmplx(0.d0)

     
     
  ! calculate derivatives
  
  bvecdt(1,0) = plmdt * coef 
  bvecdt(2,0) = ( - x * yinverse * plmdt - xl2 * plmlocal(0) ) * coef  
  bvecdt(3,0) = dcmplx(0.d0)
  

  bvecdp(1,0) = dcmplx(0.d0)
  bvecdp(2,0) = dcmplx(0.d0)
  bvecdp(3,0) = dcmplx(0.d0)
    


  
  !m=1

  fact = xl2  
  coef = dsqrt( dble(2*l+1)*quarterpiinverse / fact )
  plmdt = x * yinverse  * plmlocal(1) + plmlocal(2)
     
  bvec(1,1)  = coef * plmlocal(1) 
  bvec(1,-1) = -dconjg( bvec(1,1) )
  bvec(2,1) = coef * plmdt 
  bvec(2,-1) = -dconjg( bvec(2,1) )
  bvec(3,1)  = dcmplx( 0.d0, 1.d0 ) * yinverse * coef * plmlocal(1) 
  bvec(3,-1) = -dconjg( bvec(3,1) )
     
     
  ! calculate derivatives
     
  bvecdt(1,1) = plmdt * coef 
  bvecdt(1,-1) = -dconjg( bvecdt(1,1) )
  bvecdt(2,1) = ( - x * yinverse * plmdt + 1.d0 /(1-x*x)*plmlocal(1) - xl2 * plmlocal(1) ) * coef
  bvecdt(2,-1) = -dconjg( bvecdt(2,1) )
  bvecdt(3,1) = dcmplx( 0.d0, 1.d0 ) * ( - x / ( 1- x * x ) * plmlocal(1)+ 1.d0 * yinverse * plmdt ) * coef 
  bvecdt(3,-1) = -dconjg( bvecdt(3,1) )
  
  bvecdp(1,1) = dcmplx( 0.d0, 1.d0 ) * plmlocal(1) * coef 
  bvecdp(1,-1) = -dconjg( bvecdp(1,1) )
  bvecdp(2,1) = dcmplx( 0.d0,1.d0 ) * plmdt * coef 
  bvecdp(2,-1) = -dconjg( bvecdp(2,1) )
  bvecdp(3,1) = dcmplx(- yinverse *plmlocal(1)*coef)
  bvecdp(3,-1) = -dconjg( bvecdp(3,1) )

  !m=2
  
  fact = dble((l-1)*l*(l+1)*(l+2))
  coef = dsqrt( dble(2*l+1)*quarterpiinverse / fact )
  plmdt = 2.d0 * x * yinverse  * plmlocal(2) + plmlocal(3)
     
  bvec(1,2)  = coef * plmlocal(2) 
  bvec(1,-2) = dconjg( bvec(1,2) )
  bvec(2,2) = coef * plmdt 
  bvec(2,-2) = dconjg( bvec(2,2) )
  bvec(3,2)  = dcmplx( 0.d0, 2.d0 ) * yinverse * coef * plmlocal(2) 
  bvec(3,-2) = dconjg( bvec(3,2) )
     
     
  ! calculate derivatives
  
  bvecdt(1,2) = plmdt * coef 
  bvecdt(1,2) = dconjg( bvecdt(1,2) )
  bvecdt(2,2) = ( - x * yinverse * plmdt + 4.d0 /(1-x*x)*plmlocal(2) - xl2 * plmlocal(2) ) * coef
  bvecdt(2,-2) = dconjg( bvecdt(2,2) )
  bvecdt(3,2) = dcmplx( 0.d0, 2.d0 ) * ( - x / ( 1- x * x ) * plmlocal(2)+ 1.d0 * yinverse * plmdt ) * coef 
  bvecdt(3,-2) = dconjg( bvecdt(3,2) )
  
  bvecdp(1,2) = dcmplx( 0.d0, 2.d0 ) * plmlocal(2) * coef 
  bvecdp(1,-2) = dconjg( bvecdp(1,2) )
  bvecdp(2,2) = dcmplx( 0.d0,2.d0 ) * plmdt * coef 
  bvecdp(2,-2) = dconjg( bvecdp(2,2) )
  bvecdp(3,2) = dcmplx(- 4.d0 * yinverse *plm(1,m)*coef)
  bvecdp(3,-2) = dconjg( bvecdp(3,2) )
    
  return
end subroutine caldvecphi0_withoutplm


subroutine caldvecphi0_withoutplm_sinnonzero( l,theta,plmlocal,bvec,bvecdt,bvecdp)
  
  implicit none
  real(kind(0d0)), parameter ::  pi=3.1415926535897932d0
  real(kind(0d0)), parameter :: quarterpiinverse = 0.07957747154d0
  integer  :: l,i,j
  real(kind(0d0)) :: theta,x,plmlocal(0:3),fact,coef
  complex(kind(0d0)) :: bvec(1:3,-2:2)
  complex(kind(0d0)) :: bvecdt(1:3,-2:2),bvecdp(1:3,-2:2)
  real(kind(0d0)) :: plmdt,xl2,yinverse
  real(kind(0d0)) :: rtxl2,coeff,rtxl22,sign1,sign2

  ! This subroutine does not check if sin(theta) is zero or not

  x = dcos(theta)
  yinverse = 1.d0/dsin(theta)
  xl2 = dble(l) * dble(l+1)

  ! m = 0
    
  coef = dsqrt( dble(2*l+1)*quarterpiinverse )
  plmdt = plmlocal(1)
     
  bvec(1,0)  = coef * plmlocal(0) 
  bvec(2,0) = coef * plmdt
  bvec(3,0) = dcmplx(0.d0)

     
     
  ! calculate derivatives
  
  bvecdt(1,0) = plmdt * coef 
  bvecdt(2,0) = ( - x * yinverse * plmdt - xl2 * plmlocal(0) ) * coef  
  bvecdt(3,0) = dcmplx(0.d0)
  

  bvecdp(1,0) = dcmplx(0.d0)
  bvecdp(2,0) = dcmplx(0.d0)
  bvecdp(3,0) = dcmplx(0.d0)
    

  ! m = 1

  fact = xl2  
  coef = dsqrt( dble(2*l+1)*quarterpiinverse / fact )
  plmdt = x * yinverse  * plmlocal(1) + plmlocal(2)
     
  bvec(1,1)  = coef * plmlocal(1) 
  bvec(1,-1) = -dconjg( bvec(1,1) )
  bvec(2,1) = coef * plmdt 
  bvec(2,-1) = -dconjg( bvec(2,1) )
  bvec(3,1)  = dcmplx( 0.d0, 1.d0 ) * yinverse * coef * plmlocal(1) 
  bvec(3,-1) = -dconjg( bvec(3,1) )
     
     
  ! calculate derivatives
     
  bvecdt(1,1) = plmdt * coef 
  bvecdt(1,-1) = -dconjg( bvecdt(1,1) )
  bvecdt(2,1) = ( - x * yinverse * plmdt + 1.d0 /(1-x*x)*plmlocal(1) - xl2 * plmlocal(1) ) * coef
  bvecdt(2,-1) = -dconjg( bvecdt(2,1) )
  bvecdt(3,1) = dcmplx( 0.d0, 1.d0 ) * ( - x / ( 1- x * x ) * plmlocal(1)+ 1.d0 * yinverse * plmdt ) * coef 
  bvecdt(3,-1) = -dconjg( bvecdt(3,1) )
  
  bvecdp(1,1) = dcmplx( 0.d0, 1.d0 ) * plmlocal(1) * coef 
  bvecdp(1,-1) = -dconjg( bvecdp(1,1) )
  bvecdp(2,1) = dcmplx( 0.d0,1.d0 ) * plmdt * coef 
  bvecdp(2,-1) = -dconjg( bvecdp(2,1) )
  bvecdp(3,1) = dcmplx(- yinverse *plmlocal(1)*coef)
  bvecdp(3,-1) = -dconjg( bvecdp(3,1) )

  ! m = 2
  
  fact = dble((l-1)*l*(l+1)*(l+2))
  coef = dsqrt( dble(2*l+1)*quarterpiinverse / fact )
  plmdt = 2.d0 * x * yinverse  * plmlocal(2) + plmlocal(3)
     
  bvec(1,2)  = coef * plmlocal(2) 
  bvec(1,-2) = dconjg( bvec(1,2) )
  bvec(2,2) = coef * plmdt 
  bvec(2,-2) = dconjg( bvec(2,2) )
  bvec(3,2)  = dcmplx( 0.d0, 2.d0 ) * yinverse * coef * plmlocal(2) 
  bvec(3,-2) = dconjg( bvec(3,2) )
     
     
  ! calculate derivatives
  
  bvecdt(1,2) = plmdt * coef 
  bvecdt(1,2) = dconjg( bvecdt(1,2) )
  bvecdt(2,2) = ( - x * yinverse * plmdt + 4.d0 /(1-x*x)*plmlocal(2) - xl2 * plmlocal(2) ) * coef
  bvecdt(2,-2) = dconjg( bvecdt(2,2) )
  bvecdt(3,2) = dcmplx( 0.d0, 2.d0 ) * ( - x / ( 1- x * x ) * plmlocal(2)+ 1.d0 * yinverse * plmdt ) * coef 
  bvecdt(3,-2) = dconjg( bvecdt(3,2) )
  
  bvecdp(1,2) = dcmplx( 0.d0, 2.d0 ) * plmlocal(2) * coef 
  bvecdp(1,-2) = dconjg( bvecdp(1,2) )
  bvecdp(2,2) = dcmplx( 0.d0,2.d0 ) * plmdt * coef 
  bvecdp(2,-2) = dconjg( bvecdp(2,2) )
  bvecdp(3,2) = dcmplx(- 4.d0 * yinverse *plm(1,m)*coef)
  bvecdp(3,-2) = dconjg( bvecdp(3,2) )
    
  return
end subroutine caldvecphi0_withoutplm_sinnonzero


