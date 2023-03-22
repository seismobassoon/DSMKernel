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
