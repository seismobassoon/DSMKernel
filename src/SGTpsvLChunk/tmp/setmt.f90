program test_setmt

  implicit none
  ! We use the  rr, tt, pp, rt, rp, tp order here!!
  
  real(kind(0d0)) :: mt(3,3),start,end
  integer :: imt

  imt = 1

  call cpu_time(start)
  
  mt = 0.d0
  
  if(imt.eq.1) mt(1,1) = 1.d0
  if(imt.eq.2) mt(2,2) = 1.d0
  if(imt.eq.3) mt(3,3) = 1.d0
  if(imt.eq.4) mt(1,2) = 1.d0
  if(imt.eq.5) mt(1,3) = 1.d0
  if(imt.eq.6) mt(2,3) = 1.d0

  call cpu_time(end)

  print *, end-start

  
  call cpu_time(end)

  mt = 0.d0

  do j=1,3
     do i=1,j
        mt(i,j) = mod(dble(i-1)+3*dble(j-1)
        
