module rotate
  real(kind(0d0)) :: cc(3,3),ct(3,3)
end module rotate

module angles
  implicit none
  real(kind(0d0)), allocatable :: phi00(:),phi0(:),theta0(:)
  integer :: nphi0, ntheta,nphi
  real(kind(0d0)), allocatable :: phitheta(:,:),thetaphi(:,:)
  real(kind(0d0)), allocatable :: phi(:,:), theta(:,:)
  real(kind(0d0)), allocatable :: crq(:,:),srq(:,:),crq2(:,:),srq2(:,:)
  real(kind(0d0)), allocatable :: csq(:,:),ssq(:,:),csq2(:,:),ssq2(:,:)
  real(kind(0d0)), allocatable :: cqs(:,:),sqs(:,:),cqs2(:,:),sqs2(:,:)
  real(kind(0d0)), allocatable :: deltar(:,:),deltas(:,:)
  real(kind(0d0)) :: slat,slon,sdep,rlat,rlon
  
end module angles

subroutine translat(geodetic,geocentric)

  implicit none
  real(kind(0d0)),parameter ::  flattening = 1.d0 / 298.25d0
  real(kind(0d0)), parameter :: pi = 3.1415926535897932d0 
  real(kind(0d0)) :: geocentric, geodetic 
  integer :: flag
  flag = 0
  if(geodetic .gt. 90.d0) then
     geodetic = 1.8d2 - geodetic
     flag = 1
  endif
  
  geodetic = geodetic / 1.8d2 * pi
  geocentric = datan((1.d0-flattening)*(1.d0-flattening)* dtan(geodetic) )
  geocentric = geocentric * 1.8d2 / pi
  
  if(flag .eq. 1) then
     geocentric = 1.8d2 - geocentric
  endif

  return
end subroutine translat

subroutine convertPath2Geo
  use rotate
  use angles
  implicit none
  real(kind(0d0)) :: xp,yp,zp,x,y,z,pi
  !real(kind(0d0)) :: cc(3,3),ct(3,3)
  integer :: ith, ip
  !common/rotate/cc,ct
  
  pi=4.d0*datan(1.d0)
  
  ! phitheta and thetaphi : Path-specific system
  ! phi and theta : Geographic system
  allocate(phi(nphi,ntheta),theta(nphi,ntheta))
  do ith=1,ntheta
     do ip=1,nphi
        xp=dsin(thetaphi(ip,ith)*pi/180.d0)* &
             dcos(phitheta(ip,ith)*pi/180.d0)
        yp=dsin(thetaphi(ip,ith)*pi/180.d0)* &
             dsin(phitheta(ip,ith)*pi/180.d0)
        zp=dcos(thetaphi(ip,ith)*pi/180.d0) 
        x=ct(1,1)*xp+ct(1,2)*yp+ct(1,3)*zp
        y=ct(2,1)*xp+ct(2,2)*yp+ct(2,3)*zp
        z=ct(3,1)*xp+ct(3,2)*yp+ct(3,3)*zp
        theta(ip,ith)=datan(dsqrt(x**2+y**2)/z)
        if(z.lt.0.d0) theta(ip,ith)=theta(ip,ith)+pi
        if((x.gt.0.d0).and.(y.eq.0.d0)) then
           phi(ip,ith)=0.d0
        elseif((x.gt.0.d0).and.(y.gt.0.d0)) then
           phi(ip,ith)=datan(y/x)
        elseif((x.eq.0.d0).and.(y.gt.0.d0)) then
           phi(ip,ith)=pi/2.d0
        elseif((x.lt.0.d0).and.(y.gt.0.d0)) then
           phi(ip,ith)=pi-datan(y/(-x))
        elseif((x.lt.0.d0).and.(y.eq.0.d0)) then
           phi(ip,ith)=pi
        elseif((x.lt.0.d0).and.(y.lt.0.d0)) then
           phi(ip,ith)=pi+datan(y/x)
        elseif((x.eq.0.d0).and.(y.lt.0.d0)) then
           phi(ip,ith)=3.d0*pi/2.d0
        else
           phi(ip,ith)=2.d0*pi-datan(-y/x)
        endif
     enddo
  enddo
  return
end subroutine convertPath2Geo


subroutine calculateSineCosineAbsolute(evla,evlo,stla,stlo,geolat,geolon)
  
  !   Compute the distances, azimuths and back-azimuths needed in calculating 
  !   the kernels. Note: these calculations are done in the absolute
  !   coordinate system (cf. calculateSineCosine is based on a great circle)
  
  use angles
  
  implicit none
  real(kind(0d0)) :: pi,xlat,xlon,phisq,phiqs,phirq,phirs
  integer :: ith,ip
  real(kind(0d0)) :: distanx,azimr,bazimr,azims,bazims
  real(kind(0d0)) :: evla,evlo,stla,stlo,geolat(nphi),geolon(ntheta)
  
  pi=4.d0*datan(1.d0) 
  do ith=1,ntheta
     do ip=1,nphi
        call azimth(0,geolat(ith),geolon(ip),stla,stlon,distanx,azimr,bazimr)
        deltar(ip,ith)=distanx
        call azimth(0,evla,evlo,geolat(ith),geolon(ip),distanx,azims,bazims)
        deltas(ip,ith)=distanx
        phirq=pi-azimr*pi/180.d0
        if(phirq.lt.0.d0) phirq=phirq+2.d0*pi
        if(phirq.gt.(2.d0*pi)) phirq=phirq-2.d0*pi
        phiqs=pi-azims*pi/180.d0
        if(phiqs.lt.0.d0) phiqs=phiqs+2.d0*pi
        if(phiqs.gt.(2.d0*pi)) phiqs=phiqs-2.d0*pi
        phisq=pi-bazims*pi/180.d0
        if(phisq.lt.0.d0) phisq=phisq+2.d0*pi
        if(phisq.gt.(2.d0*pi)) phisq=phisq-2.d0*pi
        crq(ip,ith)=dcos(phirq)
        srq(ip,ith)=dsin(phirq)
        crq2(ip,ith)=dcos(2.d0*phirq)
        srq2(ip,ith)=dsin(2.d0*phirq)
        csq(ip,ith)=dcos(phisq)
        ssq(ip,ith)=dsin(phisq)
        csq2(ip,ith)=dcos(2.d0*phisq)
        ssq2(ip,ith)=dsin(2.d0*phisq)
         cqs(ip,ith)=dcos(phiqs)
        sqs(ip,ith)=dsin(phiqs)
        cqs2(ip,ith)=dcos(2.d0*phiqs)
        sqs2(ip,ith)=dsin(2.d0*phiqs)
     enddo
  enddo

  ! for computing the reference seismogram. 
  call azimth(0,evla,evlo,stla,stlo,distan,azim,bazim)
  phirs=pi-azim*pi/180.d0
  crq(0,0)=dcos(phirs)
  srq(0,0)=dsin(phirs)
  crq2(0,0)=dcos(2.d0*phirs)
  srq2(0,0)=dsin(2.d0*phirs)

  return
end subroutine calculateSineCosineAbsolute



subroutine calculateSineCosine(azim)
  
  !   Compute the distances, azimuths and back-azimuths needed in calculating 
  !   the kernels. Note: these calculations are done in the path specific 
  !   coordinate system in which the source and receiver are both at zero
  !   latitude and 0 and distan longitudes, respectively.

  use angles
  implicit none
  real(kind(0d0)) :: pi,xlat,xlon,phisq,phiqs,phirq,phirs
  integer :: ith,ip
  real(kind(0d0)) :: distanx,azimr,bazimr,azims,bazims
  real(kind(0d0)) :: azim
  
  pi=4.d0*datan(1.d0) 
  do ith=1,ntheta
     do ip=1,nphi
        xlat=90.d0-theta(ip,ith)*180.d0/pi
        xlon=phi(ip,ith)*180.d0/pi
        call azimth(0,xlat,xlon,rlat,rlon,distanx,azimr,bazimr)
        deltar(ip,ith)=distanx
        call azimth(0,slat,slon,xlat,xlon,distanx,azims,bazims)
        deltas(ip,ith)=distanx
        phirq=pi-azimr*pi/180.d0
        if(phirq.lt.0.d0) phirq=phirq+2.d0*pi
        if(phirq.gt.(2.d0*pi)) phirq=phirq-2.d0*pi
        phiqs=pi-azims*pi/180.d0
        if(phiqs.lt.0.d0) phiqs=phiqs+2.d0*pi
        if(phiqs.gt.(2.d0*pi)) phiqs=phiqs-2.d0*pi
        phisq=pi-bazims*pi/180.d0
        if(phisq.lt.0.d0) phisq=phisq+2.d0*pi
        if(phisq.gt.(2.d0*pi)) phisq=phisq-2.d0*pi
        crq(ip,ith)=dcos(phirq)
        srq(ip,ith)=dsin(phirq)
        crq2(ip,ith)=dcos(2.d0*phirq)
        srq2(ip,ith)=dsin(2.d0*phirq)
        csq(ip,ith)=dcos(phisq)
        ssq(ip,ith)=dsin(phisq)
        csq2(ip,ith)=dcos(2.d0*phisq)
        ssq2(ip,ith)=dsin(2.d0*phisq)
         cqs(ip,ith)=dcos(phiqs)
        sqs(ip,ith)=dsin(phiqs)
        cqs2(ip,ith)=dcos(2.d0*phiqs)
        sqs2(ip,ith)=dsin(2.d0*phiqs)
     enddo
  enddo

  ! for computing the reference seismogram. 
  
  phirs=pi-azim*pi/180.d0
  crq(0,0)=dcos(phirs)
  srq(0,0)=dsin(phirs)
  crq2(0,0)=dcos(2.d0*phirs)
  srq2(0,0)=dsin(2.d0*phirs)

  return
end subroutine calculateSineCosine

subroutine azimth(ellips,slat,slon,rlat,rlon,delta,azim,bazim)

  !   This routine uses Euler angles to find the geocentric distance,
  !   azimuth, and back azimuth for a source-reciever pair.
  !
  !   Input
  !
  !     slat  - source geographic latitude in decimal degrees
  !     slon  - source longitude in decimal degrees
  !     rlat  - reciever geographic latitude in decimal degrees
  !     rlon  - reciever longitude in decimal degrees
  !
  !   Output
  !
  !     delta - geocentric source-reciever distance in decimal degrees of arc
  !     azim  - geocentric azimuth from the source to the reciever
  !     bazim - geocentric back azimuth from the reciever to the source
  !
  !   The distance calculated here delta is always between 0 and 180 degrees. 
  !   Accordingly, the azimuth and back azimuth are defined for the minor 
  !   arc between (slat,slon) and (rlat,rlon).
  !
  !   if ellips = 0 then geocentric = geocentric
  !     because in NF version it is already taken into account so ellips should be 0 always

  
  implicit none
  real(kind(0d0)) :: dtor,e,slatra,slat,w,s,scolat,rlatra,rlat,rcolat,slonra,rlon,c2,s2,c1,s1,slatrc,x0,y0,z0,x1,y1,z1,x2,y2,z2,slon,rlonra,delta,azim,bazim,pi
  real(kind(0d0)), parameter :: flt = 298.25d0
  integer :: ellips
  
  
  dtor=4.d0*datan(1.d0)/180.d0
  pi=4.d0*datan(1.d0)
  
  if(ellips.ne.0) then
     e=1.d0/flt
  else
     e=0.d0
  endif

  
  !   Convert to geocentric coordinates and from latitude to colatitude.

  slatra=dtor*slat
  w=dsin(slatra)
  s=((2.d0-e)*w+4.d0*e*(w**3))*e*dcos(slatra)
  !scolat=1.5707963d0-slatra+s
  scolat=pi*5.d-1-slatra+s  
  rlatra=dtor*rlat
  w=dsin(rlatra)
  s=((2.d0-e)*w+4.d0*e*(w**3))*e*dcos(rlatra)
  !rcolat=1.5707963d0-rlatra+s
  rcolat=pi*5.d-1-rlatra+s

  slonra=slon*dtor
  rlonra=rlon*dtor
  c2=dcos(scolat)
  s2=dsin(scolat)
  c1=dcos(slonra)
  s1=dsin(slonra)
  slatrc=dsin(rcolat)



  
  !  Find the azimuth and distance by rotating the source to the north pole.
  
  x0=slatrc*dcos(rlonra)
  y0=slatrc*dsin(rlonra)
  z0=dcos(rcolat)
  x1=c1*x0+s1*y0
  
  z0=dcos(rcolat)
  x1=c1*x0+s1*y0
  y1=-s1*x0+c1*y0
  z1=z0
  x2=c2*x1-s2*z1
  y2=y1
  z2=c2*z1+s2*x1
  call findAngles(x2,y2,z2,delta,azim)
  azim=180.d0-azim
  
  !  Find the back azimuth by rotating the receiver to the north pole.
  
  c2=dcos(rcolat)
  s2=dsin(rcolat)
  c1=dcos(rlonra)
  s1=dsin(rlonra)
  slatrc=dsin(scolat)
  x0=slatrc*dcos(slonra)
  y0=slatrc*dsin(slonra)
  z0=dcos(scolat)
  x1=c1*x0+s1*y0
  y1=-s1*x0+c1*y0
  z1=z0
  x2=c2*x1-s2*z1
  y2=y1
  z2=c2*z1+s2*x1
  call findAngles(x2,y2,z2,delta,bazim)
  bazim=180.d0-bazim
  
  return
end subroutine azimth


subroutine findAngles(x,y,z,theta,phi)

  !   Finds the angles theta and phi of a spherical polar coordinate
  !   system from the cartesion coordinates x, y, and z.
  
  implicit none
  real(kind(0d0)) :: pi,rtod,arg1,x,y,theta,phi,z
  ! real(kind(0d0)), parameter :: eps = 1.d-14
  real(kind(0d0)), parameter :: eps = 0.d0
  

  pi=4.d0*datan(1.d0)
  
  rtod=180.d0/pi
  arg1=dsqrt(x*x+y*y)
  theta=datan2(arg1,z)
  if(dabs(x).le.eps.and.dabs(y).le.eps) then
     phi=0.d0
  else
     phi=datan2(y,x)
  endif
  phi=phi*rtod
  theta=theta*rtod
  
  return
end subroutine findAngles


subroutine rotmat(thetas0,phis0,thetar0,phir0,azim0)

  use rotate
  
  !   See notes for the definitions and transformations of coordinate systems.
  implicit none

  real(kind(0d0)):: thetas0,phis0,thetar0,phir0,azim0
  real(kind(0d0)) :: thetas,phis,thetar,phir,azim,alpha,pi
  real(kind(0d0)) :: r1(3,3),r2(3,3),r3(3,3)
  real(kind(0d0)) :: rr(3,3),rt(3,3),rtmp(3,3)
  integer :: i,j,k
  
  !common/rotate/rr,rt
  
  rr(1:3,1:3)=cc(1:3,1:3)
  rt(1:3,1:3)=ct(1:3,1:3)

  pi=4.d0*datan(1.d0)
  
  thetas=thetas0
  phis=phis0
  thetar=thetar0
  phir=phir0
  azim=azim0*pi/180.d0
  
  alpha=pi/2.d0-azim
  if(alpha.le.(-pi)) alpha=2.d0*pi+alpha
  
  !   Initializing all matrices.
  
  do i=1,3
     do j=1,3
        r1(i,j)=0.d0
        r2(i,j)=0.d0
        r3(i,j)=0.d0
        rtmp(i,j)=0.d0
        rr(i,j)=0.d0
        rt(i,j)=0.d0
     enddo
  enddo
  
  !   Rotation matrix r4 from geographic (x, y, z) to path defined (x', y', z').
	
  r1(1,1)=dcos(phis)
  r1(1,2)=dsin(phis)
  r1(2,1)=-r1(1,2)
  r1(2,2)=r1(1,1)
  r1(3,3)=1.d0
  r2(1,1)=dsin(thetas)
  r2(1,3)=dcos(thetas)
  r2(2,2)=1.d0
  r2(3,1)=-r2(1,3)
  r2(3,3)=r2(1,1)
  r3(1,1)=1.d0
  r3(2,2)=dcos(alpha)
  r3(2,3)=dsin(alpha)
  r3(3,2)=-r3(2,3)
  r3(3,3)=r3(2,2)
  
  do i=1,3
     do j=1,3
        do k=1,3
           rtmp(i,j)=rtmp(i,j)+r3(i,k)*r2(k,j)
        enddo
     enddo
  enddo
  do i=1,3
     do j=1,3
        do k=1,3
           rr(i,j)=rr(i,j)+rtmp(i,k)*r1(k,j)
        enddo
     enddo
  enddo
        
  !   Setting neglegible elements in rr to zero.
  
  do i=1,3
     do j=1,3
        if(dabs(rr(i,j)).le.1.d-14) rr(i,j)=0.d0
     enddo
  enddo

  !   Getting the transpose of rr.
  
  do i=1,3
     do j=1,3
        rt(i,j)=rr(j,i)
     enddo
  enddo
	
  do i=1,3
     do j=1,3
        rtmp(i,j)=0.d0
        do k=1,3
           rtmp(i,j)=rtmp(i,j)+rr(i,k)*rt(k,j)
        enddo
     enddo
  enddo
	
  cc(1:3,1:3)=rr(1:3,1:3)
  ct(1:3,1:3)=rt(1:3,1:3)


  return
end subroutine rotmat
