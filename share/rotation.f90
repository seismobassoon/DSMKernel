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

