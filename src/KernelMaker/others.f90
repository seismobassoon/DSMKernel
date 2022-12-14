subroutine gridMakingGeographic
  use parametersKernel
  use localParamKernel
  use angles
  implicit none

  real(kind(0d0)), allocatable :: geolat(:), geolon(:)
  integer :: nlat, nlon
  integer :: ilocal

  ! NF in this iCompute = 1 mode, nphi and ntheta are just symbolic
  ! but at least it is in order of lat and lon

  ! latitude grid 
  
  nlat = int((qlat_max-qlat_min)/qlat_delta)+1
  allocate(geolat(1:nlat))
  do ilocal = 1, nlat
     geolat(ilocal) = qlat_min+dble(ilocal-1)*qlat_delta
     ! geodetic -> geocentric
     if(itranslat.eq.1) call translat(geolat(ilocal),geolat(ilocal))
  enddo

  ! longitude grid

  nlon = int((qlon_max-qlon_min)/qlon_delta)+1
  allocate(geolon(1:nlon))
  do ilocal = 1, nlon
     geolon(ilocal) = qlon_min+dble(ilocal-1)*qlon_delta
  enddo

  ! lat -> phi; lon -> theta
  nphi = nlat
  ntheta = nlon

  allocate(phitheta(nphi,ntheta))
  allocate(thetaphi(nphi,ntheta))

  do ilocal=1,nlat
     phitheta(ilocal,1:ntheta) = geolat(ilocal)
  enddo

  do ilocal=1,nlon
     thetaphi(1:nphi,ilocal) = geolon(ilocal)
  enddo
  
  
  ! rapid computation
  
  allocate(iflagForRapidity(1:nphi))
  allocate(iflagForRapidityOld(1:nphi))
  allocate(iflagForRapidityNext(1:nphi))
  allocate(iflagForRapidity0(1:nphi))
  allocate(modiflag(1:nphi-nMinLengthFor0+1))
  
  ! rotation
  ! we use evla, evlo, stla, stlo (converted to geocentric but raw coordinates)


  allocate(crq(0:nphi,0:ntheta),crq2(0:nphi,0:ntheta))
  allocate(srq(0:nphi,0:ntheta),srq2(0:nphi,0:ntheta))
  allocate(csq(0:nphi,0:ntheta),csq2(0:nphi,0:ntheta))
  allocate(ssq(0:nphi,0:ntheta),ssq2(0:nphi,0:ntheta))
  allocate(cqs(0:nphi,0:ntheta),cqs2(0:nphi,0:ntheta))
  allocate(sqs(0:nphi,0:ntheta),sqs2(0:nphi,0:ntheta))
  allocate(deltar(nphi,ntheta),deltas(nphi,ntheta))

  call calculateSineCosineAbsolute(evla,evlo,stla,stlo,geolat,geolon)

  
 
end subroutine gridMakingGeographic


subroutine GreatCircleBasedGridding
  use angles
  use parametersKernel
  use localParamKernel
  implicit none
  
  call hgridsimple(distan,ph1,dph,thw,dth)
  
  allocate(iflagForRapidity(1:nphi))
  allocate(iflagForRapidityOld(1:nphi))
  allocate(iflagForRapidityNext(1:nphi))
  allocate(iflagForRapidity0(1:nphi))
  allocate(modiflag(1:nphi-nMinLengthFor0+1))
  
  call convertPath2Geo
  
  ! Calculate the cosine and sine functions in SGT expressions for scattering points (ZC10a)
  allocate(crq(0:nphi,0:ntheta),crq2(0:nphi,0:ntheta))
  allocate(srq(0:nphi,0:ntheta),srq2(0:nphi,0:ntheta))
  allocate(csq(0:nphi,0:ntheta),csq2(0:nphi,0:ntheta))
  allocate(ssq(0:nphi,0:ntheta),ssq2(0:nphi,0:ntheta))
  allocate(cqs(0:nphi,0:ntheta),cqs2(0:nphi,0:ntheta))
  allocate(sqs(0:nphi,0:ntheta),sqs2(0:nphi,0:ntheta))
  allocate(deltar(nphi,ntheta),deltas(nphi,ntheta))

  call calculateSineCosine(azim)
 
  
end subroutine GreatCircleBasedGridding

  
subroutine hgridsimple(distan,edge,dph0,width,dth0)
  ! This subroutine is generating grids in horizontal direction (2D) for iCompute=1 mode

  use angles
  implicit none
  real(kind(0d0)) :: dph0,dth0,dph,dth,distan,edge,width 
  integer :: midi
  real(kind(0d0)), parameter :: dismin = 0.d0
  integer :: i,j,ip,ith


  

  nphi0=int((distan+2.d0*edge+0.00001)/dph0)+1
  dph=(distan+2.d0*edge)/dble(nphi0-1)
  allocate(phi00(nphi0))
  do i=1,nphi0
     phi00(i)=-edge+dble(i-1)*dph
  enddo
      
  ntheta=1
  if(width.gt.0.d0) ntheta=2*int(width/dth0)+1	
  allocate(theta0(ntheta))
  
  if(width.eq.0.d0) then
     theta0(1)=90.d0
     dth=dph
  else	
     midi=int(ntheta/2)+1
     theta0(midi)=90.d0
     dth=width/dble(midi-1)
     do i=midi-1,1,-1
        theta0(i)=theta0(i+1)-dth
     enddo
     do i=midi+1,ntheta
        theta0(i)=theta0(i-1)+dth
     enddo
  endif
  theta0(ntheta/2+1)=90.d0
      
  
      
  !     Delete phi samples within dismin from source or receiver. 
  
  nphi=0
  do i=1,nphi0
     if((dabs(phi00(i)).lt.dismin).or.(dabs(phi00(i)-distan).lt.dismin)) then
     else
        nphi=nphi+1
     endif
  end do
  allocate(phi0(nphi))
  j=0
  do i=1,nphi0
     if((dabs(phi00(i)).lt.dismin).or.(dabs(phi00(i)-distan).lt.dismin)) then
     else
        j=j+1
        phi0(j)=phi00(i)
     endif
  enddo
  
      
  allocate(phitheta(nphi,ntheta))
  allocate(thetaphi(nphi,ntheta))
  
  do ip=1,nphi
     do ith=1,ntheta
        phitheta(ip,ith)= phi0(ip)
        thetaphi(ip,ith)= theta0(ith)
     enddo
  enddo
  return
end subroutine hgridsimple






subroutine coeffCalculator
  use parametersKernel
  use tmpSGTs
  implicit none
  
  ! Here I replace those expressions for Q with Fuji et al. 2010
  !
  !    I replace coeff(8) with J^lnA
  !      2013.5.6.
  !    I define jacobianFuji(ir,ifreq) 2013.5.28.
  !    I define jacobianFujiVp(ir,ifreq) 2022.11.24


  integer :: ift,it,ir,ifreq
  real(kind(0d0)) :: f0
  real(kind(0d0)), parameter :: gnormt = 1.d0, gnorma = 1.d0
  
  denomv=0.d0
  denomu=0.d0
  do ift=0,nfilter	
     do it=nt1(ift),nt2(ift)
        if((it.eq.nt1(ift)).or.(it.eq.nt2(ift))) then
           denomv(ift)=denomv(ift)+0.5d0*v0(ift,it)**2*dtn
           denomu(ift)=denomu(ift)+0.5d0*u0(ift,it)**2*dtn
        else
           denomv(ift)=denomv(ift)+v0(ift,it)**2*dtn
           denomu(ift)=denomu(ift)+u0(ift,it)**2*dtn
        endif
     enddo
  enddo

  
  allocate(coeff(0:nfilter,1:nktype,1:nr,iWindowStart:iWindowEnd))
  allocate(coeffV(1:nkvtype,1:nr))
  allocate(jacobianFuji(1:nr,fmin:fmax))
  allocate(jacobianFujiVp(1:nr,fmin:fmax))

  coeff=0.d0  
  coeffV=0.d0

  do ir=1,nr
     coeffV(1,ir) = -2.d0*gnormt*rhom(ir)*vpm(ir)**2
     coeffV(2,ir) = -4.d0*gnormt*rhom(ir)*vsm(ir)**2        
  enddo

  do ir=1,nr
     do ift=0,nfilter
        f0=(fclp(ift)+fchp(ift))/2.d0
        do it=nt1(ift),nt2(ift)
           coeff(ift,1,ir,it)=-2.d0*gnormt*rhom(ir)*vpm(ir)**2*&
                v0(ift,it)*dtn/denomv(ift)
           
           coeff(ift,3,ir,it)= 2.d0*gnorma*rhom(ir)*vpm(ir)**2*&
                u0(ift,it)*dtn/denomu(ift)
           coeff(ift,4,ir,it)=-2.d0*gnorma*rhom(ir)* &
                (vpm(ir)**2-4.d0*vsm(ir)**2/3.d0)* &
     	        (2.d0*dlog(f0)*u0(ift,it)/pi-hu0(ift,it))*dtn/ &
     	        (qkp(ir)*denomu(ift))
           coeff(ift,5,ir,it)=-4.d0*gnormt*rhom(ir)*vsm(ir)**2* &
     	        v0(ift,it)*dtn/denomv(ift)
           coeff(ift,7,ir,it)=4.d0*gnorma*rhom(ir)*vsm(ir)**2* &
                u0(ift,it)*dtn/denomu(ift)

           ! replace the older version for Q
           !if(qmm(ir).ne.0.) coeff(ift,8,ir,it)=2.d0*gnorma*rhom(ir)* &
     	   !     vsm(ir)**2*(2.d0*dlog(f0)*u0(ift,it)/pi-hu0(ift,it))* &
     	   !     dtn/(qmm(ir)*denomu(ift))


           ! This is the J^lnAn *q
           
           coeff(ift,8,ir,it)=gnorma/qmm(ir)* &
                u0(ift,it)*dtn/denomu(ift)


           !!!
           coeff(ift,9,ir,it)=-2.d0*gnormt*rhom(ir)*vpm(ir)**2* &
     	        v0(ift,it)*dtn/denomv(ift)
           coeff(ift,10,ir,it)=2.d0*gnorma*rhom(ir)*vpm(ir)**2* &
     	        u0(ift,it)*dtn/denomu(ift)
           coeff(ift,11,ir,it)=-gnormt*rhom(ir)*vpm(ir)**2* &
     	        v0(ift,it)*dtn/denomv(ift)
           coeff(ift,12,ir,it)=gnorma*rhom(ir)*vpm(ir)**2* &
                u0(ift,it)*dtn/denomu(ift)
           coeff(ift,13,ir,it)=-4.d0*gnormt*rhom(ir)*vsm(ir)**2* &
     	        v0(ift,it)*dtn/denomv(ift)
           coeff(ift,14,ir,it)=4.d0*gnorma*rhom(ir)*vsm(ir)**2* &
     	        u0(ift,it)*dtn/denomu(ift)
           ! denomu for anisotropy in aniso version I will include this, too
           coeff(ift,15,ir,it)=-4.d0*gnormt*rhom(ir)*vsm(ir)**2* &
                v0(ift,it)*dtn/denomv(ift)
           coeff(ift,16,ir,it)=-4.d0*gnormt*rhom(ir)*vpm(ir)**2* &
     	        v0(ift,it)*dtn/denomv(ift)
           coeff(ift,17,ir,it)=-2.d0*gnormt*rhom(ir)*vpm(ir)**2* &
                v0(ift,it)*dtn/denomv(ift)
           coeff(ift,18,ir,it)=-8.d0*gnormt*rhom(ir)*vsm(ir)**2* &              
                v0(ift,it)*dtn/denomv(ift)
           coeff(ift,19,ir,it)=-2.d0*gnormt*rhom(ir)*vsm(ir)**2* &
     	        v0(ift,it)*dtn/denomv(ift)
           coeff(ift,20,ir,it)=-4.d0*gnormt*rhom(ir)*vsm(ir)**2* &
                v0(ift,it)*dtn/denomv(ift)
           !!!!
        enddo
     enddo
  enddo

  ! jacobianFuji (ir,ifreq)

  do ir=1,nr
     if(qmm(ir).ne.0.d0) then

        do ifreq=fmin,fmax
            if(ifreq.ne.0) then
           !jacobianFuji(ir,ifreq)=(rhom(ir)*vsm(ir)**2*cmplx(2.d0*log(omega(ifreq)/pi),(1+4.d0*log(omega(ifreq)/pi)/pi/qmm(ir)))) &
            !    / (1+2.d0*log(omega(ifreq)/pi/qmm(ir)))/cmplx(1.d0,1.d0/qmm(ir))
             jacobianFuji(ir,ifreq)=(rhom(ir)*vsm(ir)**2)*cmplx(2.d0*log(omega(ifreq)/2.d0/pi)/pi,1.d0+4.d0*log(omega(ifreq)/2.d0/pi)/pi/qmm(ir))/ &
                     cmplx(1.d0+2*log(omega(ifreq)/2.d0/pi)/pi/qmm(ir),0.d0)/cmplx(1.d0,1.d0/qmm(ir))
           endif
       enddo
    endif


    if(qkp(ir).ne.0.d0) then
       do ifreq=fmin,fmax
          if(ifreq.ne.0) then
            
             jacobianFujiVp(ir,ifreq)=(rhom(ir)*vpm(ir)**2-1.3333333333333333d0*rhom(ir)*vsm(ir)**2)*cmplx(2.d0*log(omega(ifreq)/2.d0/pi)/pi,1.d0+4.d0*log(omega(ifreq)/2.d0/pi)/pi/qkp(ir))/ &
                  cmplx(1.d0+2*log(omega(ifreq)/2.d0/pi)/pi/qkp(ir),0.d0)/cmplx(1.d0,1.d0/qkp(ir))
          endif
             
       enddo
    endif
  enddo
           
  return
end subroutine coeffCalculator
