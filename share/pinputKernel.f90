! Nobuaki Fuji for DSM Suite (2022)
! This is an effort to unify some redundant DSM legacy subroutines
! for further maintenance



subroutine pinputKernel

  use parametersKernel
  use kernels
  implicit none
  character(200) :: tmpfile,kernelInfFile
  character(200) :: dummy
  integer :: iitmp,maxlmax
  character(200) :: commandline
  integer :: numberLines,io,iLine

  call getarg(1,argv)
  kernelInfFile=argv

  argc=iargc()

  if(argc.ne.1) then
     print *, "usage (DSM2022): (mpirun) SGTpsv/sh DSMinffile"
     print *, "cheers"
     stop
  endif

  
  open(5,file=kernelInfFile,status='unknown',action='read')
  numberLines = 0 
  do
     read(5,*,iostat=io)
     if (io/=0) exit
     numberLines = numberLines + 1
  enddo
  close(5)
  
  
  tmpfile='tmpworkingfile_for_SynViewer'
  call tmpfileNameGenerator(tmpfile,tmpfile)



  open(unit=5, file=kernelInfFile,status='old')
  open(unit=1, file=tmpfile,status='unknown')

  do iLIne=1,numberLines
  
     read(5,110) dummy
110  format(a200)
     if((dummy(1:1).ne.'#').and.(dummy(1:1).eq.'!')) write(1,110) dummy
  enddo
  close(1)
  close(5)
  
  open(unit=1,file=tmpfile,status='unknown')
  ! 0a
  read(1,110) SGTinfo
  SGTinfo = trim(SGTinfo)
  ! 0b
  read(1,110) parentDir
  parentDir = trim(parentDir)
  ! 1a
  read(1,110) eventName
  eventName = trim(eventName)
  ! 1b
  read(1,*) evla, evlo, evdepth
  ! 1c
  read(1,*) mt(1), mt(2), mt(3), mt(4), mt(5), mt(6)
  ! 2a
  read(1,110) stationName
  stationName = trim(stationName)
  ! 2b 
  read(1,*) stla, stlo
  ! 3
  read(1,110) phase
  phase = trim(phase)
  ! 4
  read(1,110) compo
  compo = trim(compo)
  ! 5
  read(1,110) paramWRT
  paramWRT = trim(paramWRT)
  timeincrementV=0.d0
  if((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.&
     (trim(paramWRT).eq.'allV').or.(trim(paramWRT).eq.'video').or.&
     (trim(paramWRT).eq.'vRSGT').or.(trim(paramWRT).eq.'vTSGT')) then
     ! alphaV, betaV, allV are for waveform partials, video for forward modelling (Green's function)
    ! 5-bis
      read(1,*) timeincrementV ! in second 
  endif  
  ! 6a
  read(1,*) ibwfilt
  ! 6b
  read(1,110) dummy
  freqid(0) = trim(dummy)
  if(ibwfilt.eq.1) then
     ! 6c
     read(1,*) fclp(0), fchp(0), npButterworth
     ! in the near future we will calculate multiple frequencies in the same time
  endif
  ! 7
  twin(1:4) = 0.d0
  read(1,*) twin(1),twin(2),twin(3),twin(4)
  ! 8
  read(1,*) itranslat
  ! Aa
  read(1,*) ipdistance
  ! Ab
  read(1,*) c_red_reci
  ! Ba
  read(1,*) ifastFFT
  if(ifastFFT.eq.1) then
     ! Bb
     read(1,*) fmin, fmax
  endif
  ! Ca
  read(1,*) dph, ph1
  ! Cb
  read(1,*) dth, thw
  ! Cd
  read(1,*) rmin,rmax,rdelta
  ! Da
  read(1,*) start, end
  ! Db
  read(1,*) samplingHz
  ! Ea
  read(1,*) calculrapide
  if(calculrapide.ne.0.d0) then
     ! Eb
     read(1,*)  nntype
  else
     nntype = 1
  endif
  allocate(minici(1:nntype,0:nfilter))
  allocate(idecidetype(1:nntype))
  
  if(calculrapide.ne.0.d0) then
     do iitmp = 1, nntype
        ! Ec 
        read(1,*) idecidetype(iitmp)
     enddo
  endif
  read(1,*) iPSVSH
  close(1,status='delete')


  commandline = 'mkdir -p '//trim(parentDir)
  call system(commandline)
  commandline = 'mkdir -p '//trim(parentDir)//'/log'
  call system(commandline)
  commandline = 'mkdir -p '//trim(parentDir)//'/tmp'
  call system(commandline)
  commandline = 'mkdir -p '//trim(parentDir)//'/tmpvideo'
  call system(commandline)
  commandline = 'mkdir -p '//trim(parentDir)//'/tmpvideoparts'
  call system(commandline)      
    commandline = 'mkdir -p '//trim(parentDir)//'/seriousfrechet'
  call system(commandline)   

  call pinputDSM(DSMconfFile,PoutputDir,psvmodel,modelname,tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta,thetamin,thetamax,thetadelta,imin,imax,rsgtswitch,tsgtswitch,synnswitch,SGTinfo)
  call readDSMconf(DSMconfFile,re,ratc,ratl,omegai,maxlmax)
 
  tmpfile = 'tmpworkingfile_for_psvmodel'
  call tmpfileNameGenerator(tmpfile,tmpfile)
  tmppsvfile=tmpfile
  call readpsvmodel(psvmodel,tmpfile)
  INFO_TSGT = trim(parentDir)//"/INFO_TSGT.TXT"
  INFO_RSGT = trim(parentDir)//"/INFO_RSGT.TXT"
  rsampletxt = trim(parentDir)//"/rsample.txt"
  modelcard = trim(parentDir)//"/"//trim(modelname)//".card"

  synnfile = trim(parentDir)//"/"//trim(stationName)//"."//trim(eventName)//"."//trim(compo)//"s.dat"




  if(itranslat.eq.1) then
     call translat (stla,stla)
     call translat (evla,evla)
  endif


  return
end subroutine pinputKernel
     
