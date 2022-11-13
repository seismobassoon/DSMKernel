! Nobuaki Fuji for DSM Suite (2022)
! This is an effort to unify some redundant DSM legacy subroutines
! for further maintenance
! I put this in share because some other softwares (e.g. MarsMTInversion)
! use this



subroutine pinputKernel

  use parametersKernel
  use kernels
  implicit none
  character(200) :: tmpfile,kernelInfFile
  character(200) :: dummy
  character(10) :: period1, period2
  character(1) :: npolesChar
  integer :: iitmp,maxlmax
  character(200) :: commandline
  integer :: numberLines,io,iLine
  integer :: iFind
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



  call searchForParams(tmpfile,"DatabaseInput",SGTinfo,0)
  call searchForParams(tmpfile,"KernelOutputDir",parentDir,0)
  call searchForParams(tmpfile,"eventName",eventName,0)
  call searchForParams(tmpfile,"eventLocation",dummy,1)
  read(dummy,*) evla, evlo, evdepth
  call searchForParams(tmpfile,"MomentTensor",dummy,1)
  read(dummy,*) mt(1), mt(2), mt(3), mt(4), mt(5), mt(6)
  call searchForParams(tmpfile,"stationName",stationName,0)
  call searchForParams(tmpfile,"stationLocation",dummy,1)
  read(dummy,*) stla, stlo
  call searchForParams(tmpfile,"phaseName",phase,0)
  call searchForParams(tmpfile,"component",compo,0)
  call searchForParams(tmpfile,"paramWRT",paramWRT,0)
  
  timeincrementV=0.d0
  
  if((trim(paramWRT).eq.'alphaV').or.(trim(paramWRT).eq.'betaV').or.&
     (trim(paramWRT).eq.'allV').or.(trim(paramWRT).eq.'video').or.&
     (trim(paramWRT).eq.'vRSGT').or.(trim(paramWRT).eq.'vTSGT')) then
     ! alphaV, betaV, allV are for waveform partials, video for forward modelling (Green's function)
     ! 5-bis
     call searchForParams(tmpfile,"timeincrementV",dummy,1)
     read(dummy,*) timeincrementV ! in second 
  endif

  call searchForParms(tmpfile,"ibwfilt",dummy,1)
  read(dummy,*) ibwfilt
 
  if(ibwfilt.eq.1) then
     call searchForParams(tmpfile,"lowHighPoles",dummy,0)
     read(dummy,*) fclp(0), fchp(0), npButterworth
     write(period1,"(A)") int(1/fchp(0))
     write(period2,"(A)") int(1/fclp(0))
     write(npolesChar,"(A)") npButterworth
     call searchForParamsOption(tmpfile,"filterName",dummy,1,iFind)
     if(iFind.eq.1) then
        freqid(0) = trim(dummy)
     else
        freqid(0) = trim(period1)//'s_'//trim(period2)//'s_np'//npolesChar
     endif    
  else
     freqid(0)="nofilter"
  endif

  twin(1:4) = 0.d0
  call searchForParams(tmpfile,"twin",dummy,0)
  read(dummy,*) twin(1),twin(2),twin(3),twin(4)
  
  call searchForParams(tmpfile,"itranslat",dummy,0)
  read(dummy,*) itranslat

  ! First we take default values for minor parameters

  ipdistance = 10.d0 
  call searchForParamsOption(tmpfile,"ipdistance",dummy,0,iFind)
  if(iFind.eq.1) read(dummy,*) ipdistance

  c_red_reci = 0.d0
  call searchForParamsOption(tmpfile,"c_red_reci",dummy,0,iFind)
  if(iFind.eq.1) read(dummy,*) c_red_reci
  
  ifastFFT=0
  call searchForParamsOption(tmpfile,"ifastFFT",dummy,0,iFind)
  if(iFind.eq.1) read(dummy,*) ifastFFT

  
  if(ifastFFT.eq.1) then
     call searchForParams(tmpfile,"fminfmax",dummy,0)
     read(dummy,*) fmin, fmax
  endif
  

  iBananaCentred = 1
  call searchForParamsOption(tmpfile,"iBananaCentred",dummy,0,iFind)
  if(iFind.eq.1) read(dummy,*) iBananaCentred


  if(iBananaCentred.eq.1) then
     dph=5.d-1
     ph1=10.d0
     dth=5.d-1
     thw=10.d0
     rmin=0.d0
     rmax=0.d0
     rdelta=0.d0

     call searchForParamsOption(tmpfile,"delta_distance",dummy,0,iFind)
     if(iFind.eq.1) read(dummy,*) dph

     call searchForParamsOption(tmpfile,"extension_distance",dummy,0,iFind)
     if(iFind.eq.1) read(dummy,*) ph1

     call searchForParamsOption(tmpfile,"delta_width",dummy,0,iFind)
     if(iFind.eq.1) read(dummy,*) dth

     call searchForParamsOption(tmpfile,"extension_width",dummy,0,iFind)
     if(iFind.eq.1) read(dummy,*) thw

     call searchForParamsOption(tmpfile,"rmin_rmax_rdelta",dummy,0,iFind)
     if(iFind.eq.1) read(dummy,*) rmin,rmax,rdelta
  endif


  ! NF starts here
  
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

  call pinputDatabaseFile(DSMconfFile,PoutputDir,psvmodel,modelname,tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta,thetamin,thetamax,thetadelta,imin,imax,rsgtswitch,tsgtswitch,synnswitch,SGTinfo)
  call readDSMconfFile(DSMconfFile,re,ratc,ratl,omegai,maxlmax)
 
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
     
