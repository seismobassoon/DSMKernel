! Nobuaki Fuji for DSM Suite (2022)
! This is an effort to unify some redundant DSM legacy subroutines
! for further maintenance

subroutine pinputDatabaseFile(DSMconfFile,outputDir,psvmodel,modelname,tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta,thetamin,thetamax,thetadelta,imin,imax,rsgtswitch,tsgtswitch,synnswitch,psgtswitch,SGTinfo)
  implicit none
  character(200) :: dummy,outputDir,psvmodel,modelname,DSMconfFile,SGTinfo
  real(kind(0d0)) :: tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta
  real(kind(0d0)) :: thetamin,thetamax,thetadelta

  real(kind(0d0)) :: shortestPeriod
  
  integer :: imin,imax,rsgtswitch,tsgtswitch,synnswitch,psgtswitch
  character(200) :: commandline
  character(200) :: tmpfile,tmpfile0
  integer(4) :: istat
  
  character(200) :: argv
  integer :: argc,iFind,numberLines,io,iLine
  character(200) :: DSMinffile
  character(200) :: paramName

  integer :: noDirMaking

  noDirMaking=0
  if(trim(SGTinfo).eq."argvModeUsed") then
     call getarg(1,argv)
     DSMinffile=argv
     
     argc=iargc()
  
     if(argc.ne.1) then
        print *, "usage (DSM2022): (mpirun) SGTpsv/sh DSMinffile"
        print *, "cheers"
        stop
     endif
     
  else
     ! if this subroutine is read without SGTinfo="argvModeUsed", it is only reading
     ! and not for SGTpsv/sh
     DSMinffile=trim(SGTinfo)
     noDirMaking=1
  endif
  
  open(5,file=DSMinffile,status='unknown',action='read')
  numberLines = 0 
  do
     read(5,*,iostat=io)
     if (io/=0) exit
     numberLines = numberLines + 1
  enddo
  close(5)



  
  open(unit=5,file=DSMinffile,status='unknown')
  
  tmpfile='tmpworkingfile_for_SGTforPinv'//trim(tmpfile)
  call tmpfileNameGenerator(tmpfile,tmpfile)
  
  open(unit=1, file=tmpfile,status='unknown')

  do iLine=1,numberLines
  
     read(5,110) dummy
110 format(a200)
     if((dummy(1:1).ne.'#').and.(dummy(1:1).eq.'!')) write(1,110) dummy
  enddo
  
  close(1)
  close(5)

  
  call searchForParams(tmpfile,"DSMconfFile",DSMconfFile,0)
  call searchForParams(tmpfile,"outputDir",outputDir,0)
  call searchForParams(tmpfile,"psvmodel",psvmodel,0)
  call searchForParams(tmpfile,"modelname",modelname,0)
  outputDir=trim(outputDir)
  psvmodel=trim(psvmodel)
  modelname=trim(modelname)

  if(noDirMaking.eq.0) then
     dummy = 'mkdir -p '//trim(outputDir)
     call system(dummy)
  
     outputDir=outputDir//modelname
     dummy = 'mkdir -p '//trim(outputDir)
     call system(dummy)
  endif

  call searchForParams(tmpfile,"tlen",dummy,1)
  read(dummy,*)tlen

  call searchForParams(tmpfile,"radiiQ",dummy,1)
  read(dummy,*)  rmin_,rmax_,rdelta_

  call searchForParams(tmpfile,"radiusSource",dummy,1)
  read(dummy,*) r0min  
  r0max=r0min
  r0delta=20.d0

  call searchForParams(tmpfile,"epicentralDistancesQ",dummy,1)
  read(dummy,*) thetamin,thetamax,thetadelta

  imax=0
  
  iFind=0
  call searchForParamsOption(tmpfile,"shortestPeriod",dummy,1,iFind)
  if(iFind.eq.1) read(dummy,*) shortestPeriod
  imin=0
  imax=int(tlen/shortestPeriod)

  iFind=0
  call searchForParamsOption(tmpfile,"imin",dummy,1,iFind)
  if(iFind.eq.1) read(dummy,*) imin

  iFind=0
  call searchForParamsOption(tmpfile,"imax",dummy,1,iFind)
  if(iFind.eq.1) read(dummy,*) imax

  if(imax.eq.0) then
     print *, "You need to define either shortestPeriod or imax"
     stop
  endif
  

  rsgtswitch = 1
  tsgtswitch = 1
  synnswitch = 1
  psgtswitch = 1

  iFind=0
  call searchForParamsOption(tmpfile,"rsgtswitch",dummy,1,iFind)
  if(iFind.eq.1) read(dummy,*) rsgtswitch

  iFind=0
  call searchForParamsOption(tmpfile,"tsgtswitch",dummy,1,iFind)
  if(iFind.eq.1) read(dummy,*) tsgtswitch

  iFind=0
  call searchForParamsOption(tmpfile,"synnswitch",dummy,1,iFind)
  if(iFind.eq.1) read(dummy,*) synnswitch

  iFind=0
  call searchForParamsOption(tmpfile,"psgtswitch",dummy,1,iFind)
  if(iFind.eq.1) read(dummy,*) psgtswitch
  
  ! delete this tmpfile when thi is safely read
  open(1,file=tmpfile,status='unknown')
  close(1,status='delete')
 
  ! making directories

  if(noDirMaking.eq.0) then
     commandline = 'mkdir -p '//trim(outputDir)//'/RSGT'
     call system(commandline)
     commandline = 'mkdir -p '//trim(outputDir)//'/TSGT'
     call system(commandline)
     commandline = 'mkdir -p '//trim(outputDir)//'/log'
     call system(commandline)  
  endif

end subroutine pinputDatabaseFile


subroutine readDSMconfFile(DSMconfFile,re,ratc,ratl,omegai,maxlmax)
  implicit none
  character(200) :: dummy,DSMconfFile
  real(kind(0d0)) :: re,ratc,ratl,omegai
  integer  :: maxlmax
  integer :: numberLines,io,iLine
  character(120) :: tmpfile
  
  tmpfile='tmpworkingfile_for_DSMconf'
  call tmpfileNameGenerator(tmpfile,tmpfile)

  open(5,file=DSMconfFile,status='unknown',action='read')
  numberLines = 0 
  do
     read(5,*,iostat=io)
     if (io/=0) exit
     numberLines = numberLines + 1
  enddo
  close(5)
  

  open(unit=2, file=DSMconfFile, status='old',action='read',position='rewind')
  open(unit=1, file=tmpfile,status='unknown')

  do iLine=1,numberLines
  
     read(2,110) dummy
110  format(a200)
     if((dummy(1:1).ne.'#').and.(dummy(1:1).eq.'!')) write(1,110) dummy
  enddo
  close(1)
  close(2)
  
 
  open(unit=1,file=tmpfile,status='unknown')
  read(1,*) re
  read(1,*) ratc
  read(1,*) ratl
  read(1,*) omegai
  read(1,*) maxlmax
  close(1,status='delete')

end subroutine readDSMconfFile


subroutine readpsvmodel(psvmodel,tmpfile)
  implicit none
  character(200) :: psvmodel, tmpfile, dummy
  integer :: numberLines,io,iLine


  open(5,file=psvmodel,status='unknown',action='read')
  numberLines = 0 
  do
     read(5,*,iostat=io)
     if (io/=0) exit
     numberLines = numberLines + 1
  enddo
  close(5)  
  
  open(unit=2, file=psvmodel, status='old',action='read',position='rewind')
  open(unit=1, file=tmpfile,status='unknown')

  do iLine=1,numberLines
     read(2,110) dummy
110  format(a200)
     if((dummy(1:1).ne.'#').and.(dummy(1:1).eq.'!')) write(1,110) dummy
  enddo
  close(1)
  close(2)
  
end subroutine readpsvmodel
