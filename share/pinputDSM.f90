! Nobuaki Fuji for DSM Suite (2022)
! This is an effort to unify some redundant DSM legacy subroutines
! for further maintenance

subroutine pinputDatabaseFile(DSMconfFile,outputDir,psvmodel,modelname,tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta,thetamin,thetamax,thetadelta,imin,imax,rsgtswitch,tsgtswitch,synnswitch,psgtswitch)
  implicit none
  character(200) :: dummy,outputDir,psvmodel,modelname,DSMconfFile
  real(kind(0d0)) :: tlen,rmin_,rmax_,rdelta_,r0min,r0max,r0delta
  real(kind(0d0)) :: thetamin,thetamax,thetadelta

  real(kind(0d0)) :: shortestPeriod
  
  integer :: imin,imax,rsgtswitch,tsgtswitch,synnswitch,psgtswitch
  character(200) :: commandline
  character(200) :: tmpfile,tmpfile0
  integer(4) :: istat
  
  character(200) :: argv
  integer :: argc,iFind
  character(200) :: DSMinffile
  character(200) :: paramName

  call getarg(1,argv)
  DSMinffile=argv

  argc=iargc()

  if(argc.ne.1) then
     print *, "DSM2022 will no more use \"<\" to specify DSM inffile for SGTpsv and SGTsh"
     print *, "cheers"
     stop
  endif

  open(unit=5,file=DSMinffile,status='unknown')
  
  tmpfile='tmpworkingfile_for_SGTforPinv'//tmpfile
  call tmpfileNameGenerator(tmpfile,tmpfile)
  
  open(unit=1, file=tmpfile,status='unknown')
100 continue
  read(5,110) dummy
110 format(a200)
  if(dummy(1:1).eq.'#') goto 100
  if(dummy(1:3).eq.'end') goto 120
  write(1,110) dummy
  goto 100
120 continue
  close(1)
  close(5)

  
  call searchForParams(tmpfile,"DSMconfFile",DSMconfFile,0)
  call searchForParams(tmpfile,"outputDir",outputDir,0)
  call searchForParams(tmpfile,"psvmodel",psvmodel,0)
  call searchForParams(tmpfile,"modelname",modelname,0)
  outputDir=trim(outputDir)
  psvmodel=trim(psvmodel)
  modelname=trim(modelname)
  
  dummy = 'mkdir -p '//trim(outputDir)
  call system(dummy)
  
  outputDir=outputDir//modelname
  dummy = 'mkdir -p '//trim(outputDir)
  call system(dummy)
  

  call searchForParams(tmpfile,"tlen",dummy,1)
  read(dummy,*)tlen

  call searchForParams(tmpfile,"radiiQ",dummy,1)
  read(dummy,*)  rmin_,rmax_,rdelta_

  call searchForParams(tmpfile,"radiusSource",dummy,1)
  read(dummy,*) r0min  
  r0max=r0min
  r0delta=20.d0

  call searchForParam(tmpfile,"epicentralDistancesQ",dummy,1)
  read(dummy,*) thetamin,thetamax,thetadelta

  iFind=0
  call searchForParamsOption(tmpfile,"shortestPeriod",dummy,1,iFind)
  if(iFind.eq.2) read(dummy,*) shortestPeriod
  imin=0
  imax=int(tlen/shortestPeriod)

  iFind=0
  call searchForParamsOption(tmpfile,"imin",dummy,1,iFind)
  if(iFind.eq.2) read(dummy,*) imin

  iFind=0
  call searchForParamsOption(tmpfile,"imax",dummy,1,iFind)
  if(iFind.eq.2) read(dummy,*) imax
  

  
  read(1,*) imin,imax
  read(1,*) rsgtswitch,tsgtswitch,synnswitch,psgtswitch
  close(1,status='delete')
 
  ! making directories

  commandline = 'mkdir -p '//trim(outputDir)
  call system(commandline)
  commandline = 'mkdir -p '//trim(outputDir)//'/RSGT'
  call system(commandline)
  commandline = 'mkdir -p '//trim(outputDir)//'/TSGT'
  call system(commandline)
  commandline = 'mkdir -p '//trim(outputDir)//'/log'
  call system(commandline)  


end subroutine pinput
