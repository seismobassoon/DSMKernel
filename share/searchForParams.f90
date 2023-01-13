! Nobuaki Fuji for DSM Suite (2022)
!   originally written for MarsMTInversion in 2019
!
! this consists of searchForParams and searchForParamsOptions

subroutine searchForParams(filenameIn,ParamNameIn,textParam,paramisText)
  ! This subroutine reads filename and searches for ParamName to get textParam
  ! if you need to read textParam as a string vector, you just need to prepare THE string
  ! character(<200) :: string_that_you_need_to_get and
  !  call searchForParams(filenmae,'XXX',string_that_you_need_to_get,0)
  ! if you need to read textParam as float or something else, you need to prepare
  ! character(200) :: dummy and call searchForParams(filename,'XXX',dummy,1)
  ! then read(dummy,*) YYY (of the type you need to obtain)
  implicit none
  character(200), intent(in) :: filenameIn
  character(200), intent(out) :: textParam
  character(200) :: text_line
  character, allocatable :: filename, ParamName
  integer :: paramLength,textLength,io
  integer, intent(in) :: paramisText
  character(200), intent(in) :: ParamNameIn
  integer :: iFind
  integer :: jtemp, iCut
  
  filename=trim(filenameIn)
  ParamName=trim(ParamNameIn)
  paramLength=len_trim(ParamName)
  iFind=0
  iCut=0
  
  
  open(20,file=filename,status='unknown')
  do while(iFind.eq.0)
     read(20,'(a)',IOSTAT=io) text_line
     if(io/=0) exit
     if(io>0) then
        print *, "oh, no"
        print *, trim(ParamName), " is not found."
        stop
     endif
     textLength=len_trim(text_line)
     
     if(text_line(1:paramLength).eq.ParamName(1:paramLength)) then
        do jtemp = 1,textLength
           if(text_line(jtemp:jtemp).eq.'=') iCut = jtemp
        enddo
        !print *, iCut, textLength
        iFind=1
        !print *,text_line(iCut+1:textLength)
        textParam=text_line(iCut+1:textLength)
        if(paramisText.eq.0) then
           textParam=trim(textParam)
           textParam=adjustl(textParam)
        endif
          !print *, textParam
     endif
  enddo
  close(20)
  if(iFind.eq.0) then
     print *, trim(ParamName), "is not found."
     stop
  endif
end subroutine searchForParams


subroutine searchForParamsOption(filenameIn,ParamNameIn,textParam,paramisText,iFind)
  ! This is only a variant for optional parameters (even if this is not defined,
  ! the programme will distribute the pre-defined values
  
  implicit none
  character(200), intent(in) :: filenameIn
  character(200), intent(out) :: textParam
  character(200) :: text_line
  character, allocatable :: filename, ParamName
  integer :: paramLength,textLength,io
  integer, intent(in) :: paramisText
  character(200), intent(in) :: ParamNameIn
  integer, intent(inout) :: iFind
  integer :: jtemp, iCut

   
  filename=trim(filenameIn)
  ParamName=trim(ParamNameIn)
  paramLength=len_trim(ParamName)
  iFind=0
  iCut=0
 
  open(20,file=filename,status='unknown')
  do while(iFind.eq.0)
     read(20,'(a)',IOSTAT=io) text_line
     if(io/=0) exit
     if(io>0) then
        !print *, "oh, no"
        !print *, trim(ParamName), " is not found. But maybe it's alright (option parameters)."
        !stop
        iFind=2
     endif
     textLength=len_trim(text_line)
     !print *, text_line(1:textLength)
     if(text_line(1:paramLength).eq.ParamName(1:paramLength)) then
        do jtemp = 1,textLength
           if(text_line(jtemp:jtemp).eq.'=') iCut = jtemp
        enddo
        !print *, iCut, textLength
        iFind=1
        !print *,text_line(iCut+1:textLength)
        textParam=text_line(iCut+1:textLength)
        if(paramisText.eq.0) then
           textParam=trim(textParam)
           textParam=adjustl(textParam)
        endif
        !print *, textParam
     endif
  enddo
  close(20)
  
  if(iFind.eq.2) iFind=0
  
end subroutine searchForParamsOption
   
