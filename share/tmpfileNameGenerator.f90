subroutine tmpfileNameGenerator(basename,tmpfile)
  ! NF 04/11/2022 This programme will just find a filename that will not crash the existing files
  implicit none
  character(200) :: tmpfile,basename
  integer :: i,j
  logical :: exists
  !print *, basename
  do i=0,999
     write(tmpfile,'(I0.3)') i
     tmpfile=trim(basename)//trim(tmpfile)
     inquire(file=tmpfile,exist=exists)
     if(exists) then
        j=0
     else
        j=i
        exit
     endif
  enddo
  !write(tmpfile,'(I0.3)') j
  !tmpfile=trim(basename)//tmpfile
  print *, tmpfile
end subroutine tmpfileNameGenerator
