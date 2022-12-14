subroutine tmpfileNameGenerator(basename,tmpfile)
  ! NF 04/11/2022 This programme will just find a filename that will not crash the existing files
  implicit none
  character(120) :: tmpfile,basename
  integer :: i,j
  logical :: exists
  do i=0,100
     write(tmpfile,'(Z3.3)') i
     tmpfile=basename//tmpfile
     inquire(file=tmpfile,exist=exists)
     if(exists) then
        j=0
     else
        j=i
     endif
  enddo
  write(tmpfile,'(Z3.3)') j
  tmpfile=basename//tmpfile
end subroutine tmpfileNameGenerator
