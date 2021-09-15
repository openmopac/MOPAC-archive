  Program Killer    
!
!  Check how many jobs of a specific type are running.
!  If the number is less than a pre-set number, quit,
!  otherwise wait six secods and check again.
!
!  For use as a gatekeeper action, to make sure that
!  a specific number of jobs are running.  When a job finishes,
!  a new one is allowed to start.
!
!  Usage:   <program> <job-name> <maximum number of jobs>
!  e.g.:    gatekeeper.exe MOPAC 10
!
!  This is the Mac version, for use with OS-X Leopard and Snow Leopard
!
    use ifport, only : system
    implicit none
    character :: location*100, watch_program*50, command*120, line*240
    integer :: itim=7, io_stat, i
    external  getarg
    integer, external :: iarg
    watch_program = "MOPAC2009"
    i = iargc()
    if (i == 1) call getarg(1,watch_program)
    command="ps -ax | grep MOP > ""/Users/jstewart/jobs_running1.lst"""
    location = "/Users/jstewart/jobs_running1.lst"
    i = SYSTEM(trim(command))
    open(unit=itim,file=trim(location),iostat = i)
    do
      read(itim,"(a)", iostat = io_stat) line
      if (io_stat /= 0) exit
      if (index(line,trim(watch_program)) /= 0) then
        if (index(line,"killer") == 0) then
          line = "kill "//line(:6)
          write(*,*)trim(line)
          i = SYSTEM(trim(line))
        end if
      end if
    end do
 !    close(itim, dispose="delete")
  end program Killer    
