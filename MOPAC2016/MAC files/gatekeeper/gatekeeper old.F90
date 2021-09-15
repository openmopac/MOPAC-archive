  Program Gatekeeper
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
    character :: location*100, watch_program*50, command*120, line*240, tmp_file*240
    integer :: maxprog, itim=7, io_stat, i, njobs
    external  getarg
    integer, external :: iarg
    maxprog = 10
    watch_program = "MOPAC2009"
    i = iargc()
    if (i == 3) then
      call getarg(1,tmp_file)
      call getarg(2,watch_program)
      call getarg(3,line)
      maxprog = ichar(line(1:1)) - ichar("0")
      if (line(2:2) /= " ") maxprog = maxprog*10 +ichar(line(2:2)) - ichar("0")
    else
      write(*,'(a)') "Wait.exe is a program to control when a job is allowed to start" 
      write(*,'(a)') "It uses three arguments:"
      write(*,'(a)') "The subdirectory where a temporary file can be created and destroyed by Wait.exe"
      write(*,'(a)') "The name of the type of executable that is to be monitored"
      write(*,'(a)') "The maximum number of executables"
      write(*,'(a)') " "
      write(*,'(a)') "For example: Wait.exe ""/Users/myname/"" MOPAC 7"
      write(*,'(a)') " "
      write(*,'(a)') "In this case, the user 'myname' owns and controls the subdirectory '/Users/myname/'"
      write(*,'(a)') "The type of process being monitored is a MOPAC job"
      write(*,'(a)') "and the maximum number of MOPAC jobs allowed is 7"
      write(*,'(a)') "When the number of MOPAC jobs drops below 7, Wait.exe"
      write(*,'(a)') "stops, and the next operation - a new MOPAC job - can start"
      call sleep(300)
      stop
    end if
    i = len_trim(tmp_file)
    if (tmp_file(i:i) /= "/" .and. tmp_file(i:i) /= "\") tmp_file(i + 1:i + 1) = "/"
    command="ps -ax | grep MOP > """//trim(tmp_file)//"jobs_running.lst"""
    location = "/Users/jstewart/jobs_running.lst"
    do
      i = SYSTEM(trim(command))
      open(unit=itim,file=trim(location),iostat = i)
      njobs = 0
      do
        read(itim,"(a)", iostat = io_stat) line
        if (io_stat /= 0) exit
        if (index(line,trim(watch_program)) /= 0) njobs = njobs + 1
      end do
      close(itim, dispose="delete")
      if (njobs <= maxprog) exit
      call sleepqq(6000)
    end do
  end program Gatekeeper
