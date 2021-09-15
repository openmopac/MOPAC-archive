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
!  This is the Windows version
!
 !   use dfport, only : system
    use ifport, only : system
    implicit none
    character :: location*100, watch_program*50, command*120, line*240, tmp_file*240, &
      command2*120, programs(10)*20
    integer :: maxprog, itim=7, io_stat, i, njobs, j, counter, nprogs, k
    logical :: windows
    external  getarg
    integer, external :: iargc
    i = iargc()
    if (i >= 3) then
      call getarg(1,tmp_file)
      call getarg(2,watch_program)
      call getarg(3,line)
      j = 1
      do nprogs = 1, 10
        k = index(watch_program(j:), " ")
        if (k == 1) exit
        k = k + j 
        programs(nprogs) = watch_program(j:k - 1)
        j = k
      end do
      nprogs = nprogs - 1
      maxprog = ichar(line(1:1)) - ichar("0")
      if (line(2:2) /= " ") maxprog = maxprog*10 +ichar(line(2:2)) - ichar("0")
    else
      write(*,'(a)') "Wait.exe is a program to control when a job is allowed to start" 
      write(*,'(a)') "It uses three arguments:"
      write(*,'(a)') "The subdirectory where a temporary file can be created and destroyed by Wait.exe"
      write(*,'(a)') "The name of the type of executable that is to be monitored"
      write(*,'(a)') "The maximum number of executables"
      write(*,'(a)') " "
      write(*,'(a)') "For example, on Windows platforms:"
      write(*,'(a)') " Wait.exe ""M:/my_mopac_jobs"" MOPAC 7"
      write(*,'(a)') " or, on Mac and Linux platforms"
      write(*,'(a)') "Wait.exe ""/Users/myname/"" MOPAC 7"
      write(*,'(a)') " "
      write(*,'(a)') "The type of process being monitored is a MOPAC job"
      write(*,'(a)') "and the maximum number of MOPAC jobs allowed is 7"
      write(*,'(a)') "When the number of MOPAC jobs drops below 7, Wait.exe"
      write(*,'(a)') "stops, and the next operation - a new MOPAC job - can start"
      write(*,'(a)') " "
      write(*,'(a)') "The name of the executable is case-sensitive, "
      write(*,'(a)') "but can be truncated, so using MOP for MOPAC2009 would be valid"
      call sleepqq(30000)
      stop
    end if
    i = len_trim(tmp_file)
    if (tmp_file(i:i) /= "/" .and. tmp_file(i:i) /= "\") tmp_file(i + 1:i + 1) = "/"
    location = trim(tmp_file)//"jobs_running_0000000.lst"
    windows = (tmp_file(2:2) == ":") 
    counter = 0
    do
      counter = counter + 1
      write(location,'(a,i6.6,a)')trim(tmp_file)//"jobs_running",counter,".lst"
      if (windows) then
!
!  Windows-specific code
!
        command = 'tasklist  > '//trim(location)
        command2 = 'echo END >> '//trim(location)
      else
!
!  Mac and Linux-specific code
!
        command="ps -ax > """//trim(location)//'"'
        command2="echo END >> """//trim(location)//'"'
      end if
      do
        i = SYSTEM(trim(command))
        if (i == 0) exit
        call sleepqq(1000)
      end do
      if (command2 /= " ") then
        do
          i = SYSTEM(trim(command2))
          if (i == 0) exit
          call sleepqq(1000)
        end do
      end if
      do
        open(unit=itim,file=trim(location), err = 99, iostat = i)
        if (i == 0) exit
        call sleepqq(1000)
      end do
      njobs = 0
      j = 0
  !    write(*,*)"'"//trim(programs(1))//"'"
 !     write(*,*)programs(2)
      do
        read(itim,"(a)", err = 99, iostat = io_stat) line
 !       write(*,*) '"'//trim(line)//'"'
        if (io_stat /= 0) exit
        do i = 1, nprogs
          if (index(line,trim(programs(i))) /= 0 .and. index(line," "//trim(programs(i))) == 0) njobs = njobs + 1
        end do
 !       write(*,*) njobs
        j = j + 1
      end do
      if (j > 1 .and. njobs < maxprog .and. index(line,"END") > 0) then
        close(itim, dispose="delete", iostat=io_stat)
        exit
      end if
!       write(*,*) njobs, j, trim(line)
 99   call sleepqq(10000)
      do 
        close(itim, dispose="delete", err = 98, iostat = io_stat)
        if (io_stat == 0) exit
        call sleepqq(1000)
      end do
 98   if (j > 1 .and. njobs < maxprog .and. index(line,"END") > 0) exit
    end do
  end program Gatekeeper
