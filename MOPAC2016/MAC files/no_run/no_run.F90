  Program no_run
!
!  Check to see if a job with this name is already running.
!  If it is, then terminate the current job.
!
    use ifport, only : system, fdate
    implicit none
    integer :: io_stat, i, j, k, now, previous, &
      day, hour, minute, itim = 8
    integer :: dim(12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 0/)
    character :: location*600, job_name*240, command*600, line*240, time*24, job*240, job_ref*240
    character :: mon(12)*3 =(/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/)
    logical :: exists, debug = .false.
    external  getarg
    call getarg(1,job_name)
    job_ref = job_name
    do i = len_trim(job_name), 1, -1 
!  Remove any unprintable characters from the end of the file-name
      if (ichar(job_name(i:i)) > 39 .and. ichar(job_name(i:i)) < 126) exit
    end do
    job_name(i + 1:) = " "
  ! debug = .true.
    i = len_trim(job_name) - 3
    if (debug) write(*,'(a)')" job name: "//"'"//trim(job_name)//"'"
    if (job_name(i:i) /= ".") job_name = trim(job_name)//".mop"
    job = job_name(:len_trim(job_name) - 4)
  !  if (debug) write(*,'(6a)')" job: ",trim(job)
    write(location,'(a)')trim(job)//".lst"
    command="ls -ls """//trim(job)//".out"" > """//trim(location)//'"'
    call fdate(time) 
    if (debug) write(*,*) "current time:       ",trim(time)
    if (debug) write(*,'(2a)') " command:       ",trim(command)
!
!  Work out current time, in minutes.
!  Thu Mar 22 06:03:56 2012
!
    read(time,'(8x,i2,1x,i2,1x,i2)') day, hour, minute
    j = 0
    do i = 1, 12
      if (index(time, mon(i)) /= 0) exit
      j = j + dim(i)
    end do
    if (debug) write(*,'(3(a,i2,3x))')"Current month:", i, "day:", day, "hour:", hour, "minute:", minute
    if (debug) write(*,'(a,i3)')" day:",j
    now = 1440*(j + day) + 60*hour + minute 
    if (debug) write(*,*) "current minute:", now
    inquire(file = trim(job)//".out", exist = exists)
    if (debug) write(*,*) "exists?", exists
    if (exists) then
      do j = 1, 2
        i = SYSTEM(trim(command))
        if (i == 0) exit
        call sleepqq(1000)
      end do
      do j = 1, 2
        open(unit=itim,file=trim(location), err = 97, iostat = i)
        if (i == 0) exit
 97     call sleepqq(1000)
      end do
      do j = 1, 2
        read(itim,"(a)", err = 99, iostat = io_stat) line
        if (debug) write(*,'(a)') ' Line read in: "'//trim(line)//'"'
        if (io_stat == 0) exit
 99     call sleepqq(10000)
      end do
!
!  Work out time of previous run of this job
!  "8 -rw-rw-rw-  1 jstewart  staff  3 Mar 22 05:42 test.out"
!
      j = 0
      do i = 1, 12
        k = index(line, mon(i))
        if (k /= 0) exit
        j = j + dim(i)
      end do
      time = line(k + 4:)
      read(time,'(i2,1x,i2,1x,i2)') day, hour, minute
      if (debug) write(*,'(3(a,i2,3x))')"File month:", i, "day:", day, "hour:", hour, "minute:", minute
      if (debug) write(*,'(a,i3)')" day:",j
      previous = 1440*(j + day) + 60*hour + minute 
      if (debug) write(*,*)"Time now, time in output file:", now, previous
      do 
        close(itim, STATUS = "delete", err = 98, iostat = io_stat)
        if (io_stat == 0) exit
 98     call sleepqq(1000)
      end do
    else
      previous = 0
    end if
!
!  If an output file of the same name was touched (written to) within
!  30 minutes, kill the current job
!
    if (now - previous > 30) then
!
!  okay
!
      inquire(file = trim(job_ref)//".okay", exist = exists)
      if (.not. exists) then
        if (debug) write(*,*)"Writing <okay> file"
      else
        if (debug) write(*,*)"<okay> file already exists!"
      end if
      if ( .not. exists) open(unit = 9, file = trim(job_ref)//".okay", iostat = io_stat)
!
!  'touch' the out file to prevent other runs from selecting this job
! 
      command="touch """//trim(job)//".out"""
      i = SYSTEM(trim(command))

    else
!
!  kill
!
      inquire(file = trim(job_name)//".okay", exist = exists)
      if (exists) then
        open(unit = 9, file = trim(job_name)//".okay")
        close(unit = 9, STATUS = "delete")
      end if
   end if
end program no_run

