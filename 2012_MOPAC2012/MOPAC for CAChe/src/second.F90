      real(kind(0.0d0)) function second (mode) 
!
!  If mode = 0:   Set t0 to the start of time (called once only by MOPAC)
!     mode = 1:   Get current CPU time - t0, and return without checking for <file>.end
!     mode = 2:   Get current CPU time - t0, and return after checking for <file>.end,
!                 if <file>.end exists, and contains anything, increase time by 10^8 seconds.
!
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE chanel_C, only : iw, iend, end_fn
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:48:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use geout_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mode 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(double) :: shut 
      logical :: setok, first, exists
      character :: x 
      real, save :: t0 = 0.0
      real :: t1
      integer :: i99
      logical, external :: shutdown

      save setok, first, shut 
!-----------------------------------------------
!******************************************************
!
!   SECOND, ON EXIT, CONTAINS THE NUMBER OF CPU SECONDS
!   SINCE THE START OF THE CALCULATION.
!
!******************************************************
      data setok/ .TRUE./  
      data shut/ 0.D0/  
      data first/ .TRUE./  
      if (first) then 
        first = .FALSE. 
        
!
!   CHECK TO SEE IF AN OLD '.end' FILE EXISTS.  IF IT DOES, DELETE IT.
!
        rewind iend 
        open(unit=iend, file=end_fn, status='UNKNOWN', position='asis', iostat = i99) 
        if (i99 /= 0) then 
          write(iw,"(a)")" File '"//end_fn(:len_trim(end_fn))//"' is unavailable for use"
          write(iw,"(a)")" Correct the error and re-submit"
          call to_screen(" File '"//end_fn(:len_trim(end_fn))//"' is unavailable for use")
          call to_screen(" Correct the error and re-submit")
          call mopend("File '"//end_fn(:len_trim(end_fn))//"' is unavailable for use")
          return
        end if
        read (iend, '(A)', end=20, err=20, iostat = i99) x 
!
!   FILE EXISTS.  DELETE IT.
!
        if (ichar(x) /= (-1)) close(iend, status='DELETE') 
      endif 
    if ( mode == 0 ) call cpu_time(t0)

!**********************************************************************
!
!   NOW TO SEE IF A FILE CALLED <FILENAME>.end EXISTS, IF IT DOES
!   THEN INCREMENT CPU TIME BY 10,000,000 SECONDS.
!
!***********************************************************************
      if (mode == 2) then 
     !   call dbreak()
        shut = 0.D0 
    !    inquire (file=end_fn, exist = exists)
     !   if (exists) then
    !      open(unit=iend, file=end_fn, status='UNKNOWN', position='asis', iostat=i99) 
      !    if (i99 /= 0) then 
      !      close(iend, err=99)   ! Do not stop job if shutdown is faulty
   ! 99      return
   !       end if
    !      read (iend, '(A)', end=10, err=10) x 
  !
  !          FILE EXISTS, THEREFORE INCREMENT TIME
  !
     if (shutdown(0)) then
          shut = 1.D7 
          if (setok) then 
            write (iw, '(3/10X,''****   JOB STOPPED BY OPERATOR   ****'')') 
            call geout (1) 
            setok = .FALSE. 
          end if 
     10   continue 
          close(iend, iostat = i99) 
          end if
        end if 
   20 continue 
      call cpu_time(t1)
      second = dble(t1 - t0) + shut 
      return  
      end function second 
