module PeekAppModule
  use dflib
  use dfmt
  use dfport

  implicit none

  integer*4 bufget, bufput

  character*1 buffer(0:255)

  integer ThreadHandle

  type(RTL_CRITICAL_SECTION) DrawLock

  contains
  subroutine PeekStart(unit)

    integer unit

    integer  j, m

    integer, static :: saveunit

    saveunit = unit

    bufput = 0
    bufget = 0

    call InitializeCriticalSection( loc(DrawLock) )

    ThreadHandle = CreateThread( 0, 0, GetCharProc, %loc(saveunit), &
     CREATE_SUSPENDED, j )

!Priority is the same as QWIN control loop and 1 step above
!the QWIN application loop.  But this is no problem since
!the thread will be asleep most of the time waiting for 
!a character to be typed in.

    m = SetThreadPriority(ThreadHandle, THREAD_PRIORITY_BELOW_NORMAL )

    m = ResumeThread(ThreadHandle)

    return

  end subroutine PeekStart

  integer function GetCharProc( unit )
    use dflib
    integer j,k, unit
    character*1 ch
   
    GetCharProc = 1

    do while(.true.)
       j = focusqq(unit)   ! avoid GRAPH1 window 
       ch = getcharqq() ! this will block until a character is typed
    
       ! interlock buffer
       call EnterCriticalSection( loc(DrawLock) )
       !insert the character
       bufput = IAND(bufput + 1,16#000000FF)
       if(bufput .eq. bufget) then
          CALL BEEPQQ(4000,500)
          bufput = IAND(bufput - 1,16#000000FF)
       else
       buffer(bufput) = ch
       endif
       ! release the buffer
       call LeaveCriticalSection( loc(DrawLock) )
    end do
  end function GetCharProc

  logical*4 function userpeekchar()
    ! interlock buffer
    call EnterCriticalSection( loc(DrawLock) )
    !check if any characters in buffer
    if(bufget .eq. bufput) then
       userpeekchar = .false.
    else
       userpeekchar = .true.
    endif
    ! release the buffer
    call LeaveCriticalSection( loc(DrawLock) )

  end function userpeekchar

  character*1 function usergetchar()
    ! interlock buffer
    call EnterCriticalSection( loc(DrawLock) )
    !get the character
    if(bufget .eq. bufput) then
       CALL BEEPQQ(4000,500)
       usergetchar = ' '
       else
       bufget = IAND(bufget + 1,16#000000FF)
       usergetchar = buffer(bufget)
    endif
    ! release the buffer
    call LeaveCriticalSection( loc(DrawLock) )
  end function usergetchar
end module PeekAppModule


program MOPAC_win
  use molkst_C, only: jobnam, run, ijulian, program_name, gui, line, &
  verson, site_no
  use chanel_C, only : iw0, end_fn, iend, input_fn, ir
  use PeekAppModule, only : windowconfig, qwinfo, GETWINDOWCONFIG, &
   SETWINDOWCONFIG, SETWSIZEQQ, QWIN$SET, PeekStart
  use dfport, only : jdate
  use DFLIB
  implicit none
  integer i, j, k, loop, date_of_creation 
  character :: ch, julian*8, nowin*5
  logical :: exists, use_password = .true., res
  integer, external :: iargc
  TYPE (windowconfig) wc
  TYPE (qwinfo)  winfo
  julian = jdate()
  call getdatestamp(line, verson)
!
!  Set ijulian to the date of creation of the executable
!
  date_of_creation    =   (ichar(verson(2:2)) - ichar('0'))*365 + &
                          (ichar(verson(4:4)) - ichar('0'))*100 + &
                          (ichar(verson(5:5)) - ichar('0'))*10 + &
                          (ichar(verson(6:6)) - ichar('0')) 
 
!
!  Set ijulian to minus the number of days lapsed since 1-Jan-2009
!
  ijulian = - ((ichar(julian(2:2)) - ichar('0'))*365 + &
               (ichar(julian(3:3)) - ichar('0'))*100 + &
               (ichar(julian(4:4)) - ichar('0'))*10 + &
               (ichar(julian(5:5)) - ichar('0'))) 
  i = iargc()
  if (i == 2) then
    call getarg (2, nowin)
    i = len_trim(nowin)
    if (i == 5) then
      call upcase(nowin, 5)
      if (nowin == "NOWIN") then
        program_name = "Standalone MOPAC "
        gui = .false.
        iw0 = -1
        call password(date_of_creation)
        call run_mopac
        i = SETEXITQQ(QWIN$EXITNOPERSIST)
        stop
      end if
    end if
  end if
  


!
!  After spending much time in dll hell, the cause of an intermittent bug was found.
!
!  About one time in three, this program failed to start, giving an obscure error
!  message.  The problem was painfully, believe me, painfully, traced to a dll in
!  the OCR software "Caere".  Deleting "Caere" corrected the problem. This problem
!  is likely to occur again!  
!
   program_name = "MOPAC for Windows"
   gui = .false.
   iw0 = 0 
   
!
!  Test to see if data sets are passed as arguments, if not then read in name
!  supplied by user
!
    i = iargc()
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                               +
!   uncomment the following line to bypass the password control +
!    use_password = .false.                                     +
!                                                               +
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      call PeekStart(0)
      CALL SLEEPQQ(100)
      i = GETWINDOWCONFIG(wc)
      call getarg (1, jobnam)     
      wc%numxpixels = 90
      wc%numypixels = 200
      wc%numtextcols = 5
      wc%numtextrows = 10
      wc%numcolors = 1
      wc%fontsize = QWIN$EXTENDFONT
      if (jobnam /= " ") then
        wc%title= jobnam(1:len_trim(jobnam))//" "C ! A complicated way of making a C string.
      else
        wc%title= "MOPAC for Windows"C
      end if
      wc%bitsperpixel = 8
      wc%numvideopages = 0
      wc%mode = 0
      wc%adapter = 0
      wc%environment = 0
      wc%extendfontname = " "
      wc%extendfontsize = 1769492
      wc%extendfontattributes = -2147483648 
      res = SETWINDOWCONFIG(wc)
    !  write(55,*)wc
      if (.not. res) res = SETWINDOWCONFIG(wc)
      winfo%type = QWIN$SET
      winfo%x = 0
      winfo%y = 0
      winfo%w = 120
      winfo%h = 54
      i =   SETWSIZEQQ(0, winfo)
      if (use_password) then
        call password(date_of_creation)
      else
        verson(6:6) = "W"
        site_no = 999        
      end if
      i = iargc()
      if (i == 0) then
      write(0,*)
      if (ijulian < 1000) then
        write(0,'(a,i3,a)')"  MOPAC for WINDOWS (This version has ",ijulian," days left)" 
      else
        write(0,'(a,i3,a)')"  MOPAC for WINDOWS" 
      end if
      write(0,*)
      write(0,*)"  To run MOPAC2012 within WINDOWS:"      
      write(0,*)
      write(0,*)"  There are three ways to run MOPAC for WINDOWS. Probably the easiest is"
      write(0,*)"  'open with', and use 'MOPAC for Windows', alternatively 'Drag"
      write(0,*)"  and Drop.' 'Stand-alone' (using this window) is the slowest. "
      write(0,*)"  For information on using 'Drag and Drop' with MOPAC for WINDOWS, see "
      write(0,*)"  'Help'-> 'Instructions'."
      write(0,*)
      write(0,*)
      write(0,*)"Type in the name of the data set to be run, complete with path."
      write(0,*)"For example: Z:\mydatasets\test.dat"
      i = 0 
      jobnam = " "
      call get_line(jobnam)
      run = 2
      call run_mopac
    else      
      i = iargc()
      do run = 1,i
        call run_mopac  ! the user has supplied data sets as arguments
      end do
  end if
!
!  Delete window on completion of run.
!
  i = SETEXITQQ(QWIN$EXITNOPERSIST)
end program MOPAC_win


logical function InitialSettings()
      use DFLIB
      logical :: L
      external :: Instructions, Shutdown, About
      TYPE (qwinfo) qwi
 ! Set window frame size.
     qwi%x = 0
     qwi%y = 0
     qwi%w = 990
     qwi%h = 1000
     qwi%type = QWIN$SET
     i = SetWSizeQQ( QWIN$FRAMEWINDOW, qwi )

      L = appendmenuqq(1, $MENUENABLED,   'File'C,                  NUL)
      L = appendmenuqq(1, $MENUENABLED,   'Save as BitMap'C,    WINSAVE)
      L = appendmenuqq(1, $MENUENABLED,   'Print'C,            WINPRINT)
      L = appendmenuqq(1, $MENUSEPARATOR, 'sep'C,                   NUL)
      L = appendmenuqq(1, $MENUENABLED,   'Exit'C,              WINEXIT)

      L = appendmenuqq(2, $MENUENABLED,   'Edit'C,                  NUL)
      L = appendmenuqq(2, $MENUENABLED,   'Select All'C,   WINSELECTALL)
      L = appendmenuqq(2, $MENUENABLED,   'Copy'C,              WINCOPY)

      L = appendmenuqq(3, $MENUENABLED,   'State'C,                 NUL)
      L = appendmenuqq(3, $MENUENABLED,   'Pause'C,            WINSTATE)

      L = appendmenuqq(4, $MENUENABLED,   'Help'C,                  NUL)
      L = appendmenuqq(4, $MENUENABLED,   'Instructions'C, Instructions)
      L = appendmenuqq(4, $MENUENABLED,   'About'C,               About)

      L = appendmenuqq(5, $MENUENABLED,   'Shutdown'C,         Shutdown)
      InitialSettings = L
      return
end
subroutine Instructions(checked)
      use DFLIB
      logical :: checked, lstatus
      integer :: i4
      lstatus = checked
      i4 = messageboxqq('                    Instructions for running MOPAC for WINDOWS in screen modes\r\r &
      & MOPAC for WINDOWS runs MOPAC2012 in screen and command-prompt modes.\r &
      & These instructions apply to the screen mode only.\r\r &
      & "Drag and Drop" mode:\r &
      & Locate the data set to be run, and drag it to the MOPAC for WINDOWS icon.\r\r &
      & The data set will be run, and the results placed in the folder &
      & where the data set came from.\r\r &
      & "Open with" mode:\r &
      & Within Windows Explorer, locate the file to be run\r &
      & Right-click the file, and use "Open with" -> "MOPAC2012.exe"\r\r &
      & If, at that point,  you can''t see "MOPAC2012.exe" then:\r &
      & Use "Choose program" -> "Browse" then navigate to the file "MOPAC2012.exe".\r &
      & Highlight it, then click on "Open".\r &
      & 'C,'MOPAC Instructions'C, MB$OK)
      return
end
subroutine About(checked)
      use DFLIB
      logical :: checked, lstatus
      integer :: i4
      lstatus = checked
      i4 = messageboxqq('   MOPAC for WINDOWS\r\r &
      & Copyright: Stewart Computational Chemistry\r\r &
      & Version 2012 (2012)\r\r &
      &  'C,'MOPAC Instructions'C, MB$OK)
      return
end
subroutine Shutdown(checked)
      use DFLIB
      use chanel_C, only: iend, end_fn
      logical :: checked
      open (unit=iend, file=end_fn, status="UNKNOWN", err=1000)
      write(iend,*)"Shut"
      close(iend)
1000  return 
end

subroutine get_line(line)
use DFLIB
  use PeekAppModule
  use dfport
  character (len=120) :: line
  integer i, j, k, loop
  character :: ch, julian*8
  logical :: exists
  TYPE (windowconfig) wc
  TYPE (qwinfo)  winfo
!  Write out the text: ":> |" with the "|" blinking every 0.3 seconds.  Wait for the
!  user to type something.
!
      i = 0
      do 
        call sleepqq(0)
        if(userpeekchar()) then
          ch = usergetchar()     ! get the character
          if (ch == char(8)) then           
            line(i:i) = " "
             i = i - 1
          else
            if(ichar(ch) .eq. 3 .or. ichar(ch) .eq. 13 ) exit  ! stop for (control C) or CR.
            i = i + 1
            line(i:i) = ch
          end if
        endif
!
!  Write out the text: ":> <filename> |" with the "|" blinking every 0.3 seconds. 
!  Wait for the user to type something.
!
        do loop = 1,1000000
          if(userpeekchar()) exit
          call sleepqq(10)
          j = fputc(0,char(13))
          j = fputc(0,":")
          j = fputc(0,">")
          j = fputc(0," ")
          do j = 1, i
            k = fputc(0,line(j:j))
          end do
          if (mod(loop/30,2) == 0) then
            k = fputc(0," ")
          else
            k = fputc(0,"|")
          end if
        end do
!
!  Write out a blank line to clear any characters already printed.
!
          j = fputc(0,char(13))
          do j = 1, i + 3
              k = fputc(0," ")
            end do
      end do
!
! Tidy up input data line (do a carriage return and write out spaces.)
!
      j = fputc(0,char(13))
      do j = 1, i + 3
        k = fputc(0," ")
      end do
end subroutine get_line
