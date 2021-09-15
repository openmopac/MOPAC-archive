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

    m = SetThreadPriority(ThreadHandle, THREAD_PRIORITY_ABOVE_NORMAL )

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
  use molkst_C, only: jobnam
  use DFLIB
  use PeekAppModule
  use dfport
  integer i, j, k, loop
  character :: ch
!
!  After spending much time in dll hell, the cause of an intermittent bug was found.
!
!  About one time in three, this program failed to start, giving an obscure error
!  message.  The problem was painfully, believe me, painfully, traced to a dll in
!  the OCR software "Caere".  Deleting "Caere" corrected the problem. This problem
!  is likely to occur again!  
!
   call sleepqq(0)
  !  call wininit()
!
!  Test to see if data sets are passed as arguments, if not then read in name
!  supplied by user
!
    i = iargc()
    if (i == 0) then
      call PeekStart(0)
       write(0,*)
      write(0,*)"  MOPAC in WINDOWS: To run MOPAC 2005 within WINDOWS."      
      write(0,*)
      write(0,*)"  There are two ways to run MOPAC in WINDOWS. Probably the easier is 'Drag"
      write(0,*)"  and Drop' with the alternative of 'stand-alone' (using this window). "
      write(0,*)"  For information on using 'Drag and Drop' with MOPAC in WINDOWS, see "
      write(0,*)"  'Help'-> 'Instructions'."
      write(0,*)"'MOPAC in WINDOWS' Version 1.00, Copyright: J. J. P. Stewart, 14-Jan-2004."
      write(0,*)
      write(0,*)
      call setmsgbuf(len("Type in the name of the data set to be run, complete with path."), &
            &"Type in the name of the data set to be run, complete with path.",1)
      call setmsgbuf(len("For example: Z:\mydatasets\test.dat"), &
            &"For example: Z:\mydatasets\test.dat",1)
      !j = fputc(0,":")
      !j = fputc(0,">")
      !j = fputc(0," ")
      i = 0
      jobnam = " "
!
!  Write out the text: ":> |" with the "|" blinking every 0.3 seconds.  Wait for the
!  user to type something.
!
      do 
        call sleepqq(0)
        if(userpeekchar()) then
          ch = usergetchar()     ! get the character
          if (ch == char(8)) then           
            jobnam(i:i) = " "
             i = i - 1
          else
            if(ichar(ch) .eq. 3 .or. ichar(ch) .eq. 13 ) exit  ! stop for (control C) or CR.
            i = i + 1
            jobnam(i:i) = ch
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
            k = fputc(0,jobnam(j:j))
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
      call mopac()
    else
      do i = 1,50 
        call mopac()  ! the user has supplied data sets as arguments
      end do
  end if
end program MOPAC_win

subroutine wininit()
      use DFLIB
      use DFWIN
      implicit none

      type (windowconfig) wc
      type (QWINFO) qwi
      integer :: i
     
      logical :: STATUS
       open(unit=0, file='user', title = 'MOPAC for Windows')
       i = aboutboxqq('MOPAC for Windows\r\r &
       & Jimmy''s First Attempt - For David''s use only.\r\r &
       & Version 0.0001'C)
      i = GetLastError()
! Kill scroll bars and unwanted features
      i = GetWindowLong( GetHWndQQ(QWIN$FRAMEWINDOW), GWL_STYLE )
      i = ior( iand( i, not(WS_THICKFRAME) ), WS_BORDER )
      i = iand( i, not(WS_MAXIMIZEBOX) )
      i = SetWindowLong( GetHWndQQ(QWIN$FRAMEWINDOW), GWL_STYLE, i )!
	    
      i = GetWindowLong( GetHWndQQ(0), GWL_STYLE )
      i = ior(iand( i, not(WS_CAPTION.or.WS_SYSMENU.or.WS_THICKFRAME)), &
     &    WS_BORDER)
      i = SetWindowLong( GetHWndQQ(0), GWL_STYLE, i )  
!
!  Set up parent window
!   
      status = getwindowconfig(wc)
      qwi.x = 0   !  "x" - origin
      qwi.y = 0   !  "y" - origin
      qwi.w = 639 !   width (in pixels)
      qwi.h = 450 !   height (in pixels)
      qwi.type = QWIN$SET
      status = setwsizeqq(QWIN$FRAMEWINDOW, QWI)
!
!  Set up child window
! 
      wc.numxpixels  =  -1
      wc.numxpixels  =  -1
      wc.numtextcols =  80
      wc.numtextrows =  28
      wc.numcolors   =  -1
      wc.fontsize    =  524304
      wc.mode        =  -1
      
      wc.numypixels =  -1
      if( .not. setwindowconfig( wc ) ) then
        stop
      endif
            
      i = MoveWindow(GetHWndQQ(0), -1, -1, 0, 0, .TRUE.)
      i = setbkcolor(15)  ! White screen = 15
      i = settextcolor(0) ! Black text = 0
      call clearscreen($GCLEARSCREEN)

      status = UpdateWindow(GETHANDLEFRAMEQQ())
      return
end
logical function InitialSettings()
      use DFLIB
      logical :: L
      external :: Instructions, Shutdown
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
      L = appendmenuqq(4, $MENUENABLED,   'About'C,            WINABOUT)

      L = appendmenuqq(5, $MENUENABLED,   'Shutdown'C,         Shutdown)
      InitialSettings = L
      return
end
subroutine Instructions(checked)
      use DFLIB
      logical :: checked, lstatus
      integer :: i4
      lstatus = checked
      i4 = messageboxqq('Instructions for MOPAC in WINDOWS\r\r &
      & MOPAC in WINDOWS runs MOPAC 2002 in either batch or stand-alone modes\r\r &
      & Batch mode:\r &
      & Highlight the data sets, up to a maximum of 50, to be run.\r &
      & Drag them to the MOPAC in WINDOWS icon.\r\r &
      & The data sets will be run, and the results placed in the folder(s) &
      & where the data sets came from.\r\r &
      & Stand-alone mode:\r &
      & Double-click on the MOPAC in WINDOWS icon\r &
      & Type the name of the data set, complete with path information (Note: paste does NOT work!)\r\r &
      &  &
      & The data set will be run, and the results placed in the folder &
      & where the data set came from.\r\r &
      & More than one window can be run at the same time.\r &
      & Jobs can be paused (temporarily stopped) and resumed by use of the "state" button.\r &
      & The output in the MOPAC in WINDOWS window is identical to that in the CAChe WorkSpace window'C,'MOPAC Instructions'C, MB$OK)
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

!
!  Write message to window, to reassure users that the job is running.
!  - these messages are the ones that CAChe Workspace displays.
!
subroutine setmsgbuf (text_length,text,priority)
      character (len = *) :: text
      integer :: text_length, priority
      if (text(text_length:text_length) == char(0))then
        write(0,"(a)")text(1:text_length - 1)
        else
        write(0,"(a)")text(1:text_length)
        end if
      return
end
