subroutine password(date_of_creation)
  use molkst_C, only: ijulian, line, academic, site_no, &
  verson
  use reada_I
  implicit none
  integer :: date_of_creation
  integer :: i, j, pass, p_new = 0
  character(len=80) :: mopac_pw   
  integer, parameter :: n_ac = 54, n_co = 77
  character (len=90):: academic_text(n_ac), commercial_text(n_co)
  integer, external :: getenvqq

  logical exists, new, site_ok
  integer, external :: iargc ! , from_MAC_address
  data academic_text &
  
  /"Academic End User Software License Agreement", &
  "Important:  ", &
  "This License Agreement (""Agreement"") is a legal agreement between you, ", &
  "the end user (either an individual or an entity), and Stewart Computational ", &
  "Chemistry (""SCC"").  By typing the word ""Yes"" below, you signify that you ", &
  "have read the SCC License Agreement and accept its terms.  If you do not agree ", & 
  "to the Agreement's terms, please delete all copies of this Software.", &
  "Grant of License. ", & 
  "SCC grants, and you hereby accept, a non-exclusive license to install and use", &
  "the enclosed software product (""Software"") in accordance with the", &
  "terms of this Agreement.  This licensed copy of the Software may only be used ", &
  "at a single site. You may not: (a) electronically transfer the Software from", &
  "one site to another, (b) distribute copies of the Software to other sites, or ", &
  "(c) modify or translate the Software without the prior written consent of SCC.  ", &
  "The Software may be placed on a file or disk server connected to a network. ", &
  "You may make only those copies of the Software which are necessary ", &
  "to install and use it as permitted by this Agreement, or are for purposes of ", &
  "backup and archival records; all copies shall bear SCC's copyright and ", &
  "proprietary notices.  You may not make copies of any accompanying materials ", &
  "without prior, written notice from SCC.", &
  " ", &
  "Ownership.  ", &
  "The Software is and at all times shall remain the sole property of SCC or ", &
  "its Licensors.  This ownership is protected by the copyright laws of the ", &
  "United States and by international treaty provisions. You may not modify, ", &
  "decompile, reverse engineer, or disassemble the Software.", &
  " ", &
  "Assignment Restrictions.  ", &
  "You shall not rent, lease, or otherwise sublet the Software or any part thereof.  ", &
  " ", &
  "No Warranty.", &
  "SCC offers no warranties whatsoever.", &
  " ", &
  "SCC's Liability.  ", &
  "In no event shall SCC or its agents be liable for any indirect, special, or consequential", &
  "damages, such as, but not limited to, loss of anticipated profits or other economic loss", &
  "in connection with or arising out of the use of the software by you or the ", &
  "services provided for in this Agreement, even if SCC has been advised of the ", &
  "possibility of such damages.  ", &
  " ", &
  "No Other Warranties.  ", &
  "SCC and its agents disclaims other implied warranties, including, but not limited to, ", &
  "implied warranties of merchantability or fitness for any purpose, and implied warranties ", &
  "arising by usage of trade, course of dealing, or course of performance.  ", &
  " ", &
  "Governing Law.  ", &
  "This Agreement shall be construed according to the Laws of the State of Colorado.", &
  " ", &
  "----------------------------------------------------------------------------------", &
  "Stewart Computational Chemistry", &
  "15210 Paddington Circle, Colorado Springs, Colorado  80921-2512, USA.", &
  "URL: http://openmopac.net/                          mail: MrMOPAC@OpenMOPAC.net", &
  "----------------------------------------------------------------------------------", &
  "++++" /
  data commercial_text /&
'Commercial End User Software License Agreement ', &
' ', &
'Important:  ', &
'This License Agreement ("Agreement") is a legal agreement between you, the end ', &
'user (either an individual or an entity), and Stewart Computational Chemistry ("SCC").  ', &
'By typing "Yes" below, you signify that you have read the SCC License Agreement ', &
'and accept its terms.  If you do not agree to the Agreement''s terms, please ', &
'delete all copies of this Software.', &
' ', &
'Grant of License.  ', &
'SCC grants, and you hereby accept, a non-exclusive license to install and use ', &
'one copy of the enclosed software product ("Software") in accordance with the ', &
'terms of this Agreement.  This licensed copy of the Software may only be used ', &
'on a single computer. You may not: (a) electronically transfer the Software from ', &
'one computer to another, (b) distribute copies of the Software to others, or ', &
'(c) modify or translate the Software without the prior written consent of SCC.  ', &
'The Software may be placed on a file or disk server connected to a network, ', &
'provided that a license has been purchased or legally acquired and registered ', &
'for every computer with access to that server or some license management software ', &
'is present and in use to allow only licensed users to access the Software.  ', &
'You may make only those copies of the Software which are necessary to install ', &
'and use it as permitted by this Agreement, or are for purposes of backup and ', &
'archival records; all copies shall bear SCC''s copyright and proprietary notices.  ', &
'You may not make copies of any accompanying materials without prior, written ', &
'notice from SCC.', &
' ', &
'Ownership.  ', &
'The Software is and at all times shall remain the sole property of SCC or its ', &
'Licensors.  This ownership is protected by the copyright laws of the United States ', &
'and by international treaty provisions.  Upon expiration or termination of this ', &
'Agreement, you shall promptly delete all copies of the Software and accompanying ', &
'materials.  You may not modify, decompile, reverse engineer, or disassemble the ', &
'Software.', &
' ', &
'Assignment Restrictions.  ', &
'You shall not rent, lease, or otherwise sublet the Software or any part thereof.  ', &
'You may transfer on a permanent basis the rights granted under this license provided ', &
'you transfer this Agreement and all copies of the Software, including prior versions, ', &
'and all accompanying materials (included in the product download).  The recipient ', &
'must agree to the terms of this Agreement in full and register this transfer with SCC.', &
' ', &
'SCC Limited Warranty.', &
'SCC''s sole warranty with respect to the Software is that it shall be free of errors ', &
'in program logic or documentation attributable to SCC and of errors that prevent the ', &
'performance of the principal computing functions of the Software.  SCC warrants this ', &
'for a period of ninety (90) days from the date of receipt of the Software.', &
' ', &
'SCC''s Liability.  ', &
!123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
'In no event shall SCC or its agents be liable for any indirect, special, or consequential', &
'damages, such as, but not limited to, loss of anticipated profits or other economic loss', &
'in connection with or arising out of the use of the software by you or the services ', &
'provided for in this Agreement, even if SCC has been advised of the possibility of ', &
'such damages.  SCC''s entire liability and your exclusive remedy shall be, at SCC''s ', &
'discretion, to return the Software and proof of purchase to SCC for either (a) return ', &
'of any license fee, or (b) correction or replacement of Software that does not meet ', &
'the terms of this limited warranty.', &
' ', &
'No Other Warranties.  ', &
'SCC and its agents disclaims other implied warranties, including, but not limited to,  ', &
'implied warranties of merchantability or fitness for any purpose, and implied warranties ', &
'arising by usage of trade, course of dealing, or course of performance.  Some states ', &
'do not allow the limitation of the duration or liability of implied warranties, so ', &
'the above restrictions might not apply to you.', &
' ', &
'Governing Law.  ', &
'This Agreement shall be construed according to the Laws of the State of Colorado.', &
' ', &
'----------------------------------------------------------------------------------', &
'Stewart Computational Chemistry', &
'15210 Paddington Circle, Colorado Springs, Colorado  80921-2512, USA.', &
'URL: http://openmopac.net/                          Email: MrMOPAC@OpenMOPAC.net', &
'----------------------------------------------------------------------------------', &
"*******************************************************************************", &
"** Cite this work as: MOPAC2012, James J. P. Stewart, Stewart Computational  **", &
"** Chemistry, Colorado, USA      web: HTTP://OpenMOPAC.net                   **", &
"*******************************************************************************", &
'++++'/
  i = getenvqq("MOPAC_LICENSE",mopac_pw) 
  if (i /= 0) then
    i = len_trim(mopac_pw)
    if (mopac_pw(2:2) ==":") then  !  Is a Windows system
      if (mopac_pw(i:i) /= "\") then !  Add a terminal slash if it is not already present
        mopac_pw(i + 1:i + 1) ="\"
      end if
      verson(7:7) = "W"
      inquire (file = trim(mopac_pw), exist = exists)
      if ( .not. exists) then
        write(0,*)" Environmental variable ""MOPAC_LICENSE"" exists, but its value" 
        write(0,*)" does not represent a folder on this computer"
        write(0,*)" (An attempt was made to detect the folder '"//trim(mopac_pw)//"')"
        stop
      end if
      inquire (file = trim(mopac_pw)//"MOPAC2012.exe", exist = exists)
      if (exists) then
        mopac_pw= trim(mopac_pw)//"password for MOPAC2012"
      else
        write(0,*)" Environmental variable ""MOPAC_LICENSE"" exists, but its value"
        write(0,*)" does not represent a folder on this computer that contains MOPAC2012.exe"
        write(0,*)" (An attempt was made to detect the file '"//trim(mopac_pw)//"MOPAC2012.exe')"
        stop
      end if
    else if (index(mopac_pw, "/") /= 0) then ! It is a Linux system
      if (mopac_pw(i:i) /= "/") then !  Add a terminal slash if it is not already present
        mopac_pw(i + 1:i + 1) ="/"
      end if
      inquire (file = trim(mopac_pw)//"mopac2012.exe", exist = exists)
      if (exists) then
        verson(7:7) = "M"
      else
        inquire (file = trim(mopac_pw)//"MOPAC2012.exe", exist = exists)
        verson(7:7) = "L"
      end if
      if (exists) then
        mopac_pw= trim(mopac_pw)//"password_for_mopac2012"
      else
        write(0,*)" Environmental variable ""MOPAC_LICENSE"" exists, and the directory it points to exists,"
        write(0,*)" but the directory does not contain the MOPAC2012 executable"
        write(0,*)" (MOPAC_LICENSE points to directory '"//trim(mopac_pw)//"')"
        stop
      end if   
    end if
  else
!
!  Environmental variable not set.  Test for defaults
!
    inquire (file = "C:\", exist = exists)
    if (exists) then
!
! Platform is Windows
!  
      inquire (file = "C:\program files", exist = exists)
      if (exists) then
        inquire (file = "C:\program files\mopac", exist = exists)
        if ( .not. exists) then
          write(0,*) 
          write(0,*) '  The MOPAC2012.exe file MUST be put in folder C:\program files\mopac'
          write(0,*) '  unless the Environmental Variable MOPAC_LICENSE is set.'
          write(0,*) ' '
          write(0,*) '  If a different location is used, set the location in the Environmental Variable "MOPAC_LICENSE".'
          write(0,*) '  For example, the Variable: MOPAC_LICENSE could be given the Value: M:\Utility.'
          call web_message(0,"password_inaccessible.html")
          write(0,*) " Correct this fault before continuing"
          read(5,"(a)")line
          stop
        end if
        mopac_pw   = 'C:\program files\mopac\password for MOPAC2012'
      else
!
!  The Operating System is WINDOWS, but the folder "program files" does not exist.
!  So put the password in the root folder, "C:\".
!
        mopac_pw   = 'C:\password for MOPAC2012'
      end if
      verson(7:7) = "W"
    else
!
! Check for Linux or Macintosh.  This is a poor test: Macintosh platforms are case-insensitive
! and Linux platforms are case-sensitive.  This is used in deciding whether to use "W" or "M".
! The test is poor because there is no guarantee that the case sensitivity will always be the same.
! but since the only use of the case is in the output, this is not very important.
!
      inquire (file = "/opt/mopac/mopac2012.exe", exist = exists)
      if (exists) then
        verson(7:7) = "M"
        mopac_pw = "/opt/mopac/password_for_mopac2012"
      else
        inquire (file = "/opt/mopac/MOPAC2012.exe", exist = exists)
        verson(7:7) = "L"
        mopac_pw = "/opt/mopac/password_for_mopac2012"
      end if
      if (.not. exists) then
!
! Platform is neither Linux nor Macintosh
!
        write(0,*)' The MOPAC executable must be put into a directory named "/opt/mopac"'
        write(0,*)' (If it cannot be put there, use environmental variable MOPAC_LICENSE to re-define the directory)'
        call web_message(0,"trouble_shooting.html#default location")
        stop
      end if
    end if
  end if
  
  line = mopac_pw(:len_trim(mopac_pw) - 22)//"MOPAC-2012.exe"
  inquire (file = trim(line), exist = exists)
  if ( exists ) then
    academic = .true.
    ijulian = ijulian + 1337 !  Set license to expire a year later - cut down on requests.
    site_no = -1
    return
  end if
  j = 0
 98 continue
  j = j + 1
  inquire (file = trim(mopac_pw), exist = exists)
  if (.not. exists) then
!
! Password for MOPAC2012 does not exist, so check for Password for MOPAC2009
!
    i = index(mopac_pw,"MOPAC20") + index(mopac_pw,"mopac20")+ 6
    mopac_pw = mopac_pw(:i)//"09"
    inquire (file = trim(mopac_pw), exist = exists)
    if (exists) then
      p_new = 76
      open(p_new, file = mopac_pw(:i)//"12", form="unformatted")
    else
      mopac_pw = mopac_pw(:i)//"12"
    end if
  end if
  if (exists .and. j < 3) then
    open(unit=7, file=trim(mopac_pw), form="unformatted", iostat=i)
    if (i /= 0) then
      write(0,*) 'Password file "' //trim(mopac_pw) //'" exists, ','but is currently inaccessible'
      call web_message(0,"password_inaccessible.html")
      write(0,*) " Correct this fault before continuing"
      read(5,"(a)")line
      stop
    end if  
    read(7, iostat=i)pass, site_no
    if (i /= 0) then
      if (i == -1) then
        close (7, STATUS = "DELETE",iostat=i)        
        if (i /= 0) then
          write(line,'(2a)')" Unable to delete corrupt password file in '", trim(mopac_pw)//"'"
          write(0,'(//10x,a,/)')trim(line)
          write(0,'(10x,a)')" Press 'Enter' to finish"
          call mopend(trim(line))
          read(*,*)
          stop
        end if
        goto 98
      else
        write(line,'(2a)')" Unable to read password file in '", trim(mopac_pw)//"'"
      end if
      write(0,'(//10x,a,/)')trim(line)
      write(0,'(10x,a)')" (To correct this fault, delete the file '"//trim(mopac_pw)//"' and try again.)"
      call mopend(trim(line))
      stop
    end if     
    new = .false.
    if  (p_new /= 0) then
      write(p_new, iostat=i)pass, site_no
      close (p_new)
    end if
  else
!
!  "exists" is true only if a password exists in the appropriate place.
!   
    new = .true.    
    line = " "
!
!
!                    Read in the password
!
!
!
    i = iargc()
    if (i == 0) then
    !  i = from_MAC_address()
      write(line,'(2a)')" MOPAC is a semiempirical quantum chemistry program"
      write(0,'(//10x,a,/)')trim(line)
      call mopend(trim(line))
      write(0,'(10x,a)')" To install the MOPAC license, use the command"
      write(0,'(10x,a)')" 'MOPAC2012.exe <license-key>'"
      write(0,'(10x,a)')" e.g., 'MOPAC2012.exe 1234567a1234567'"
      call web_message(0,"running_MOPAC.html")
      write(0,'(10x,a)')" Press (return) to continue"
      read(5,*) 
      stop
    end if
    call getarg (1, line)
    do i = 1,60 
      if(line(1:1) == " ")then
        line = line(2:)
      else if (index(line,"\") /= 0) then
        j = index(line,"\")
        line = line(j + 1:)
      else
        exit
      end if
    end do
    pass = nint(reada(line,1)) 
    i = index(line,"a")
    if (i /= 0) then
      site_no = nint(reada(line, i))
      i = Mod(pass,1523)
      j = Mod(site_no,1511)
    end if
    site_ok = .false.
    if (site_no /= 15125110) then ! Not a previously-approved site
      if (i == 0 .and. pass > 0 .and. j == 0 .and. site_no > 0)then
!
!  This is the Academic license
!
        do i = 1, n_ac
          if (Index(academic_text(i),"++++") /= 0) exit
          write(0,"(a)")academic_text(i)(:len_trim(academic_text(i)))
          if (mod(i,52) == 0) then
            write(0,*)" Press (return) to continue"
            read(5,*) 
          end if
        end do
        site_ok = .true.
      end if
      i = Mod(pass,1511)
      j = Mod(site_no,1523)      
      if (i == 0 .and. pass > 0 .and. j == 0 .and. site_no > 0)then
!
!  This is the Commercial license
!
        ijulian = ijulian + pass/1523
        if (ijulian < -59) then
          write(0,"(///10x,a)")" This version of MOPAC is now out-of-date"
          close(7, status="delete")
          stop
        end if
        do i = 1, n_co
          if (Index(Commercial_text(i),"++++") /= 0) exit
          write(0,"(a)")Commercial_text(i)(:len_trim(Commercial_text(i)))
          if (mod(i,52) == 0) then
            write(0,*)" Press (return) to continue"
            read(5,*) 
         end if
        end do
        site_ok = .true.
      end if
      if ( .not. site_ok) then
        write(0,"(//,a)")" Before MOPAC2012 can be used, a password must be supplied"
        write(0,"(/,a)")" To get a password, send an E-mail request to MrMOPAC@OpenMOPAC.net"
        write(0,"(a)")" "
        write(0,"(a)")" To install the password, run MOPAC with the file name equal to the password"
        write(0,"(a)")" For example, if the password is 123456789, then run ""MOPAC2012.exe 123456789"""
        close(7, status="delete")
        write(0,"(//,a)") "Password invalid"
        open(unit=7, file=trim(mopac_pw), form="unformatted")
        close(7, status="delete")
        read(5,"(a)")line
        stop
      end if
      write(0,"(a,//,a)")"Scroll down to see the next part", &
        "Are these conditions are acceptable?"
      do
        write(0,"(a)")" Type a ""Yes"" or ""No"" on the following line and press (return) to continue"
        read(5,"(a)")line
        call upcase(line,len_trim(line))
        if (index(line,"ES") /= 0) then
          exit
        else if (index(line,"O") /= 0) then
          write(0,*)" Please send a message to MrMOPAC@OpenMOPAC.net giving details", &
         " why the conditions are unacceptable."
          close(7, status="delete")
          stop
        end if
      end do
    end if
    open(unit=7, file=trim(mopac_pw), form="unformatted", iostat=i)
    write(7, iostat=i)pass, site_no
    if (i /= 0) then
      write(0,'(a)')" Cannot write password to file '"//trim(mopac_pw)//"'"
      do i = 1,100
        write(0,'(a)')' '
      end do
      stop
    end if
    close(7, status="keep")
  end if
!
!   A valid password is now in the file mopac_pw
!        
!  On entry to password, ijulian is set to minus the number of days that have
!  passed since 1 January 2009
!
!  The password should be translate at the number of days since 1 January 2007 
!  plus the number of days the password is good for.
!  Academic sites get 366 days
!  Commercial evaluation sites get 31 days
!  All other commercial sites get 366 or more days
!
!
  open(unit=7, file=trim(mopac_pw), form="unformatted", iostat=i)
  if (i /= 0) then
    write(line,'(2a)')" Password file: '"//trim(mopac_pw)//"' exists, but it cannot be opened."
    write(0,'(//10x,a,//)')trim(line)
    call mopend(trim(line))
    stop
  end if
  read(7, iostat=i)pass, site_no
  if (i < -1 .or. i > 0) then
    write(line,'(2a)')" Password file: '"//trim(mopac_pw)//"' exists, but it cannot be read."
    write(0,'(//10x,a,//)')trim(line)
    call mopend(trim(line))
    stop
  end if
!
!  CHECK TO SEE IF THE LICENSE IS ACADEMIC
!
  i = Mod(pass,1523)
  j = Mod(site_no,1511)
  site_ok = .false.
  if (i == 0 .and. pass > 0 .and. j == 0 .and. site_no > 0)then
!
!  Prevent MOPAC running if other specific programs are present
!
! do i = 2009, 2015
!   write(line,'(a,i4,a)')"C:\Program files\Cambridgesoft\ChemOffice",i,"\Chem3D"
!   inquire (file=trim(line), exist = exists)
!   if (exists) exit
! end do
! if (i < 2016) then
!   write(0,"(/10x,a)")" MOPAC requires a commercial license key to work with Chem3D,"
!   write(0,"( 10x,a)")" and this key was not found. The free Academic license key "
!   write(0,"( 10x,a)")" for stand-alone MOPAC is not valid for Chem3D. "
!   write(0,"( 10x,a)")" The commercial license key is available at a discount to "
!   write(0,"( 10x,a)")" Academics from: http://cacheresearch.com/mopac.html"
!   write(0,"(/10x,a)")" Press press 'enter' to continue in Demo mode only (accepts  "
!   write(0,"( 10x,a)")" only molecules with 1-12, 50-60, or 110-120 atoms)."
!   read(5,*)
!   ijulian = ijulian + 10000 !  
!   site_no = -1
!   return
! end if
!
!  This is the Academic license.  Set the executable to expire
!  in 365 days time, or Julian date 75 in 2008, whichever is later.
!
!  ijulian = ijulian + date_of_creation + 365 !  Set license to expire a year after executable is created - cut down on requests.
    ijulian = ijulian + date_of_creation + 100 !  Set license to expire "soon"
    academic = .true.
    site_no = site_no/1511 - 10000
    if (ijulian < 0) then
      write(0,"(///,10(10x,a,/))") &
        " Your MOPAC executable has expired.", &
        " Please go to web-site: http://openmopac.net/Download_MOPAC_Executable_Step2.html to get a new version of MOPAC.", &
        " ", &
        " Press (enter) to continue."
      ijulian = 0
      read(5,*)
    end if
    site_ok = .true.
  end if
! j = from_MAC_address()
!
!  CHECK TO SEE IF THE LICENSE IS COMMERCIAL
!
  i = Mod(pass,1511)
  if (j /= i) then
    j = i
  end if
  j = Mod(site_no,1523)
  if (i == 0 .and. pass > 0 .and. j == 0 .and. site_no > 0)then
!
!  This is the commercial license
!
    ijulian = ijulian + date_of_creation + pass/1511 - 1000
    academic = .false.
    site_no = site_no/1523 - 10000
    if (ijulian < 0) then
      write(0,"(///,10(10x,a,/))") &
 " The license for this copy of MOPAC has expired", &
 " Please go to web-site: HTTP://OpenMOPAC.net for instructions on", &
 " renewing the license", &
 " ", &
 " Press (return) to continue."
      read(5,*)
    end if
    site_ok = .true.
    if (ijulian < -60) then
      write(0,"(a)")
      write(0,"(a)")" The time allowed by the password has expired."
      open(unit=7, file=trim(mopac_pw), form="unformatted")
      close(7, status="delete")
      stop
    end if      
  end if
  if (.not. site_ok) then
    write(0,"(//,a)")" Before MOPAC2012 can be used, a password must be supplied"
    write(0,"(/,a)")" To get a password, send an E-mail request to MrMOPAC@OpenMOPAC.net"
    write(0,"(a)")" "
    write(0,"(a)")" To install the password, run MOPAC with the file name equal to the password"
    write(0,"(a)")" For example, if the password is 123456789, then run ""MOPAC2012.exe 123456789"""
    write(0,"(//,a)") "Password invalid"
    open(unit=7, file=trim(mopac_pw), form="unformatted")
    close(7, status="delete")
    stop
  end if
  if (new) then
    write(0,"(//a,////)")"         Password for MOPAC2012 successfully installed. Enjoy!"
    stop
  end if 
end subroutine password
!function from_MAC_address()
!  use dfport, only : system
!  use molkst_C, only: verson
!  implicit none
!  integer :: from_MAC_address
!  character :: line*120, location*30, command*60
!  integer :: i, j, mac_no
!  location = "get_MAC_address.txt"
!
!  if (verson(7:7) == "W") then
!!
!!  Windows-specific code
!!
!    command = 'ipconfig /all  > '//trim(location)
!    j = SYSTEM(trim(command))
!    if (j /= 0) then
!      j = 0
!    end if
!    open(unit=8, file=trim(location), iostat = j)
!    if (j /= 0) then
!      j = 0
!    end if
!    do 
!      read(8,'(a)', iostat = j)line
!      if (j /= 0) then
!        j = 0
!      end if
!      if (index(line, "Physical Address") /= 0) exit
!    end do
!    close(8)
!    command = 'del /F /q '//trim(location)  
!    j = SYSTEM(trim(command))
!    if (j /= 0) then
!      j = 0
!    end if
!    line = line(index(line,":") + 2:)         
!    mac_no = 0
!    do i = 1, 6
!      mac_no = 10*mac_no + 100*ichar(line(i*3 - 2: i*3 - 2)) + 10*ichar(line(i*3 - 1:i*3 - 1))
!    end do
!    from_MAC_address = mod(mac_no,71) + 100*mod(mac_no,73) + 10000*mod(mac_no,79) + 1000000*mod(mac_no, 83)
!    return
!  end if
!        
!end function from_MAC_address
