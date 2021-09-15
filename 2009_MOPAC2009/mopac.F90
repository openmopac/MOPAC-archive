program mopac
  use chanel_C, only : iw0
  use molkst_C, only : ijulian, program_name, gui, line, verson, site_no
  use dfport
  implicit none
  character  :: julian*8
  integer :: date_of_creation
  ! character (len=8), external :: jdate   ! Use this line if a Linux or Mac version, and comment out the line "use dfport"
  program_name = "Standalone MOPAC "
  call getdatestamp(line, verson)
  gui = .false.
  iw0 = -1
!  iw0 = 0
  julian = jdate()
!
!  Set ijulian to the date of creation of the executable
!
  date_of_creation    =   (ichar(verson(2:2)) - ichar('0'))*365 + &
                          (ichar(verson(4:4)) - ichar('0'))*100 + &
                          (ichar(verson(5:5)) - ichar('0'))*10 + &
                          (ichar(verson(6:6)) - ichar('0'))
 
!
  ijulian = - ((ichar(julian(2:2)) - ichar('0'))*365 + &
               (ichar(julian(3:3)) - ichar('0'))*100 + &
               (ichar(julian(4:4)) - ichar('0'))*10 + &
               (ichar(julian(5:5)) - ichar('0'))) 

! 
!
! The call to "password" checks that the password is valid.  If it's not valid, the run will be stopped.
! if it is valid, ijulian will be incremented to, e.g. ijulian = 365.
! To by-pass "password" replace the following line with: "ijulian = 10000.  
!
! The password can be disabled for time-limited versions. 
!
   site_no = 999  
    ijulian = date_of_creation + ijulian      ! ijulian is now set to the creation date of the executable
                                   !            minus the current date
    ijulian = ijulian + 30         ! Number of days before this version will stop working
    verson(6:6) = "W"
  call run_mopac
end program mopac
  
