  Program password
    use dfport, only : jdate
    implicit none
    character (len=8) :: julian
    character (len=120) :: line, location, sites(30000) !  Be optimistic in number of sites
    character (len=9) :: months(12)
    integer :: nsites, i, ndays, j, msite=0, k, ijulian, ncopies = 0, loop, &
    ref_julian
    integer :: days_in_month(12), dmy(3)
    logical exists, commercial
!                      JanFebMarAprMayJunJulAugSepOctNovDec
    data days_in_month/31,28,31,30,31,30,31,31,30,31,30,31/
    data months/"January","February","March","April","May","June", &
    "July","August","September","October","November","December"/
    call GetDateStamp(sites(1), line)
!
!  Read in all sites already assigned
!
    location = "m:/Password/"
    inquire (file = trim(location)//"sites.txt", exist = exists)
    if (.not. exists) then
      location = "M:\Software\Password\"
      inquire (file = trim(location)//"sites.txt", exist = exists)
    end if      
    open(unit=7, file=trim(location)//"sites.txt")
    if (.not. exists) then       
      write(7,'(i5)') 1
      write(7,"(a)")" End of list of E-mail addresses"
      close(7)
      open(unit=7, file=trim(location)//"sites.txt")
    end if    
    read(7,*) nsites
    rewind (7)
    read(7,"(a120)", iostat = j) (sites(i), i = nsites, 1, -1)
    if (j /= 0) then
      write(*,*)" Read of 'sites.txt' errored after reading line",i
      stop
    end if
!
!  Generate new site
!
    nsites = nsites + 1
    open(unit=8, file=trim(location)//"new_site.txt")
    read(8,"(a)")sites(nsites)
    i = index(sites(nsites), ":") + 1
    if (i > 1) then
      do 
        if (sites(nsites)(i:i) /= " ") exit
        i = i + 1
      end do
      sites(nsites) = sites(nsites)(i:)
    end if
!
!  Check to see if the E-mail address is buried in a "<" ">" set.
!
    i = index(sites(nsites), "<")
    if (i > 0) sites(nsites) = sites(nsites)(i + 1 : index(sites(nsites), ">") - 1)
    line = sites(nsites)
    call upcase(line, len_trim(line))
    i = index(line,"COMMERCIAL")
    if (i /= 0) then
      commercial = .true.
      ncopies = 0
      j = i + 11
      do k = j, j + 10
        if (line(k:k) /= " ") then
          if(ichar(line(j:j)) <= ichar("9") .and. ichar(line(j:j)) >= ichar("0")) then
            do j = k, k+2            
              ncopies = ncopies*10 + ichar(line(j:j)) - ichar("0")
              if(line(j+1:j+1) == " ") exit
            end do
            exit
          end if
        end if
      end do    
      line(i:) = " "
    else
      commercial = .false.
    end if
    ncopies = max(ncopies,1)
    if (line(4:4) == " " .or. line(3:3) == " " .or. line(2:2) == " ") then
!
!   Special password for fixed number of weeks.
!
      ndays = ichar(sites(nsites)(1:1)) - ichar("0")
      if (sites(nsites)(2:2) /= " ") then
        ndays = ndays*10 + ichar(sites(nsites)(2:2)) - ichar("0")
        if (sites(nsites)(3:3) /= " ") then
          ndays = ndays*10 + ichar(sites(nsites)(3:3)) - ichar("0")
        end if
      end if
      ndays = ndays*7
    !  if (ndays > 366 .or. ndays < 0) then
      if (ndays < 0) then
        write(*,*) "A password valid for 31 days will be made"
        write(*,*) "If you want a specific number of weeks, "
        write(*,*) "supply that number in place of the E-mail address."
   !     pause
        ndays = 31
      end if      
    else      
      ndays = 366
    end if
!
!  Find out if the site already exists (only if duration is a year or more)
!
    if (ndays > 365) then
      j = len_trim(sites(nsites))
      msite = nsites
      do i = 1, nsites
        if (index(sites(i), sites(nsites)(:j)) /= 0) then
          if (index(sites(i), "999") == 0) then
            msite = i
            exit
          end if
        end if
      end do
    else
      nsites = nsites - 1
    end if   
    close(8)
    open(unit=8, file=trim(location)//"new_site.txt")
    write(8,"(a)")"E-mail address:"
    close(8)
   
    open(unit=5, file=trim(location)//"Password.txt")
    if (msite /= nsites .and. msite /= 0) then
      write(5,"(/,a,i5)")" License key already issued to site number:",msite
      write(5,"(/,a)")" "
      write(5,"(a,/)")" Licenses are now valid for one year from the creation of the executable."
      write(5,"(a)")" If your license is about to expire, please download a new executable from  "
      write(5,"(a)")" the web-site: http://openmopac.net/Download_MOPAC_Executable_Step2.html"


    else
       if (msite /= 0)write(5,"(/,a,i5)")" New site:",msite
    end if
   
    julian = jdate()
!
!  ref_julian is the number of days since the end of 2006
!  (1096 = number of days between the end of 2006 and the end of 2009)
!
    ref_julian = (ichar(julian(2:2)) - ichar('0'))*365 + &
                 (ichar(julian(3:3)) - ichar('0'))*100 + &
                 (ichar(julian(4:4)) - ichar('0'))*10  + &
                 (ichar(julian(5:5)) - ichar('0'))     + 1096
   
    if (ndays == 366 .and. commercial) ndays = 400
    if ( .not. commercial) ndays = ndays + 3651 
   
    if (commercial)  write(5,"(a,i5)")" Commercial license"
    if (msite == 0)  write(5,"(a,i5)")" Evaluation license"
    if (commercial)then
!
!  dmy will hold the day, month, and year that the license will expire on.
!
      dmy(3) = ichar(julian(2:2)) - ichar('0') + 10*(ichar(julian(1:1)) - ichar('0'))
      dmy(1) = (ichar(julian(3:3)) - ichar('0'))*100 + &
              (ichar(julian(4:4)) - ichar('0'))*10 + &
              (ichar(julian(5:5)) - ichar('0')) 
      dmy(1) = dmy(1) + 365*dmy(3) + ndays
      do j = 0, 40
        do i = 1,12
          dmy(1) = dmy(1) - days_in_month(i)
          if (dmy(1) < 1) exit
        end do      
        if (dmy(1) < 1) exit
      end do
      dmy(1) = dmy(1) + days_in_month(i)
      dmy(2) = i
      dmy(3) = j 
      if (ndays > 999) then
        if (ndays == 6993) then
          write(5,"(/5x,a,i5,a,i2,a,i2.2,a)")" This is a permanent license."
        else
          write(5,"(/5x,a,i5,a,i2,a,i2.2,a)")" This license is valid for", ndays, &
            " days, and expires on ",dmy(1),"-"//trim(months(dmy(2)))//"-20",dmy(3),"."
        end if
      else if(ndays > 99) then
        write(5,"(/5x,a,i5,a,i2,a,i2.2,a)")" This license is valid for", ndays, &
          " days, and expires on ",dmy(1),"-"//trim(months(dmy(2)))//"-20",dmy(3),"."
      else if(ndays > 99) then
        write(5,"(/5x,a,i4,a,i2,a,i2.2,a)")" This license is valid for", ndays, &
          " days, and expires on ",dmy(1),"-"//trim(months(dmy(2)))//"-20",dmy(3),"."
      else
        write(5,"(/5x,a,i3,a,i2,a,i2.2,a)")" This license is valid for", ndays, &
        " days, and expires on ",dmy(1),"-"//trim(months(dmy(2)))//"-20",dmy(3),"."
      end if 
    end if   
    write(5,"(/)")
    do loop = 1, ncopies
      if (msite == nsites) then
  !
  !  Write out all sites, including this one
  !
       close(7)
       open(unit=7, file=trim(location)//"sites.txt")
  !
  !  Write out the new site
  !
       write(7,"(i5,i6,3x,a)")nsites, ref_julian, sites(30000)(3:5)//" "//trim(sites(nsites))
  !
  !  Write out all the old sites
  !
       write(7,"(i5,a)")(i, sites(i)(6:len_trim(sites(i))), i = nsites - 1, 1, -1)
     end if
 !    if (msite /= nsites .and. msite /= 0) then
 !     write(5,"(/,10(5x,a,/))")"A license has already been issued to you", &
 !       "To install a new license, first find and delete the file ""password for MOPAC2012"" ", &
 !       "Then make up a data-set as before (with the name equal to the license key)", &
 !       "Open the data-set using MOPAC2012, and when you see the black screen with white text,", &
 !       "press (return) three times, then type "" yes"" (with the space), followed by another (return)", &
 !      "The job will end, and you should now have a new file called ""password for MOPAC2012"" ", &
 !       "MOPAC2012 should now be good for a year.", &
 !       " "
  !  end if
  !
  !   Generate passwords
  !     
      if ( .not. commercial) ndays = ndays - 365
      ijulian = ref_julian + ndays  
      if (msite == 0) msite = 4000 
      if (commercial) then
!
!  ijulian is the number of days since 31 December 1999 that the license will run for.
!  In other words, ijulian = today's date plus the number of days that are allowed.
!
        if (ncopies > 1) then
          write(5,"(20x,a,i3,a1,5x,i8,'a',i8)") "    License key", loop,":", &
          ijulian*1511,(msite + 10000)*1523
        else
          write(5,"(20x,a,i8,'a',i8)") "    License key:",ijulian*1511,(msite + 10000)*1523
        end if
      else
        write(5,"(20x,a,i8,'a',i8)") "    License key:",ijulian*1523,(msite + 10000)*1511
      end if
      sites(nsites + 1) = sites(nsites)
      write(sites(nsites),"(i6,i6,3x,a)")nsites, ref_julian, &
      sites(30000)(3:5)//" "//trim(sites(nsites))
      msite = msite + 1
      nsites = nsites + 1
    end do
    write(5,"(/)")
    write(5,"(a)")"  To install the license key for MOPAC2012, follow the instructions"
    write(5,"(a)")"  in the file ""installation instructions.txt"" "
    write(5,"(a)")
    write(5,"(a)")"  (""installation instructions.txt"" is in the zip file you downloaded from OpenMOPAC.net)"
  end
