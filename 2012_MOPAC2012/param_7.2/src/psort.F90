subroutine psort (refers, mode, nmols, reftxt, allref, ifile, l, lp)
    use param_global_C, only : source, maxrfs
    implicit none
    logical, intent (in) :: lp
    integer, intent (in) :: ifile, mode, nmols
    integer, intent (out) :: l
    character (len=5), dimension (nmols), intent (out) :: reftxt
    character (len=8), dimension (nmols), intent (in) :: refers
    character (len=13), dimension (maxrfs), intent (inout) :: allref
    character (len=90) :: line
    character (len=300) :: iw
    integer :: i, ichara, icharb, ii, j, k, ll, m, mm
    logical :: first = .true., exists
    intrinsic Char, Ichar, Index
!----------------------------------------------------------------------
    if (mode /= 1) then
      l = 0
      ichara = Ichar ("a") - 1
      icharb = Ichar ("A") - 1
      do i = 1, nmols
        if (refers(i) == " ") then
          reftxt (i) = "  "
        else
        !#         WRITE(8,'('' <'',A,''>'')')REFERS(I)
          do j = 1, l
            if (allref(j) (6:13) == refers(i)) go to 1000
          end do
          l = l + 1
          j = l
          allref(j) (6:13) = refers(i)
1000      k = (j-1) / 26
          ll = ichara + j - k * 26
          mm = icharb + j - k * 26
          if (k == 0) then
            reftxt (i) = "    " // Char (ll)
          else if (k == 1) then
            reftxt (i) = "   " // Char (ll) // Char (ll)
          else if (k == 2) then
            reftxt (i) = "  " // Char (ll) // Char (ll) // Char (ll)
          else if (k == 3) then
            reftxt (i) = " " // Char (ll) // Char (ll) // Char (ll) // Char (ll)
          else if (k == 4) then
            reftxt(i) = Char(ll) // Char (ll) // Char(ll) // Char(ll) // Char(ll)
          else if (k == 5) then
            reftxt (i) = "    " // Char (mm)
          else if (k == 6) then
            reftxt (i) = "   " // Char (mm) // Char (mm)
          else if (k == 7) then
            reftxt (i) = "  " // Char (mm) // Char (mm) // Char (mm)
          else if (k == 8) then
            reftxt (i) = " " // Char (mm) // Char (mm) // Char (mm)
          else if (k == 9) then
            reftxt(i) = Char(mm) // Char(mm) // Char(mm) // Char(mm) // Char(mm)
          end if
          allref(j) (1:5) = reftxt(i)
        end if
      end do
      if ( .not. lp) return
    end if
    inquire (file="S:/utility/REFERENCES", exist= exists)
    if (exists) then
      open (14, status="UNKNOWN", file="S:/utility/REFERENCES", blank="ZERO")
    else
      inquire (file="~/M/utility/REFERENCES", exist= exists)
      if (exists) then
        open (14, status="UNKNOWN", file="~/M/utility/REFERENCES", blank="ZERO")
      else
        open (14, status="UNKNOWN", file="REFERENCES", blank="ZERO")
      end if
    end if
    rewind (14)
    m = l
    do i = 1, l
      source (3, i) = "      *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+* +&
     &* "
      source (2, i) = "      *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+* +&
     &* "
      source (1, i) = " " // allref (i) (5:12) // " NO REFERENCE *+*+*+*+" // &
     & "*+*+*+*+*+*+*+*+*+*"
    end do
    do ii = 1, 10000
      read (14, "(A)", end=1200, err=1200) line
!
! Having read in one reference, is that reference used by this run?
! Check mnemonics only
!
      do j = 1, l
        i = Index(line, " "//allref(j) (6:13))
        if (i /= 0) go to 1100
      end do
!
! Have references become messed up?  References mnemonics should have less than 10 characters
!
      if (line(11:) /= " ") then
        if(first) then
          first = .false.
          write(ifile,*)" Some mnemonics apear to be out-of sequence.  These include:"
        end if
        write(ifile,"(a)")line
      end if
!
! Reference is NOT used, therefore skip over next three lines
!
      read (14, "(A)", end=1200, err=1200) line, line, line
      cycle
1100  read (14, "(A)", end=1200, err=1200) source (1, j), source (2, j), &
     & source(3, j)
      m = m - 1
    end do
1200 close (14)
    do i = 1, l
      write (ifile,*)
      iw = source(1, i)
      if (source(2, i) /= " ") then
        iw(len_trim(iw) + 1:) = source(2, i)
        if (source(3, i) /= " ") then
          iw(len_trim(iw) + 1:) = source(3, i)
        end if
      end if

      write (ifile, "(A)")  allref (i) (1:5) // ":" // iw(:len_trim(iw))      
    end do
end subroutine psort
