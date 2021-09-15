subroutine getpdb ()
    use molkst_C, only: maxtxt, natoms, numat, na1, keywrd, line
    use common_arrays_C, only : txtatm, atmass, na, nb, nc, &
    labels, lopt, geo, coord
    use chanel_C, only: iw, ir
    use parameters_C, only: ams
    use reada_I
    implicit none
 !
 !***********************************************************************
 !
 !  GETPDB READS IN THE GEOMETRY, IN BROOKHAVEN PROTEIN DATA BASE FORMAT.
 !
 !  ON INPUT   IR  = CHANNEL NUMBER FOR READ, NORMALLY 5
 !
 ! ON OUTPUT LABELS = ATOMIC NUMBERS OF ALL ATOMS, INCLUDING DUMMIES.
 !           GEO    = CARTESIAN COORDINATES, IN ANGSTROMS
 !           LOPT   = INTEGER ARRAY FILLED WITH '1'S.
 !                    (PDB FILES DO NOT SUPPORT OPTIMIZATION FLAGS)
 !           NA     = INTEGER ARRAY FILLED WITH '0'S.
 !           NB     = INTEGER ARRAY FILLED WITH '0'S.
 !           NC     = INTEGER ARRAY FILLED WITH '0'S.
 !***********************************************************************
!
    integer, parameter :: maxel = 100
    integer, parameter :: defined_elements = 66 ! in elemnt array
    character, save :: comma, space
    character :: typea, typer
    character (len=3) :: ele
    character (len=80) :: chains = "CHAINS=("
    logical :: leadsp, lxyz, lchain = .true.
    integer :: i, icomma, ii, j, k, khar, label, nline, npdb, nvalue, ichain = 9
    integer :: new_elements
    double precision :: degree
    character :: commas(20), number
    character (len=2), dimension (maxel), save :: elemnt
    character (len=4), dimension (20) :: txtpdb
    integer, dimension (20) :: ntxt_loc
    integer, dimension (maxel) :: ielem
    intrinsic Abs, Ichar, Index, Nint
 !
 !.. Data Declarations ..
    data elemnt / "H ", "LI", "BE", "B ", "C ", "N ", "O ", "F ", "NA", "MG", &
   & "AL", "SI", "P ", "S ", "CL", "K ", "CA", "ZN", "GA", "GE", "AS", "SE", &
   & "BR", "CD", "IN", "SN", "SB", "TE", "I ", "HG", "TL", "PB", "BI", "X ", &
   & "L ", "FE", 64 * "  " /
    data ielem / 1, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 19, 20, &
   & 30, 31, 32, 33, 34, 35, 48, 49, 50, 51, 52, 53, 80, 81, 82, 83, 1, 0, 26, &
   & 64 * 0 /
    data commas / 20 * "," /
    data ntxt_loc / 20 * 2 /
    data comma, space, txtpdb / ",", 21 * " " /
 !
 ! ... Executable Statements ...
 !
    new_elements = 0 
    i = Index (keywrd, " PDB(")
    if (i /= 0) then
!
!  THE USER IS GOING TO DEFINE VARIOUS SYMBOLS AND ATOMIC NUMBERS
!
      i = i + 5
      j = Index (keywrd(i:241), ")") + i
      if (j /= i) then
        k = defined_elements +1
        new_elements = 1
        do
          elemnt(k) = keywrd(i:j)
          ii = Index (elemnt(k), ":")
          if (ii /= 0) then
            elemnt (k) (ii:2) = " "
          end if
          ii = Index (keywrd(i:j), ":") + i
          if (ii == i) exit
          ielem(k) = Nint (reada (keywrd, ii))
          i = Index (keywrd(ii:j), ",") + ii
          if (i /= ii) then
            k = k + 1
            new_elements = new_elements +1
          else
            go to 1000
          end if
        end do
      end if
      write (iw,*) " KEYWORD PDB IS INCORRECT"
      call mopend ("KEYWORD PDB IS INCORRECT")
    end if
1000 typea = " "
    i = Index (keywrd, " ALT_A=")
    if (i /= 0) then
      typea = keywrd(i+7:i+7)
    end if
    typer = "9"
    i = Index (keywrd, " ALT_R=")
    if (i /= 0) then
      typer = keywrd(i+7:i+7)
    end if
    maxtxt = 0
    natoms = 0
    numat = 0
    npdb = 0
    maxtxt = 16
    nline = 0
    outer_loop: do
!
      read (ir, "(A)", end=1020, err=1020) line
!
      nline = nline + 1
      if (natoms > 0 .and. line == " ") exit

      if( .not. ( line(1:4) == "ATOM" .or. line(1:6) == "HETATM" ) ) then
        if (line(1:3) == "TER") lchain = .true.
        cycle
      endif
      do i = 1, len_trim(line)
        j = ichar(line(i:i))
        if (j == 32) then
        end if
      end do

      if (line(17:17) /= " ") then
        if (typea == " ") then
          write (iw,*) " The data set contains alternative location", &
         & " indicators.  Keyword ALT_A must be used"
          write (iw,*) nline, " lines have been read in"
          write (iw,*) " Faulty line:", line
          call mopend ("The data set contains alternative location" // &
         & " indicators.  Keyword ALT_A must be used")
        end if
        if (line(17:17) /= typea) cycle
      end if
      if (line(27:27) /= " ") then
        if (typer == "9") then
          write (iw,*) " The data set contains alternative location", &
         & " indicators.  Keyword ALT_R must be used"
          write (iw,*) nline, " lines have been read in"
          write (iw,*) " Faulty line:", line
          call mopend ("The data set contains alternative location" // &
         & " indicators.  Keyword ALT_R must be used")
        end if
        if (line(27:27) /= typer) cycle
      end if
      natoms = natoms + 1
!   CLEAN THE INPUT DATA
      call upcase (line, 80)
      icomma = Ichar (comma)
      do i = 1, 80
        khar = Ichar (line(i:i))
        if (khar == icomma) then
          line(i:i) = space
        end if
      end do
      if (lchain) then
        lchain = .false.
        if (line(21:21) == " ") then
          chains(ichain:ichain) = line(22:22)
          ichain = ichain + 1
        else
          chains(ichain:ichain + 1) = line(21:22)
          ichain = ichain + 2
        end if
        if (natoms > 1) txtatm(natoms - 1)(8:8) = "+"
      end if
!
!  Format of txtatm: "( nnnnn rrr mmmm)abc"
!  where: nnnnn = atom number (integer)
!         rrr   = residue three-letter abbreviation (text)
!         mmmm  = residue number (integer)
!         a     = A space or an integer, e.g., 1, 2 or 3, etc.
!         b     = A space or an integer, e.g., 1, 2 or 3, etc.
!         c     = A space or a Greek letter, e.g. a: alpha, g: gamma, etc.
!
      txtatm (natoms) = " "
      txtatm(natoms)(1:1) = "("
      txtatm(natoms)(3:7) = line(7:11)      
      txtatm(natoms)(9:11) = line(18:20)
      txtatm(natoms)(12:15) = line(23:26)
      txtatm(natoms)(16:16) = ")"
      number = line(13:13)
      if (number < "0" .or. number > "9") number = " "
      txtatm(natoms)(17:17) = number 
      txtatm(natoms)(18:18) = line(16:16)   ! number or a space
      txtatm(natoms)(19:19) = line(15:15)   ! greek letter or a space
!
!  If the user wants to keep the original text, use txtatm(20:)
!
      txtatm(natoms)(20:) = line(5:26)
      if (line(:6) == "HETATM") txtatm(natoms)(19:19) = "J"
!
      leadsp = .true.
      nvalue = 0
      do i = 1, 80
        if (leadsp .and. line(i:i) /= space) then
          nvalue = nvalue + 1
        end if
        leadsp = (line(i:i) == space)
      end do

!
! ESTABLISH THE ELEMENT'S NAME AND ISOTOPE, CHECK FOR ERRORS OR E.O.DATA
!

      if( line(1:4) == "ATOM" .or. line(1:6) == "HETATM" ) then
!
! Is there an element field present?
!
        ele = line(77:78)
        if( scan( " 0123456789.", ele(1:1) ) /= 0 ) then
          ele(1:1) = ele(2:2)
          ele(2:2) = " "
          if( scan( " 0123456789.", ele(1:1) ) /= 0 ) then ! no
!
! Use 13-14 field
!
            ele = line(13:14)
!
! Special case - old insight format
!
            if ( line(1:4) == "ATOM" .and. ele(1:1) == "H" ) then
            ele(2:2) = " "
            else if( scan( " 0123456789", ele(1:1) ) /= 0 ) then
            ele(1:1) = ele(2:2)
            ele(2:2) = " "
            endif
            
            if( scan( " 0123456789", ele(2:2) ) /= 0 ) then
              ele(2:2) = " "
            endif
          endif
        endif
      else
        natoms = natoms -1
        cycle
      endif

      do i = defined_elements+1, defined_elements + new_elements
        if (ele == elemnt(i)) go to 1010
      end do

      do i = 1, defined_elements
        if (ele == elemnt(i)) go to 1010
      end do

      natoms = natoms - 1

      do i = 1, npdb
        if (ele == txtpdb(i)) cycle outer_loop
      end do
      npdb = npdb + 1
      txtpdb(npdb) = ele
      if (ele(2:2) == " ") then
        ntxt_loc(npdb) = 1
      end if
      write (iw, "('  UNRECOGNIZED SPECIES: (',A,')' ,A,' ON LINE',I6)") ele &
     & (1:ntxt_loc(npdb)), ele(ntxt_loc(npdb)+1:3), nline
      cycle
1010  label = ielem(i)
      if (label == 0) then
        natoms = natoms - 1
      else
 !
 ! ALL O.K.
 !
        if (label == 99) then
          label = 1
        end if
        numat = numat + 1
        labels(natoms) = label
        if (label /= 99) then
          atmass(numat) = ams(label)
        end if
        geo(1, natoms) = reada (line, 32)
        geo(2, natoms) = reada (line, 40)
        geo(3, natoms) = reada (line, 48)
        lopt(1, natoms) = 1
        lopt(2, natoms) = 1
        lopt(3, natoms) = 1
        na(natoms) = 0
        nb(natoms) = 0
        nc(natoms) = 0
      end if
    end do outer_loop
1020 if (npdb /= 0) then
      write (iw,*) " THE SPECIES THAT WERE NOT RECOGNIZED CAN BE"
      write (iw,*) " RECOGNIZED BY USING THE FOLLOWING KEYWORD"
      write (iw,*)
      commas (npdb) = ")"
      write (iw,*) " PDB(", (txtpdb(i) (1:ntxt_loc(i)), ":Z", commas(i), i=1, &
     & npdb)
      write (iw,*)
      write (iw,*) " WHERE 'Z' IS/ARE ATOMIC NUMBERS"

      call mopend( " UNRECOGNIZED ELEMENT ")
    end if
    if (ichain > 9 .and. index(keywrd, "NORES") /= 0) then
      i = index(keywrd,"CHAIN")
      if (i > 0) then
        do j = i, i + 50
          if (keywrd(j:j) == " ") exit
        end do
        keywrd(i:) = keywrd(j:)
      endif
      i = len_trim(keywrd)
      keywrd(i + 1:) = " "//trim(chains)//")"
      txtatm(natoms)(8:8) = "+"      
    end if
 !
 !  SWITCH:   Coordinates are in Cartesian.  Should they be internal?
 !#      LXYZ= (INDEX(KEYWRD,' coord').NE.0.OR.VELO)
    lxyz = (Index (keywrd, " INT") == 0)
    if (lxyz) then
      na1 = 99
      return
    end if
    do i = 1, natoms
      do j = 1, 3
        coord(j, i) = geo(j, i)
      end do
    end do
    degree = 57.29577951308232d0
    call xyzint (coord, natoms, na, nb, nc, degree, geo)
 !
 !  UNCONDITIONALLY SET FLAGS FOR INTERNAL COORDINATES
 !
    do i = 1, 3
      do j = i, 3
        lopt(j, i) = 0
      end do
    end do
    if (natoms > 2) then
      if (Abs (geo(2, 3)-180.d0) < 1.d-4 .or. Abs (geo(2, 3)) < 1.d-4) then
        write (iw, "(A)") " DUE TO PROGRAM BUG, THE FIRST THREE " // &
                        & "ATOMS MUST NOT LIE IN A STRAIGHT LINE."
        call mopend ("FIRST 3 ATOMS MUST NOT LIE IN A LINE.")
        return
      end if
    end if
 !
 !  COORDINATES ARE INTERNAL, BUT IN DEGREES.  CONVERT TO RADIANS
 !
    degree = 1.7453292519943d-02
    do j = 2, 3
      do i = 1, natoms
        geo(j, i) = geo(j, i) * degree
      end do
    end do
    na1 = 0
end subroutine getpdb
