      subroutine readmo 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!
      use chanel_C, only : iw, ir, iarc, ilog, log_fn, archive_fn, &
      ires, restart_fn, output_fn
!
      USE maps_C, ONLY: latom, lparam, lpara1, latom1, lpara2, latom2, &
      react 
!
      USE symmetry_C, ONLY: idepfn, locdep, depmul, locpar 
!
      use molkst_C, only : ndep, numat, numcal, natoms, nvar, keywrd, dh, &
      & verson, method_mndo, method_am1, method_pm3, is_PARAM, line, nl_atoms, &
      & method_mndod, moperr, maxatoms, koment, title, method_pm6, refkey, &
      isok, ijulian, method_rm1, gui, Academic, site_no, density, method_pm6_dh2, &
      method_pm6_dh_plus, method_pm7, method_PM7_ts, method_pm6_plus, jobnam, id
!
      use elemts_C, only : elemnt
!
      use parameters_C, only : ams
!
      use common_arrays_C, only : xparam, loc, labels, na, nb, nc, & 
      & geo, coord, atmass, lopt, geoa, c, nat, pibonds, l_atom
!
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:00  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use gettxt_I 
      use mopend_I 
      use getgeg_I 
      use getgeo_I 
      use geout_I 
      use wrtkey_I 
      use getsym_I 
      use symtry_I 
      use nuchar_I 
      use wrttxt_I 
      use gmetry_I 
      use reada_I
      use maksym_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ireact 
      integer , dimension(19,2) :: idepco 
      integer :: naigin, i, j, k, iflag, nreact, ij, iend, l, ii, jj, &
        i4, j4, k4, iquit, ir_temp, l_iw
      real(double), dimension(40) :: value 
      real(double), dimension(400) :: xyzt 
      real(double) :: degree, convrt, dum1, dum2, sum, rms, rms_min, sum1, sum2, sum3, &
        toler, xmin
      double precision, external :: snapth
      logical :: intern = .true., aigeo, xyz, opend, exists, is_PDB
      logical, allocatable :: same(:)
      character :: space, space2*2, ch, ch2*2, idate*24, line_1*241
      save  space, space2, intern, ireact
!-----------------------------------------------
!
! MODULE TO READ IN GEOMETRY FILE, OUTPUT IT TO THE USER,
! AND CHECK THE DATA TO SEE IF IT IS REASONABLE.
! EXIT IF NECESSARY.
!
!
!
!  ON EXIT NATOMS    = NUMBER OF ATOMS PLUS DUMMY ATOMS (IF ANY).
!          KEYWRD    = KEYWORDS TO CONTROL CALCULATION
!          KOMENT    = COMMENT CARD
!          TITLE     = TITLE CARD
!          LABELS    = ARRAY OF ATOMIC LABELS INCLUDING DUMMY ATOMS.
!          GEO       = ARRAY OF INTERNAL COORDINATES.
!          LOPT      = FLAGS FOR OPTIMIZATION OF MOLECULE
!          NA        = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.
!          NB        = ARRAY OF LABELS OF ATOMS, BOND ANGLES.
!          NC        = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.
!          LATOM     = LABEL OF ATOM OF REACTION COORDINATE.
!          LPARAM    = RC: 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL
!          REACT     = REACTION COORDINATE PARAMETERS
!          LOC(1,I)  = LABEL OF ATOM TO BE OPTIMIZED.
!          LOC(2,I)  = 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL.
!          NVAR      = NUMBER OF PARAMETERS TO BE OPTIMIZED.
!          XPARAM    = STARTING VALUE OF PARAMETERS TO BE OPTIMIZED.
!
!***********************************************************************
! *** IR THE TRIAL GEOMETRY  (IE.  KGEOM=0)
!   LABEL(I) = THE ATOMIC NUMBER OF ATOM(I).
!            = 99, THEN THE I-TH ATOM IS A DUMMY ATOM USED ONLY TO
!              SIMPLIFY THE DEFINITION OF THE MOLECULAR GEOMETRY.
!   GEO(1,I) = THE INTERNUCLEAR SEPARATION (IN ANGSTROMS) BETWEEN ATOMS
!              NA(I) AND (I).
!   GEO(2,I) = THE ANGLE NB(I):NA(I):(I) IR IN DEGREES; STORED IN
!              RADIANS.
!   GEO(3,I) = THE ANGLE BETWEEN THE VECTORS NC(I):NB(I) AND NA(I):(I)
!              IR IN DEGREES - STORED IN RADIANS.
!  LOPT(J,I) = -1 IF GEO(J,I) IS THE REACTION COORDINATE.
!            = +1 IF GEO(J,I) IS A PARAMETER TO BE OPTIMIZED
!            =  0 OTHERWISE.
! *** NOTE:    MUCH OF THIS DATA IS NOT INCLUDED FOR THE FIRST 3 ATOMS.
!     ATOM1  IR LABELS(1) ONLY.
!     ATOM2  IR LABELS(2) AND GEO(1,2) SEPARATION BETWEEN ATOMS 1+2
!     ATOM3  IR LABELS(3), GEO(1,3)    SEPARATION BETWEEN ATOMS 2+3
!              AND GEO(2,3)              ANGLE ATOM1 : ATOM2 : ATOM3
!
!***********************************************************************
!
      data space, space2/ ' ', '  '/  
      data naigin/ 0/  
      data idepco/ 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 2, 2, 0, 1, 1, 2, &
        3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 0/  
      aigeo = .FALSE. 
      nvar = 0 
      ndep = 0 
      if (.not. allocated(lopt)) allocate(lopt(3,maxatoms))
   10 continue 
      keywrd = " "
      call gettxt 
      if (moperr) then
        natoms = 0
        return  
      end if
      if (index(keywrd,' ECHO') /= 0) then 
        rewind ir 
        if (.not.isok) then 
          write (iw, '(A)', iostat=l_iw) ' ECHO is not allowed at this point'  
          call mopend ('ECHO is not allowed at this point') 
          return  
        endif 
        isok = .FALSE. 
        do i = 1, 1000 
          read (ir, '(A)', end=60) keywrd 
          do j = 80, 2, -1 
            if (keywrd(j:j) /= ' ') go to 30 
          end do 
          j = 1 
   30     continue 
          do k = 1, j 
            if (ichar(keywrd(k:k)) >= 32) cycle  
            keywrd(k:k) = '*' 
          end do 
          write (iw, '(1X,A)', iostat=l_iw) keywrd(1:j) 
          if (l_iw /= 0) exit
        end do 
   60   continue 
        rewind ir 
        call gettxt 
        if (moperr) return  
      endif 
      if (keywrd(1:1) /= space) then 
        ch = keywrd(1:1) 
        keywrd(1:1) = space 
        do i = 2, 239 
          ch2 = keywrd(i:i) 
          keywrd(i:i) = ch 
          ch = ch2(1:1) 
          if (keywrd(i+1:i+2) /= space2) cycle  
          keywrd(i+1:i+1) = ch 
          go to 80 
        end do 
        ch2 = keywrd(240:240) 
        keywrd(240:240) = ch 
        keywrd(241:241) = ch2(1:1) 
   80   continue 
      endif 
      if (koment(1:1) /= space) then 
        ch = koment(1:1) 
        koment(1:1) = space 
        do i = 2, 79 
          ch2 = koment(i:i) 
          koment(i:i) = ch 
          ch = ch2(1:1) 
          if (koment(i+1:i+2) /= space2) cycle  
          koment(i+1:i+1) = ch 
          go to 100 
        end do 
        ch2 = koment(80:80) 
        koment(80:80) = ch 
        koment(81:81) = ch2(1:1) 
  100   continue 
      endif 
      if (title(1:1) /= space) then 
        ch = title(1:1) 
        title(1:1) = space 
        j = len_trim(title)
        do i = 2, j - 1 
          ch2 = title(i:i) 
          title(i:i) = ch 
          ch = ch2(1:1) 
          if (title(i+1:i+2) /= space2) cycle  
          title(i+1:i+1) = ch 
          go to 120 
        end do 
        ch2 = title(j:j) 
        title(j:j) = ch 
        title(j + 1: j + 1) = ch2(1:1) 
  120   continue 
      endif 
      latom  = 0 
      lparam = 0 
      xyz    = index(keywrd,' XYZ') + index(keywrd,' IRC') + index(keywrd,' DRC') /= 0 
!
!   Top level
!
      if (index(keywrd,' OLDGEO') == 0) then 
!
!  Read in a new geometry
!
        nvar = 0 
        ndep = 0 
        is_PDB = .false.
        if (aigeo .or. index(keywrd,' AIGIN')/=0) then 
          intern = .false.
          call getgeg (ir, labels, geo, lopt, na, nb, nc) 
          if (moperr) return  
          if (xyz) then 
            write (iw, '(A)') &
              ' CARTESIAN CALCULATION NOT ALLOWED WITH GAUSSIAN INPUT'
              call mopend (&
               'CARTESIAN CALCULATION NOT ALLOWED WITH GAUSSIAN INPUT')  
            return  
          endif 
          if (nvar == 0) then 
            lopt(:,:natoms) = 0 
          endif 
        else
          call getgeo (ir, labels, geo, coord, lopt, na, nb, nc, intern) 
          if (natoms == -2) then
            rewind(ir)
            call getpdb()
            is_PDB = .true.
          end if
          if (natoms == -3) goto 10
          if (moperr) return  
          if (Index (keywrd, " SNAP") /= 0) then
            !
            !   If any angles are near to important angles (such as 109.47...)
            !   snap the angle to the exact angle
            !
            do i = 1, natoms
              if (na(i) /= 0) then
                geo(2, i) = snapth (geo(2, i))
                geo(3, i) = snapth (geo(3, i))
              end if
            end do
          end if
          if (natoms < 0 ) then 
            if (numcal == 1) rewind ir 
            if (.not.isok) then 
              write (iw, '(A)') &
                ' Use AIGIN to allow more geometries to be used' 
                call mopend ('Use AIGIN to allow more geometries to be used') 
!
!   This is a deadly error - to prevent an infinite loop, kill the job.
!
              stop  
            endif 
            isok = .FALSE. 
            if (numcal > 2) then 
              naigin = naigin + 1 
              write (iw, '(2/,2A)') '   GAUSSIAN INPUT REQUIRES', &
                ' STAND-ALONE JOB' 
              write (iw, '(/,A)') '   OR KEYWORD "AIGIN"'
              call mopend (&
                 'GAUSSIAN INPUT REQUIRES STAND-ALONE JOB OR KEYWORD "AIGIN"')               
              return  
            endif 
            aigeo = .TRUE. 
            go to 10 
          endif 
        endif 
        if (natoms == 0 .and. numcal == 1) then 
          write (iw, '(A)') 'NO ATOMS IN SYSTEM'
          call mopend ('NO ATOMS IN SYSTEM')  
          return  
        endif 
        if (.not. is_PDB .and. index(keywrd," NORES") /= 0) then
          write (iw, '(A)') 'NORES can only be used when the geometry is in PDB format.' 
          call mopend ('NORES can only be used when the geometry is in PDB format.') 
        end if
      else 
!
!   Use the old geometry, if one exists
!
        if (numcal == 1) then
          write(line,'(a)')" Keyword OLDGEO cannot be used in the first calculation - there is no old geometry"
          write(iw,'(//10x,a)')trim(line)
          call to_screen(trim(line))
          call mopend(trim(line))
          return
        end if
      endif 
      if (natoms == 0) return
      if (index(keywrd,' FORCE')/=0 .and. labels(natoms)==107) then 
        do i = 1, na(natoms) 
          if (labels(i) /= 99) cycle  
          write (iw, '(A)') ' NO DUMMY ATOMS ALLOWED BEFORE TRANSLATION' 
          write (iw, '(A)') ' ATOM IN A FORCE CALCULATION'
          call mopend (&
       'NO DUMMY ATOMS ALLOWED BEFORE TRANSLATION ATOM IN A FORCE CALCULATION')  
          return  
        end do 
      endif 
!
!
! OUTPUT FILE TO UNIT 6
!
!    WRITE HEADER
      idate = ' ' 
      call fdate (idate) 
      write (iw, '(1X,15(''*****''),''****'')',iostat=i)
      if ( .not. gui) then
        if (i /= 0) then
          write(line,'(2a)')" Unable to write to file '", trim(output_fn)//"'"
          write(0,'(//10x,a,//)')trim(line)
          call mopend(trim(line))
          return
        else
          if (numcal == 1 .and. numat > 50) write(0,'(10x,a)')idate//"  Job: '"//trim(jobnam)//"' started successfully"
        end if
      end if
      if (Academic) then
        if (site_no == -1) then
          write (iw, '(A,i5,a)') ' **   Demo version - limited to 1-12, 50-60, or 110-120 atoms. Version '//verson//' **' 
        else if (site_no > 9999) then
          write (iw, '(A,i6,a)') ' ** Site#:',site_no,'           For non-commercial use only        Version '//verson//' **' 
        else
          write (iw, '(A,i5,a)') ' ** Site#:',site_no,'            For non-commercial use only        Version '//verson//' **' 
        end if
      else
        if (site_no == -1) then
          write (iw, '(A)') ' **     Evaluation copy        E-mail support: MrMOPAC@OpenMOPAC.net          **'
        else if (site_no > 9999) then
          write (iw, '(A,i6,a)') ' ** Site#:',site_no,'       E-mail support: MrMOPAC@OpenMOPAC.net                  **' 
        else
          write (iw, '(A,i5,a)') ' ** Site#:',site_no,'        E-mail support: MrMOPAC@OpenMOPAC.net                  **' 
        end if
      end if
        
      write (iw, '(1X,a)')"*******************************************************************************"
      write (iw, '(1X,a)')"** Cite this work as: MOPAC2012, James J. P. Stewart, Stewart Computational  **"
      if (ijulian > 410) then
        write (iw, '(1X,a, a,a)') &
                          "** Chemistry, Version ",verson," web: HTTP://OpenMOPAC.net                      **"
      else
        write (iw, '(1X,a, a,a,i4,a)') &
                          "** Chemistry, Version ",verson," web: HTTP://OpenMOPAC.net  Days remaining:",ijulian," **"
      end if
      
      write (iw, '(1X,a)')"*******************************************************************************"
      write (iw, '(1X,a)')"**                                                                           **"
      write (iw,"(1x,a)") "**                                MOPAC2012 BETA VERSION                     **"
      write (iw, '(1X,a)')"**                                                                           **"
      write (iw, '(1X,a)')"*******************************************************************************"
      if (.not. is_PARAM) then
        method_am1   = (index(keywrd,' AM1') /= 0)
        method_pm3   = (index(keywrd,' PM3') /= 0)
        method_pm6   = (index(keywrd,' PM6') /= 0)
        method_mndo  = (index(keywrd,' MNDO ') /= 0)
        method_rm1   = (index(keywrd,' RM1 ') /= 0)
        method_mndod = (index(keywrd,' MNDOD') /= 0)
        method_pm7   = (index(keywrd,' PM7') /= 0)
        method_pm7_ts= (index(keywrd,' PM7-TS') /= 0)
        dh = " "
        i = index(keywrd, " PM6-D")
        if (i /= 0) then 
          j = index(keywrd(i + 6:)," ") + i + 4
          if (index(keywrd(i + 6:j),'(') /= 0) j = index(keywrd(i + 6:),"(") + i + 4
          dh = keywrd(i + 5:j)                                  
        else if (index(keywrd, " PM6-H") /= 0)  then  
          dh = "H   "
        end if
!
!  If at SCC, set default method to PM7, otherwise PM6
!
        method_pm6   = (index(keywrd,' PM6') /= 0)
        method_pm6_dh2 = (index(keywrd,' PM6-D') + index(keywrd,' PM6-H') /= 0)
        method_pm6_dh_plus = (index(keywrd,' PM6-DH+') /= 0)
        if (method_pm6_dh_plus) method_pm6_dh2 = .false.
        method_pm7 = (.not.(method_am1 .or. method_pm3 .or. method_mndod .or. &
          method_rm1 .or. method_mndo .or. method_pm6))                    
      endif
      method_pm6_plus = (method_pm6 .or. method_pm6_dh2 .or. method_pm7)      
      line = '   MNDO' 
      if (method_am1)           line = '    AM1' 
      if (method_pm3)           line = '    PM3' 
      if (method_rm1)           line = '    RM1' 
      if (method_mndod)         line = 'MNDO-d ' 
      if (method_pm6)           line = '    PM6' 
      if (method_pm7)           line = '    PM7' 
      if (method_pm7_ts)        line = '    PM7-TS' 
      if (dh /= " ")            line = 'PM6-'//dh
      write (iw, &
      '(/24X,A,'' CALCULATION RESULTS'',28X,3/1X,15(''*****''),''****'' )') trim(line) 
      write (iw,'(" *  CALCULATION DONE: ",31x,2a)') idate,"  *"
!
! WRITE KEYWORDS BACK TO USER AS FEEDBACK
      call wrtkey
      if (moperr) &
        write(iw,'(a)')" *", &
      & " *  Errors detected in keywords.  Job stopped here to avoid wasting time."," *"          
      write (iw, &
        '(1X,14(''*****''),''*********'')')
      if (moperr) return
      if (index(keywrd, "INVERT") /= 0) then
        nvar = 0
        do i = 1, natoms
          do j = 1,3
            if (lopt(j,i) == 1) then
              lopt(j,i) = 0
            else if (lopt(j,i) == 0) then
              lopt(j,i) = 1
            end if
          end do
        end do
      end if
!
! FILL IN GEO MATRIX IF NEEDED
!
      if (index(keywrd,' SYMM') + index(keywrd," SYM ") /=0 .or. ndep > 0) then 
        call getsym(locpar, idepfn, locdep, depmul) 
        if ((xyz .or. intern)) then          
          write (iw, *) ' SYMMETRY CANNOT BE USED WHEN COORDINATE SYSTEMS ARE CHANGED' 
          ndep = 0 
          if (index(keywrd," IRC") + index(keywrd," DRC") == 0) then           
            if (index(keywrd,' XYZ') /= 0) write(iw,"(13x,a)")"(Remove either SYMMETRY or XYZ)"
            if (intern) write(iw,"(13x,a)")"(Remove either SYMMETRY or intern)"
            call mopend(' SYMMETRY CANNOT BE USED WHEN COORDINATE SYSTEMS ARE CHANGED')
            return
          end if         
        endif 
      endif 
      
      if (index(keywrd, " NOOPT") /= 0)   then
        line = keywrd
        do
          i = index(line, " NOOPT")
          if (i == 0) exit
          j = index(line(i + 4:)," ") + i + 2
          line(i:i + 6) = " "     
          if (j - i == 8) then
            line(1:1) = line(j - 1:j - 1)
            line(2:2) = char( ichar(line(j:j)) + ichar("a") - ichar("A"))
          else if (j - i == 7) then
            line(1:1) = " "
            line(2:2) = line(j:j)
          else
            do i = 1, natoms
              lopt(1, i) = 0
              lopt(2, i) = 0
              lopt(3, i) = 0
            end do
            cycle
          end if               
          do l = 1, 99
            if (line(1:2) == elemnt(l)) exit
          end do        
          do i = 1, natoms
            j = labels(i)
            if (j == l) then
              lopt(1, i) = 0
              lopt(2, i) = 0
              lopt(3, i) = 0
            end if
          end do
        end do 
      end if
      if (index(keywrd, " OPT") /= 0)   then
        line = keywrd
        do
          i = index(line, " OPT")
          if (i == 0) exit
          j = index(line(i + 2:)," ") + i 
          line(i:i + 4) = " "     
          if (j - i == 6) then
            line(1:1) = line(j - 1:j - 1)
            line(2:2) = char( ichar(line(j:j)) + ichar("a") - ichar("A"))
          else  if (j - i == 5) then
            line(1:1) = " "
            line(2:2) = line(j:j)
          else if (j - i == 3) then
            do i = 1, natoms
              lopt(1, i) = 1
              lopt(2, i) = 1
              lopt(3, i) = 1
            end do
            cycle
          else
            cycle
          end if               
          do l = 1, 99
            if (line(1:2) == elemnt(l)) exit
          end do        
          do i = 1, natoms
            j = labels(i)
            if (j == l) then
              lopt(1, i) = 1
              lopt(2, i) = 1
              lopt(3, i) = 1
            end if
          end do
        end do 
      end if
!
!   FORCE OPTIMIZATION FLAGS OFF FOR DEPENDENT COORDINATES
!
      j = 1 
      if (xyz) j = 2 
      do i = 1, ndep 
        lopt(idepco(idepfn(i),j),locdep(i)) = 0 
      end do 
      if (ndep /= 0) call symtry 
!
! INITIALIZE FLAGS FOR OPTIMIZE AND PATH
      iflag = 0 
      latom = 0 
      numat = 0 
      if (nvar /= 0) then 
        numat = natoms 
      else 
       loc = 0
        do i = 1, natoms 
          if (labels(i)/=99 .and. labels(i)/=107) numat = numat + 1 
          do j = 1, 3 
            if (lopt(j,i) > 0) go to 200 
            if (lopt(j,i) == 0) cycle             
            if (iflag /= 0) then 
              if (index(keywrd,' STEP1') /= 0) then 
                lpara1 = lparam 
                latom1 = latom 
                lpara2 = j 
                latom2 = i 
                latom = 0 
                iflag = 0 
                cycle  
              else 
                write (iw,'('' ONLY ONE REACTION COORDINATE PERMITTED'')') 
               call mopend ('ONLY ONE REACTION COORDINATE PERMITTED') 
                return  
              endif 
            endif 
            latom = i 
            lparam = j 
             convrt = 1.d0 
            if (j > 1 .and. na(latom) > 0) convrt = 0.01745329252D00
            if (allocated(react)) deallocate(react)
            allocate(react(4000))
            react(1) = geo(lparam, latom)
            ireact = 1 
            iflag = 1 
            cycle  
!    FLAG FOR OPTIMIZE
  200       continue 
            nvar = nvar + 1 
            loc(1,nvar) = i 
            loc(2,nvar) = j 
            xparam(nvar) = geo(j,i) 
          end do 
        end do 
      endif 
      if (index(keywrd, "MINI") /= 0) then
        l_atom = .false.        
        do i = 1, nvar
          l_atom(loc(1,i)) = .true.
        end do
        nl_atoms = 0
        do i = 1, numat
          if (l_atom(i)) nl_atoms = nl_atoms + 1
        end do
!
!  Because MINI is used, optimization flags need to be set to 2 not 1
!  in order for then to be used by the functions in MOPAC
!
        loc = 0
        nvar = 0
        do i = 1, natoms 
          do j = 1, 3 
            if (lopt(j,i) > 1) then
              nvar = nvar + 1 
              loc(1,nvar) = i 
              loc(2,nvar) = j 
              xparam(nvar) = geo(j,i) 
            end if
          end do 
        end do
      else
        nl_atoms = numat
        l_atom = .true.
      end if
      if (index(keywrd," RESTART") /= 0 .and. &
      index(keywrd,' IRC') + index(keywrd,'FORCE') + index(keywrd," THERMO") == 0) then
        open (unit=ires, file=restart_fn, status="UNKNOWN", &
                   & form="UNFORMATTED")
        rewind (ires)
               !
               !  Read in the geometric variables
               !
         read (ires, end=1900, err=1900) i,i, (xparam(i), i=1, nvar)
         do i = 1, nvar
           k = loc(1, i)
           l = loc(2, i)
           geo(l, k) = xparam(i)
          end do
        end if
! READ IN PATH VALUES
      if (iflag /= 0) then 
        if (index(keywrd,' NLLSQ') /= 0) then 
          write (iw, '(A)') &
            ' NLLSQ USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED' 
          call mopend (&
             'NLLSQ USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED') 
          return  
        endif 
        if (index(keywrd,' SIGMA') /= 0) then 
          write (iw, '(A)') &
            ' SIGMA USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED'
          call mopend (&
             'SIGMA USED WITH REACTION PATH; THIS OPTION IS NOT ALLOWED')  
          return  
        endif 
        if (index(keywrd,' STEP=') + index(keywrd,' POINT=') /= 0) then 
          go to 250 
        endif 
  220   continue 
        read (ir, '(A)', end=240) line 
        call nuchar (line, len_trim(line), value, nreact) 
        if (nreact == 0) go to 240 
        do i = 1, nreact 
          ij = ireact + i 
          react(ij) = value(i)*convrt 
          if (abs(react(ij)-react(ij-1)) >= 1.D-12) cycle  
          dum1 = react(ij)/convrt 
          dum2 = react(ij-1)/convrt 
          write (iw, &
      '(3/,'' TWO ADJACENT POINTS ARE IDENTICAL:  '',    F7.3,2X,F7.3,/,'' THIS&
      & IS NOT ALLOWED IN A PATH CALCULATION'')') dum1, dum2 
          call mopend (&
       'TWO ADJACENT POINTS ARE IDENTICAL: THIS IS NOT ALLOWED IN A PATH CALCULATION') 
          return  
        end do 
        ireact = ireact + nreact 
        go to 220 
  240   continue 
        degree = 1.D0 
        if (lparam > 1 .and. na(latom) > 0) degree = 57.29577951308232D0 
         if (index(keywrd,' 0SCF') == 0) then 
          if (ireact <= 1) then 
            write (iw, &
        '(2/10X,'' NO POINTS SUPPLIED FOR REACTION PATH'')') 
            write (iw, '(2/10X,'' GEOMETRY AS READ IN IS AS FOLLOWS'')') 
            xparam(1) = -1.D0 
            call geout (1)
            call mopend ('NO POINTS SUPPLIED FOR REACTION PATH')  
            return  
          else 
            write (iw, '(2/10X,'' POINTS ON REACTION COORDINATE'')') 
            write (iw, '(10X,8F8.2)') (react(i)*degree,i=1,ireact) 
          endif 
          iend = ireact + 1 
          react(iend) = -1.D12 
        end if
      end if 
  250 continue 
      if (nvar > 0 .and. index(keywrd,' PM7-TS') /= 0) then 
        write(iw,'(a)')" Keyword PM7-TS can only be used for single-point calculations."
        i = Int(Log10(1.d0*nvar))
        ch = char(i + ichar("2"))
        write(iw,'(a,i'//ch//',a)')" The system supplied has",nvar," geometric variables."
        write(iw,'(a)')" All geometric variables have been changed to fixed, to allow the run to continue."
        nvar = 0
      end if
      call wrttxt (iw) 
      if (index(keywrd,' 0SCF') == 0) then 
!
! CHECK DATA
!
        if (xyz) then 
          if (index(keywrd,' IRC') + index(keywrd,' DRC') + &
          index(keywrd,' 1SCF') == 0) then 
            if (nvar/=0 .and. intern .and. nvar<3*numat-6) then 
              write (iw, &
      '(2/10X,'' INTERNAL COORDINATES READ IN, AND CALCULATION '',/10X, &
      & ''TO BE RUN IN CARTESIAN COORDINATES, '',/10X, &
      & ''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')') 
              write (iw, &
      '(2/10X,'' THIS INVOLVES A LOGICALLY ABSURD CHOICE'',/10X, &
      & '' SO THE CALCULATION IS TERMINATED AT THIS POINT'')') 
              call mopend ('INCONSISTENT USE OF OPTIMIZATION FLAGS') 
              return  
            endif 
          endif 
        else 
          if (index(keywrd,' 1SCF')==0 .or. index(keywrd,' GRAD')/=0) then 
            if (intern .and. nvar/=0 .and. nvar<3*numat-6) then 
              write (iw, &
         '(2/10X,'' CARTESIAN COORDINATES READ IN, AND CALCULATION '',/10X, &
      & ''TO BE RUN IN INTERNAL COORDINATES, '',/10X, &
      & ''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')') 
              write (iw, &
      & '(2/10X,''MOPAC, BY DEFAULT, USES INTERNAL COORDINATES'',/10&
      & X,''TO SPECIFY CARTESIAN COORDINATES USE KEY-WORD :XYZ:'')') 
              write (iw, &
      & '(10X,''YOUR CURRENT CHOICE OF KEY-WORDS INVOLVES A LOGICALLY'',/10X, &
      & ''ABSURD CHOICE SO THE CALCULATION IS TERMINATED AT THIS POINT'')') 
              call mopend ('INCONSISTENT USE OF OPTIMIZATION FLAGS') 
              return  
            endif 
          endif 
        endif 
      endif 
      if (index(keywrd,' LOG') /= 0) then 
        open(unit=ilog, form='FORMATTED', status='UNKNOWN', file=log_fn, position='asis') 
        call wrttxt (ilog) 
      endif  
      call geout(1)
      write(iw,*)
!
!  Check for isotopes.  If found, print them out
!
      j = 0
      k = 0
      do i = 1, natoms
        if (labels(i) == 99 .or. labels(i) == 107) cycle
        k = k + 1
        if (Abs(atmass(k) - ams(labels(i))) > 1.d-3) then
          if (j == 0) then
            write(iw,"(/30x, a,/)")"  Isotopes Used"
            j = 1
          end if
        write(iw,"(a,i5,1x,2a,f8.4,a,f8.4,a)")"              Atom:", i, elemnt(labels(i)), &
   "  Default mass", ams(labels(i)), ",  mass used:", atmass(k), " amu"
        end if 
      end do    
      call gmetry (geo, coord)
      if(index(keywrd," PKA") /= 0) then
!
!  Do any ionizable hydrogen atoms exist (hydrogens attached to an oxygen)
!
        k = 0
        i = 0
        do ii = 1, natoms
          if (labels(ii) < 99) i = i + 1
          if (labels(ii) /= 1) cycle        
          j = 0
          do jj = 1, natoms
            if (labels(ii) < 99) j = j + 1
            if (labels(jj) /= 8) cycle           
            sum = (coord(1,i) - coord(1,j))**2 + &
                & (coord(2,i) - coord(2,j))**2 + &
                & (coord(3,i) - coord(3,j))**2 
            if (sum < 1.69d0) then
              k = 1
              exit         
            end if
          end do
        end do
        if (k == 0) then
          write(iw,"(/,3(10x,a,/))")"A request was made to print the pKa values for this system,", &
      "but there are no hydrogen atoms attached to an oxygen atom,", &
      "so the pKa calculation cannot be completed."
          call mopend(" No '-O-H' groups found.  pKa cannot be calculated")
          return
        end if
      end if
      if (moperr) return  
      if (index(keywrd, " SETPI") /= 0) then
        if (allocated(pibonds)) deallocate(pibonds)
        allocate (pibonds(numat,2))
        pibonds = 0
!
!  The user supplies some pi bonds.
!
        i = index(keywrd, " SETPI")
        j = index(keywrd(i + 6:), " ") + i + 4
        l = index(keywrd(i:j), "=")
        if (l /= 0) then
!
!  Read pi bonds from a file in this folder
!
          l = l + i
          line = keywrd(l:j)
          inquire (file=line, exist = exists)
          if (exists) then
            ir_temp = 27
            open (unit=ir_temp, file=line)  
            rewind (ir_temp)          
          else
            write(line,'(a)')" Pi-bond file '"//trim(line)//"' does not exist. (required by SETPI="//trim(line)//")"
            write(iw,'(//10x,a,/)')trim(line)
            call mopend(trim(line))
            return
          end if
        else
          ir_temp = ir
        end if
         l = 0
        do 
          read (ir_temp,'(a)',end=98, err=98) line
          if (line == " ") exit
!
!  Read in the first of the two atom numbers
!
        do i = 1, len_trim(line) 
            if (line(i:i) /= " ") then 
              ii = nint(reada(line,i))
              exit
            endif 
          end do 
          do i = i + 1, len_trim(line) 
            if (line(i:i) == " ") then 
              jj = nint(reada(line,i))
              exit
            endif 
          end do 
          l = l + 1
          if (ii > numat .or. jj > numat) then
            write(line,'(a,i5,a,i5,a)') " pi bond between atoms",ii," and", jj," is impossible - atom number too large"
            write(iw,'(a)')trim(line)
            call mopend(trim(line))
            return
          end if
          pibonds(l,1) = ii
          pibonds(l,2) = jj       
        end do
    98  if (l == 0) then
          write(iw,'(/11x,a)')"Keyword SETPI used, but no pi bonds specified"
          call web_message(iw,"SETPI.html")
          if (index(keywrd,' 0SCF') + index(keywrd, " RESEQ") == 0 ) then 
            call mopend("Keyword SETPI used, but no pi bonds specified")
            return
          end if
        else
         write(iw,'(/11x,a)')"Keyword SETPI used, pi bonds specified are:"
         write(iw,'(/12x,a)')"   Bond No.          Atom    to    Atom"
         do i = 1, l
           write(iw,'(12x,i7,12x,a2,i5,a,a2,i5)')i, elemnt(labels(pibonds(i,1))), &
            pibonds(i,1), "   -   ", elemnt(labels(pibonds(i,2))), pibonds(i,2)
         end do
        end if
        if (ir_temp == 27) close(ir_temp)
      end if  
      i = index(keywrd," GEO_REF")
      if (i /= 0) then
        if (index(keywrd," 0SCF") == 0) then
!
!   For Geo-Ref to work, some very specific conditions must be satisfied.  
!   So before attempting a GEO_REF calculation, check that the data are okay
!
          do j = 2, natoms
            if (na(j) /= 0) exit
          end do       
          if (nvar /= 3*numat + 3*id) then
            write(iw,"(a)")" GEO_REF requires all parameters to be optimized"
            call mopend(" GEO_REF requires all parameters to be optimized")
          else if (j <= natoms) then
            write(iw,"(a)")" GEO_REF requires Cartesian coordinates"
            call mopend(" GEO_REF requires Cartesian coordinates")
          end if       
        
          if (moperr) return
          allocate(geoa(3,natoms + 300), c(3,natoms + 300)) ! Generous safety factor for second geometry
          do ii = 1, 6
            line = " "//refkey(ii)
            call upcase(line, len_trim(line))
            i = index(line," GEO_REF")
            if (i /= 0) exit
          end do
          j = index(refkey(ii)(i + 10:),' ') + i + 8
          if (index(line(i:j), '"') == 0) then
            write(line,'(a)')" File name after GEO_REF must be in quotation marks."
            write(iw,'(//10x,a,//)')trim(line)
            call mopend(trim(line))
            return
          end if
          j = index(refkey(ii)(i + 10:),'"') 
          if (j == 0) then
            write(line,'(a)')" File name after GEO_REF must end with a quotation mark."
            write(iw,'(//10x,a,//)')trim(line)
            call mopend(trim(line))
            return
          end if
          j = j + i + 8
          line = refkey(ii)(i + 9:j)
          line_1 = trim(line)
          call upcase(line_1, len_trim(line_1))
          if (line_1 == "SELF") then
            line = output_fn(:len_trim(output_fn) - 3)//"mop"
            refkey(ii)(i:) = "geo_ref="""//trim(line)//refkey(ii)(j + 1:)
          end if
          inquire (file=trim(line), exist = exists)
          if (.not. exists) then
            write(iw,"(//10x,3a)")"GEO_REF file '", trim(line), "' does not exist."
            call mopend ("File "//trim(line)//" does not exist.")
            return
          end if
          open(unit = 99, file = trim(line), iostat = i)
          if (i /= 0) then
            write(iw,"(//10x,2a)")"Problem opening file ", trim(line)
            call mopend ("Problem opening file "//trim(line))
            return
          end if
          if (index(keywrd, '.ARC"') /= 0) then
            do
              read(99,"(a)")line_1
              if (index(line_1," FINAL GEOMETRY OBTAINED") /= 0) exit
            end do
          end if
          i = 0
          do
            i = i + 1
            read(99,"(a)", err=99)idate  !  Dummy read over first three lines
            if (idate(1:1) == "*") i = i - 1
            if (i == 3) exit
          end do
          goto 97
    99    write(iw,*)" File' "//trim(line)//"' is faulty"
          return
    97    i = index(keywrd," GEO_REF") + 11
          do
            if (keywrd(i:i) == '"') exit
            i = i + 1
          end do
          if (keywrd(i + 1: i + 1) == " ") then
            write(iw,'(a)')"   By default, a restraining force of 3 kcal/mol/A^2 will be used"
            density = 3.d0
          else
            density = reada(keywrd, j)
            write(iw,'(a,f8.1,a)')"   A restraining force of",density," kcal/mol/A^2 will be used"
          end if
          nat(:natoms) = labels(:natoms)
          i = natoms       
          call getgeo (99, labels, geoa, c, lopt, na, nb, nc, intern) 
          if (moperr) then
            i = index(keywrd," GEO_REF")
            j = index(keywrd(i + 10:),'"') + i + 8
            line = keywrd(i + 10:j)
            write(line,'(a)')"Fault detected in GEO_REF data set: '"//trim(line)//"'"
            write(iw,'(a)')trim(line)
            call mopend(trim(line))
            return
          end if
          do j = 2, natoms
            if (na(j) > 0) exit
          end do
          if (j <= natoms) then
            call gmetry(geoa, coord)
            geoa(:,:natoms) = coord
            na(:natoms) = 0
          end if    
          if (natoms /= i) then
            write(line,'(a,i5,a,i5,a)') &
              " Number of atoms in the two systems (",i," and",natoms,") are different."
            write(iw,'(//10x,a)')trim(line)
            call mopend(trim(line))
            return
          end if
          if (numat /= natoms) then
            write(iw,'(/10x,a)') &
                " Data set must not contain dummy atoms"
            call mopend( " Data set must not contain dummy atoms")
            return
        end if
        do i = 1, numat
          if (nat(i) /= labels(i)) then
!
! Try to re-sequence atoms to correct the fault
!
            do j = i, numat
              if (labels(j) == nat(i)) then
                k = labels(j)
                labels(j) = labels(i)
                labels(i) = k
                sum = atmass(j)
                atmass(j) = atmass(i)
                atmass(i) = sum
                do k = 1,3
                  sum = geoa(k,j)
                  geoa(k,j) = geoa(k,i)
                  geoa(k,i) = sum
                end do
                exit
              end if
            end do            
          end if
        end do
        j = 0
        do i = 1, numat
          if (nat(i) /= labels(i)) then
            if (j == 0) write(iw,'(/10x,a)') &
              " REF_GEO data set must have the atoms in the same sequence as in the data set used."
              write(line,'(11x,a,i5,5a)')"Atom ",i," has symbol ", elemnt(nat(i)), &
              " in data set, but has symbol ",elemnt(labels(i))," in GEO-REF"
              write(iw,'(a)')trim(line)
              j = j + 1
          end if
        end do
        if (j /= 0) then
          call mopend(" GEO_REF data set must have all atoms in the same sequence as in the data set used.")
          call web_message(iw,"geo_ref.html")
          call mopend(trim(line))
          return
        end if
        rms_min = 1.d6
        allocate(same(numat))
        write(iw,'(" ",10("----"))')
        write(iw,'(/,a)')"            Before docking" 
        call geo_diff(sum, rms, j)       
        if (sum < 99999.d0) then
          write(iw,'(3x,a,f8.2,a,f8.4,a,f8.4,a)') "Difference to Geo-Ref:", sum, &
            " = total,", sum/j, " = Average,", sqrt(rms/j)," = RMS movement, in Angstroms"   
        else
          write(iw,'(3x,a,f9.2,a,f8.4,a,f8.4,a)') "Difference to Geo-Ref:", min(999999.d0,sum), &
            " = total,", sum/j, " = Average,", sqrt(rms/j)," = RMS movement, in Angstroms"   
        end if      
       
        toler = 25.d0        
        i4 = 1
        j4 = numat 
        k4  = 1      
        iquit = 0
        xmin = 1.d8  
        do ii = 1,30
!
! Call dock twice, first to center and orient geo, then to center and orient geo
!
        call dock (geoa, geo, sum)
        call dock (geo, geoa, sum)  
        write(iw,'(" ",10("----"))')      
        write(iw,'(/,a)')"   After docking (rotation and translation)" 
        call geo_diff(sum, rms, jj)         
        if (Abs(rms_min - rms ) > 1.d-1) then    
          sum1 = sum/jj      
          sum2 = sqrt(rms/jj)
          write(iw,'(3x,a,f8.2,a,f8.4,a,f8.4,a)') "Difference to Geo-Ref:", sum, &
            " = total,", sum1, " = Average,", sum2," = RMS movement, in Angstroms"       
        end if       
        sum = 0.d0
        do i = 1, numat
          j = j + 1
          sum = sum + sqrt((geo(1,i) - geoa(1,i))**2 + &
                           (geo(2,i) - geoa(2,i))**2 + &
                           (geo(3,i) - geoa(3,i))**2)
        end do 
!
!  Check that atoms are in maximum coincidence.
!  If they are not, then re-arrange geoa
!       
        do i = 1, numat            
          sum = (geo(1,i) - geoa(1,i))**2 + &
                (geo(2,i) - geoa(2,i))**2 + &
                (geo(3,i) - geoa(3,i))**2
          same(i) = (sum < toler)
        end do
        exists = .true.
        do i = i4, j4, k4          
          sum = (geo(1,i) - geoa(1,i))**2 + &
                (geo(2,i) - geoa(2,i))**2 + &
                (geo(3,i) - geoa(3,i))**2
          if (sum > toler) then
            sum = 1.d8
            k = i
            do j = i4, j4, k4
              if (nat(i) == labels(j) .and. .not. same(j)) then
                sum3 = ((geo(1,i) - geoa(1,i))**2 + &
                        (geo(2,i) - geoa(2,i))**2 + &
                        (geo(3,i) - geoa(3,i))**2) 
                dum1 = ((geo(1,i) - geoa(1,j))**2 + &
                        (geo(2,i) - geoa(2,j))**2 + &
                        (geo(3,i) - geoa(3,j))**2) 
                dum2 = ((geoa(1,i) - geo(1,j))**2 + &
                        (geoa(2,i) - geo(2,j))**2 + &
                        (geoa(3,i) - geo(3,j))**2)
!
!  If swapping the atoms around will improve the overlap, then do so.
!
                if (dum1 + dum2 < sum .and. dum1 + dum2 < 2.d0*sum3) then
                  sum = dum1 + dum2
                  k = j
                end if
              end if
            end do
            if (i /= k) then
              if (exists) then
               exists = .false.
               write(iw,'(" ",10("----"))')
               write(iw,'(/,a)')"  List of atoms that are swapped in order to maximize overlap", &
                 "       Atom    and     Atom"            
              end if
              write(iw,'(i9, a, i13, a)')i, " "//elemnt(nat(i)), k, " "//elemnt(labels(k))
              do j = 1,3
                sum = geoa(j,i)
                geoa(j,i) = geoa(j,k)
                geoa(j,k) = sum
              end do              
            end if
            same(i) = .true.
          end if                
        end do
        if (.not. exists) write(iw,*)" "
        k = 0
        do i = 1, numat
          do l = 1,3
            k = k + 1
            xparam(k) = geo(l,i)
          end do
        end do
        if (.not. exists) then
          
          write(iw,'(/,a)')"    After swapping atoms around"
          call geo_diff(sum, rms, jj)         
          sum1 = sum/jj       
          sum2 = sqrt(rms/jj)
          write(iw,'(3x,a,f8.2,a,f8.4,a,f8.4,a)') "Difference to Geo-Ref:", sum, &
            " = total,", sum1, " = Average,", sum2," = RMS movement, in Angstroms"       
        end if
        if (abs(rms_min - rms) < 1.d-4) then
          write(iw,'(/,a)')"   The two geometries are now in maximum overlap"      
          write(iw,'(3x,a,f8.2,a,f8.4,a,f8.4,a)') "Difference to Geo-Ref:", sum1*jj, &
            " = total,", sum1, " = Average,", sum2," = RMS movement, in Angstroms"      
          exit
        end if
        if (xmin > rms) then
          xmin = rms
          iquit = 0
        else
          iquit = iquit + 1
          if (iquit > 3) then
            write(iw,'(/,a)')"   The two geometries are now in maximum overlap"         
            exit
          end if
        end if
        rms_min = rms
        toler = max(1.d0, 0.7d0*toler)   
        k4 = i4
        i4 = j4
        j4 = k4
        if (i4 == 1) then 
          k4 = 1
        else
          k4 = -1
        end if
        end do
        close(99)
        coord(:,:natoms) = geo(:,:natoms)
        do ii = 1, 6
          line = " "//refkey(ii)
          call upcase(line, len_trim(line))
          i = index(line," GEO_REF")
          if (i /= 0) exit
        end do
        j = index(refkey(ii)(i + 10:),'"') + i + 4
        line = refkey(ii)(i + 9:j)//".new"
        if (index(keywrd, " TS") /= 0) then
          write(iw,'(/,a)')"    The average of the supplied and reference geometry will be written to:"
          geo(:,:natoms) = 0.5d0*(geoa(:,:natoms) + geo(:,:natoms))
        else
          write(iw,'(/,a)')"    The rotated-translated-re-arranged reference geometry will be written to:"
          geo(:,:natoms) = geoa(:,:natoms)
        end if
        write(iw,'(a)')"    '"//trim(line)//"'"
        open(unit = 99, file = trim(line), iostat = i)
        call geout(99) ! Write out the re-organized reference data set.
        close(99)
        deallocate(c, same)
        if (index(keywrd," 0SCF") + index(keywrd, " TS") /= 0) then
          write (iw, '(/,'' == MOPAC DONE =='')') 
          call mopend(" GEO_REF with 0SCF: Job complete")
          return
        end if
        geo(:,:natoms) = coord(:,:natoms)
      else
        keywrd(i:i + 7) = " "
        end if
      end if
      if (index(keywrd,' AUTOSYM') /= 0) call maksym(loc, xparam, xyzt) 
      if (index(keywrd,' 0SCF') + index(keywrd, " RESEQ") /= 0) then  
        inquire(unit=iarc, opened=opend) 
        if (opend) close (iarc)
        if (index(keywrd, "PDBOUT") /= 0) archive_fn = archive_fn(:len_trim(archive_fn) - 3)//"pdb" 
      endif 
      if (index(keywrd,' NOXYZ') == 0 .or. gui) then 
         write (iw, '(2/10X,''CARTESIAN COORDINATES '',/)') 
        write (iw, &
      '(4X,''NO.'',7X,''ATOM'',9X,''X'',9X,''Y'',9X,''Z'',/)') 
        l = 0 
        do i = 1, natoms 
          if (labels(i)==99 .or. labels(i)==107) cycle  
          l = l + 1 
          if (l_atom(i)) write (iw, '(I6,8X,A2,4X,3F10.4)') l, elemnt(labels(i)), (coord(j,l),j=1,3) 
          end do 
      endif 
      return  
1900  write (iw,*) " RESTART file missing or corrupt"
      call mopend (" RESTART file missing or corrupt")
      return
      end subroutine readmo 
      double precision function snapth (theta)
      implicit none
      double precision, intent (in) :: theta
      integer :: i, j, k
      double precision :: angle, const, phase, sum
      intrinsic Abs, Acos, Asin, Cos, Int, Mod, Nint, Sign, Sqrt, Dble
        angle = Cos (theta)
        phase = Sign (1.d0, theta)
        if (Abs (angle) < 1.d-4) then
          !
          !   Cos(Theta) is zero - this is 90 or 270 degrees
          !
          const = 2 * Asin (1.d0)
          if (Abs (theta) < const) then
            snapth = phase * Acos (0.d0)
          else
            snapth = phase * Acos (0.d0) + const
          end if
        else
          sum = Abs (1.d0/angle) ** 2
          do i = 1, 7
            j = Nint (sum*i)
            if (Abs (j-sum*i) < 5.d-3) go to 1000
          end do
          snapth = theta
          return
    1000  sum = Sqrt (Dble(i)/Dble(j))
          const = 2 * Asin (1.d0)
          k = Int (Abs (theta)/const)
          if (Mod(k, 2) == 0) then
             !
             !   Theta is in domain -180 - 0 - +180 degrees
             !
            snapth = phase * Acos (sign(sum, angle))
          else
             !
             !   Theta is in domain -180 - 360 - +180 degrees
             !
            snapth = phase * (2*const-Acos (sign(sum, angle)))
          end if
        end if
  end function snapth
  subroutine geo_diff(sum, rms, j)
    use common_arrays_C, only : nat, geo, geoa
    use molkst_C, only : numat
    use elemts_C, only : elemnt
    use chanel_C, only : iw
    implicit none
    double precision :: sum, rms
    integer :: j
!    
    integer :: i
    logical :: exists
    double precision :: dum1
!
!  Calculate the average and RMS differences of two geometries
!
    sum = 0.d0
    rms = 0.d0
    j = 0
    exists = .true.
    do i = 1, numat
      j = j + 1
      dum1 =  (geo(1,i) - geoa(1,i))**2 + &
              (geo(2,i) - geoa(2,i))**2 + &
              (geo(3,i) - geoa(3,i))**2
      rms = rms + dum1
      dum1 = sqrt(dum1)
      if (dum1 > 1.d0) then
        if (exists) then
          exists = .false.
          write(iw,'(/,a)')"        Atoms that move a lot","          Atom      Movement"
        end if
        write(iw,'(i11, a, f12.2)')i, " "//elemnt(nat(i)), dum1
      end if
      sum = sum + dum1
    end do 
end subroutine geo_diff
  
