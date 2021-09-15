      subroutine geout(mode1) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use common_arrays_C, only : labels, na, nb, nc, geo, nat, loc, txtatm, &
      & p, atmass, all_comments, pibonds, l_atom
      USE maps_C, ONLY: latom, lparam 
      USE parameters_C, only : tore, ams
      USE molkst_C, ONLY: numat, natoms, ndep, keywrd, maxtxt, gui, line, &
      & ncomments
      USE symmetry_C, ONLY: depmul, locpar, idepfn, locdep 
      USE elemts_C, only : elemnt
      USE chanel_C, only : iw 
      use maps_C, only : lpara1, latom1, lpara2, latom2
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:50:09  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use wrttxt_I 
      use chrge_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: mode1 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: mode, iprt, i, j, n, ia, ii, k, igui
      real(double), dimension(natoms) :: q2 
      real(double) :: degree, w, x 
      logical :: cart, lxyz, isotopes
      character , dimension(3) :: q*2 
      character :: flag1*2, flag0*2, flagn*2, blank*80, fmt1*4, fmt23*4
!*********************************************************************
!
!   GEOUT PRINTS THE CURRENT GEOMETRY.  IT CAN BE CALLED ANY TIME,
!         FROM ANY POINT IN THE PROGRAM AND DOES NOT AFFECT ANYTHING.
!
!   mode1:   1 write geometry in normal MOPAC *.out format
!           -n write geometry in normal MOPAC *.arc format, but do not print keywords, title
!              symmetry data, etc.
!            n write geometry in normal MOPAC *.arc format
!
!*********************************************************************
      mode = mode1 
      lxyz = index(keywrd,' COORD') + index(keywrd,'VELO') /= 0 
      if (index(keywrd,' 0SCF')/=0 .and. lxyz) then 
!
!  If 0SCF and coord and a polymer, then get rid of TV.
!
        natoms = numat 
        lxyz = .FALSE. 
      endif 
      if (mode == 1) then 
        flag1 = ' *' 
        flag0 = '  ' 
        flagn = ' +' 
        iprt = iw 
      else 
        if (gui) then
          flag1 = ' 1' 
          flag0 = ' 0' 
        else
          flag1 = '+1' 
          flag0 = '+0' 
        end if
        flagn = '-1' 
        iprt = abs(mode) 
      endif 
      degree = 57.29577951308232D0 
      if (lxyz) degree = 1.D0 
      blank = ' ' 
      if (mode /= 1 .and. nat(1) /= 0) then
        do i = 1, numat
          if (Abs(ams(nat(i)) - atmass(i)) > 1.d-6) exit
        end do
        isotopes = (i <= numat)
      end if
      cart = .true.
      do i = 1, natoms
        if (na(i) > 0) cart = .false.
      end do
      if (mode > 1 .and. ncomments > 0) &
          write(iprt,"(a)")(all_comments(i)(:len_trim(all_comments(i))), i = 1, ncomments)
      fmt1  = "13.8"
      fmt23 = "13.7"
      if (cart) then 
        fmt23 = "13.8"
        if (mode == 1) then
          if (maxtxt == 0) then
            i = 6
          else
            i = maxtxt/2 + 1
          end if
          write (iprt,*) &
             & "  ATOM "//blank(:maxtxt/2 + 1)//" CHEMICAL  "//blank(:i)//"  X               Y               Z"
          write (iprt,'(a)') " NUMBER    SYMBOL" & 
          &//blank(:i)//"(ANGSTROMS)     (ANGSTROMS)     (ANGSTROMS)"
          write (iprt,*)
        else if (mode > 0) then         
          call wrttxt (iprt)
        end if
      else if (mode == 1) then
        write (iprt,*) &
           & " ATOM"//blank(:maxtxt/2 + 1)//" CHEMICAL "//blank(:maxtxt/2 + 1)//" BOND LENGTH    BOND ANGLE    TWIST ANGLE "
        write (iprt,*) &
           & "NUMBER    SYMBOL   "//blank(:maxtxt/2 + 1)//"(ANGSTROMS)    (DEGREES)     (DEGREES) "
        write (iprt, "(A)") &
           & "   (I)      "//blank(:maxtxt + 2)//"   NA:I          NB:NA:I    " // &
           & "   NC:NB:NA:I " // "      NA    NB    NC "
      else if (mode > 0) then
        call wrttxt (iprt)
      end if 
      if (mode/=1 .and. allocated(p)) then 
        call chrge (p, q2)      
        q2(:numat) = tore(nat(:numat)) - q2(:numat) 
      else 
        q2(:numat) = 0.D0 
      endif 
      n = 1 
      ia = loc(1,1) 
      ii = 0 
      do i = 1, natoms 
        do j = 1, 3 
          q(j) = flag0 
          if (ia /= i) cycle  
          if (j/=loc(2,n)) cycle  
          q(j) = flag1 
          n = n + 1 
          ia = loc(1,n) 
        end do         
        if (na(i) > 0) then
          w = geo(2,i)*degree 
          x = geo(3,i)*degree  
!
!  CONSTRAIN ANGLE TO DOMAIN 0 - 180 DEGREES
!
          w = w - aint(w/360.D0)*360.D0 
          if (w < -1.d-6) w = w + 360.D0 
          if (w > 180.000001D0) then 
            x = x + 180.D0 
            w = 360.D0 - w 
          endif 
!
!  CONSTRAIN DIHEDRAL TO DOMAIN -180 - 180 DEGREES
!
          x = x - aint(x/360.D0 + sign(0.5D0 - 1.D-9,x) - 1.D-9)*360.D0 
        else
          w = geo(2,i) 
          x = geo(3,i)  
        endif 
        if (latom == i) q(lparam) = flagn 
        if ( .not. gui .and. latom1 == i) q(lpara1) = flagn 
        if (latom2 == i) q(lpara2) = flagn 
        if (gui) then
          blank = elemnt(labels(i))//"("//txtatm(i)(3:)//'  '
          igui = -10
        else
          blank = elemnt(labels(i))//txtatm(i)//'  '
          igui = 0
        end if
        j = index(blank,")")
        if (j /= 0) blank(j + 1:) = " "
        if (labels(i)/=99 .and. labels(i)/=107) ii = ii + 1 
        if (labels(i) == 1) then  ! Check for Deuterium and tritium
          if (Abs(atmass(ii) - 2.014d0) < 1.d-3) blank(1:2) = " D" 
          if (Abs(atmass(ii) - 3.016d0) < 1.d-3) blank(1:2) = " T" 
        end if
        k = 0
        if (mode /= 1 .and. ii > 0 .and. nat(1) /= 0) then 
          if (Abs(ams(nat(ii)) - atmass(ii)) > 1.d-6) then
            j = int(log10(atmass(ii)))
            line(40:) = "(f8."//char(6 - j + ichar("0"))//")"
            write(line,line(40:))atmass(ii)
            do j = len_trim(line), 1, -1
              if (line(j:j) /= "0") exit
              line(j:j) = " "
            end do
            j = index(blank,"(")
            if (j == 0) then
              blank(len_trim(blank) + 1:) = line
            else
              blank(j:) = line(:len_trim(line))//blank(j:)
            end if
          end if
          j = max(4,maxtxt + 2) 
          if (isotopes) j = j + 8
          k = max(0,8 - j) 
        else           
          if (mode /= 1) then
            j = max(4,maxtxt + 2) 
          else
            j = max(9,maxtxt + 3) 
          end if
        endif 
        if (labels(i) == 0) cycle  
        if ( .not. l_atom(i)) cycle
        if (na(i) == igui .or. cart) then 
          if (mode /= 1) then !  Print suitable for reading as a data-set
            if (labels(i)/=99 .and. labels(i)/=107) then 
              write (iprt, '(1X,A,F'//fmt1//',1X,A2,F'//fmt23//',1X,A2,F'//fmt23//',1X,A2,A,F8.4)')&
                 blank(:j), geo(1,i), q(1), w, q(2), x, q(3), blank(41:59+k), &
                q2(ii) 
            else 
              write (iprt, '(1X,A,F'//fmt1//',1X,A2,F'//fmt23//',1X,A2,F'//fmt23//',1X,A2,a)') &
              blank(:j), geo(1,i), q(1), w, q(2), x, q(3), " "
            endif 
          else !  Print in output style
            write (iprt, '(I6,6X,A,F'//fmt1//',1X,A2,F'//fmt23//',1X,A2,F'//fmt23//',1X,A2)') i&
              , blank(:j), geo(1,i), q(1), w, q(2), x, q(3) 
          endif 
        else 
          if (mode /= 1) then  !  Print suitable for reading as a data-set
            if (labels(i)/=99 .and. labels(i)/=107) then 
              write (iprt, &
                '(1X,A,F'//fmt1//',1X,A2,F'//fmt23//',1X,A2,F'//fmt23//',1X,A2,3I6,A,F8.4)') &
                & blank(:j), geo(1,i), q(1), w, q(2), x, q(3), na(i), nb(i), nc(i), &
                blank(41:41+k), q2(ii) 
            else 
              write (iprt, '(1X,A,F'//fmt1//',1X,A2,F'//fmt23//',1X,A2,F'//fmt23//',1X,A2,3I6)') &
                blank(:j), geo(1,i), q(1), w, q(2), x, q(3), na(i), nb(i), nc(i) 
            endif 
            else !  Print in output style
            write (iprt, &
              '(I6,6X,A,F'//fmt1//',1X,A2,F'//fmt23//',1X,A2,F'//fmt23//',1X,A2,I6,2I6)') i, &
              blank(:j), geo(1,i), q(1), w, q(2), x, q(3), na(i), nb(i), nc(i) 
          endif 
        endif 
      end do 
      if (mode == 1) return  
      write (iprt, *) 
      if (ndep /= 0) then  
!
!   OUTPUT SYMMETRY DATA.
!
        n = 1
        i = 1
        outer_loop: do
          j = i
          do
            if (j == ndep) exit outer_loop
             !
             !  Group together symmetry functions of the same type
             !  (same reference atom, same reference function, same multiplier,
             !   if function 18 or 19)
             !  (Maximum number of dependent atoms on a line: 9)
             !
            if (locpar(j) /= locpar(j+1) .or. idepfn(j) /= idepfn(j+1) .or. &
                 & j-i >= 9) exit
            if (idepfn(i) == 18 .or. idepfn(i) == 19) then
              if (Abs(depmul(n) - depmul(n+1)) > 1.d-10) exit
              n = n + 1
            end if
            j = j + 1
          end do
          if (idepfn(i) == 18 .or. idepfn(i) == 19) then
            write (iprt, "(I4,I3,F13.9,10I5)") locpar (i), idepfn (i), &
                 & depmul(n), (locdep(k), k=i, j)
            n = n + 1
          else
            write (iprt, "(I4,I3,10I5)") locpar (i), idepfn (i), &
                 & (locdep(k), k=i, j)
          end if
          i = j + 1
        end do outer_loop
        if (idepfn(i) == 19 .or. idepfn(i) == 18) then
          write (iprt, "(I4,I3,F13.9,10I5)") locpar (i), idepfn (i), &
               & depmul(n), (locdep(k), k=i, j)
        else
          write (iprt, "(I4,I3,10I5)") locpar (i), idepfn (i), &
               & (locdep(k), k=i, j)
        end if
        write (iprt,*)
      end if
      if (index(keywrd, " SETPI") /= 0) then
!
!  The user has supplied some pi bonds.  Write them out.
!
        do n = 1, numat
          i = pibonds(n,1)
          if (i == 0) exit
          j = pibonds(n,2)     
          if (n == 1) then
            write(iprt,"(2i6, a)")i,j, "  User-supplied pi bonds"
          else
            write(iprt,"(2i6)")i,j
          end if            
        end do
      end if
      end subroutine geout 
