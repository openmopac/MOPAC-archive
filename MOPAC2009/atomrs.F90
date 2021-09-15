subroutine atomrs (lused, ioptl, ires, n1, io, uni_res, first_res)
    use molkst_C, only: natoms, numat, line
    use chanel_C, only: iw
    use MOZYME_C, only: nbackb, jatom, iatom, k, loop, mxeno, txeno, &
       & nxeno,  allres, maxres,  ions, at_res, res_start, &
       tyres, size_mres
    use common_arrays_C, only : txtatm, nat, ibonds, nbonds
    use elemts_C, only: elemnt, atom_names
    implicit none
    integer, intent (in) :: io, n1, uni_res
    integer, intent (inout) :: ires
    logical, dimension (numat), intent (inout) :: ioptl
    integer, dimension (natoms), intent (inout) :: lused
    logical, intent (in) :: first_res
    integer, parameter :: natomr = 500
!
    character :: letter
    integer :: i2
    integer :: ibad = 0
    integer :: i, i3, ico, ihcr, j, j2, j3, j4, j5, j6, j7, OXT, &
      j8, j9, j10, jofco, l, ninres, nlive, nh, jj3, extra_atoms(2)
    character (len=3) :: loc_tyres
    integer, dimension (500) :: live
    integer, dimension (natomr) :: inres
    integer, dimension (natomr) :: npack  
    integer, dimension (5, size_mres) :: mres
    logical :: odd_h, okay
    intrinsic Abs, Max
    logical, external :: peptide_n
    data inres / natomr * 0 /
! No. of        C  N  O  S  H    C  N  O  S  H     C  N  O  S  H
    data mres / 2, 1, 1, 0, 3,   3, 1, 1, 0, 5,    5, 1, 1, 0, 9, & !  GLY ALA VAL
                6, 1, 1, 0,11,   6, 1, 1, 0,11,    3, 1, 2, 0, 5, & !  LEU ILE SER
                4, 1, 2, 0, 7,   4, 1, 3, 0, 5,    4, 2, 2, 0, 6, & !  THR ASP ASN
                6, 2, 1, 0,12,   5, 1, 3, 0, 7,    5, 2, 2, 0, 8, & !  LYS GLU GLN
                6, 4, 1, 0,12,   6, 3, 1, 0, 7,    9, 1, 1, 0, 9, & !  ARG HIS PHE
                3, 1, 1, 1, 5,  11, 2, 1, 0,10,    9, 1, 2, 0, 9, & !  CYS TRP TYR
                5, 1, 1, 1, 9,   6, 2, 1, 0, 7,    5, 1, 2, 0, 9, & !  MET PRO PRO
                5, 1, 2, 0, 9,   0, 0, 0, 0, 0/                     !  PRO
    odd_h = .true.
    ninres = 0
   !             NBACKB(1) =  Carbon CHR  of N-CHR-CO-N'
   !             NBACKB(2) =  Carbon CO   of N-CHR-CO-N'
   !             NBACKB(3) =  Oxygen CO   of N-CHR-CO-N'
   !             NBACKB(4) =  Nitrogen N' of N-CHR-CO-N'
    ihcr = nbackb(1)
    ico = nbackb(2)
    jofco = nbackb(3)
    jatom = nbackb(4)
   !
   !   Starting with the nitrogen, work out the atoms in the residue.
   !
    nlive = nbonds(iatom)
  !  ioptl(iatom) = .true.
    if (ires == 65) then
      j3 = 0
    end if
    j3 = 0
    j4 = 0
    extra_atoms = 0
    do i2 = 1, nlive
      j2 = ibonds(i2, iatom)
      if (nat(j2) == 1 .or. j2 == ihcr) then
 !
 ! Select atoms that are definitely in the residue
 !
        j3 = j3 + 1
        live(j3) = j2
      else
 !
 ! Store list of atoms that are probably not in the residue. (Think of proline)
 !
        j4 = j4 + 1
        extra_atoms(j4) = j2
      end if
    end do
    nlive = j3
    outer_loop: do
      l = live(1)
      if (l == 0) go to 1100
      if (l == jatom .or. ioptl(l) .or. l == iatom .or. peptide_n(l)) then
         !
         !   Atom L is either already known to be in the residue, or is known
         !   to not be in the residue.
         !
        if (nlive == 0) go to 1100
        live(1) = live(nlive)
        nlive = nlive - 1
      else
        ioptl(l) = .true.
        ninres = ninres + 1
        if (ninres > natomr) then
          write (iw, "(A,I4,A,I4)") " There are more than", natomr, &
         & " atoms in residue", ires
          write (iw,*) " Atoms in residue", ires
          write (iw, "(10(1X,A2,I5))") (elemnt(nat(inres(l))), &
               & inres(l), l=1, natomr)
          call mopend ("Too many atoms in residue")
          go to 1000
        else
            !
            !  Assign atom L to the residue.
            !
          inres(ninres) = l
          if (nbonds(l) /= 0) then
               !
               !  THERE IS AT LEAST ONE ATOM ATTACHED TO THE 'LIVE' ATOM
               !
            loop1: do i2 = 2, nbonds(l)
              j = ibonds(i2, l)
              do i3 = 1, nlive
                if (live(i3) == j) cycle loop1
              end do
              nlive = nlive + 1
                  !
                  !   Panic if number of live atoms is greater than 500
                  !
              if (nlive > 500) exit outer_loop
              live(nlive) = j
            end do loop1
            live(1) = ibonds(1, l)
          else
            if (nlive == 0) go to 1100
            live(1) = live(nlive)
            nlive = nlive - 1
          end if
        end if
      end if
    end do outer_loop
    write (iw, "(A,I4,A)") " Number of live atoms in residue", ires, &
                         & " greater than 500"
    write (iw, "(A)") " Live atoms in residue"
    do i = 1, nlive - 1
      write (iw, "(I5,I5,3X,A2)") i, live (i), elemnt (nat(live(i)))
    end do
    call mopend ("More than 500 atoms in residue")
1000 if (nat(io) == 8) then
      write (iw, "(10X,A,I5,A,3(/10X,A))") "Oxygen atom", io, &
     & " was incorrectly assumed to be in the backbone ", &
     & "of a residue.  To correct the fault, please move this atom", &
     & "to the end of the data set."
    end if
    return
   !
   !   Check that atoms are not counted twice
   !
1100 do j = 1, ninres
      do k = j + 1, ninres
        if (inres(j) == inres(k)) then
          inres(j) = 0
        end if
      end do
    end do
    npack(:) = 0
!
!  If the starting atom of the residue was a nitrogen, then count it 
!
    if (nat(iatom) == 7) npack(7) = 1
    jj3 = 0
    do i2 = 1, ninres
      if (inres(i2) /= 0) then
        j2 = inres(i2)
!
!  Test for strange atoms
!
        do j4 = 1, nbonds(j2)
          j5 = nat(ibonds(j4,j2))
          select case (j5)
          case (1,6:8,16)
          case default
            jj3 = j5
          end select
        end do        
        npack(nat(j2)) = npack(nat(j2)) + 1
      end if
      lused(inres(i2)) = ires
    end do
    lused(iatom) = -ires
    lused(ihcr) = -ires
    if (ico >0) lused(ico) = -ires
    if (jofco /= 0) then
      lused(jofco) = -ires
    end if
   !
   !   Determine the residue name (it must be one of the 21 amino acids,
   !   or an amino acid plus something attached.
   !
   OXT = 0
   if (ico > 0) then
!
!  Check for terminal -COOH  If present, delete one oxygen from formula count.
!
      k = 0
      do j = 1, nbonds(ico)
        if (nat(ibonds(j,ico)) == 8) then
          k = k + 1
          if (OXT == 0) OXT = ibonds(j,ico)
          if (nbonds(ibonds(j,ico)) == 2) OXT = ibonds(j,ico)
        end if
      end do
    end if
    if (k == 2) then
      npack(8) = npack(8) - 1
    else
      OXT = 0
    end if
    nh = npack(1)  !  Store number of hydrogen atoms in the residue
    k = 0
    if (ires == 139) then
      k = 0  ! use in debugging only
    end if
    if (jj3 == 0) then     
      do loop = 1, mxeno
        npack(1) = npack(6) - nxeno(1, loop)
        npack(2) = npack(7) - nxeno(2, loop)
        npack(3) = npack(8) - nxeno(3, loop)
        npack(4) = npack(16) - nxeno(4, loop)
        if (iatom == jatom) then
          npack(3) = Max (0, npack(3)-1)
        end if
        loop2: do j = 1, size_mres - 1
          do k = 1, 4
            if (npack(k) /= mres(k, j)) cycle loop2
          end do
          if (j == 3) then
  !
  !  Check that the side chain involves isopropyl.  If so, it's valine
  !
  !
            j4 = 0
            do k = 1, nbonds(ihcr)
              l = 0
              j2 = ibonds(k,ihcr)
              if (nat(j2) == 6) then
                do j3 = 1, nbonds(j2)
                  if (nat(ibonds(j3,j2)) == 6) l = l + 1
                end do
              end if
              j4 = max(j4,l) 
            end do
            if (j4 == 2) then
  !
  ! No, it's not valine.  Check for terminal proline
  !
              okay = .false.
              proline_check: do k = 1, nbonds(ihcr)
                if (nat(ibonds(k,ihcr)) == 7) then
  !
  ! Found the nitrogen
  !
                  j2 = ibonds(k,ihcr)
                  do j3 = 1, nbonds(j2)
                    if (nat(ibonds(j3,j2)) == 6 .and. ibonds(j3,j2) /= ihcr ) then
  !
  ! Found a C attached to N, and the C is not the C of -CHR-
  !
                      j4 = ibonds(j3,j2)
                      do j5 = 1,nbonds(j4)
                        if (nat(ibonds(j5,j4)) == 6) then
                          j6 = ibonds(j5,j4)
                          do j7 = 1, nbonds(j6)
                            if (nat(ibonds(j7,j6)) == 6) then
  !
  ! Found the C of N-C-C
  !
                              j8 = ibonds(j7,j6)
                              do j9 = 1, nbonds(j8)
                                do j10 = 1, nbonds(ihcr)
                                  if (ibonds(j9,j8) == ibonds(j10,ihcr)) then
                                    okay = .true.
                                    exit proline_check
                                  end if
                                end do
                              end do
                            end if
                          end do
                        end if
                      end do
                    end if                  
                  end do
                end if
              end do proline_check
              if (okay) then
                k = 20
              else
                cycle loop2
              end if
            end if
          end if
          k = j
          go to 1200
        end do loop2
        j = 0
        if (iatom == jatom) then
           !
           !   SPECIAL CASE:  THE -COOH END OF THE PROTEIN IS ONLY -C=O
           !
          k = 1
          npack(3) = npack(3) + 1
        else if (jatom == 0) then
           !
           !   SPECIAL CASE:  SINGLE AMINO ACID, THEREFORE MAKE END -COO
           !
          k = 1
          npack(3) = npack(3) - 1
        end if
        if (k /= 0) then
           !
           !   CHECK ALL SPECIAL CASES
           !
          loop3: do k = 1, size_mres - 1
            do j = 1, 4
              if (npack(j) /= mres(j, k)) cycle loop3
            end do
             if (k == 3) then
              j4 = 0
              do j = 1, nbonds(ihcr)
                l = 0
                j2 = ibonds(j,ihcr)
                do j3 = 1, nbonds(j2)
                  if (nat(ibonds(j3,j2)) == 6) l = l + 1
                end do
                j4 = max(j4,l) 
              end do
              if (j4 /= 3) cycle loop3
            end if
            go to 1200
          end do loop3
        end if
      end do
    else
      do j = 1, 10
        if (atom_names(jj3)(j:j) /= " ") exit
      end do
      write(iw,"(a,i5,a)")"         Residue:",ires," contains "//atom_names(jj3)(j:)//" so identification not done"
      loop = 1
      goto 1200
    end if
    if (npack (6) /= 0 .or. npack (7) /= 1 .or. npack (8) /= 0 .or. npack (16) /= 0) then
      if (ibad <= 20) then
        ibad = ibad + 1
        write (iw, "(A,I5,A,I6,4X,4(A,I2),A)") " Unknown residue:", &
       & ires, " atom", jatom, "C:", npack (6), " N:", npack (7), &
       & " O:", npack (8), " S:", npack (16)
       write(line,"(a,4(i2,a))")" (Use XENO pattern '(",npack (6) - 2, ",", npack (7) - 1, &
       & ",", npack (8) - 1, " ,", npack (16),",RES)"
       do j = 21, len_trim(line)
       if (line(j:j) == " ") line(j:) = line(j + 1:)//" "
       end do
       write(iw,'(a)')trim(line)//"' to specify this residue)"
       write(iw,'(a)')" (This will convert the XENO group into a modified glycine, and give it the name RES.)"
       if (ibad == 1) call web_message(iw,"xeno.html")
       odd_h = .true.
      else if (ibad == 21) then
        ibad = 22
        write (iw,*) " REMAINING ERRORS SUPPRESSED"
      end if
    else
      ioptl(jatom) = .true.
      ires = ires - 1
      return
    end if
1200 if (k == 0) then
       k = size_mres 
     end if
    if (k == 4) then
      !
      !   RESIDUE IS EITHER LEU OR ILE.  TO DISTINGUISH BETWEEN THEM,
      !   CHECK THE NUMBER OF CARBON ATOMS ATTACHED TO THE CARBON
      !   OF THE SIDE-CHAIN.  IF IT IS 3 THEN ILE, ELSE LEU.
      !
      do i2 = 1, nbonds(iatom)
        j = ibonds(i2, iatom)
         !
         !  IS ATOM ATTACHED TO NITROGEN A CARBON?
         !
        if (nat(j) == 6) then
          loop4: do j2 = 1, nbonds(j)
            j3 = ibonds(j2, j)
               !
               !  IS THE ATOM ATTACHED TO THE CARBON THAT IS ATTACHED 
               ! TO THE NITROGEN A CARBON?
               !
            if (nat(j3) == 6) then
              l = 0
              do j4 = 1, nbonds(j3)
                j5 = ibonds(j4, j3)
                     !
                     !  ARE ALL THE ATTACHED ATOMS, CARBON ATOMS?
                     !
                if (nat(j5) == 6) then
                  l = l + 1
                end if
                if (nat(j5) /= 1 .and. nat(j5) /= 6) cycle loop4
              end do
              if (l == 3) then
                k = 5
              end if
            end if
          end do loop4
        end if
      end do
    else if (k == 3) then
      !
      !    RESIDUE IS EITHER VAL OR PRO
      !
      !
      !  Check for both start of chain and (middle or end) of chain
      !
      l = 0
      do j = 1, nbonds(iatom)
        if (nat(ibonds(j, iatom)) /= 1) then
          l = l + 1
        end if
      end do
      if (l == 3 .and. iatom /= n1 .or. l == 2 .and. iatom == n1) then
        k = 20
      end if
    end if
    if (nat(n1) == 1) then  !  If the starting nitrogen atom is in fact a hydrogen,
      ninres = ninres + 1   !  then add it to the atoms in the residue.
      inres(ninres) = n1
    end if
   
    loc_tyres = tyres(k)
    if (len_trim(txeno(loop)) == 3) loc_tyres = txeno(loop)(:3)
    do j = 1, ninres
      l = inres(j)
      if (l /= 0) then
        letter = "*"
        if (lused(l) >= 0) then
          letter = " "
        end if
        if (ires > 999) then
          write (txtatm(l), "(I6,1X,A3,I4)") l, loc_tyres, ires 
       ! else  if (ires == 0) then
       !   write (txtatm(l), "(I6,1X,A3,A1)") l, loc_tyres, letter
        else
          write (txtatm(l), "(I6,1X,A3,A1,I3)") l, loc_tyres, letter, ires
        end if
        
        at_res(l) = uni_res
      end if
    end do
    res_start(uni_res) = iatom
    if (k /= 23) then
      call greek (inres, ninres, jatom)
    else
      do i = 1, ninres
        txtatm(inres(i))(16:17) = " "
      end do
    end if
    if (k == 20) then            !  Check that N of proline is present
      do i = 1, ninres            !  If it is, and the nitrogen is in the
        if (inres(i) == jatom) exit    !  residue, remove the label
      end do
      if (i == ninres + 1 .and. jatom > 0) then
        txtatm(jatom) = " "
      end if
    end if 
    if (txtatm(iatom)(:15) /= " ") then
      allres (ires) = loc_tyres // " "
      return
    end if
    if (ires == 1 .and.  txtatm(ihcr)(8:10) == "PRO") then
      write(txtatm(n1), "(I6,1X,A3, i4)") n1, "PRO", 1  ! Label N of proline in
    end if 
    if (OXT /= 0) txtatm(OXT)(16:17) = "TX"
    if (extra_atoms(1) > 0) then
!
! Assign labels to atoms that are associated with this residue, but call them "UNK"
!
      live(:2) = extra_atoms(:2)
      nlive = 1   
      if (live(2) /= 0) nlive = 2 
      ninres = 0
      outer_loop_extra: do
        l = live(1)
        if (l == jatom .or. ioptl(l) .or. l == iatom) then
           !
           !   Atom L is either already known to be in the residue, or is known
           !   to not be in the residue.
           !
          if (nlive == 0) go to 1101
          live(1) = live(nlive)
          nlive = nlive - 1
        else
          ioptl(l) = .true.
          ninres = ninres + 1
          if (ninres > natomr) then
            write (iw, "(A,I4,A,I4)") " There are more than", natomr, &
           & " atoms in residue", ires
            write (iw,*) " Atoms in residue", ires
            write (iw, "(10(1X,A2,I5))") (elemnt(nat(inres(l))), &
                 & inres(l), l=1, natomr)
            call mopend ("Too many atoms in residue")
            go to 1000
          else
              !
              !  Assign atom L to the residue.
              !
            inres(ninres) = l
            if (nbonds(l) /= 0) then
                 !
                 !  THERE IS AT LEAST ONE ATOM ATTACHED TO THE 'LIVE' ATOM
                 !
              loop1_extra: do i2 = 2, nbonds(l)
                j = ibonds(i2, l)
                do i3 = 1, nlive
                  if (live(i3) == j) cycle loop1_extra
                end do
                nlive = nlive + 1
                    !
                    !   Panic if number of live atoms is greater than 500
                    !
                if (nlive > 500) exit outer_loop_extra
                live(nlive) = j
              end do loop1_extra
              live(1) = ibonds(1, l)
            else
              if (nlive == 0) go to 1101
              live(1) = live(nlive)
              nlive = nlive - 1
            end if
          end if
        end if
      end do outer_loop_extra
1101  do j = 1, ninres
        l = inres(j)
        if (l /= 0) then
          if (ires > 999) then
            write (txtatm(l), "(I6,1X,A3,I4)") l, "UNK", ires 
          else
            write (txtatm(l), "(I6,1X,A3,A1,I3)") l, "UNK", " ", ires
          end if
        end if
      end do
    end if
                                                 ! residue 1 "by hand"
    letter = "*"
    if (lused(iatom) >= 0) then
      letter = " "
    end if
    if (Abs(mres(5,k) - nh) > 1 .and. .not. first_res .and. loop == 1) then
      if(odd_h .and. k /= size_mres) then
        odd_h = .false.
        write(iw,"(/a, a,/)")"         Residues where the number of hydrogen", &
                       " atoms is not equal to that expected"
        write(iw,"(a)")"                      atom number of ", &
                       "      Residue number  an atom in res.   Type     No of extra hydrogen atoms"
      end if
      if (k /= size_mres) write(iw,"(10x, i4,i17,9x,a,i17)")ires, iatom, loc_tyres, nh - mres(5,k) 
    end if 
    if (ires > 999) then
      write (txtatm(iatom), "(I6,1X,A3,I4)") iatom, loc_tyres, ires 
    else
      write (txtatm(iatom), "(I6,1X,A3,A1,I3)") iatom, loc_tyres, letter, ires
    end if   
    at_res(iatom) = uni_res
  !  res_start(uni_res) = iatom
    if (ires > maxres) then
      write (iw,*) " MODIFY SUBROUTINE NAMES"
      write (iw,*) " MAXIMUM NUMBER OF RESIDUES ALLOWED:", maxres
      call mopend ("Too many residues")
    end if
    allres (ires) = loc_tyres // " "
    do i = 1, numat
      if (ions(i) == 1 .and. Abs (lused(i)) == ires) then
        allres (ires) (4:4) = "+"
      end if
    end do
    j = 0
    l = 0 
    do i = 1, numat
      if (ions(i) > 0 .and. Abs (lused(i)) == ires) j = j + ions(i)
      if (ions(i) < 0 .and. Abs (lused(i)) == ires) l = l + ions(i)
    end do
    if (j /= 0 .or. l /= 0) then
      if (j + l == 0) then
        if (j == 1) write (iw, "(8X,2A,I4,A)") " Residue:  '", allres(ires)(1:3), &
               & ires, "' is Zwitterionic"
      else if (j > 0) then
        allres (ires) (4:4) = "+"
      else
        allres (ires) (4:4) = "-"
      end if
    end if
    return
  end subroutine atomrs
  logical function peptide_n(l)
    use common_arrays_C, only : nat, ibonds, nbonds
    implicit none
    integer :: l
    integer :: i, j, k, nc, nh, nco
    if (nat(l) /= 7) then
      peptide_n = .false.
      return
    else
!
!  "l" is a nitrogen.  Check that it has three bonds, two to carbon and one to hydrogen,
!  and that one carbon is attached to an oxygen
!
      if (nbonds(l) /= 3) then
        peptide_n = .false.
        return
      end if
      nc = 0
      nh = 0
      nco = 0
      do i = 1, 3
        if (nat(ibonds(i,l)) == 6)then
          nc = nc + 1
          k = ibonds(i,l)
          if (nbonds(k) == 3) then
            do j = 1, 3
              if (nat(ibonds(j,k)) == 8) then
                if (nbonds(ibonds(j,k)) == 1) then
                  nco = nco + 1
                else
                  peptide_n = .false.         ! Structure N-C-O-R detected
                  return
                end if
              end if
            end do
          else if (nbonds(k) == 4) then
            do j = 1, 4
              if (nat(ibonds(j,k)) == 8) then ! Structure N-C-O-R detected
                peptide_n = .false.
                return                
              end if
            end do
          end if
        end if
        if (nat(ibonds(i,l)) == 1) nh = nh + 1
      end do
      if (nc /= 2 .or. nh /= 1 .or. nco /= 1) then
        peptide_n = .false.   ! Structure C-NH-C=O not detected
        return
      end if
!
!  "l" is a nitrogen.  Check that it has three bonds, two to carbon and one to hydrogen
!
      peptide_n = .true.
      return
    end if
  end function peptide_n
