subroutine reseq (iopt, lused, n1, new, io)
    use molkst_C, only: numat, natoms
    use chanel_C, only: iw
    use elemts_C, only: elemnt
    use common_arrays_C, only : nat, ibonds, nbonds
    implicit none
    integer, intent (in) :: n1
    integer, intent (inout) :: new
    integer, intent (out) :: io
    logical, dimension (numat), intent (inout) :: iopt
    integer, dimension (natoms), intent (inout) :: lused
!
    integer, parameter :: natomr = 1000
    integer :: i, i2, i3, iatom, ico, ihcr, ires_loc, j, j2, jatom, k, l, &
   & ninres, nlive
    integer, dimension (4) :: nbackb
    integer, dimension (50) :: live
    integer, dimension (natomr) :: inres
    logical, external :: peptide_n
!
    nbackb = 0
    inres = 0
   !
   !  Break all intra-chain bonds, so that the residues can easily be
   !  identified.
   !
    call lyse ! Break all S-S bonds
   !
   !   WORK OUT THE BACKBONE
   !
    iatom = n1
    do ires_loc = 1, 100000
      call nxtmer (iatom, nbackb)
      ihcr = nbackb(1)
      ico = nbackb(2)
      io = nbackb(3)
      jatom = nbackb(4)
      !
      !    STORE INDICES FOR  C (OF -HCR-) AND C (OF -CO-) in 'N-C(H,R)-C(=O)'
      !
      if (iopt(iatom) .or. iopt(ihcr) .or. iopt(ico)) then
        l = 0
        j = 0
        io = 0
        if (iatom /= 0 .and. iatom == jatom) then
          !
          !  Not a residue, but there is something attached to the N at the END
          !  of the protein (at the COO end), so turn off everything to do with
          !  the backbone, and treat it as a xeno-group.
          !
          jatom = 0
          ico = 0
          io = 0
          ihcr = 0
          if ( .not. iopt(iatom)) then
            iopt(iatom) = .true.
            new = new + 1
            lused(new) = iatom
          end if
        end if
      else if (io == 0) then
        lused(new+1) = iatom
        lused(new+2) = ihcr
        lused(new+3) = ico
        iopt(iatom) = .true.
        iopt(ihcr) = .true.
        iopt(ico) = .true.
        new = new + 3
        l = ihcr
        j = ico
      else if (iopt(io)) then
        l = 0
        j = 0
        io = 0
        if (iatom /= 0 .and. iatom == jatom) then
          !
          !  Not a residue, but there is something attached to the N at the END
          !  of the protein (at the COO end), so turn off everything to do with
          !  the backbone, and treat it as a xeno-group.
          !
          jatom = 0
          ico = 0
          io = 0
          ihcr = 0
          if ( .not. iopt(iatom)) then
            iopt(iatom) = .true.
            new = new + 1
            lused(new) = iatom
          end if
        end if
      else
        lused(new+1) = iatom
        lused(new+2) = ihcr
        lused(new+3) = ico
        lused(new+4) = io
        iopt(iatom) = .true.
        iopt(ihcr) = .true.
        iopt(ico) = .true.
        iopt(io) = .true.
        new = new + 4
        l = ihcr
        j = ico
      end if
      !
      !  The residue is attached to IATOM and does not involve JATOM
      !
      !  NOW TO WORK OUT THE ATOMS IN THE RESIDUE
      !
      nlive = nbonds(iatom)
      ninres = 0
      do i2 = 1, nlive
        live(i2) = ibonds(i2, iatom)
      end do
      if (l /= 0) then
        do i2 = 1, nbonds(l)
          live(nlive+i2) = ibonds(i2, l)
        end do
        nlive = nlive + nbonds(l)
        do i2 = 1, nbonds(j)
          live(nlive+i2) = ibonds(i2, j)
        end do
        nlive = nlive + nbonds(j)
      end if
      do
        l = live(1)
        if (l == jatom .or. iopt(l) .or. l == iatom .or. peptide_n(l)) then
          if (nlive == 0) exit
!
!  Pick the lowest numbered atom
!
          i2 = 1000000
          do i = 1, nlive
            if (i2 > live(i)) then
              i3 = i
              i2 = live(i)
            end if
          end do
          live(1) = live(i3)
          nlive = nlive - 1
          do i = i3, nlive
            live(i) = live(i + 1)
          end do          
        else
          iopt(l) = .true.
          ninres = ninres + 1
          if (ninres > natomr) then
            write (iw, "(A,I4,A,I4)") " There are more than", natomr, &
           & " atoms in residue", ires_loc
            write (iw,*) " Atoms in residue", ires_loc
            write (iw, "(10(1X,A2,I5))") (elemnt(nat(inres(l))), inres(l), &
           & l=1, natomr)
            call mopend ("Too many atoms in residue")
            go to 1000
          else
            inres(ninres) = l
            if (nbonds(l) /= 0) then
                  !
                  !  THERE IS AT LEAST ONE ATOM ATTACHED TO THE 'LIVE' ATOM
                  !
              loop: do i2 = 2, nbonds(l)
                j = ibonds(i2, l)
                do i3 = 1, nlive
                  if (live(i3) == j) cycle loop
                end do
                nlive = nlive + 1
                if (nlive > 50) then
                  call mopend ("Too many atoms in residue")
                  go to 1000
                else
                  live(nlive) = j
                end if
              end do loop
              live(1) = ibonds(1, l)
            else
              if (nlive == 0) exit
              live(1) = live(nlive)
              nlive = nlive - 1
            end if
          end if
        end if
      end do
      !
      !   Check that atoms are not counted twice
      !
      do j = 1, ninres
        do k = j + 1, ninres
          if (inres(j) == inres(k)) then
            inres(j) = 0
          end if
        end do
      end do
      !
      !  Put hydrogen atoms at the end of the list
      !
      k = ninres
      do j = 1, ninres
        l = inres(j)
        if (l /= 0) then
          if (nat(l) == 1) then
            k = k + 1
            if (k > natomr) then
              write(iw,"(a)")"  Too many atoms in residue, system unrecognizable"
              call mopend("  Too many atoms in residue, system unrecognizable")
              return
            end if
            inres(k) = l
            inres(j) = 0
          end if
        end if
      end do
      ninres = k
      if (io == 0 .and. iatom /= 0 .and. .not. iopt(iatom)) then
        new = new + 1
        lused(new) = iatom
        iopt(iatom) = .true.
      end if
      do i2 = 1, ninres
        j2 = inres(i2)
        if (j2 /= 0 .and. j2 /= iatom .and. j2 /= ihcr .and. j2 /= ico) then
          new = new + 1
          lused(new) = j2
        end if
      end do
      j = new
      if (iatom == jatom .or. jatom == 0) go to 1100
      iatom = jatom
    end do
    return
1000 if (nat(io) == 8) then
      write (iw, "(10X,A,I5,A,3(/10X,A))") "Oxygen atom", io, &
     & " was incorrectly assumed to be in the backbone ", &
     & "of a residue.  To correct the fault, please move this atom", &
     & "to the end of the data set."
    end if
    return
1100 if (io == 0) return
   !
   !  SPECIAL CASE - IS THERE A HYDROGEN ATTACHED TO THE FINAL OXYGEN?
   !
    do i = 1, nbonds(io)
      j = ibonds(i, io)
      if ( .not. iopt(j) .and. nat(j) == 1) then
        iopt(j) = .true.
        new = new + 1
        lused(new) = j
      end if
    end do
    return
end subroutine reseq
