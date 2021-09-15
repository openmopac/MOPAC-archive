subroutine ligand (ires)
    use elemts_C, only: elemnt
    use common_arrays_C, only : txtatm, nat, labels, nbonds, ibonds
    use molkst_C, only : natoms, id
    implicit none
    integer :: ires
!
    integer :: i, j
!
    do i = 1, natoms - id
  !    if (i == 3574) then
  !      j = 1
  !    end if
      if (txtatm(i) (14:14) == " ") then
!
!  Residue number is missing, therefore not a residue.
!
         !
         !    Test for specific molecules
         !
        if (labels(i) == 15) then
          if (nbonds(i) == 4) then
            do j = 1, 4
              if (labels(ibonds(j, i)) /= 8) go to 1000
            end do
               !
               !                                 Phosphate
               !
            ires = ires + 1
            write (txtatm(i), "(I6,1X,A3,I4,A3)") i, "PO4", ires, "  J"
            do j = 1, 4
              write (txtatm(ibonds(j, i)), "(I6,1X,A3,I4,A3)") ibonds(j, i), &
                   & "PO4", ires, "  J"
            end do
          end if
        else if (labels(i) == 16) then
          if (nbonds(i) == 4) then
            do j = 1, 4
              if (labels(ibonds(j, i)) /= 8) go to 1000
            end do
               !
               !                                 Sulfate
               !
            ires = ires + 1
            write (txtatm(i), "(I6,1X,A3,I4,A3)") i, "SO4", ires, "  J"
            do j = 1, 4
              write (txtatm(ibonds(j, i)), "(I6,1X,A3,I4,A3)") ibonds(j, i), &
                   & "SO4", ires, "  J"
            end do
          end if
        else if (labels(i) == 8 .and. nbonds(i) /= 1) then
          if (nbonds(i) == 2 .and. nat(ibonds(1, i)) == 1 .and. &
               & nat(ibonds(2, i)) == 1 .or. nbonds(i) == 0) then
               !
               !                                 Water
               !
            ires = ires + 1
            write (txtatm(i), "(I6,1X,A3,I4,A3)") i, "HOH", ires, "  J"
            do j = 1, nbonds(i)
              write (txtatm(ibonds(j, i)), "(I6,1X,A3,I4,A3)") ibonds(j, i), &
                   & "HOH", ires, "  J"
            end do
          end if
        end if
1000    if(nat(i) == 1)then
          write(txtatm(i), "(i6,a11)")i,txtatm(ibonds(1,i))(7:17)
        else
          txtatm (i) (17:17) = "J"
        end if
        if (txtatm(i)(8:10) == " " .and. labels(i) /= 99) then
          txtatm(i) (9:10) = elemnt(nat(i))
        end if
      end if
    end do
   !
   !  LABEL THE HYDROGEN ATOMS
   !
    do j = 1, natoms
   !  if (j == 3517) then
   !     i = 1
   !   end if
      if (labels(j) == 1 .and. txtatm(j) (17:17) == " " .and. &
      txtatm(j)(8:10) /= "UNK") then
        write (txtatm(j), "(I6,A10)") j, txtatm (ibonds(1, j)) (7:)
        txtatm (j) (17:17) = "J"
      else if (txtatm(j)(6:6) == " ") then
        write (txtatm(j)(1:6), "(I6)") j
      end if
    end do
end subroutine ligand
subroutine moiety (iopt, lused, istart, new)
    use molkst_C, only: numat, natoms
    use common_arrays_C, only : nat, nbonds, ibonds
    use chanel_C, only: iw
    use elemts_C, only: elemnt
   !
   !.. Implicit Declarations ..
    implicit none
   !
   !.. Formal Arguments ..
    integer, intent (in) :: istart
    integer, intent (inout) :: new
    logical, dimension (numat), intent (inout) :: iopt
    integer, dimension (natoms), intent (inout) :: lused
   !
   !.. Parameters ..
    integer, parameter :: natomr = 200
   !
   !.. Local Scalars ..
    integer :: i2, i3, iatom, j, j2, k, l, ninbit, nlive
   !
   !.. Local Arrays ..
    integer, dimension (50) :: live
    integer, dimension (natomr) :: inres
   !.. Data Declarations ..
    data inres / natomr * 0 /
   !
   ! ... Executable Statements ...
   !
    iatom = istart
   !
   !   The first atom identified in the moiety is atom IATOM.
   !
    iopt(iatom) = .true.
   !
   !  NOW TO WORK OUT THE ATOMS IN THE MOIETY
   !
    nlive = nbonds(iatom)
    if (nlive == 0) then
      ninbit = 1
      inres(1) = iatom
    else
      ninbit = 0
      do i2 = 1, nlive
        live(i2) = ibonds(i2, iatom)
      end do
      do
        l = live(1)
        if (iopt(l) .or. l == iatom) then
          if (nlive == 0) exit
          live(1) = live(nlive)
          nlive = nlive - 1
        else
          iopt(l) = .true.
          ninbit = ninbit + 1
          if (ninbit > natomr) then
            write (iw, "(A,I4,A)") " There are more than", natomr, &
                 & " atoms in moiety "
            write (iw,*) " Atoms in moiety"
            write (iw, "(10(1X,A2,I5))") (elemnt(nat(inres(l))), inres(l), &
                 & l=1, natomr)
            call mopend ("Too many atoms in moiety")
          end if
          inres(ninbit) = l
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
              live(nlive) = j
            end do loop
            live(1) = ibonds(1, l)
          else
            if (nlive == 0) exit
            live(1) = live(nlive)
            nlive = nlive - 1
          end if
        end if
      end do
    end if
   !
   !   Check that atoms are not counted twice
   !
    do j = 1, ninbit
      do k = j + 1, ninbit
        if (inres(j) == inres(k)) then
          inres(j) = 0
        end if
      end do
    end do
   !
   !  Put hydrogen atoms at the end of the list
   !
    k = ninbit
    do j = 1, ninbit
      l = inres(j)
      if (l /= 0) then
        if (nat(l) == 1) then
          k = k + 1
          inres(k) = l
          inres(j) = 0
        end if
      end if
    end do
    ninbit = k
    if (iatom /= 0) then
      new = new + 1
      lused(new) = iatom
      iopt(iatom) = .true.
    end if
    do i2 = 1, ninbit
      j2 = inres(i2)
      if (j2 /= 0 .and. j2 /= iatom) then
        new = new + 1
        lused(new) = j2
      end if
    end do
end subroutine moiety
