subroutine findn1 (n1, ioptl, io)
!**********************************************************************
!
!  Locate the nitrogen atom of a peptide linkage.
!
!  On input, ioptl is a logical array of atoms which might contain a 
!  peptide linkage.
!
!  On output:
!
!  n1 is the atom-number of the nitrogen atom of the peptide
!  or zero, if a nitrogen could not be found.
! 
!  io is the oxygen of the "C=O" of the peptide linkage.
!
!**********************************************************************

    use molkst_C, only: numat
    use common_arrays_C, only : nat, nbonds, ibonds
    implicit none
    integer, intent (out) :: n1, io
    logical, dimension (numat), intent (in) :: ioptl
    logical :: peptide
!
    integer :: i, iatom, ii, ires_loc, j, k, l
!
    n1 = 0
   !
   !   FIND A N-C-C=O SYSTEM
   !
    outer_loop: do iatom = 1, numat
      if ( .not. ioptl(iatom)) then
         !
         !   START WITH OXYGEN OF C=O
         !
        if (nat(iatom) == 8) then
          do i = 1, nbonds(iatom)
            if (nat(ibonds(i, iatom)) == 6) then
            !
            !    THERE IS A CARBON ATTACHED TO THE OXYGEN
            !
              l = ibonds(i, iatom)
              do k = 1, nbonds(l)
                if (nat(ibonds(k, l)) == 6) then
            !
            !  THERE IS A CARBON ATTACHED TO THE CARBON ATTACHED 
            !  TO THE OXYGEN
            !
                j = ibonds(k, l)
                  do ii = 1, nbonds(j)
                    if (nat(ibonds(ii, j)) == 7 .and. .not. ioptl(ibonds(ii, j))) then
            !
            !  THERE IS A NITROGEN ATTACHED TO THE CARBON ATTACHED TO THE
            !  CARBON ATTACHED TO THE OXYGEN
            !
            !
            !   YES, A PEPTIDE LINKAGE HAS BEEN FOUND.
            !   THE NITROGEN IS N1
            !
                      n1 = ibonds(ii, j)
                      exit outer_loop
                    end if
                  end do
                end if
              end do
            end if
          end do
        end if
      end if
    end do outer_loop
    io = iatom
    if (n1 == 0) return
   !
   !  N1 IS A NITROGEN IN AN O=C-C-N SYSTEMS.
   !
    j = 0
    do i = 1, nbonds(n1)
      if (nat(ibonds(i, n1)) /= 1) then
        j = j + 1
      end if
    end do
    if (j == 1) return
   !
   !  AT THIS POINT, THE SEQUENCE O-C-C-N HAS BEEN IDENTIFIED.
   !  NOW TO MOVE ALONG BACKBONE TO THE START OF THE PROTEIN
   !
   peptide = .false.
    do ires_loc = 1, 1000000
      do i = 1, nbonds(n1)
        j = ibonds(i, n1)
        if (nat(j) == 6) then
          do k = 1, nbonds(j)
            if (nat(ibonds(k, j)) == 8) go to 1000
          end do
          cycle
            !
            !   J IS THE CARBON OF C=O
            !
1000      do k = 1, nbonds(j)
            if (nat(ibonds(k, j)) == 6) then
              peptide = .true.
              go to 1010
            end if
          end do
        end if
      end do
      cycle
1010  j = ibonds(k, j)      
      do k = 1, nbonds(j)
        if (nat(ibonds(k, j)) == 7) go to 1020
      end do
      if (peptide) then
!
!  The residue at the start does not contain a nitrogen atom.
!  So, temporarily, artificially convert a hydrogen into a nitrogen.
!  This will allow the second residue to be correctly identified.
!  (The first residue might be almost correctly identified)
! 
        do k = 1, nbonds(j)
          if (nat(ibonds(k, j)) == 1) n1 = ibonds(k, j)
        end do
      end if
      exit
1020  if (n1 == ibonds(k, j)) exit
      if (.not. ioptl(ibonds(k, j))) n1 = ibonds(k, j)
      j = 0
      do ii = 1, nbonds(n1)
        if (nat(ibonds(ii, n1)) /= 1) then
          j = j + 1
        end if
      end do
      if (j == 1) exit
    end do
end subroutine findn1
