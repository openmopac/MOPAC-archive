  subroutine lyse 
!
!  Before a residue sequence can be worked out, all inter-protein bonds
!  must first be broken.  These bonds are of type N-S, O-S, and S-S
!
    use common_arrays_C, only : nat, nbonds, ibonds
    use molkst_C, only : numat
    implicit none
    integer :: i, j, k, l, ll
    logical :: special 
   !
   !   FIRST, GET RID OF H ATOMS AND ATOMS ALREADY DEFINED
   !
    do i = 1, numat
      if (nat(i) /= 1) then
        if (nat(i) == 6) then
          l = 0
          do k = 1, nbonds(i)
            l = l + 1
            ibonds(l, i) = ibonds(k, i)
          end do
        else
          special = .false.
          if (nat(i) == 8) then
!
!  Atom is oxygen - check to see if it's attached to a sulfate or phosphate.
!  If so, do NOT lyse the bond.
!
            do k = 1, nbonds(i)
              if (nat(ibonds(k, i)) == 16 .or. nat(ibonds(k, i)) == 15) then
                if (nbonds(ibonds(k,i)) == 4) special = .true.
              end if
            end do
          end if
          if (special) then
            l = nbonds(i)
            cycle
          end if
            !
            ! EXCLUDE ALL N-S, O-S, and S-S BONDS
            !
          l = 0
          do k = 1, nbonds(i)
            if (nat(ibonds(k, i)) /= 16) then
              l = l + 1
              ibonds(l, i) = ibonds(k, i)
            end if
          end do
            !
            ! EXCLUDE ALL BONDS TO NON-PROTEIN ATOMS
            !
          ll = l
          l = 0
          do k = 1, ll
            j = ibonds(k, i)
            if (nat(j) == 1 .or. nat(j) == 6 .or. nat(j) == 7 .or. nat(j) == 8 &
           & .or. nat(j) == 16) then
              l = l + 1
              ibonds(l, i) = ibonds(k, i)
            end if
          end do
        end if
        nbonds(i) = l
      end if
    end do
  end subroutine lyse
