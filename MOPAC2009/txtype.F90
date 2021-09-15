  subroutine txtype (jj, jtype, letter)
!
!  Construct the Greek letter for each atom using
!  the PDB convention.  The letter will go in character 17
!  of the entries in array "txtatm"  If there is also a number,
!  it will go in character 16.
!
    use common_arrays_C, only : txtatm, nat, nbonds, ibonds
    implicit none
    character, intent (in) :: letter
    integer, intent (inout) :: jj
    integer, dimension (jj), intent (inout) :: jtype
!
    integer :: i, j, k, l, loop, m
   !
   !   Remove any duplicates from JTYPE
   !
    j = 1
    outer_loop: do i = 2, jj
      do k = 1, j
        if (jtype(k) == jtype(i)) cycle outer_loop
      end do
      j = j + 1
      jtype(j) = jtype(i)
    end do outer_loop
    jj = j
    do loop = 1, 4
      m = 0
      do i = 1, jj
        if (nat(jtype(i)) /= 1) then
          m = m + 1
          k = i
        end if
      end do
      !
      !   Make symbol for atom.  Greek letter goes in 17:17
      !                            and number goes in 16:16
      !
      if (m == 1) then
        txtatm (jtype(k)) (17:17) = letter 
        j = jtype(k)
        m = 0
        do i = 1, nbonds(j)
          if (txtatm(ibonds(i, j)) (16:16) /= " ") then
            txtatm(j) (16:16) = txtatm(ibonds(i, j)) (16:16)
            m = m + 1
          end if
        end do
        if (m /= 1) then
          txtatm (j) (16:16) = " "
        end if
      else
        m = 0
!
!  Special case: Tryptophan zeta starts with 2, not 1
!
        if (txtatm(j)(8:10) == "TRP" .and. letter == "Z") m = 1
        do i = 1, jj
          j = jtype(i)
          if (nat(jtype(i)) /= 1) then
            m = m + 1
            txtatm(j) (17:17) = letter
            txtatm (j) (16:16) = Char (m+Ichar ("0"))
          end if
        end do
      end if
    end do
   !
   !   Now to assign hydrogens, if any
   !
    do i = 1, jj
      if (nat(jtype(i)) == 1 .and. txtatm(jtype(i)) (17:17) == " ") then
         !
         !   This is a hydrogen, unlabeled.  Form the label based on the
         !   atom it is attached to.
         !
        j = jtype(i)
        k = ibonds(1, j)
        m = 0
        do l = 1, nbonds(k)
          if (nat(ibonds(l, k)) == 1) then
            m = m + 1
          end if
        end do
        if (m > 1) then
            !
            !  Yes, there is more.  Use labels.
            !
          m = 0
          do l = 1, nbonds(k)
            if (nat(ibonds(l, k)) == 1) then
              m = m + 1
              txtatm (ibonds(l, k)) (15:15) = Char (m+Ichar ("0"))
              txtatm(ibonds(l, k)) (16:17) = txtatm(k) (16:17)
            end if
          end do
        else
          txtatm(j) (16:17) = txtatm(k) (16:17)
        end if
      end if
    end do
end subroutine txtype
