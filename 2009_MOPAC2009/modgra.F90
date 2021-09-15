subroutine modgra ()
   !***********************************************************************
   !
   !   MODGRA prints the gradients due to (a) backbone residue atoms, and
   !   (b) all side-chain atoms in each residue.
   !
   !***********************************************************************
    use molkst_C, only: natoms, nvar
    use chanel_C, only: iw
    use MOZYME_C, only : nres, iopt, allres
    use common_arrays_C, only : labels, grad, loc
    implicit none
    double precision :: work(-nres:nres), rgrad(natoms), sum
    integer :: i, j
!
    do i = -nres, nres
      work(i) = 0.d0
    end do
   !
   !    WORK collects gradient contributions from atoms.  The negative
   !    addresses refer to the residue backbone, the positive addresses
   !    refer to the side-chain.
   !
   !    (Backbone atoms are -NH-CH-CO-)
   !
    do i = 1, natoms
      rgrad(i) = 0.d0
    end do
    do i = 1, nvar
      j = loc(1, i)
      rgrad(j) = rgrad(j) + grad(i) ** 2
    end do
    do i = 1, natoms
      if (labels(i) < 99) then
        j = iopt(i)
        if (abs(j) <= nres) work(j) = work(j) + rgrad(i)
      end if
    end do
    sum = 0.d0
    do i = -nres, nres
      work(i) = Sqrt (work(i))
      sum = sum + work(i)
    end do
    if (sum < 0.5d0) then
      write (iw,"(/12x,a,/)") " ALL GRADIENTS FOR ALL BACKBONES PLUS SIDE CHAINS ARE VERY SMALL"
    else
      write (iw,"(/12x,a,/)") " GRADIENTS FOR ALL BACKBONES"
      write (iw,"(a)")"     Residue        Backbone    Side-Chain      Total"
      write (iw, "(I7,2x,a,F14.3, f13.3, f13.3)") &
    (i, allres(i), work(-i), work(i), work(-i) + work(i), i=1, nres)
      write(iw,*)
    end if
    return
end subroutine modgra
