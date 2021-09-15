subroutine modchg ()
   !***********************************************************************
   !
   !   MODGRA prints the charge due to (a) backbone residue atoms, and
   !   (b) all side-chain atoms in each residue.
   !
   !***********************************************************************
    use molkst_C, only: numat
    use MOZYME_C, only : nres, at_res, res_start
    use common_arrays_C, only : q, txtatm
    use chanel_C, only: iw
    implicit none
    double precision, dimension (0:nres) :: work
    integer :: i, j
    character :: residue*7
   !
   ! ... Executable Statements ...
   !
    work = 0.d0
    if (.not. allocated(q)) then
      write(iw,"(a)") "Density matrix is not available.  Run a 1SCF."
    !  call mopend("Density matrix is not available.  Run a 1SCF.)
      return
    end if
   !
   !    WORK collects charge contributions from atoms.  The negative
   !    addresses refer to the residue backbone, the positive addresses
   !    refer to the side-chain.
   !
   !    (Backbone atoms are -NH-CH-CO-)
   !
    do i = 1, numat
      j = at_res(i)
      work(j) = work(j) + q(i)
    end do
    write (iw,"(/12x,a,/)") " NET CHARGE ON RESIDUES"
    write (iw,"(a,/)")"     Residue        Charge"
    do i = 1, nres
      if (res_start(i) < 1) cycle
      residue = txtatm(res_start(i))(9:11)//" "//txtatm(res_start(i))(13:15)
      if (work(i) > 0.5d0) then
         write (iw, "(6x,a,F14.3, a)") residue, work(i), " CATION"
      else if (work(i) < -0.5d0) then
         write (iw, "(6x,a,F14.3, a)") residue, work(i), " ANION"
      else
         write (iw, "(6x,a,F14.3)") residue, work(i)
      end if   
    end do
    write(iw,*)
end subroutine modchg
