subroutine names (ioptl, lused, n1, ires, nfrag, io, uni_res, mres)
    use molkst_C, only: natoms, numat, keywrd, id
    use MOZYME_C, only : nres, allres, maxres, nbackb, nxeno, mxeno, k, iatom, jatom, &
       & loop, bbone, angles, txeno
    use common_arrays_C, only : nat, labels, txtatm, ibonds, coord
    use funcon_C, only : pi
    use chanel_C, only: iw
    use reada_I
    implicit none
    integer, intent (in) :: n1, nfrag, io
    integer, intent (inout) :: ires, uni_res, mres
    logical, dimension (numat), intent (inout) :: ioptl
    integer, dimension (natoms), intent (inout) :: lused
!
    logical :: lreseq, first_res
    integer :: i, iold, irold, j, kl, ku, l, n10, ires_start, ires_loop
    character, dimension (23) :: tyr
    character, dimension (0:maxres) :: allr
    intrinsic Index, Max, Min, Nint
    data tyr / "G", "A", "V", "L", "I", "S", "T", "D", "N", "K", "E", "Q", &
         & "R", "H", "F", "C", "W", "Y", "M", "P", "P", "P", "?" /
!
    lreseq = (Index (keywrd, " RESEQ") /= 0)
    do i = 1, 4
      nbackb(i) = 0
      nxeno(i, 1) = 0
    end do
    mxeno = 1
    i = Index (keywrd, " XENO")
    if (i /= 0) then
      !
      !  There are xeno groups in the protein
      !
      i = i + 6
      l = Index (keywrd(i:), " ") + i - 3
      do j = 2, 11
        do k = 1, 4
          nxeno(k, j) = Nint (reada (keywrd(:l), i))
          i = Index (keywrd(i:l), ",") + i
        end do
        if (Index (keywrd(i:l), ";") == 0) then
          txeno (j) = keywrd (i:Index (keywrd(i:l+1), ")")+i-2)
          exit
        else
          txeno (j) = keywrd (i:Index (keywrd(i:l), ";")+i-2)
        end if
      end do
      mxeno = j
    end if
   !
   !  Break all intra-chain bonds, so that the residues can easily be
   !  identified.
   !
    call lyse !
   !
   !   WORK OUT THE BACKBONE
   !
    iatom = n1
    iold = -999
    if (nfrag == 1) then
      do i = 1, numat
        txtatm (i) = " "
      end do
    end if
    irold = ires + 1
    bbone(1,ires + 1) = n1
    ires = ires + 1
    ires_start = ires
    allr(ires:) = " "
    first_res = .true.
    do ires_loop = ires_start, maxres
      
      if (iatom == 0) goto 999
      !
      !  NXTMER identifies the backbone atoms to the next residue.
      !  The nitrogen of the next residue is in NBACKB(4)
      !
      ioptl(iatom) = .true.
      call nxtmer (iatom, nbackb)
      jatom = nbackb(4)
      !
      !  The residue is attached to IATOM and does not involve JATOM
      !
      !  NOW TO WORK OUT THE ATOMS IN THE RESIDUE
      !
      if (ires_loop == 25) then
        jatom = nbackb(4)  ! used in debugging only
      end if
      uni_res = uni_res + 1
      call atomrs (lused, ioptl, ires, n1, io, uni_res, first_res)
      first_res = .false.
      bbone(1,ires + 1) = jatom  !  N of residue
      bbone(2,ires) = nbackb(1)  !  C(alpha) of residue
      bbone(3,ires) = nbackb(2)  !  C1 of residue (Carboxylic)
      if (allr(ires) == " " .and. k > 0 .and. k < 24) allr(ires) = tyr(k)
      if (loop /= 1 .and. k > 0 .and. k /= 23 .and. i /= 1) then
        i = Max (Index (txeno(loop), " "), 2) - 1
        if (i /= 1) write (iw,"(a,i5, 4a,i5,a)") "         Residue:", ires, &
        " ("//allres(ires)(:3)//") contains xeno group '", txeno (loop) (:i)//"'"
        if (allres(ires) (4:4) == "+") then
          write (iw, "(A,I5,A)") "         Residue:", ires, &
               & " plus xeno group has a positive charge"
        else if (allres(ires) (4:4) == "-") then
          write (iw, "(A,I5,A)") "         Residue:", ires, &
               & " plus xeno group has a negative charge"
        end if
        allres (ires) = allres (ires) (1:3) // "*"
      end if
      if (iatom == jatom) go to 1000
      if (iold == jatom) then
        write (iw,*) " STRUCTURE UNRECOGNIZABLE"
        call mopend("Structure Unrecognizable")
        exit
      end if
      iold = iatom
      iatom = jatom
      if (iatom == 0 .and. ires_loop == ires_start) go to 1010
      ires = ires + 1
    end do
    return
 999 ires = ires - 1
1000 n10 = 10
    if (ires > 0) then
      if (nfrag == 1) then
        write (iw, "(/,20X,A,/)") "RESIDUE SEQUENCE IN PROTEIN"
      else
        write (iw, "(/,20X,A,I3/)") "RESIDUE SEQUENCE IN PROTEIN FRAGMENT:", &
             & nfrag
      end if
      first_res = .true.
    !  write(iw,"(a)")"  Residue     phi    psi  omega"
      do i = irold, ires
        mres = mres + 1
    !    write(iw,"(i4,2x,a,3i5)")i, allres(i), (bbone(j,i),j = 1,3)
       if (.not. first_res) then
  !
  ! Work out phi
  !
          if (bbone(2,i) /= 0) then
            call dihed(coord, bbone(3,i - 1), bbone(1,i), bbone(2,i), bbone(3,i), angles(1,mres)) 
          else
            angles(1,mres) = 0.d0
          end if
        end if
        if (i /= ires) then
  !
  ! Work out psi
  !
          call dihed(coord, bbone(1,i), bbone(2,i), bbone(3,i), bbone(1,i + 1), angles(2,mres)) 
          !
  ! Work out omega
  !
          if (bbone(2,i + 1) /= 0) then
            call dihed(coord, bbone(2,i), bbone(3,i), bbone(1,i + 1), bbone(2,i + 1), angles(3,mres))
          else
            angles(3,mres) = 0.d0
          end if 
        end if
        do j = 1,3
          if (angles(j,mres) >  pi) angles(j,mres) = angles(j,mres) - 2.d0*pi
          if (angles(j,mres) < -pi) angles(j,mres) =0.d0
          angles(j,mres) = angles(j,mres)*180.d0/pi
        end do
     !   write(iw,"(i4,2x,a,3f7.1)")i, allres(i), (angles(j,i),j = 1,3)
        first_res = .false.
      end do
      
      l = Min (n10, ires-irold+1)
      j = ((irold-1)/10) * 10
      write (iw, "(9X,10I6,/)") (i, i=irold-j, l+irold-1-j)
      write (iw, "(5X,I4,3X,10(A4,'  '))") irold - 1, (allres(l), l=irold, &
           & Min(ires, 9+irold))
      do k = irold + 10, ires, 10
        kl = k
        ku = Min (ires, k+9)
        write (iw, "(5X,I4,3X,10(A4,'  '))") k - 1, (allres(l), l=kl, ku)
      end do
      write (iw,*)
      if (nfrag == 1) then
        write (iw, "(/,20X,A,/)") "RESIDUE SEQUENCE IN PROTEIN"
      else
        write (iw, "(/,20X,A,I3/)") "RESIDUE SEQUENCE IN PROTEIN FRAGMENT:", &
             & nfrag
      end if
      if (irold == 1) then
        l = Min (50, ires)
        write (iw, "(11X,5(10A1,1X))") (allr(i), i=1, l)
        do k = 51, ires - 1, 50
          kl = k
          ku = Min (ires, k+49)
          write (iw, "(5X,I4,2X,5(10A1,1X))") k - 1, (allr(i), i=kl, ku)
        end do
      else
        write (iw, "(/10X,A,I5,A,/)") " (FIRST RESIDUE IS NUMBER", irold, ")"
        do k = irold, ires - 1, 50
          kl = k
          ku = Min (ires, k+49)
          write (iw, "(5X,I4,2X,5(10A1,1X))") k - 1, (allr(i), i=kl, ku)
        end do
      end if
      write (iw,*)
    end if
   !
   !  LABEL THE HYDROGEN ATOMS
   !
    do j = 1, numat
      if (nat(j) == 1 .and. txtatm(j)(7:7) /= "+") then
        write (txtatm(j) (:14), "(I6,A8)") j, txtatm (ibonds(1, j)) (7:)
        lused(j) = lused(ibonds(1, j))
      end if
    end do
    if (numat /= natoms + id.and. .not. lreseq) then
      j = natoms - numat
      !
      !  MODIFY TXTATM TO ALLOW FOR DUMMY ATOMS
      !
      l = numat
      do i = natoms - id, 1, -1
        if (labels(i) /= 99) then
          txtatm(i) = txtatm(l)
          lused(i) = lused(l)
          l = l - 1
        else
          write (txtatm(i), "(I6)") j
          j = j - 1
        end if
      end do
    end if
    nres = ires
    return
1010 nres = 1 
    write (iw,*) " AMINO ACID = ", allres (ires) (:3)
   !
   !   LABEL EVERY NON-BACKBONE ATOM AS A SIDE-CHAIN ATOM
   !
    do i = 1, natoms
      if (lused(i) /=-1) then
        lused(i) = 1
      end if
    end do
    do j = 1, numat
      if (nat(j) == 1) then

        write (txtatm(j) (:15), "(I6,A9)") j, txtatm(j)(7:7)//txtatm (ibonds(1, j)) (8:)
        lused(j) = lused(ibonds(1, j))
      end if
    end do
    if (numat == natoms) return
    j = natoms - numat
   !
   !  MODIFY TXTATM TO ALLOW FOR DUMMY ATOMS
   !
    l = numat
    do i = natoms, 1, -1
      if (labels(i) /= 99) then
        txtatm(i) = txtatm(l)
        lused(i) = lused(l)
        l = l - 1
      else
        write (txtatm(i), "(I6)") j
        j = j - 1
      end if
    end do
end subroutine names
