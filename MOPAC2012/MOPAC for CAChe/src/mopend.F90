subroutine mopend (txt)
    use molkst_C, only: moperr, errtxt
    implicit none
    character (len=*), intent (in) :: txt
    moperr = .true.
    errtxt = txt
    call to_screen(txt)
    return
end subroutine mopend 

   logical function shutdown(i)
      implicit none
      interface
        logical function shutdownc(a1)
          !DEC$ ATTRIBUTES C, ALIAS:'_shutdownc' :: shutdownc
          INTEGER a1 [REFERENCE]
         end function shutdownc
      endinterface
      integer i
      shutdown = shutdownc(i)
      return
      end


   subroutine dbreak()
    implicit none
    interface
      subroutine DbgBreak()
        !DEC$ ATTRIBUTES C, ALIAS:'_DbgBreak' :: DbgBreak
       end subroutine DbgBreak
    endinterface
    call DbgBreak()
    return
    end

subroutine to_screen(text)
use molkst_C, only: n_screen
  interface
    subroutine setmsgbufc(length,text,priority)
!DEC$ ATTRIBUTES C, ALIAS:'_setmsgbufc' :: setmsgbufc
      INTEGER length [REFERENCE], priority [REFERENCE]
      CHARACTER (len=*) text [REFERENCE]
    end subroutine setmsgbufc
  end interface
    character (len=*) text
  integer :: j
  integer i
  if (n_screen /= 0) then
    i= n_screen
  else
    i = 1
  end if
  n_screen = 0
  if (text(:8) == "To_file:") return
  if (index(text,"Job:") /= 0) return
  if (index(text,"% of ") /= 0) then
    i = 4
  else
  end if
  call setmsgbufc(len_trim(text),text,i)
  return
end subroutine to_screen

subroutine doreord (c,norbs,nfirst,nlast,numat)
  implicit none
  integer, intent(in) :: norbs, numat
  double precision, dimension(norbs,norbs), intent(inout) :: c
  integer, dimension(numat), intent(in) :: nfirst, nlast
  integer :: atom, mo
  double precision xz, z2, yz, xy
  !
  do mo = 1, norbs
    do atom = 1, numat
      if(nlast(atom)-nfirst(atom) == 8) then
        ! Change order of d-orbitals for CAChe Tabulator.
        ! Original order of d-orbitals in MOPAC is         x2-z2, xz, z2, yz, xy.
        ! CAChe Tabulator expects d-orbitals in order      x2-y2, z2, xy, xz, yz.
        xz = c(nfirst(atom) + 5, mo)
        z2 = c(nfirst(atom) + 6, mo)
        yz = c(nfirst(atom) + 7, mo)
        xy = c(nfirst(atom) + 8, mo)
        c(nfirst(atom) + 5, mo) = z2
        c(nfirst(atom) + 6, mo) = xy
        c(nfirst(atom) + 7, mo) = xz
        c(nfirst(atom) + 8, mo) = yz
      end if
    end do
  end do
   !
end subroutine doreord
!
!  Write formatted data for an IRC or DRC calculation
!
! write all atoms including dummy ones
subroutine ntm_wrtmap(coord, Charge, ncycle, escf_loc, ekin, etotal, time, distance)
  use molkst_C, only : numat
  use chanel_C, only : iw
  use common_arrays_C, only:  atmass, l_atom
  implicit none
  double precision, dimension(3,numat), intent(inout) :: coord
  double precision, dimension(numat), intent(in) :: Charge
  integer, intent(in) :: ncycle
  double precision, intent(in) :: escf_loc, ekin, etotal, time, distance
  integer, parameter :: iunit = 99
  integer :: i, ii, j, size_of_block
  double precision, save :: molecular_weight
  double precision :: sum
  integer, save :: nout = 0
  character (len=80), dimension(:), allocatable :: all_text
  character (len=80) :: one_line
  if (ncycle /= -2) then
    !
    open (unit = iunit, file = 'xyzmap', status = 'unknown', position = 'append')
    write (iunit,'(i6,5f16.8)') ncycle, escf_loc, ekin, etotal, time, distance
    !
    !   Center coordinates on center of mass
    !
    if (ncycle < 2) then
      molecular_weight = 0.d0
      do i = 1, numat
        molecular_weight = molecular_weight + atmass(i)
      end do
    end if
    do j = 1,3
      sum = 0.d0
      do i = 1, numat
        sum = sum + coord(j,i)*atmass(i)
      end do
      sum = - sum / molecular_weight
      do i = 1, numat
        coord(j,i) = coord(j,i) + sum
      end do
    end do
    do j = 1,numat
      if (l_atom(j)) write (iunit,'(3f16.8,f16.4)') (coord(i,j), i=1,3), Charge(j)
    enddo
    !
    !  Write out bond orders.  The eigenvectors (c) are used as scratch space here.
    !
    i = iw
    iw = iunit
    call bonds()
    iw = i
    write (iunit,'(a10)') "EndOfBlock"
    close (iunit)
    nout = nout + 1
  else
    !
    !  Reverse storage of blocks
    !
    call to_screen("Reversing first half of IRC curve")
    open (unit = iunit, file = 'xyzmap', status = 'unknown')
    do size_of_block = 1,10000
      read(iunit,"(a80)", iostat=ii) one_line
      if (ii < 0) return
      if (index(one_line,"EndOfBlock") /= 0) exit
    end do
    allocate (all_text(size_of_block*nout))
    rewind (iunit)
    do i = 1, size_of_block*nout
      read (iunit,"(a80), iostat = ii") all_text(i)
      if (ii < 0) exit
    end do
    if (ii >= 0) then
      rewind (iunit)
      do ii = nout - 1, 0, -1
        i = ii*size_of_block
        do j = 1, size_of_block
          write (iunit,"(a)") Trim(all_text(i + j))
        end do
      end do
    end if
    deallocate (all_text)
    call to_screen("Second half of IRC curve")
  end if
end subroutine ntm_wrtmap

subroutine cache_bonds (p, b, c, bondab, cbeta, pa, pb)
  use molkst_C, only: numat, norbs, mpack, nopen, nclose, nalpha, &
       & nbeta, nelecs, fract
  use molkst_C, only: keywrd
  use common_arrays_C, only: nfirst, nlast
  !
  !.. Implicit Declarations ..
  implicit none
  !
  !.. Formal Arguments ..
  double precision, dimension (norbs**2), intent (in) :: c, cbeta
  double precision, dimension (mpack), intent (in) :: p, pa, pb
  double precision, dimension (norbs, norbs), intent (inout) :: b
  double precision, dimension (numat*(numat+1)/2), intent (out) :: bondab
  !
  !.. Local Scalars ..
  logical :: ci, kci, nci
  integer :: i, ih, ij, il, ilih, j, k, kk, l, linear, ll, m, mu, n, nopn
  double precision :: a, aa, da, sum, x, zkappa
  !
  !.. Local Arrays ..
  double precision, dimension(:,:), allocatable :: aux
  double precision, dimension(:), allocatable :: pspin, sdm, spinab, v
  !
  !.. External Calls ..
  !
  !.. Intrinsic Functions ..
  intrinsic Dble, Index
  !
  ! ... Executable Statements ...
  !
  ci = (Index (keywrd, "C.I.")+Index (keywrd, "MECI") /= 0)
  kci = (Index (keywrd, "MICROS") == 0)
  nci = (Index (keywrd, "ROOT")+Index (keywrd, "OPEN") == 0)
  nopn = nopen - nclose

  allocate (pspin(mpack), sdm(mpack), aux(numat,numat), &
         &  spinab((numat*(numat+1))/2), &
         &  v(numat))

  !*****   CALCULATE THE DEGREE OF BONDING   ************
  !
  k = 0
  do i = 1, norbs
    do j = 1, i
      k = k + 1
      b(i, j) = p(k)
      b(j, i) = p(k)
    end do
  end do
  !
  ! *******  CALCULATE KAPPA FACTOR FOR UHF OR ROHF  ******************
  !
  if (Index (keywrd, "UHF") /= 0) then
    !****** UHF CASE
    zkappa = 0.d0
    do n = 1, nalpha
      do m = 1, nbeta
        sum = 0.d0
        do mu = 1, norbs
          l = mu + norbs * (n-1)
          k = mu + norbs * (m-1)
          sum = sum + c(l) * cbeta(k)
        end do
        zkappa = zkappa + sum ** 2
      end do
    end do
    zkappa = 1.d0 / (zkappa/dble(nalpha+nbeta)+0.5d0)
  else if ( .not. ci .and. nopn == 0 .and. nci .and. kci) then
    zkappa = 1.d0
  else
    !****** ROHF CASE
    zkappa = 1.d0 / (1.d0-(dble(nopn)/dble(nelecs))/2.d0)
  end if
  ij = 0
  do i = 1, numat
    a = 0.0d00
    l = nfirst(i)
    ll = nlast(i)
    do j = 1, i
      ij = ij + 1
      k = nfirst(j)
      kk = nlast(j)
      x = 0.0d0
      do il = l, ll
        do ih = k, kk
          x = x + b(il, ih) * b(il, ih)
        end do
      end do
      bondab(ij) = x
    end do
    x = -bondab(ij)
    do j = l, ll
      a = a + b(j, j)
      x = x + 2.d0 * b(j, j)
    end do
    v(i) = x
  end do
  !
  !       
  ! *******************************************
  !
  k = 0
  do i = 1, numat
    do j = 1, i
      k = k + 1
      bondab(k) = bondab(k) * zkappa
      aux(i, j) = bondab(k)
      aux(j, i) = bondab(k)
    end do
    bondab(k) = v(i)
  end do
  do i = 1, numat
    da = 0.0d0
    do j = 1, numat
      if (j /= i) then
        da = da + aux(i, j)
      end if
    end do
  end do
  !
  !  ********   OUTPUT    *****************
  !
  !
  ! THIS SUBROUTINE CALL ADDED BY VIC L. TO WRITE OUT THE BOND INDICES
  !  TO THE SYBYL MOPAC OUTPUT FILE *.SYB. (29JULY86)
  !  SUBROUTINE MPC_BONDS IS IN MODULE WRITE.FOR.
  !

  !  call mpcbds (bondab)

  if (index(keywrd, " LARGE") == 0) go to 1100
  !****** PERFORM SPIN POPULATION STATISTICAL ANALYSIS
  linear = norbs * (norbs+1) / 2
  if (Index (keywrd, "UHF") /= 0) then
    do i = 1, linear
      pspin(i) = pa(i) - pb(i)
    end do
    sum = 0.d0
    l = 0
    do i = 1, norbs
      do j = 1, i
        aa = 2.d0
        if (i == j) then
          aa = 1.d0
        end if
        l = l + 1
        sum = sum + aa * (pspin(l)*p(l))
      end do
    end do
  else if ( .not. ci .and. nopn == 0 .and. nci .and. kci) then
    go to 1100
  else
    call dopen (c, norbs, norbs, nclose, nopen, fract, sdm)
    do j = 1, linear
      pspin(j) = sdm(j)
    end do
  end if
  ij = 0
  do i = 1, numat
    l = nfirst(i)
    ll = nlast(i)
    do j = 1, i
      ij = ij + 1
      k = nfirst(j)
      kk = nlast(j)
      x = 0.d0
      do il = l, ll
        do ih = k, kk
          if (il >= ih) then
            ilih = ((il*(il-1))/2) + ih
          else
            ilih = ((ih*(ih-1))/2) + ij
          end if
          x = x + b(il, ih) * pspin(ilih)
        end do
      end do
      spinab(ij) = x
    end do
  end do
  ! EVALUATE THE TOTAL ATOMIC SPIN POPULATIONS
  k = 0
  do i = 1, numat
    do j = 1, i
      k = k + 1
      aux(i, j) = spinab(k)
      aux(j, i) = spinab(k)
    end do
  end do
  do i = 1, numat
    da = 0.d0
    do j = 1, numat
      if (j /= i) then
        da = da + aux(i, j)
      end if
    end do
  end do
1100 continue
  deallocate (pspin, sdm, spinab, v,  &
       &  aux)
end subroutine cache_bonds

subroutine cache_muln (c, cbeta, emoa, emob, q, &
     & h, f, norbs_loc, vecs, geo, coord, store, nat, nfirst, &
     & nlast, ifact, i1fact, bondord, mtxtrn)
  use molkst_C, only : numat, mpack, natoms, nalpha, nbeta, &
       & nelecs, fract, nopen, nclose, escf, wtmol => mol_weight, gnorm, id, &
       norbs
  use cosmo_C, only : area, ediel
  use chanel_C, only: igpt
  use common_arrays_C, only: tvec, p, l_atom
  use molkst_C, only : keywrd, ux, uy, uz
  use parameters_C, only : zs, zp, zd
  use parameters_C, only : tore
  use parameters_C, only: betad, betas, betap
  implicit none
  integer, intent (in) :: norbs_loc, mtxtrn
  integer, dimension (norbs_loc), intent (in) :: i1fact, ifact
  integer, dimension (numat), intent (in) :: nat, nfirst, nlast
  double precision, dimension (norbs_loc*norbs_loc), intent (inout) :: c, f
  double precision, dimension (norbs_loc*norbs_loc), intent (in) ::  cbeta
  double precision, dimension (norbs_loc*norbs_loc), intent (out) ::  vecs
  double precision, dimension (norbs_loc), intent (in) :: emoa, emob
  double precision, dimension (mpack), intent (inout) :: h
  double precision, dimension (mpack), intent (out) :: store
  double precision, dimension (3, natoms), intent (inout) :: geo
  double precision, dimension (3, numat), intent (inout) :: coord
  double precision, dimension (numat), intent (inout) :: q
  double precision, dimension (mtxtrn), intent (in) :: bondord
  !
  !.. Local Scalars ..
  integer :: ivect
  logical :: delements
  integer :: i, if, ii, ij, il, im1, j, jf, jj, jl, k, linear, alloc_stat, &
       & ijbond
  double precision :: bi, bj, sum, summ
  !.. Local Arrays ..
  double precision, dimension (:), allocatable :: eigs
  double precision, allocatable, dimension (:) :: bonds
  integer, allocatable, dimension (:) :: ibond, jbond
  !
  !
  !.. Intrinsic Functions ..
  intrinsic Abs, Index, Sqrt
  !
  ! ... Executable Statements ...
  !
  !     PATAS
  !*********************************************************************
  !
  !  FIRST, RE-CALCULATE THE OVERLAP MATRIX
  !
  !*********************************************************************
  !
  !     PATAS

  allocate (eigs(norbs_loc), stat=alloc_stat)

! This will produce GARBAGE for MINDO/3!!!!
! Garbage is better than possible crash.
! Results will be ignored by CAChe anyway
! PLONKAW 22 may 2000
! call dbreak()
  do i = 1, numat
      if = nfirst(i)
      il = nlast(i)
      if (il >= if) then
        eigs(if) = betas(nat(i))
        if (il > if) then
          eigs(if+1) = betap(nat(i))
          eigs(if+2) = eigs(if+1)
          eigs(if+3) = eigs(if+1)
          if (il > if+3) then
            eigs(if+4) = betad(nat(i))
            eigs(if+5) = eigs(if+4)
            eigs(if+6) = eigs(if+4)
            eigs(if+7) = eigs(if+4)
            eigs(if+8) = eigs(if+4)
          end if
        end if
    else
      if = nfirst(i)
      il = nlast(i)
      if (il >= if) then
        eigs(if) = betas(nat(i))
        if (il > if) then
          eigs(if+1) = betap(nat(i))
          eigs(if+2) = eigs(if+1)
          eigs(if+3) = eigs(if+1)
          if (il > if+3) then
            eigs(if+4) = betad(nat(i))
            eigs(if+5) = eigs(if+4)
            eigs(if+6) = eigs(if+4)
            eigs(if+7) = eigs(if+4)
            eigs(if+8) = eigs(if+4)
          end if
        end if
      end if
    end if
    im1 = i - 1
    bi = betas(nat(i))
    do k = if, il
      bi = eigs(k)
      ii = (k*(k-1)) / 2
      do j = 1, im1
        jf = nfirst(j)
        jl = nlast(j)
        do jj = jf, jl
          bj = eigs(jj)
          ij = ii + jj
          h(ij) = 2.d0 * h(ij) / (bi+bj) + 1.d-14
          !  THE  +1.D-14 IS TO PREVENT POSSIBLE ERRORS IN THE DIAGONALIZATION.
          store(ij) = h(ij)
        end do
      end do
      do jj = if, k
        ij = ii + jj
        store(ij) = 0.d0
        h(ij) = 0.d0
      end do
    end do
  end do
  do i = 1, norbs_loc
    store(i1fact(i)) = 1.d0
    h(i1fact(i)) = 1.d0
  end do
  call rsp (h, norbs_loc, norbs_loc, eigs, vecs)
  do i = 1, norbs_loc
    eigs(i) = 1.d0 / Sqrt (Abs (eigs(i)))
  end do
  ij = 0
  do i = 1, norbs_loc
    do j = 1, i
      ij = ij + 1
      sum = 0.d0
      do k = 1, norbs_loc
        sum = sum + vecs(i+ (k-1)*norbs_loc) * eigs(k) * vecs &
             & (j+ (k-1)*norbs_loc)
      end do
      f(i+ (j-1)*norbs_loc) = sum
      f(j+ (i-1)*norbs_loc) = sum
    end do
  end do
  !
  call gmetry (geo, coord)
  !
  ! write following data for CAChe:
  !
  ! number of atoms, AOs, electrons
  ! all atomic coordinates
  ! orbital counters
  ! orbital exponents S, P, D, and atomic numbers
  ! alpha and beta eigenvectors (not re-normalized MOs)
  ! alpha and beta eigenvalues
  ! inverse-square root of the overlap matrix
  ! alpha and beta orbital occupations
  ! energy
  ! calculated atomic charges
  ! bond orders
  ! dipole vector
  ! molecular weight, gradient norm, dielectric energy, COSMO area.
  !
  open (unit=igpt, file='MOPAC Graphics', form='UNFORMATTED', &
       & status='UNKNOWN')
  !


 write(33,*)nalpha, nbeta, nelecs
  if (nalpha /= 0 .or. nbeta /= 0) then
    write (igpt) numat+id, norbs_loc, -nelecs, ((coord(i, j), j=1, numat), (tvec(i,j), j=1, id), i=1, 3)
  else
    write (igpt) numat+id, norbs_loc, nelecs, ((coord(i, j), j=1, numat), (tvec(i,j), j=1, id), i=1, 3)
  end if
  write (igpt) (nlast(i), nfirst(i), i=1, numat),(nlast(numat), nlast(numat), i = 1, id)
  write (igpt) (zs(nat(i)), i=1, numat),(0.d0, i = 1, id), (zp(nat(i)), i=1, numat),(0.d0, i = 1, id), &
         & (zd(nat(i)), i=1, numat),(0.d0, i = 1, id), (nat(i), i=1, numat),(107, i = 1, id)
  linear = norbs_loc * norbs_loc
  !
  ! check for d-elements
  !
  delements = .false.
  atoms: do i = 1, numat
    if( nlast(i) - nfirst(i) == 8 ) then
      delements = .true.
      exit atoms
    end if
  end do atoms
  !
  if (nalpha /= 0 .or. nbeta /= 0) then
    ! write alpha eigenvectors
    if (delements) then
      ! reorder d-orbitals according to CAChe Tabulator requirements
      do i = 1, linear
        vecs(i) = c(i)
      enddo
      call doreord(vecs,norbs_loc,nfirst,nlast,numat)
      !      write (igpt) (vecs(i), i=1, linear)
    else
      !     write (igpt) (0.d0, i=1, linear)!   write (igpt) (c(i), i=1, linear)
    end if
    ! write beta eigenvectors
    if (delements) then
      ! reorder d-orbitals according to CAChe Tabulator requirements
      do i = 1, linear
        !     vecs(i) = cbeta(i)
      enddo
      call doreord(vecs,norbs_loc,nfirst,nlast,numat)
      !    write (igpt) (vecs(i), i=1, linear)
    else
      !    write (igpt) (cbeta(i), i=1, linear)
    endif
    ! write MO energies
    write (igpt) (emoa(i), i=1, norbs_loc)
    write (igpt) (emob(i), i=1, norbs_loc)
    ! write inverse-square root of overlap matrix
    !  write (igpt) (0.d0, i=1, linear) !write (igpt) (f(i), i=1, linear)
    ! write orbital populations
    write (igpt) (1.0d0, i=1, nalpha), (0.0d0, i=nalpha+1, norbs_loc)
    write (igpt) (1.0d0, i=1, nbeta),  (0.0d0, i=nbeta+1,  norbs_loc)
  else
    ! write alpha eigenvectors
    if (delements) then
      ! reorder d-orbitals according to CAChe Tabulator requirements
      do i = 1, linear
        vecs(i) = c(i)
      enddo
      call doreord(vecs,norbs_loc,nfirst,nlast,numat)
      !     write (igpt) (vecs(i), i=1, linear)
    else
      !     write (igpt) (c(i), i=1, linear)
    endif
    ! write MO energies
    write (igpt) (emoa(i), i=1, norbs_loc)
    ! write inverse-square root of overlap matrix
    !   write (igpt) (f(i), i=1, linear)
    ! write orbital populations
    if(nopen > nclose) then
      write (igpt) (2.0d0, i=1, nclose), (fract, i=nclose+1, nopen), &
    & (0.0d0, i=nopen+1, norbs_loc)
    else
      write (igpt) (2.0d0, i=1, nclose), (0.0d0, i=nclose+1, norbs_loc)
    endif
  end if
  write (igpt) escf
 
  call chrge (p, q)
  write (igpt) (tore(nat(i))-q(i), i=1, numat), (0.d0, i = 1, id)
  allocate (bonds(5*numat), ibond(5*numat), jbond(5*numat), stat=alloc_stat)
  if (alloc_stat /= 0) then
    return
  end if
  ij = 1
  ijbond = 0
  do i = 1, numat
    do j = 1, i-1
      if (bondord(ij) > 0.1d0) then
        ijbond = ijbond + 1
        ibond(ijbond) = i
        jbond(ijbond) = j
        bonds(ijbond) = bondord(ij)
      end if
      ij = ij + 1
    end do
    ij = ij + 1
  end do
  write (igpt) ijbond,(ibond(i), i=1,ijbond), &
       & (jbond(i), i=1,ijbond), (bonds(i), i=1,ijbond)
  deallocate (bonds, ibond, jbond, stat=alloc_stat)
  if (alloc_stat /= 0) then
    return
  end if
  write (igpt) ux, uy, uz 
  write (igpt) wtmol, gnorm, ediel, area
  close (igpt, status="KEEP")
  !
  ! write re-normalized eigenvectors into a CAChe file
  !
  ivect = 99999
  open (unit=ivect, file='MOPAC Vectors', form='UNFORMATTED', status='UNKNOWN')
  call mult (c, f, vecs, norbs_loc)
  if (delements) then
    ! reorder d-orbitals according to CAChe Tabulator requirements
    call doreord(vecs,norbs_loc,nfirst,nlast,numat)
  end if
  call output_compressed_MOs(vecs,norbs_loc, ivect)
  if (nalpha /= 0 .or. nbeta /= 0) then
    call mult (cbeta, f, vecs, norbs_loc)
    if (delements) then
      ! reorder d-orbitals according to CAChe Tabulator requirements
      call doreord(vecs,norbs_loc,nfirst,nlast,numat)
    end if
    call output_compressed_MOs(vecs,norbs_loc, ivect)
  end if
  close (ivect, status="KEEP")
  if (nalpha /= 0 .or. nbeta /= 0 .or. Index (keywrd, "MULLIK") == 0) goto 1100
  !
  ! do Mulliken analysis
  !
  if (delements) then
    ! recalculate VECS
    call mult (c, f, vecs, norbs_loc)
  end if
  call densit (c, norbs, norbs, nclose, 2.d0, nopen, fract, p, 1) 
  linear = (norbs_loc*(norbs_loc+1)) / 2
  do i = 1, linear
    f(i) = f(i) * store(i)
  end do
  summ = 0.d0
  do i = 1, norbs_loc
    sum = 0
    do j = 1, i
      sum = sum + f(ifact(i)+j)
    end do
    do j = i + 1, norbs_loc
      sum = sum + f(ifact(j)+i)
    end do
    summ = summ + sum
    f(i1fact(i)) = sum
  end do
  call vecprt (f, norbs_loc)
  
1100 continue

  if ( allocated(eigs) ) deallocate(eigs)

end subroutine cache_muln





subroutine ntmOpenForce(nVibrations)
  implicit none
  integer, intent(in) :: nVibrations
  !
  open (unit=3333, file='ForceData', form='FORMATTED', status='UNKNOWN')
  write (3333,"(a/a/a)") &
     & '# Number of Vibrations', &
     & '# ModeID, ModeValue, TransitionDipole', &
     & '# ModeID, AtomA, AtomB, Contribution(%)'
  write (3333,"(i0)") nVibrations
end subroutine ntmOpenForce

subroutine ntmCloseForce()
  implicit none
  close(3333)
end subroutine ntmCloseForce

subroutine ntmWrtFreq(freqIndex, freqValue, transDipole)
  implicit none
  integer,          intent(in) :: freqIndex
  double precision, intent(in) :: freqValue, transDipole
  write (3333,"(i0,2x,f15.6,2x,f15.6)") freqIndex, freqValue, transDipole
end subroutine ntmWrtFreq

subroutine ntmWrtFreqAt(freqIndex, atomA, atomB, percent)
  implicit none
  integer,          intent(in) :: freqIndex, atomA, atomB
  double precision, intent(in) :: percent
  write (3333,"(3(i0,2x),f7.2)") freqIndex, atomA, atomB, percent
end subroutine ntmWrtFreqAt

subroutine output_compressed_MOs(vecs,norbs, ivect)
  implicit none
  integer, intent (in) :: norbs, ivect
  double precision, dimension (norbs, norbs), intent(in) :: vecs
  integer, dimension (norbs) :: ic, ivec
  double precision, dimension (norbs) :: vec, avec, c1
!
  integer :: i,j,k, m, n, mo
  double precision :: sum, phase
  !
  !  Put occupied and virtual LMOs into into a single array, c.
  !
  !  eigs = energy levels from MOZYME.  Do NOT alter these, as they might be used elsewhere.
  !
  !  On exit:
  !  zeigs = ordered energy levels.
  !  c     = ordered LMOs in conventional square matrix form
  !
  !
  ! Convert LMOs to Eigenvectors
  !
     
  !
  !  Generate "compressed" molecular orbitals.
  !
  m = Min (60, norbs)
  write (ivect) m
  do mo = 1, norbs
    !
    !  Extract one entire molecular orbital
    !
      do j = 1,norbs
        vec(j) = vecs(j, mo)
        avec(j) = Abs (vec(j))
      end do
    !
    ! Identify all the significant coefficients
    !
    n = 0
    do j = 1,norbs
      if (avec(j) > 0.002d0) then
        n = n + 1
        ivec(n) = j
        vec(n) = vec(j)
        avec(n) = avec(j)          
      end if
    end do
    !
    !  Re-sequence the coefficients so that the largest is first
    !
    do i = 1, n
      sum = 0.d0
      do j = 1, n
        if (sum < avec(j)) then
          sum = avec(j)
          k = j
        end if
      end do
      ic(i) = ivec(k)
      c1(i) = vec(k)
      avec(k) = 0.d0
    end do
    phase = 1.d0
    if (c1(1) < 0.d0) phase = -1.d0
    !
    !  Write out all finite indices, followed by zeros.
    !
    j = Min (m, n)
    write (ivect) (ic(i),i=1,j), (0,i = j+1, m)
    write (ivect) (phase*c1(i),i=1,j),  (0.d0,i=j+1,m)
  end do
end subroutine output_compressed_MOs
subroutine fill_overlap_matrix(overlap)
!
!  Set up an array to hold the overlap matrix
!
  use parameters_C, only : betas, betap, betad, natorb
  use molkst_C, only : norbs, numat
  use common_arrays_C, only : nat, nfirst, nlast, h
  double precision, intent (out) :: overlap((norbs*(norbs + 1))/2)
  integer :: i, im1, j, ifact(norbs + 1)
  double precision :: bi(9), bj(9)
    do i = 1, norbs 
      ifact(i) = (i*(i - 1))/2 
    end do 
    ifact(norbs+1) = (norbs*(norbs + 1))/2 
    overlap = 0.d0
    do i = 1, numat 
      if = nfirst(i) 
      im1 = i - 1 
      bi = betas(nat(i))
      ni = nat(i) 
      bi(1) = betas(ni)*0.5D0 
      bi(2) = betap(ni)*0.5D0 
      bi(3) = bi(2) 
      bi(4) = bi(2) 
      bi(5) = betad(ni)*0.5D0 
      bi(6) = bi(5) 
      bi(7) = bi(5) 
      bi(8) = bi(5) 
      bi(9) = bi(5) 
      norbi = natorb(ni)    
      do j = 1, im1 
        nj = nat(j)
        bj(1) = betas(nj)*0.5D0 
        bj(2) = betap(nj)*0.5D0 
        bj(3) = bj(2) 
        bj(4) = bj(2) 
        bj(5) = betad(nj)*0.5D0 
        bj(6) = bj(5) 
        bj(7) = bj(5) 
        bj(8) = bj(5) 
        bj(9) = bj(5) 
        norbj = natorb(nj) 
        jf = nfirst(j) 
        do ii = 1, norbi
          do jj = 1, norbj
            ij = ((if + ii - 1)*(if + ii - 2))/2 + jf + jj - 1
            overlap(ij) = h(ij)/(bi(ii) + bj(jj))
          end do
        end do
      end do 
    end do 
    overlap(ifact(2:norbs+1)) = 1.D0 
  end subroutine fill_overlap_matrix
