!
!   This subroutine is very inefficient.  It should be replaced by dcart
!
subroutine dcart_for_MOZYME (dxyz, mode, oldxyz)
    use molkst_C, only: numat, l1u, l2u, l3u, id, keywrd, &
    l123
!
    use common_arrays_C, only : tvec, coord, p, nat, nfirst, nlast
    use MOZYME_C, only : iorbs, jopt
    use cosmo_C, only: useps
    use funcon_C, only: fpc_9
    implicit none
   !
   !***********************************************************************
   !
   !    dcart_for_MOZYME  CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
   !           CARTESIAN COORDINATES. THIS IS DONE BY FINITE DIFFERENCES.
   !
   !   For use with the MOZYME function
   !
   !    THE MAIN ARRAYS IN DCART ARE:
   !        DXYZ   ON EXIT CONTAINS THE CARTESIAN DERIVATIVES.
   !
   !***********************************************************************
    integer, intent (in) :: mode
    double precision, dimension (3, numat*l123), intent (in) :: oldxyz
    double precision, dimension (3, numat*l123), intent (inout) :: dxyz
    logical, save :: debug
    logical :: large, refeps
    integer, save :: icalcn = 0
    integer :: i, i1, i2, icuc, if, ii, iii, ij, ik, il, io, j, j1, j2, &
         & jf, jj, jjj, jk, jl, jo, k, kkkk, kl, l, ncells, numtot
    double precision :: aa
    double precision, save :: chnge = 1.d-4, chnge2
    double precision :: const, deriv, ee
    integer, dimension (2) :: ndi
    double precision, dimension (3) :: dstat=0.d0
    double precision, dimension (171) :: padi, pdi
    double precision, dimension (3, 2) :: cdi
    integer, external :: ijbo
    if (icalcn /= 1) then
      ! CHNGE IS A MACHINE-PRECISION DEPENDENT CONSTANT
      chnge2 = chnge * 0.5d0
      icalcn = 1
    end if
    debug = (Index (keywrd, " DCART") /= 0)
    large = (Index (keywrd, "LARGE") /= 0)
    ncells = (2*l1u+1) * (2*l2u+1) * (2*l3u+1)
    icuc = (ncells+1) / 2
    numtot = numat * ncells
   !
   !   MODE = 1:   ADD NEW DERIVATIVES ON TO OLD DERIVATIVES
   !          0:   CALCULATE ALL THE DERIVATIVES 'DE NOVO'
   !         -1:   CALCULATE 'OLD' DERIVATIVES, GIVEN ALL THE DERIVATIVES
    if (mode == 0) then
      dxyz(1:3, 1:numtot) = 0.d0
    else if (mode == 1) then
      dxyz(1:3, 1:numtot) = oldxyz(1:3, 1:numtot)
    else if (mode ==-1) then
      dxyz(1:3, 1:numtot) = -oldxyz(1:3, 1:numtot)
    end if
    refeps = useps
    useps = .false.
    i2 = 1
    do i1 = 1, numat
      if (mode == 0 .or. jopt(i2) == i1) then
        const = 0.5d0
        i2 = i2 + 1
      else
        const = 1.d0
      end if
      j2 = 1
      do j1 = 1, numat
        if (mode == 0 .or. jopt(j2) == j1) then
          j2 = j2 + 1
          if (i1 /= j1 .or. id > 0) then
            if (i1 < j1) then
              ii = j1
              jj = i1
            else
              ii = i1
              jj = j1
            end if
            iii = ncells * (ii-1)
            jjj = ncells * (jj-1)
            if = nfirst(ii)
            il = nlast(ii)
            io = iorbs(ii)
            ndi(2) = nat(ii)
            cdi(1:3, 2) = coord(1:3, ii)
            jf = nfirst(jj)
            jl = nlast(jj)
            jo = iorbs(jj)
            ndi(1) = nat(jj)
            cdi(1:3, 1) = coord(1:3, jj)
            if (ijbo(ii, jj) >= 0) then
                  ! GET FIRST ATOM
              k = ijbo (jj, jj)
              ij = 0
              do i = 1, jo
                do j = 1, i
                  ij = ij + 1
                  k = k + 1
                  padi(ij) = p(k) * 0.5d0
                  pdi(ij) = p(k)
                end do
              end do
                  ! GET SECOND ATOM FIRST ATOM INTERSECTION
              if (i1 == j1) then
                 ij = jo
                 do i = 1, io
                  ij = ij + 1
                  l = (ij*(ij-1)) / 2
                  do j = 1, jo
                    l = l + 1
                    padi(l) = 0.d0
                    pdi(l) = 0.d0
                  end do
                end do
              else
                ij = jo
                k = ijbo (ii, jj)
                do i = 1, io
                  ij = ij + 1
                  l = (ij*(ij-1)) / 2
                  do j = 1, jo
                    l = l + 1
                    k = k + 1
                    padi(l) = p(k) * 0.5d0
                    pdi(l) = p(k)
                  end do
                end do
              end if
                  ! GET SECOND ATOM
              k = ijbo (ii, ii)
              ij = jo
              do i = 1, io
                ij = ij + 1
                l = (ij*(ij-1)) / 2 + jo
                do j = 1, i
                  k = k + 1
                  l = l + 1
                  padi(l) = p(k) * 0.5d0
                  pdi(l) = p(k)
                end do
              end do
              kkkk = 0
              do ik = -l1u, l1u
                do jk = -l2u, l2u
                  do kl = -l3u, l3u
                    kkkk = kkkk + 1
                    if ( .not. (i1 == j1 .and. ik == 0 .and. jk == 0 .and. kl == 0)) then
                      do l = 1, 3
                        cdi(l, 1) = coord(l, jj) + tvec(l, 1) * ik &
                           & + tvec(l, 2) * jk + tvec(l, 3) * kl
                      end do
                      cdi(1, 1) = cdi(1, 1) + chnge2
                      cdi(2, 1) = cdi(2, 1) + chnge2
                      cdi(3, 1) = cdi(3, 1) + chnge2
                      call dhc (pdi, padi, padi, cdi, ndi, jf, jl, if, &
                         & il, aa, 1)
                      do k = 1, 3
                        cdi(k, 2) = cdi(k, 2) + chnge
                        call dhc (pdi, padi, padi, cdi, ndi, jf, jl, if, &
                           & il, ee, 2)
                        cdi(k, 2) = cdi(k, 2) - chnge
                        deriv = (aa-ee) * fpc_9 / chnge
                        dxyz(k, iii+icuc) = dxyz(k, iii+icuc) - deriv * const
                        dxyz(k, jjj+kkkk) = dxyz(k, jjj+kkkk) + deriv * const
                      end do
                    end if
                  end do
                end do
              end do
            else
              kkkk = 0
              do ik = -l1u, l1u
                do jk = -l2u, l2u
                  do kl = -l3u, l3u
                    kkkk = kkkk + 1
                    do l = 1, 3
                      cdi(l, 1) = coord(l, jj) + tvec(l, 1) * ik &
                           & + tvec(l, 2) * jk + tvec(l, 3) * kl
                    end do
                    call delsta (nat, iorbs, p, cdi, dstat, ii, jj)  
                    dxyz(1:3, iii+icuc) = dxyz(1:3, iii+icuc) - dstat(1:3) *  const
                    dxyz(1:3, jjj+kkkk) = dxyz(1:3, jjj+kkkk) + dstat(1:3) *  const
                  end do
                end do
              end do
            end if
          end if
        end if
      end do
    end do
 !   if (nnhco /= 0) then
  !    call mm_peptide_correction (coord, dxyz)
  !  end if
  !  if (nnccr /= 0) then   WARNING
 !     call mm_ring_correction (coord, dxyz)
 !   end if
    useps = refeps
   ! COSMO change A. Klamt
   ! analytic calculation of the gradient of the dielectric energy A.Klamt
    if (useps) then
  !    call diegrd (coord, dxyz, nfirst, nlast, nat, cosurf, iatsp, &
  !         & isude, sude, qscnet, qdenet, qscat, arat) WARNING
    end if
    if (mode ==-1) then
      dxyz(1:3, 1:numat) = -dxyz(1:3, 1:numat)
    end if
    if ( .not. debug) return
    call output_derivatives(dxyz, ncells, large)
end subroutine dcart_for_MOZYME
subroutine output_derivatives(dxyz, ncells, large)
  use molkst_C, only: numat,  l1u, l2u, l3u, id, l123
  use chanel_C, only: iw
  use common_arrays_C, only: tvec, nat
  use elemts_C, only: elemnt
  implicit none
  integer, intent (in) :: ncells
  logical, intent (in) :: large
  double precision, dimension (3, numat*l123), intent (in) :: dxyz
!
! Local
!
  double precision :: sumx, sumy, sumz
  integer :: i, j, ij, k, ik, jk, kl, numtot, loop, alloc_stat
 
  double precision, dimension(:,:), allocatable :: work2
  numtot = numat * ncells
10000 format (//10 x, "CARTESIAN COORDINATE DERIVATIVES",/2 x, "NO. AT.", &
           & 5 x, "X", 12 x, "Y", 12 x, "Z", /)
    write (iw, 10000)
    if (ncells == 1) then
      write (iw, "(I6,A2,3F13.6)") (i, elemnt(nat(i)), &
           & (dxyz(j, i), j=1, 3), i=1, numtot)
    else if (large) then
      write (iw, "(/,A)") &
           & " (At the gradient minimum, these will NOT be zero.)"
      loop = 0
      allocate (work2(3,numat), stat=alloc_stat)
      if (alloc_stat /= 0) then
         call memory_error ("output_derivatives")
         return
      end if
      do i = 1, numat
        sumx = 0.d0
        sumy = 0.d0
        sumz = 0.d0
        ij = 0
        do ik = -l1u, l1u
          do jk = -l2u, l2u
            do kl = -l3u, l3u
              ij = ij + 1
              loop = loop + 1
              sumx = sumx + dxyz(1, loop)
              sumy = sumy + dxyz(2, loop)
              sumz = sumz + dxyz(3, loop)
              if (Abs (dxyz(1, loop)) + Abs(dxyz(2, loop)) &
                   & + Abs(dxyz(3, loop)) > -1.d-5) then
                write (iw, "(3I6,A2,F13.6,2F13.6,3I4)") loop, ij, i, elemnt &
                     & (nat(i)), (dxyz(k, loop), k=1, 3), ik, jk, kl
              end if
            end do
          end do
        end do
        work2(1, i) = sumx
        work2(2, i) = sumy
        work2(3, i) = sumz
      end do
      write (iw,*) " Central Unit Cell Derivatives"
      write (iw, "(/,A)") " (At the gradient minimum, these will be zero.)"
      write (iw, "(I6,A2,F13.6,2F13.6)") (i, elemnt(nat(i)), (work2(j, i), &
           & j=1, 3), i=1, numat)
      if (id == 3) then
        call xyzcry (tvec, numat, work2, iw)
      end if
      deallocate (work2)
    else
      write (iw, "(I6,A2,F13.6,2F13.6)") (i, elemnt(nat((i-1)/ncells+1)), &
           & (dxyz(j, i)+dxyz(j, i+1)+dxyz(j, i+2), j=1, 3), i=1, numtot, 3)
    end if

end subroutine output_derivatives


