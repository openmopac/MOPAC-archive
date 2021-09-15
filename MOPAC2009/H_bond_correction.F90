subroutine h_bond_correction(correction, l_grad)  
!
!    Add a dispersion E_disp, a coulombic, EC, and a repulsive correction, ER,
!    to improve intermolecular interaction energies.
!
  use molkst_C, only : numat, keywrd, line, numcal, E_disp, E_hb, &
    method_pm7, method_PM6, nscf, l123, l1u, l2u, l3u, id
  use parameters_C, only : tore, dh2_a_parameters, &
    par10, par11, par12, par13, par14, par15, par16, par17, par18, par19, par20, par21
  use common_arrays_C, only: coord, nat, q, p, hblist, Vab, &
    set_a, set_b, dxyz, cell_ijk
  use chanel_C, only : iw
  implicit none
  double precision, intent(out) ::  correction
  logical :: l_grad
!
!
!
  integer :: i, j, k, l, D, H, A, i1, nd_list, d_list(9), d_l(9), max_h_bonds, &  ! 
    i_cell, j_cell, nrpairs, iX(100), jOorN(100), &
    ii, nsa, nsb, icalcn = 0, iii, jjj, kkkk, nrbondsc, set(1), bondlist(5,3)
  double precision :: EC, ER, vector(numat), sum, sum1, delta = 1.d-8, &
  xa_dist, xb_dist, old_dist, xh_dist, xc_dist, sum2, a_X(3,2), b_X(3,2), Rab, sum3
  integer, allocatable :: nrbondsa(:), nrbondsb(:)
  double precision, external :: EC_plus_ER, EH_plus, bonding, distance
  
  logical :: twosets, l_disp, l_h_bonds, calc_grad, hbs1_ok, hbs2_ok, first = .true., l_set, &
    DH_plus, D2X, D3
  logical, external :: connected
 ! save :: icalcn, nsa, nsb, l_disp, l_h_bonds, delta, twosets, nrpairs, &
 !   nrbondsa, nrbondsb, set, DH_plus
  save !  Something not saved in above "save" so save everything
  double precision covrad(94)
!
! covalent radii in Angstroms from Pyykko and Atsumi,  Chem. Eur. J. 15,  2009,  188 -197
! values for metals decreased by 10 %,   all multiplied by 4/3 both done as for D3 
!
  data covrad /& 
  &0.32d0,  0.46d0,  1.20d0,  0.94d0,  0.77d0,  0.75d0,  0.71d0,  0.63d0,  0.64d0,  0.67d0, &
  &1.40d0,  1.25d0,  1.13d0,  1.04d0,  1.10d0,  1.02d0,  0.99d0,  0.96d0,  1.76d0,  1.54d0, &
  &1.33d0,  1.22d0,  1.21d0,  1.10d0,  1.07d0,  1.04d0,  1.00d0,  0.99d0,  1.01d0,  1.09d0, &
  &1.12d0,  1.09d0,  1.15d0,  1.10d0,  1.14d0,  1.17d0,  1.89d0,  1.67d0,  1.47d0,  1.39d0, &
  &1.32d0,  1.24d0,  1.15d0,  1.13d0,  1.13d0,  1.08d0,  1.15d0,  1.23d0,  1.28d0,  1.26d0, &
  &1.26d0,  1.23d0,  1.32d0,  1.31d0,  2.09d0,  1.76d0,  1.62d0,  1.47d0,  1.58d0,  1.57d0, &
  &1.56d0,  1.55d0,  1.51d0,  1.52d0,  1.51d0,  1.50d0,  1.49d0,  1.49d0,  1.48d0,  1.53d0, &
  &1.46d0,  1.37d0,  1.31d0,  1.23d0,  1.18d0,  1.16d0,  1.11d0,  1.12d0,  1.13d0,  1.32d0, &
  &1.30d0,  1.30d0,  1.36d0,  1.31d0,  1.38d0,  1.42d0,  2.01d0,  1.81d0,  1.67d0,  1.58d0, &
  &1.52d0,  1.53d0,  1.54d0,  1.55d0 / 
!
!             Cl - N     Br - N    I - N      Cl - O     Br - O     I - O
  data a_X / 1.0489d12, 1.0226d5, 1.2751d12, 4.6783d8,  9.6021d3,  6.0912d5/
  data b_X / -9.946d0, -3.236d0, -9.534d0,  -6.867d0,  -2.900d0,  -4.154d0/
    if (icalcn /= numcal .or. numat < 200 .or. mod(nscf,20) == 0) then 
!
!  First time setup.
!
      if (icalcn /= numcal) then
        if (method_pm6) then
!
!  Load in PM6 parameters
!
          dh2_a_parameters(1) = 1.48d0    ! Nitrogen
          dh2_a_parameters(2) = 1.56d0    ! Oxygen, generic
          dh2_a_parameters(3) = 1.55d0    ! Oxygen, acid
          dh2_a_parameters(4) = 0.96d0    ! Oxygen, peptide
          dh2_a_parameters(5) = 0.76d0    ! Oxygen, water
          dh2_a_parameters(6) = 0.85d0    ! Sulfur
        end if
        if (method_pm7) then
        
!  Load in PM7 parameters
!
          a_X(1,1) = par10*1.d12
          a_X(2,1) = par11*1.d5
          a_X(3,1) = par12*1.d12
          a_X(1,2) = par13*1.d8
          a_X(2,2) = par14*1.d3
          a_X(3,2) = par15*1.d5
          b_X(1,1) = par16
          b_X(2,1) = par17
          b_X(3,1) = par18
          b_X(1,2) = par19
          b_X(2,2) = par20
          b_X(3,2) = par21
        end if
        if (first) then
          covrad = 4.d0/3.d0*covrad !  This must be done once per entire run
          first = .false.
          iX(17) = 1
          iX(35) = 2
          iX(53) = 3
          jOorN(7) = 1
          jOorN(8) = 2
        end if
        icalcn = numcal
      end if
      if (method_pm7) then
        
!  Load in PM7 parameters
!
          a_X(1,1) = par10*1.d12  !  Cl - N
          a_X(2,1) = par11*1.d5   !  Br - N
          a_X(3,1) = par12*1.d12  !  I - N
          a_X(1,2) = par13*1.d8   !  Cl - O
          a_X(2,2) = par14*1.d3   !  Br - O
          a_X(3,2) = par15*1.d5   !  I - O
          b_X(1,1) = par16        !  Cl - N
          b_X(2,1) = par17        !  Br - N
          b_X(3,1) = par18        !  I - N
          b_X(1,2) = par19        !  Cl - O
          b_X(2,2) = par20        !  Br - O
          b_X(3,2) = par21        !  I - O
        end if
!
!  Work out the list of potential hydrogen bonds, hblist.
!
!   Arrays common to all methods
!
      l_disp = (index(keywrd," PM6-D") /= 0 .or. method_PM7)
      D3 = (index(keywrd," PM6-D3 ") /= 0)
      l_h_bonds = (index(keywrd," PM6-DH") + index(keywrd," PM6-H")/= 0 .or. method_PM7)
      j = index(keywrd," PM6-") + index(keywrd," PM7") 
      DH_plus = (index(keywrd, "PM6-DH+") /= 0 .or. method_PM7)
      D2X = (index(keywrd, "PM6-D2X") + index(keywrd, "PM6-DH2X") /= 0 ) !.or. method_PM7)
      if (j /= 0 .or. method_PM7) then
        do l = j + 1, len_trim(keywrd)
          if (keywrd(l:l) == " ") exit  !  Look for blank space marking end of keyword
        end do
        i = 0
        if (j > 0) i = index(keywrd(j:l),"(")
        if (i /= 0) then
          if (allocated(set_a))  deallocate(set_a)
          if (allocated(set_b))  deallocate(set_b)
          allocate (set_a(numat), set_b(numat))   ! two fragments
          j = index(keywrd(i:),")") + i - 1
          line = keywrd(i :j)
!
!   Put one set of atoms in set_a, the other set into set_b
!
          call select_atoms(line, len_trim(line), set_a, nsa, set_b, nsb) 
          twosets = .true.
        else
          nsa = numat
          nsb = 0
          twosets = .false.
          if (allocated(set_a))  deallocate(set_a)      
          allocate (set_a(numat))  
          do i = 1, numat
            set_a(i) = i
          end do 
        end if
      end if
      if (D3) then
        call dftd3(correction, l_grad, dxyz)  
        return
      end if


      if (l_h_bonds) then
!
!  Work out list of of potential hydrogen bonds
!
!   First, find all N-H and O-H groups 
!
        if (allocated(hblist)) deallocate(hblist) 
        max_h_bonds = 0
        do j = 1, numat
          if (nat(j) == 8 .or. nat(j) == 7) max_h_bonds = max_h_bonds + 1
        end do
        if (id == 0) then
          max_h_bonds = max_h_bonds*110
        else
          max_h_bonds = max_h_bonds*250
        end if
        if (max_h_bonds == 0) l_h_bonds = .false.
      end if
      if (l_h_bonds) then
        if (DH_plus) then          
          if (allocated(nrbondsa))   deallocate(nrbondsa)
          if (allocated(nrbondsb))   deallocate(nrbondsb) 
          if (allocated(hblist))     deallocate(hblist) 
          allocate (hblist(max_h_bonds,10), nrbondsa(max_h_bonds), nrbondsb(max_h_bonds), stat=i)   
          if (i /= 0) then
            line = " Cannot allocate arrays for PM6-DH+"
            write(iw,*)" Cannot allocate arrays for PM6-DH+"
            call to_screen(trim(line))
            call mopend(trim(line))
            return
          end if
          hblist(:,:) = 0
        else            
          allocate(hblist(max_h_bonds,7), stat=i)
          if (i /= 0) then
            line = " Cannot allocate arrays for PM6-DH2"
            write(iw,*)" Cannot allocate arrays for PM6-DH2"
            call to_screen(trim(line))
            call mopend(trim(line))
            return
          end if
        end if  
        nrpairs = 0
        if (DH_plus) then        
          call all_h_bonds(hblist(1,1), hblist(1,9), hblist(1,5), max_h_bonds, nrpairs, nsa, nsb) 
        else
          call all_h_bonds(hblist(1,1), hblist(1,2), hblist(1,3), max_h_bonds, nrpairs, nsa, nsb)
        end if          
!
!
        if (DH_plus) then
!
!  Identify atoms associated with the hydrogen bonds
!
          do i = 1, nrpairs
            nrbondsa(i) = 0
            nrbondsb(i) = 0
            bondlist(:,  :) = 0
            do j = 1, numat
              xa_dist = distance(j, hblist(i, 1))
              xb_dist = distance(j, hblist(i, 5))
              if (xa_dist < bonding(j, hblist(i, 1), covrad) .and. hblist(i, 1) /= j) then
!
!  Atom j is covalently bonded to atom hblist(i,1) (either the H bond donor or accepter)
!
                nrbondsa(i) = nrbondsa(i) + 1 
                bondlist(nrbondsa(i), 1) = j
                if (nrbondsa(i) == 5) then 
!
!  Too many bonds - eliminate the longest bond
!
                  i1 = hblist(i, 1)
                  sum = 0.d0
                  do k = 1, 5
                    sum1 = distance(i1, bondlist(k,1))
                    if (sum < sum1) then
                      sum = sum1
                      l = k
                    end if
                  end do
                  do k = l, 4
                    bondlist(k,1) = bondlist(k + 1,1)
                  end do
                  nrbondsa(i) = 4
                end if
              endif
              if (xb_dist < bonding(j, hblist(i, 5), covrad) .and. hblist(i, 5) /= j) then
!
!  Atom j is covalently bonded to atom hblist(i,5) (either the H bond donor or accepter)
!
                nrbondsb(i) = nrbondsb(i) + 1
                bondlist(nrbondsb(i), 2) = j
                if (nrbondsb(i) == 5) then 
!
!  Too many bonds - eliminate the longest bond
!
                  i1 = hblist(i, 5)
                  sum = 0.d0
                  do k = 1, 5
                    sum1 = distance(i1, bondlist(k,2))
                    if (sum < sum1) then
                      sum = sum1
                      l = k
                    end if
                  end do
                  do k = l, 4
                    bondlist(k,2) = bondlist(k + 1,2)
                  end do
                  nrbondsb(i) = 4
                end if
              endif
            end do 
! check for 1-3 and 1-4 case - very seldom needed,  but ... 
!
!  Is an atom, attached to hblist(i,1) equal to atom hblist(i,5)
!  That is, are the donor and accepter atoms the same.
!
            do j = 1, nrbondsa(i)
              if (bondlist(j, 1) == hblist(i, 5)) hblist(i, 10) = -666     !  1-3
              do k = 1, nrbondsb(i)
!
!  Is an atom, attached to hblist(i,1) equal to atom attached to hblist(i,5)
!  and not the hydrogen atom
!  That is, are both donor and accepter atoms attached to the same atom, other than the hydrogen
!
                if (bondlist(j, 1) == bondlist(k, 2) .and. bondlist(k, 2) /= hblist(i,9)) hblist(i, 10) = -666 !  1-4
              end do
            end do
            if (hblist(i, 10) == -666) cycle
! hbs1 part
            hbs1_ok = .true.
            if (nrbondsa(i) == 3 .or. nrbondsa(i) == 4) then
              old_dist = -1
              do k = 1, nrbondsa(i)
                xh_dist = distance(bondlist(k, 1), hblist(i, 9))
                if (xh_dist > old_dist) then
                  old_dist = xh_dist
                  hblist(i, 2) = bondlist(k, 1)
                endif
              end do 
              old_dist = -1
              do k = 1, nrbondsa(i)
                xh_dist = distance(bondlist(k, 1), hblist(i, 9))
                if (xh_dist > old_dist .and. bondlist(k, 1) /= hblist(i, 2)) then
                  old_dist = xh_dist
                  hblist(i, 3) = bondlist(k, 1)
                endif
              end do
              old_dist = -1
              do k = 1, nrbondsa(i)
                xh_dist = distance(bondlist(k, 1), hblist(i, 9))
                if (xh_dist > old_dist .and. bondlist(k, 1) /= hblist(i, 2) .and. bondlist(k, 1) /= hblist(i, 3)) then
                  old_dist = xh_dist
                  hblist(i, 4) = bondlist(k, 1)
                endif
              end do    ! possible forth atom should be h
            else if (nrbondsa(i) == 2) then
              old_dist = -1
              do k = 1, nrbondsa(i)
                xh_dist = distance(bondlist(k, 1), hblist(i, 9))
                if (xh_dist > old_dist) then
                  old_dist = xh_dist
                  hblist(i, 2) = bondlist(k, 1)
                endif
              end do
              do k = 1, nrbondsa(i)
                if (bondlist(k, 1) /= hblist(i, 2)) then
                  hblist(i, 3) = bondlist(k, 1)
                endif
              end do
              if (distance(hblist(i, 1), hblist(i, 9)) < bonding(hblist(i, 1), hblist(i, 9), covrad)) then ! not needed 
                  hblist(i, 4) = hblist(i, 9)
              else
                  hblist(i, 4) = hblist(i, 1) 
              endif                  
            else if (nrbondsa(i) == 1) then
              hblist(i, 2) = bondlist(1, 1)
! need bonds on first bonded atom here
              nrbondsc = 0
              do k = 1, numat
                xc_dist = distance(k, hblist(i, 2))
                if (xc_dist < bonding(k, hblist(i, 2), covrad) .and. hblist(i, 2) /= k) then
                  nrbondsc = nrbondsc + 1
                  bondlist(nrbondsc, 3) = k
                endif
              end do
              old_dist = -1
              do k = 1, nrbondsc
                xh_dist = distance(bondlist(k, 3), hblist(i, 9))
                if (xh_dist > old_dist) then
                  old_dist = xh_dist
                  hblist(i, 3) = bondlist(k, 3)
                endif
              end do
              if (distance(hblist(i, 1), hblist(i, 9)) < bonding(hblist(i, 1), hblist(i, 9), covrad)) then ! not needed
                  hblist(i, 4) = hblist(i, 9)
              else
                  hblist(i, 4) = hblist(i, 1)
              endif
            else if (nrbondsa(i) == 0) then
              if (distance(hblist(i, 1), hblist(i, 9)) < bonding(hblist(i, 1), hblist(i, 9), covrad)) then ! not needed
                hblist(i, 2) = hblist(i, 9) 
                hblist(i, 3) = hblist(i, 9)
                hblist(i, 4) = hblist(i, 9)
              else
                hblist(i, 2) = hblist(i, 1) 
                hblist(i, 3) = hblist(i, 1)
                hblist(i, 4) = hblist(i, 1)
              endif
            else
              hbs1_ok = .false.
            endif 
! hbs2 part
            hbs2_ok = .true.
            if (nrbondsb(i) == 3 .or. nrbondsb(i) == 4) then
              old_dist = -1
              do k = 1, nrbondsb(i)
                xh_dist = distance(bondlist(k, 2), hblist(i, 9))
                if (xh_dist > old_dist) then
                  old_dist = xh_dist
                  hblist(i, 6) = bondlist(k, 2)
                endif
              end do 
              old_dist = -1
              do k = 1, nrbondsb(i)
                xh_dist = distance(bondlist(k, 2), hblist(i, 9))
                if (xh_dist > old_dist .and. bondlist(k, 2) /= hblist(i, 6)) then
                  old_dist = xh_dist
                  hblist(i, 7) = bondlist(k, 2)
                endif
              end do
              old_dist = -1
              do k = 1, nrbondsb(i)
                xh_dist = distance(bondlist(k, 2), hblist(i, 9))
                if (xh_dist > old_dist .and. bondlist(k, 2) /= hblist(i, 6) .and. bondlist(k, 2) /= hblist(i, 7)) then
                  old_dist = xh_dist
                  hblist(i, 8) = bondlist(k, 2)
                endif
              end do      ! possible forth atom should be h
            else if (nrbondsb(i) == 2) then
              old_dist = -1
              do k = 1, nrbondsb(i)
                xh_dist = distance(bondlist(k, 2), hblist(i, 9))
                if (xh_dist > old_dist) then
                  old_dist = xh_dist
                  hblist(i, 6) = bondlist(k, 2)
                endif
              end do
              do k = 1, nrbondsb(i)
                if (bondlist(k, 2) /= hblist(i, 6)) then
                  hblist(i, 7) = bondlist(k, 2)
                endif
              end do
              if (distance(hblist(i, 5), hblist(i, 9)) < bonding(hblist(i, 5), hblist(i, 9), covrad)) then ! not needed
                  hblist(i, 8) = hblist(i, 9)
              else
                  hblist(i, 8) = hblist(i, 5)
              endif
            else if (nrbondsb(i) == 1) then
              hblist(i, 6) = bondlist(1, 2)
! need bonds on first bonded atom here
              nrbondsc = 0
              do k = 1, numat
                xc_dist = distance(k, hblist(i, 6))
                if (xc_dist < bonding(k, hblist(i, 6), covrad) .and. hblist(i, 6) /= k) then
                  nrbondsc = nrbondsc + 1
                  bondlist(nrbondsc, 3) = k
                endif
              end do
              old_dist = -1
              do k = 1, nrbondsc
                xh_dist = distance(bondlist(k, 3), hblist(i, 9))
                if (xh_dist > old_dist) then
                  old_dist = xh_dist
                  hblist(i, 7) = bondlist(k, 3)
                endif
              end do
              if (distance(hblist(i, 5), hblist(i, 9)) < bonding(hblist(i, 5), hblist(i, 9), covrad)) then ! not needed
                  hblist(i, 8) = hblist(i, 9)
              else
                  hblist(i, 8) = hblist(i, 5)
              endif
            else if (nrbondsb(i) == 0) then
              if (distance(hblist(i, 5), hblist(i, 9)) < bonding(hblist(i, 5), hblist(i, 9), covrad)) then ! not needed
                  hblist(i, 6) = hblist(i, 9) 
                  hblist(i, 7) = hblist(i, 9)
                  hblist(i, 8) = hblist(i, 9)
              else
                  hblist(i, 6) = hblist(i, 5) 
                  hblist(i, 7) = hblist(i, 5)
                  hblist(i, 8) = hblist(i, 5)
              endif
            else
              hbs2_ok = .false.
            endif 
          end do
! check for problems
          if ( .not. hbs1_ok .or. .not. hbs2_ok ) then
!
!  No hydrogen bonds found
!
            l_h_bonds = .false.   
          endif
        else
!
!   Determine hydrogen bond type in PM6-DH2
!
!   -- typing done in EC_plus_ER
!
        end if
      end if
    end if
    if (l_disp) then
!
!
!   Now calculate the dispersion (E_disp) and hydrogen-bond (E_hb) contributions
!
!
      if (twosets) then
        call PM6_DH_Dispersion (E_disp, set_a, set_b, nsa, nsb, twosets)
      else
        call PM6_DH_Dispersion (E_disp, set_a, set_a, nsa, nsa, twosets)
      end if
    else
      E_disp = 0.d0
    end if
    correction = 0.d0
    if (D2X) then
!
! Add in Rezac and Hobza's correction: "A halogen-bonding correction for the semiempirical PM6 method"
! Chem. Phys. Lett. 506 286-289 (2011)
!
      sum = 0.d0
      do i = 1, numat
        if (nat(i) /= 17 .and. nat(i) /= 35 .and. nat(i) /= 53) cycle ! crude, but fast
        k = nat(i)
        do j = 1, numat
          if (nat(j) /= 7 .and. nat(j) /= 8) cycle
          l = nat(j)
          sum1 = 1.1d0*(covrad(k) + covrad(l))
          Rab = distance(i, j)
          sum2 = a_X(iX(k),jOorN(l))
          sum3 = b_X(iX(k),jOorN(l))
          sum = sum + sum2*exp(sum3*max(Rab, sum1))
          if (l_grad) then
            if (connected(i,j, 8.d0**2)) then
              if (Rab > sum1) then
!
!   kkkk is the cell that atom j is in, relative to atom i
!
                iii = l123*(i - 1)
                jjj = l123*(j - 1)
                kkkk = (l3u - cell_ijk(3)) + (2*l3u + 1)*(l2u - cell_ijk(2) + (2*l2u + 1)*(l1u - cell_ijk(1)))
                i_cell = iii + kkkk 
                j_cell = jjj - kkkk             
                do l = 1,3
                  dxyz(i_cell*3 + l) = dxyz(i_cell*3 + l) + Vab(l)*sum2*sum3*exp(sum3*Rab)/Rab
                  dxyz(j_cell*3 + l) = dxyz(j_cell*3 + l) - Vab(l)*sum2*sum3*exp(sum3*Rab)/Rab
                end do
              end if
            end if
          end if
        end do
      end do
      correction = sum
    end if
    if (l_grad) then
!
!  Only calculate gradient contributions when geometry is nearly optimized
!
    !  sum = dot(dxyz, dxyz, l123*numat*3)
      calc_grad = .true. ! (sum/numat < 100.d0) 
    else
      calc_grad = .false.
    end if
    E_hb = 0.d0
    if (l_h_bonds) then
      if( .not. DH_plus) then
        call chrge (p, vector)  ! PM6-DH2 needs partial charges
        do i = 1, numat 
          j = nat(i) 
          q(i) = tore(j) - vector(i)
        end do
      end if
!
!  Calculate Hydrogen bond energy correction
!
!    The hydrogen-bond acceptor types are as follows:
!
!    (1) nitrogen with no hydrogens bonded to it (mostly in aromatic rings) interacting with any hydrogen,
!    (2) nitrogen with one hydrogen (secondary amines) interacting with any hydrogen,
!    (3) nitrogen with two or more hydrogens (primary amines, ammonia) interacting with any hydrogen,
!    (4) oxygen except carbonyl interacting with HN,
!    (5) carbonyl oxygen interacting with HN,
!    (6) oxygen interacting with HO hydrogen different from 7 and 8,
!    (7) oxygen interacting with H in a water molecule, and
!    (8) oxygen interacting with H in a carboxyl group.
!
     
      do ii = 1, nrpairs
        if (DH_plus) then
          H = hblist(ii,9)
          A = hblist(ii,1)
          D = hblist(ii,5)
          sum = EH_plus(ii, hblist, max_h_bonds, nrbondsa, nrbondsb)
          E_hb = E_hb + sum
          nd_list=0
          do i = 1,9
            if (hblist(ii,i) > 0) then
              j = hblist(ii,i)
              do l = 1, nd_list
                if (d_list(l) == j) exit
              end do
              if (l > nd_list) then
                nd_list = nd_list + 1
                d_list(nd_list) = hblist(ii,i)
              end if 
            end if
          end do
        else
          H = hblist(ii,2)
          A = hblist(ii,3)
          D = hblist(ii,1)
          sum = EC_plus_ER(hblist(ii,1), hblist(ii,2), hblist(ii,3), q(hblist(ii,2)), q(hblist(ii,3)), EC, ER, d_list, nd_list)       
          E_hb = E_hb + EC + ER
        end if
        correction = correction + sum
!
! Add in gradient contribution, if requested
!
        if (calc_grad .and. sum < -0.01d0) then
          do j = 1, nd_list
            k = d_list(j)
            iii = l123*(k - 1)
            if (connected(H, k, 8.d0**2)) then
!
!   kkkk is the cell that atom k is in, relative to atom H
!
              kkkk = (l3u - cell_ijk(3)) + (2*l3u + 1)*(l2u - cell_ijk(2) + (2*l2u + 1)*(l1u - cell_ijk(1)))
              i_cell = iii + kkkk
              do i = 1, 3             
                coord(i,k) = coord(i,k) + delta
                if (DH_plus) then
                  sum1 = EH_plus(ii, hblist, max_h_bonds, nrbondsa, nrbondsb)
                else
                  sum1 = EC_plus_ER(D, H, A, q(H), q(A), EC, ER, d_l, l)
                end if           
                sum1 = (sum1 - sum)/delta             
                if (Abs(sum1) < 50.d0) then
                  dxyz(i_cell*3 + i) = dxyz(i_cell*3 + i) + sum1
                end if
                coord(i,k) = coord(i,k) - delta
              end do
            end if
          end do
        end if     
      end do
    end if
!
! Add in dispersion contribution to gradient, if requested
!
    if (calc_grad .and. l_disp) then
!
!   Dispersion contribution is small, so only calculate the CUC.
!
      do k = 1, numat
        l_set = .false.
        do i = 1, nsa
          if (set_a(i) == k) then
            l_set = .true.
            exit
          end if
        end do
        set(1) = k
        if (l_set) then
          if (twosets) then
            call PM6_DH_Dispersion (sum2, set, set_b, 1, nsb, .true.)
          else
            call PM6_DH_Dispersion (sum2, set, set_a, 1, nsa, .true.)
          end if
        else
          if (twosets) then
            call PM6_DH_Dispersion (sum2, set_a, set, nsa, 1, .true.)
          else
            call PM6_DH_Dispersion (sum2, set_a, set, nsa, 1, .true.)
          end if
        end if 
        do i = 1, 3             
          coord(i,k) = coord(i,k) + delta   
          if (l_set) then
            if (twosets) then
              call PM6_DH_Dispersion (sum, set, set_b, 1, nsb, .true.)
            else
              call PM6_DH_Dispersion (sum, set, set_a, 1, nsa, .true.)
            end if
          else
            if (twosets) then
              call PM6_DH_Dispersion (sum, set_a, set, nsa, 1, .true.)
            else
              call PM6_DH_Dispersion (sum, set_a, set, nsa, 1, .true.)
            end if
          end if 
          sum1 = (sum2 - sum)/delta             
          if (Abs(sum1) < 50.d0) then
            dxyz((k - 1)*3 + i) = dxyz((k - 1)*3 + i) - sum1
          end if
          coord(i,k) = coord(i,k) - delta
        end do
      end do
    end if
    correction = correction  + E_disp
    return
end subroutine h_bond_correction









logical function connected(atom_i, atom_j, criterion)
!
!   "connected" is true if atom_i and atom_j are within sqrt(criterion) Angstroms
!   of each other, false otherwise.
!
!   if "connected" then:
!
!   If the system is infinite (polymer, layer or solid),
!   then cell_ijk will hold the unit cell translation indices that move atom_j near to
!   atom_i, zero therwise.
!
!   Vab is the postition of atom_i relative to atom_j
!
!   Rab is distance from atom_i to atom_j
!
!   if not "connected" then Rab, Vab, and cell_ijk are meaningless.
!
  use molkst_C, only : id, l11, l21, l31, Rab
  use common_arrays_C, only: tvec, coord, cell_ijk, Vab
  implicit none
  double precision ::  criterion
  integer :: atom_i, atom_j, ii, jj, kk
  double precision :: V_ab(3), R_ab
!
  if (id == 0) then
    Vab = coord(:,atom_i) - coord(:,atom_j)
    Rab = Vab(1)**2 + Vab(2)**2 + Vab(3)**2 
  else
    Rab = 1.d8
    do ii = -l11, l11 
      do jj = -l21, l21 
        do kk = -l31, l31 
          V_ab = coord(:,atom_i) - coord(:,atom_j) + tvec(:,1)*ii + tvec(:,2)*jj + tvec(:,3)*kk 
          R_ab = V_ab(1)**2 + V_ab(2)**2 + V_ab(3)**2 
          if (R_ab < Rab) then
            Rab = R_ab
            Vab = V_ab
            cell_ijk(1) = ii
            cell_ijk(2) = jj
            cell_ijk(3) = kk
          end if                  
        end do 
      end do 
    end do 
  end if 
  connected = (Rab < criterion) 
  if (connected) Rab = sqrt(Rab)
  return       
end function connected
subroutine select_atoms(text, len_text, set_a, nsa, set_b, nsb)
  use molkst_C, only : numat
  use reada_I 
  implicit none
  integer :: len_text, set_a(numat), set_b(numat), nsa, nsb
  character (len = len_text):: text
  integer :: i, j, k, l
  set_a = 0
!
! Identify all blocks of numat
!
  do
    i = index(text,"-") 
    if (i ==  0) i = index(text,":")
    if (i ==  0) exit
    do j = i - 1, 1, -1
      if (ichar(text(j:j)) > ichar("9") .or.  ichar(text(j:j)) < ichar("0")) exit
    end do
    k = nint(reada(text, j))
    l = nint(reada(text, i))
    do l = k,l
      set_a(l) = 1
    end do
    do k = i + 1, len_text
      if (ichar(text(k:k)) > ichar("9") .or.  ichar(text(k:k)) < ichar("0")) exit
    end do
    text(j:k) = " "
  end do
!
!  Identify single numat
!
  i = 1
  do
    do j = i, len_text
      if (ichar(text(j:j)) <=  ichar("9") .and.  ichar(text(j:j)) >=  ichar("0")) exit
    end do
    if (j >=  len_text) exit
    set_a(nint(reada(text, j))) = 1
    do i = j, len_text
      if (ichar(text(i:i)) > ichar("9") .or.  ichar(text(i:i)) < ichar("0")) exit
    end do
  end do
  j = 0
  k = 0
  do i = 1, numat
    if (set_a(i) /=  0) then
      j = j + 1
      set_a(j) = i
    else
      k = k + 1
      set_b(k) = i
    end if
  end do
  nsa = j
  nsb = k
  return
end 
subroutine find_XH_bonds(set_a, nsa, acc, nacc, h_b, nhb)
!
!  Locate all hydrogen atoms involved in O-H and N-H bonds
!  On exit:
!  nhb:   Number of hydrogen atoms involved in O-H or N-H bonds
!  h_b:   Atom numbers of the hydrogen atoms
!
  use molkst_C, only : numat, keywrd, method_PM7
  use common_arrays_C, only: nat
  implicit none
  integer :: nsa, nacc, nhb

  integer :: set_a(nsa), acc(numat), h_b(numat)
  integer :: ii, i, jj, j, is
  double precision :: RAH
  logical :: used(numat)
  logical, external :: connected
    if (index(keywrd, "PM6-DH+") /= 0 ) then
      RAH = 1.4d0
      is = 8
    else if (method_PM7) then
      RAH = 1.4d0
      is = 8  ! Change to 16 ASAP
    else
      RAH = 1.15d0
      is = 16
    end if
    used = .false.
    nacc = 0
    nhb  = 0
    do ii = 1, nsa
      i = set_a(ii)
      if (nat(i) ==  7 .or. nat(i) ==  8 .or. nat(i) ==  is) then
        nacc = nacc + 1
        acc(nacc) = i
        do jj = 1, nsa
          j = set_a(jj)
          if (nat(j) ==  1 .and. .not. used(j)) then
            if (connected(i, j, RAH**2)) then
              nhb = nhb + 1
              h_b(nhb) = j
              used(j) = .true.
            end if
          end if
        end do
      end if
    end do
end subroutine find_XH_bonds
subroutine find_H__Y_bonds(acc_a, nacc_a, acc_b, nacc_b, bonding_a_h, nb_a_h, hblist1, hblist2, hblist3, max_h_bonds, nrpairs)
!
!  Find all sets of three atoms that form a hydrogen bond
!
!   acc_a:       All O and N atoms 
!   bonding_a_h: Hydrogen atoms bonded to acc_a, not in any particular order
!
  use chanel_C, only : iw
  use funcon_C, only : pi
  use molkst_C, only : keywrd, method_pm7
  implicit none
  integer :: nacc_a, nacc_b, nb_a_h, nrpairs, max_h_bonds
  integer :: acc_a(nacc_a), acc_b(nacc_b), bonding_a_h(nb_a_h), hblist1(max_h_bonds), hblist2(max_h_bonds), hblist3(max_h_bonds)
  integer:: ii, i, jj, j, kk, k, i1
  double precision :: RAH, cutoff
  logical, external :: connected
  double precision, external :: angle
  if (index(keywrd, "PM6-DH+") /= 0 ) then
      RAH = 1.4d0
      cutoff = 10.d0
    else if (method_PM7) then
      RAH = 1.4d0 
      cutoff = 7.d0
    else
      RAH = 1.15d0
      cutoff = 7.d0
    end if
    do ii = 1, nacc_a
      i = acc_a(ii)   !  i = Acceptor atom bonded to H  
      do jj = 1, nb_a_h
        j = bonding_a_h(jj)   !  j = Hydrogen bonded to acceptor atom
        if (connected(i, j, RAH**2)) then
          do kk = 1, nacc_b
            k = acc_b(kk)  
            if (k /= i) then
              if (connected(k, j, cutoff**2)) then
                if (angle(k,j,i) > pi*0.5d0) then
!
!  Eliminate bonds of type O(n) - H - O(m) if O(m) - H - O(n) exists
!
                  do i1 = 1, nrpairs
                    if (hblist2(i1) /= j) cycle
                    if (hblist1(i1) /= k) cycle
                    if (hblist3(i1) /= i) cycle
                    exit
                  end do
                  if (i1 /= nrpairs + 1) exit                  
                  nrpairs = nrpairs + 1 !  k = Distant acceptor atom
                  if (nrpairs > max_h_bonds) then
                    write(iw,'(a)')" The default array size for hydrogen bonds is too small"
                    write(iw,'(a,i6,a,i6)')" Array size:", max_h_bonds,", estimated size needed:", &
                      nint(float(max_h_bonds)*float(nacc_a)/float(ii))
                    if (index(keywrd, "PM6-DH+") /= 0 ) then
                      write(iw,'(a)')" If possible, use PM6-DH+=(text)"
                      call web_message(iw,"PM6_DH_plus.html")
                    else
                      write(iw,'(a)')" If possible, use PM6-DH2=(text)"
                      call web_message(iw,"PM6_DH2.html")
                    end if
                    call mopend(" The default array size for hydrogen bonds is too small")
                    nrpairs = nrpairs - 1
                    return
                  end if
                  hblist3(nrpairs) = k ! O or N hydrogen-bonded to hydrogen j
                  hblist2(nrpairs) = j ! H singly bonded to i
                  hblist1(nrpairs) = i ! O or N of single bond to H
                end if
              end if
            end if
          end do                
        end if          
      end do
    end do
  end subroutine find_H__Y_bonds
  subroutine PM6_DH_Dispersion(E_disp, set_a, set_b, nsa, nsb, twosets)
!
!  Based on materials provided by Jan Rezak, as described in:
!
!  "A Transferable H-bonding Correction For Semiempirical Quantum-Chemical Methods"
!  Martin Korth, Michal Pitonak, Jan Rezac and Pavel Hobza, 
! 
!  J Chem Theory and Computation 6:344-352 (2010)
!
    use common_arrays_C, only: nat, nbonds
    use parameters_C, only : par5, par6, par7
    use molkst_C, only : Rij => Rab, method_PM7, numcal, line
    use chanel_C, only : iw
    use elemts_C, only : atom_names
    implicit none
    double precision :: E_disp
    integer :: nsa, nsb
    integer :: set_a(nsa), set_b(nsb)
    logical :: twosets, first = .true., l_eles(110)
    double precision :: alpha = 20.d0, s = 1.04d0, cscale = 0.89d0, C6i, C6j
    integer :: ii, jj, i, j, ni, nj, j_start, icalcn = 0
    double precision :: C(86),R(86),N(86) 
    double precision :: C6, R0, E_disp_tmp, damp
    logical, external :: connected
    save :: icalcn

! C6 parameters (J.nm^6/mol)
    data C/ &
     0.16D0, 0.084D0,0.00D0, 0.00D0, 5.79D0, 1.65D0, 1.11D0, 0.70D0,   &  !  H He Li Be  B  C  N  O
     0.57D0, 0.45D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 3.25D0, 5.79D0,   &  !  F Ne Na Mg Al Si  P  S
     5.97D0, 3.71D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Cl Ar  K Ca Sc Ti  V Cr
     0.00D0, 0.00D0, 0.04D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Mn Fe Co Ni Cu Zn Ga Ge
     0.00D0, 0.00D0,11.60D0, 4.47D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! As Se Br Kr Rb Sr  A Zr
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Nb Mo Tc Ru Rh Pd Ag Cd
     0.00D0, 0.00D0, 0.00D0, 0.00D0,25.80D0,16.50D0, 0.00D0, 0.00D0,   &  ! In Sn Sb Te  I Xe Cs Ba
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! La Ce Pr Nd Pm Sm Eu Gd
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Tb Dy Ho Er Tm Yb Lu Hf
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Ta  W Re Os Ir Pt Au Hg
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0/                      ! Tl Pb Bi Po At Rn

! R0 parameters (pm)
    data R/ &
      156.d0,  140.d0,  000.d0,  000.d0,  180.d0,  170.d0,  155.d0,  152.d0,   &  !  H He Li Be  B  C  N  O
      147.d0,  154.d0,  000.d0,  000.d0,  000.d0,  000.d0,  180.d0,  180.d0,   &  !  F Ne Na Mg Al Si  P  S
      175.d0,  188.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,   &  ! Cl Ar  K Ca Sc Ti  V Cr
      000.d0,  000.d0,  140.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,   &  ! Mn Fe Co Ni Cu Zn Ga Ge
      000.d0,  000.d0,  185.d0,  202.d0,  000.d0,  000.d0,  000.d0,  000.d0,   &  ! As Se Br Kr Rb Sr  A Zr
      000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,   &  ! Nb Mo Tc Ru Rh Pd Ag Cd
      000.d0,  000.d0,  000.d0,  000.d0,  198.d0,  216.d0,  000.d0,  000.d0,   &  ! In Sn Sb Te  I Xe Cs Ba
      000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,   &  ! La Ce Pr Nd Pm Sm Eu Gd
      000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,   &  ! Tb Dy Ho Er Tm Yb Lu Hf
      000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0,   &  ! Ta  W Re Os Ir Pt Au Hg
      000.d0,  000.d0,  000.d0,  000.d0,  000.d0,  000.d0/                        ! Tl Pb Bi Po At Rn

! Neff (electrons) - Slater-Kirkwood effective number of electrons
    data N/ &
     0.80D0, 1.42D0, 0.00D0, 0.00D0, 2.16D0, 2.50D0, 2.82D0, 3.15D0,   &  !  H He Li Be  B  C  N  O
     3.48D0, 3.81D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 4.50D0, 4.80D0,   &  !  F Ne Na Mg Al Si  P  S
     5.10D0, 5.40D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Cl Ar  K Ca Sc Ti  V Cr
     0.00D0, 0.00D0, 2.90D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Mn Fe Co Ni Cu Zn Ga Ge
     0.00D0, 0.00D0, 6.00D0, 6.30D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! As Se Br Kr Rb Sr  A Zr
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Nb Mo Tc Ru Rh Pd Ag Cd
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 6.95D0, 7.25D0, 0.00D0, 0.00D0,   &  ! In Sn Sb Te  I Xe Cs Ba
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! La Ce Pr Nd Pm Sm Eu Gd
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Tb Dy Ho Er Tm Yb Lu Hf
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0,   &  ! Ta  W Re Os Ir Pt Au Hg
     0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0/                      ! Tl Pb Bi Po At Rn
!
! dispersion energy 
!
    E_disp = 0.d0
    if (method_PM7) then
      alpha  = par5
      s      = par6
      cscale = par7
    end if
    l_eles = .true.
    do ii = 1, nsa
      i = set_a(ii)
      ni = nat(i)
      if (ni > 86) cycle
      if (R(ni).eq.0.0.or.C(ni).eq.0.0 .or. N(ni).eq.0.0) then
        if (method_PM7) cycle
        if (icalcn /= numcal .and. l_eles(ni)) then
          l_eles(ni) = .false.
          line = atom_names(ni)
          do
            if (line(1:1) /= " ") exit
            line(1:) = line(2:)
          end do
          if (first) write(iw,'(///,6(a,/))') "          *********************************************", &
                                              "          *                                           *", &
                                              "          *                 WARNING                   *", &  
                                              "          *                                           *", &    
                                              "          *********************************************"          
          write(iw,'(10x,a)') "Dispersion parameters missing for "//trim(line)
        end if
        first = .false.
        cycle
      end if
      if (twosets) then
        j_start = 1
      else
        j_start = ii + 1
      end if
      do jj  = j_start, nsb
        j = set_b(jj) 
        if (i == j) cycle    
        nj = nat(j)
        if (ni == 6) then
          if (nbonds(i) == 4) then
            C6i = 0.95d0
          else
            C6i = 1.65d0
          end if
        else
          C6i = C(ni)
        end if
        if (nj == 6) then
          if (nbonds(j) == 4) then
            C6j = 0.95d0
          else
            C6j = 1.65d0
          end if
        else
          C6j = C(nj)
        end if
        if (R(nj).eq.0.0.or.C(nj).eq.0.0 .or. N(nj).eq.0.0) cycle
        C6 = 2.d0*(C6i**2*C6j**2*N(ni)*N(nj))**(1.d0/3.d0)/((C6i*N(nj)**2)**(1.d0/3.d0) + &
          (C6j*N(ni)**2)**(1.d0/3.d0))
        R0 = (R(ni)**3+R(nj)**3)/(R(ni)**2+R(nj)**2) /1000.d0*2.d0    ! pm to nm
! atomic distance Rij, in nm
        if (connected(i, j, 100.d0**2)) then
          Rij = Rij*0.1d0
          damp = 1.d0 / (1.d0 + exp(-alpha * (Rij/(s*R0) - 1.d0)))
! i-j dispersion energy
          E_disp_tmp = C6/Rij**6*damp/(1000.d0*4.184d0)
          E_disp = E_disp - E_disp_tmp
        end if
      end do
    end do
    if (icalcn /= numcal .and. .not. first) &
    write(iw,'(/,6(a,/))')   "          *********************************************", &
                             "          *                                           *", &
                             "          *              END OF WARNING               *", &  
                             "          *                                           *", &    
                             "          *********************************************"          

    icalcn = numcal
    E_disp = E_disp*cscale
    return
  end subroutine PM6_DH_Dispersion
  double precision function EC_plus_ER(D, H, A, q1, q2, EC, ER, d_list, nd_list)
!
! Based on "A Transferable H-bonding Correction For Semiempirical Quantum-Chemical Methods", 
! by Martin Korth, Michal Pitonak, Jan Rezac and Pavel Hobza, J. Chem. Theory Comput., 2010, 6 (1), pp 344:352
!
  use common_arrays_C, only: coord, nat, nbonds, ibonds
  use molkst_C, only : Rab
  use parameters_C, only : dh2_a_parameters
  use funcon_C, only : pi, a0, eV, fpc_9
  implicit none
  integer :: D, H, A, R1, R2, R3, C_of_CO, i, nH, nC, nO, nN, N_of_HNCO, ii, nd_list, d_list(8)
  double precision :: q1, q2, EC, ER
  double precision :: angle_cos, r, torsion_check, angle, angle2_shift, &
    angle2_shift_2, torsion_shift, angle2_cos, angle2, angle2_cos_2, &
    torsion, torsion_cos, torsion2, torsion_2, torsion_ref, &
    torsion_cos_2, unit_part, attraction, torsion_correct, &
    expo, repulsion, rep_pre, rep_exp, c, sum, multiplier_a, sum_max
  logical :: torsion_check_set, peptide, torsion_check_set2
  double precision, external :: truncation
  logical, external :: connected
  EC_plus_ER = 0.d0
  EC = 0.d0
  ER = 0.d0
!
!  D is the atom that hydrogen atom H is attached to, and A is the distant acceptor
!
!
!  First angle
!
    call bangle (coord, D, H, A, angle)
    d_list(1) = D
    d_list(2) = H
    d_list(3) = A
    nd_list = 3  
    angle_cos = -cos(angle)
    if (angle_cos < 0.d0) return
!
! get target angles
!
    torsion_check = 0.0 
    torsion_check_set = .false.
    torsion_check_set2 = .false.
    if (nat(A) == 8 .or. nat(A) == 16) then
      if (nbonds(A) == 1) then 
        angle2_shift = pi 
        angle2_shift_2 = pi/(180.d0/120.d0) 
        torsion_shift = 0.0
        torsion_check_set2 = .true.
      else 
        angle2_shift = pi/(180.d0/109.48d0) 
        angle2_shift_2 = angle2_shift 
        torsion_shift = pi/(180.d0/54.74d0) 
      end if
      if (nat(A) == 8) then   
        multiplier_a = 0.d0     
        if (nbonds(A) == 2) then
          if (nat(ibonds(1,A)) == 1 .and. nat(ibonds(2,A)) == 1) multiplier_a = dh2_a_parameters(5)   ! Water
        else if (nat(ibonds(1,A)) == 6) then
          C_of_CO =  ibonds(1,A)
          if (nbonds(C_of_CO) == 3) then
            nH = 0
            nC = 0
            nO = 0
            nN = 0
            do i = 1, 3
              if (nat(ibonds(i,C_of_CO)) == 1) nH = nH +1
              if (nat(ibonds(i,C_of_CO)) == 6) nC = nC +1
              if (nat(ibonds(i,C_of_CO)) == 7) then
                nN = nN +1
                N_of_HNCO = ibonds(i,C_of_CO)
              end if
              if (nat(ibonds(i,C_of_CO)) == 8) nO = nO +1
            end do
            if (nC + nH == 1 .and. nN == 1 .and. nO == 1) then
              peptide = .false.
!
!  Check that H-N-C-O exists and is trans
!
              nH = 0
              do i = 1, nbonds(N_of_HNCO)
                if (nat(ibonds(i,N_of_HNCO)) == 1) then
                  nH = nH + 1
                  call dihed (coord, A, C_of_CO, N_of_HNCO, ibonds(i,N_of_HNCO), sum) 
                  sum = min(sum, 2*pi - sum)
                  if (sum > 0.5d0*pi) peptide = .true.
                end if
              end do
              if (peptide .and. nH > 0) multiplier_a = dh2_a_parameters(4)   ! peptide oxygen
            else if (nO == 2) then
              multiplier_a = dh2_a_parameters(3)   ! acid (carboxylate) oxygen
            end if
          end if
        end if
        if (multiplier_a < 1.d-20) multiplier_a  = dh2_a_parameters(2)   ! generic oxygen
      else
        multiplier_a = dh2_a_parameters(6)   ! sulfur
      end if
    else if (nat(A) == 7) then
      multiplier_a = dh2_a_parameters(1)   ! nitrogen
      if (nbonds(A) == 2) then 
        angle2_shift = pi/(180.d0/120.d0) 
        angle2_shift_2 = angle2_shift 
        torsion_shift = 0.d0
      else 
        angle2_shift = pi/(180.d0/109.48d0) 
        angle2_shift_2 = angle2_shift 
        torsion_shift = pi/(180.d0/54.74d0) 
        torsion_check_set = .true.! NR3 group 
      end if
    end if
!
!  extrapolation between tetrahedral and planar NR3 group
!
    R1 = 0
    if (nbonds(A) == 1) then
      R1 = ibonds(1,A)
      sum_max = 0.d0
      do ii = 1, nbonds(R1)
        i = ibonds(ii,R1)
!  if (i == A) cycle
       if ( .not. connected(i, H, 1000.d0**2)) return
        if (sum_max < Rab) then
          R2 = i
          sum_max = Rab
        end if
      end do
      R3 = H
    else if (nbonds(A) == 2) then
      sum_max = 0.d0
      do ii = 1, nbonds(A)
        i = ibonds(ii,A)
        if ( .not. connected(i, H, 1000.d0**2)) return 
        if (sum_max < Rab) then
          sum_max = Rab
          R1 = i
        end if
      end do
      do ii = 1, nbonds(A)
        i = ibonds(ii,A)
        if (i == R1) cycle
        R2 = i
      end do
      R3 = H
    else if (nbonds(A) == 3) then
      sum_max = 0.d0
      do ii = 1, nbonds(A)
        i = ibonds(ii,A)
        if ( .not. connected(i, H, 1000.d0**2)) return
        if (sum_max < Rab) then
          sum_max = Rab
          R1 = i
        end if
      end do
      sum_max = 0.d0
      do ii = 1, nbonds(A)
        i = ibonds(ii,A)
        if (i == R1) cycle
        if ( .not. connected(i, H, 1000.d0**2)) return
        if (sum_max < Rab) then
          sum_max = Rab
          R2 = i
        end if
      end do
      do ii = 1, nbonds(A)
        i = ibonds(ii,A)
        if (i == R1 .or. i == R2) cycle
        R3 = i
      end do
      if (torsion_check_set) call dihed (coord, R2, R1, A, R3, torsion2) 
      torsion_check = torsion2
      if (torsion_check <   -pi) torsion_check = torsion_check + 2.d0*pi
      if (torsion_check >    pi) torsion_check = torsion_check - 2.d0*pi
      if (torsion_check < 0.d0) then
        torsion_check = -pi - torsion_check
      else
        torsion_check =  pi - torsion_check
      end if
      sum = torsion_check
      if (sum <  0.d0) sum = -sum 
      sum = 180.d0/pi*sum
      torsion_shift = torsion_shift + pi/(180.d0/((54.74d0 - sum)/54.74d0*35.26d0))
      angle2_shift=angle2_shift - pi/(180.d00/((54.74d0 - sum)/54.74d0*19.48d0))
      angle2_shift_2 = angle2_shift     
    end if
    if (R1 == 0) return
!
! second angle
!
    call bangle (coord, R1, A, H, angle2) 
    nd_list = nd_list + 1   
    d_list(nd_list) = R1    
    angle2_cos = cos(angle2_shift - angle2)
    angle2_cos_2 = cos(angle2_shift_2 - angle2)
    if (angle2_cos_2 > angle2_cos) angle2_cos = angle2_cos_2
    if (angle2_cos <= 0.d0) return
!
! torsion angle
! correction of NR3 torsion angle for through-bond case 
!
      call dihed (coord, R2, R1, A, H, torsion_ref)
      nd_list = nd_list + 1   
      d_list(nd_list) = R2  
      torsion_correct = torsion_ref      
      if (torsion_correct < -pi) torsion_correct = torsion_correct + 2.d0*pi
      if (torsion_correct >  pi) torsion_correct = torsion_correct - 2.d0*pi
      if (.not. torsion_check_set2 .or. Abs(torsion_correct) > 0.5d0*pi) then
        if (torsion_correct < 0.d0) then
          torsion_correct = -pi - torsion_correct
        else
          torsion_correct =  pi - torsion_correct
        end if
      end if

     
      if (torsion_check < 0.0) then ! negative torsion angle occupied by -NR3 r3 
        torsion = torsion_shift - torsion_correct
        if (torsion <  -pi) torsion = torsion + 2.d0*pi
        if (torsion >   pi) torsion = torsion - 2.d0*pi
        torsion_cos = cos(torsion)
      else if (torsion_check > 0.0) then ! positive torsion angle occupied by -NR3 r3
        torsion = -torsion_shift - torsion_correct
        if (torsion <  -pi) torsion = torsion + 2.d0*pi
        if (torsion >   pi) torsion = torsion - 2.d0*pi
        torsion_cos = cos(torsion) 
      else ! planar -NR3 or general case       
        torsion = torsion_shift - torsion_correct
        torsion_2 = -torsion_shift - torsion_correct
        if (torsion <  -pi) torsion = torsion + 2.d0*pi
        if (torsion >   pi) torsion = torsion - 2.d0*pi
        if (torsion_2 <  -pi) torsion_2 = torsion_2 + 2.d0*pi
        if (torsion_2 >   pi) torsion_2 = torsion_2 - 2.d0*pi
        torsion_cos = cos(torsion)
        torsion_cos_2 = cos(torsion_2)    
        if (torsion_cos_2 > torsion_cos) torsion_cos = torsion_cos_2 
      end if
      if ( .not. connected(A, H, 1000.d0**2)) return
      r = Rab
      if ( .not. connected(R1, H, 1000.d0**2)) return
      if (torsion_check_set2 .and. r > Rab) torsion_cos = -1.d0
      if (torsion_cos <= 0.0) return
!
! distance cutoff as ad-hoc solution ...
!
!  To prevent a discontinuity in the gradients, make r a function of distance:
!
!  Above 1.85 Angstroms, r = r
!  Below 1.75 Angstroms r is increased to 1.80 Angstroms
!
    r = truncation(r, 1.80d0, 0.05d0)   
    r = r/a0 
    expo    = 3.d0    ! "b" in equation 2
    rep_pre = 0.65d0    ! "c" in equation 2
    rep_exp = 5.d0    ! "d" in equation 2
    unit_part = fpc_9*eV
    attraction = multiplier_a*q1*q2/r**expo*unit_part
    repulsion = rep_pre*rep_exp**(-r)*unit_part
    c = (attraction + repulsion)*angle_cos*angle2_cos*torsion_cos    
    EC =       attraction*angle_cos*angle2_cos*torsion_cos  
    ER =       repulsion*angle_cos*angle2_cos*torsion_cos                 
    EC_plus_ER = c    
  end function EC_plus_ER
  function truncation(R, limit, spread)
!
!  Truncation has the values:
!
!  limit, when R < limit - spread
!  greater than R when R is in the range: limit - spread < R < limit + spread
!  R, when R > limit + spread
!
!  At limit + spread, the slope of truncation is 1, i.e., equal to the slope for R > limit + spread
!  At limit - spread, the slope is zero, i.e., equal to the slope of the constant "limit"
!
  implicit none
  double precision, intent(in) :: R, limit, spread
  double precision :: truncation, a, b
    a = limit - spread
    b = limit + spread  
    if (R < b) then
      if (R < a) then
        truncation = limit
      else      
        truncation = limit + (limit - a)/(a - b)**2*(R - a)**2
      end if
    else
      truncation = R
    end if
    return
  end function truncation



  subroutine all_h_bonds(hblist1, hblist2, hblist3, max_h_bonds, nrpairs, nsa, nsb)
!
!  all_h_bonds detects all potential hydrogen bonds.  A hydrogen bond is a set of three atoms,
!  an oxygen or nitrogen, a hydrogen, and an oxygen or nitrogen atom.  The hydrogen atom must
! be within RAH Angstroms of either an oxygen or a nitrogen atom.
!
!  On exit:
!
!    nrpairs: number of pairs of atoms
!    hblist1: Atom number of oxygen or nitrogen attached to hydrogen
!    hblist2: Atom number of hydrogen atom involved in hydrogen bonding
!    hblist3: Atom number ofoxygen or nitrogen hydrogen bonded to the hydrogen atom
!    
!
  use common_arrays_C, only: bonding_a_h, bonding_b_h, &
    acceptor_a, acceptor_b, set_a, set_b
  use molkst_C, only : numat, line, moperr
  use chanel_C, only : iw
  implicit none
    integer, intent (in) :: max_h_bonds
    integer, intent (out) :: hblist1(max_h_bonds), hblist2(max_h_bonds), hblist3(max_h_bonds), nrpairs
!
!  Local variables
!
    integer :: i, nsa, nsb, nacceptor_a, nacceptor_b, nbonding_b_h, nbonding_a_h
!
!  Work out list of of potential hydrogen bonds
!
!   First, find all N-H and O-H groups 
!
    if (allocated(acceptor_a))   deallocate(acceptor_a)
    if (allocated(acceptor_b))   deallocate(acceptor_b)
    if (allocated(bonding_a_h))  deallocate(bonding_a_h)
    if (allocated(bonding_b_h))  deallocate(bonding_b_h)
    allocate (acceptor_a(numat), acceptor_b(numat), bonding_a_h(numat), &
      bonding_b_h(numat), stat=i)
    if (i /= 0) then
      line = " Cannot allocate arrays for hydrogen bonds"
      write(iw,*)" Cannot allocate arrays for hydrogen bonds"
      call to_screen(trim(line))
      call mopend(trim(line))
      return
    end if
    call find_XH_bonds(set_a, nsa, acceptor_a, nacceptor_a, bonding_a_h, nbonding_a_h)
    if (nsb /= 0) then
      call find_XH_bonds(set_b, nsb, acceptor_b, nacceptor_b, bonding_b_h, nbonding_b_h)
    else
      nbonding_b_h = 0
    end if
!
!  acceptor_a now holds the list of acceptor O and N atoms (for the first fragment)
!  acceptor_b now holds the list of acceptor O and N atoms (for the second fragment, if it exists)
!  bonding_a_h now holds the list of H atoms attached to an O or N (for the first fragment)
!  bonding_b_h now holds the list of H atoms attached to an O or N (for the second fragment, if it exists)
!  
!
!  Find if there is a hydrogen bond
!     
    nrpairs = 0
    if (nsb /= 0) then 
      call find_H__Y_bonds(acceptor_a, nacceptor_a, acceptor_b, nacceptor_b, &
        bonding_a_h, nbonding_a_h, hblist1, hblist2, hblist3, max_h_bonds, nrpairs)
      call find_H__Y_bonds(acceptor_b, nacceptor_b, acceptor_a, nacceptor_a, &
        bonding_b_h, nbonding_b_h, hblist1, hblist2, hblist3, max_h_bonds, nrpairs)
!
! Convert from two sets to one set
!
      do i = 1, nbonding_b_h
        nbonding_a_h = nbonding_a_h + 1
        bonding_a_h(nbonding_a_h) = bonding_b_h(i)
      end do
    else
      call find_H__Y_bonds(acceptor_a, nacceptor_a, acceptor_a, nacceptor_a, &
        bonding_a_h, nbonding_a_h, hblist1, hblist2, hblist3, max_h_bonds, nrpairs)
        if (moperr) return
    end if
  end subroutine all_h_bonds

  double precision function distance(a, b)
    use common_arrays_C, only: coord
      integer :: a,  b
      distance = sqrt((coord(1,a) - coord(1,b))**2 + (coord(2,a) - coord(2,b))**2 + (coord(3,a) - coord(3,b))**2)
  end
  double precision function angle(a, b, c) 
    use common_arrays_C, only: coord
      integer :: a,  b,  c
      call bangle(coord, a, b, c, angle)
  end
  double precision function torsion(i, j, k, l)
      use common_arrays_C, only: coord
      integer :: i,  j,  k,  l
      call dihed (coord, i, j, k, l, torsion)
  end

  double precision function bonding(x, y, covrad)
    use common_arrays_C, only: nat
    implicit none
    integer,  INTENT(IN) :: x, y 
    double precision :: covrad(94)
      bonding = covrad(nat(x)) + covrad(nat(y)) 
    return
  end function bonding


  double precision function EH_plus(i, hblist, max_h_bonds, nrbondsa, nrbondsb)
!
!  Third-Generation Hydrogen-Bonding Corrections for Semiempirical QM Methods and Force Fields
!  Martin Korth, J. Chem. Theory Comput., 2010, 6 (12), pp 3808-3816
!
    use common_arrays_C, only : nat
    use molkst_C, only : method_PM7
    use funcon_C, only : eV, fpc_9, a0, pi
    use parameters_C, only : par8, par9
    implicit none
    integer, intent (in) :: max_h_bonds, i, hblist(max_h_bonds,10), nrbondsa(max_h_bonds), nrbondsb(max_h_bonds)
    double precision :: angle_cos, angle, torsion_check, &
      angle2_shift, angle2_shift_2, torsion_shift, angle2, torsion_check_bac, &
      angle2_cos, angle2_cos_2, torsion_correct, torsion_value, torsion_cos, torsion_value_2, &
      torsion_cos_2, angle2_cos_new, angle2_cos_2_new, torsion_cos_new, torsion_cos_2_new, &
      scale_a, scale_nsp3, scale_nsp2, scale_osp3, scale_osp2, scale_b, scale_c, &
      hb_dist, xc_dist, ha_dist, damping, hartree2kcal, XY_dist, short
    double precision :: shortcut = 2.4d0    !  2.4 Angstrom
    double precision :: longcut  = 7.0d0    !  7.0 Angstrom 
    double precision :: covcut   = 1.2d0    !  1.2 Angstrom (hb mid point)
    logical :: torsion_check_set, torsion_check_set2
    double precision, external :: torsion, distance
    if (method_PM7) then
       scale_nsp3 = par8*a0**2
       scale_osp3 = par9*a0**2 
       scale_nsp2 = par8*a0**2
    else
       scale_nsp3 = -0.16d0*a0**2
       scale_osp3 = -0.12d0*a0**2 
       scale_nsp2 = scale_nsp3        
    end if
    scale_osp2 = scale_osp3
    hartree2kcal = eV*fpc_9 
    EH_plus = 0.d0 
    if (hblist(i, 10) /= -666) then
! first angle
      angle_cos = -cos(angle(hblist(i, 1), hblist(i, 9), hblist(i, 5)))   !  cos(pi-angle)
      if (angle_cos <= 0) return 
! get target angles for hblist(i, 1)
! - no explicit lone pair representation to keep gradient simpler
! - no coordinate dependent target shifts for the same reason (except NR3)
      torsion_check = 0.d0 
      torsion_check_set = .false. 
      torsion_check_set2 = .false.
      if (nat(hblist(i, 1)) == 8) then ! sulphur?
        if (nrbondsa(i) == 1) then
!
!  >C=O - - H-X
!
          angle2_shift = pi 
          angle2_shift_2 = pi/(180.d0/120.d0) 
          torsion_shift = 0.d0
          torsion_check_set2 = .true.! H...O = C -R1 
        else 
!
! >O - - H-X
!
          angle2_shift = pi/(180.d0/109.48) 
          angle2_shift_2 = angle2_shift 
          torsion_shift = pi/(180.d0/54.74) 
        endif
      else if (nat(hblist(i, 1)) == 7) then 
        if (nrbondsa(i) == 2) then
!
! >N - - H-X
!
          angle2_shift = pi/(180.d0/120.d0) 
          angle2_shift_2 = angle2_shift 
          torsion_shift = 0.d0
        else 
!
! >N- - - H-X   WHAT ABOUT Cyanide?
!
          angle2_shift = pi/(180.d0/109.48) 
          angle2_shift_2 = angle2_shift 
          torsion_shift = pi/(180.d0/54.74) 
          torsion_check_set = .true.! NR3 group 
        endif
      endif
!
! extrapolation between tetragonal and planar NR3 group
!
      if (torsion_check_set) then 
        torsion_check = torsion(hblist(i, 3), hblist(i, 2), hblist(i, 1), hblist(i, 4))   !  torsion2 
        if (torsion_check <= -pi) torsion_check = torsion_check  +  2.d0 * pi
        if (torsion_check > pi) torsion_check = torsion_check - 2.d0 * pi 
        if (torsion_check < 0) then 
          torsion_check = -pi -torsion_check 
        else 
          torsion_check = pi -torsion_check 
        endif
        torsion_check_bac = torsion_check ! save sign 
        if (torsion_check < 0) torsion_check = -1.d0 * torsion_check
        torsion_check = torsion_check*180.d0/pi
        torsion_shift = torsion_shift + pi/(180.d0/((54.74d0 - torsion_check)/54.74d0*35.26d0))
        angle2_shift = angle2_shift - pi/(180.d0/((54.74d0 - torsion_check)/54.74d0*19.48d0)) 
        angle2_shift_2 = angle2_shift 
        torsion_check = torsion_check_bac ! restore sign 
      endif
! second angle
      angle2 = angle(hblist(i, 2), hblist(i, 1), hblist(i, 9))  !angle2   
      angle2_cos = cos(angle2_shift - angle2) 
      angle2_cos_2 = cos(angle2_shift_2 - angle2) 
      if (angle2_cos_2 > angle2_cos) angle2_cos = angle2_cos_2
      if (angle2_cos <= 0.d0) return
! torsion angle
      torsion_correct = torsion(hblist(i, 3), hblist(i, 2), hblist(i, 1), hblist(i, 9))   !  torsion1 
      if (torsion_correct <= -pi) torsion_correct = torsion_correct + 2.d0 * pi
      if (torsion_correct  >  pi) torsion_correct = torsion_correct - 2.d0 * pi
      if ((.not.torsion_check_set2) .or. (abs(torsion_correct*180.d0/pi) > 90.d0)) then 
        if (torsion_correct < 0.d0) then
          torsion_correct = -pi - torsion_correct
        else
          torsion_correct =  pi - torsion_correct
        endif
      endif
!
! correction of NR3 torsion angle for through-bond case
!
      if (torsion_check < 0) then ! negative torsion angle occupied by -NR3 r3 
        torsion_value = torsion_shift - torsion_correct 
        if (torsion_value <= -pi) torsion_value = torsion_value + 2.d0 * pi
        if (torsion_value  >  pi) torsion_value = torsion_value - 2.d0 * pi 
        torsion_cos = cos(torsion_value)
      else if (torsion_check > 0) then ! positive torsion angle occupied by -NR3 r3
        torsion_value = -torsion_shift - torsion_correct 
        if (torsion_value <= -pi) torsion_value = torsion_value + 2.d0 * pi 
        if (torsion_value  >  pi) torsion_value = torsion_value - 2.d0 * pi 
        torsion_cos = cos(torsion_value)
      else ! planar -NR3 or general case
        torsion_value = torsion_shift - torsion_correct 
        torsion_value_2 = -torsion_shift - torsion_correct 
        if (torsion_value   <= -pi) torsion_value   = torsion_value   + 2.d0 * pi 
        if (torsion_value   >   pi) torsion_value   = torsion_value   - 2.d0 * pi 
        if (torsion_value_2 <= -pi) torsion_value_2 = torsion_value_2 + 2.d0 * pi 
        if (torsion_value_2 >   pi) torsion_value_2 = torsion_value_2 - 2.d0 * pi 
        torsion_cos = cos(torsion_value)
        torsion_cos_2 = cos(torsion_value_2)
        if (torsion_cos_2 > torsion_cos) torsion_cos = torsion_cos_2 
      endif
      if (distance(hblist(i, 9), hblist(i, 1)) > distance(hblist(i, 9), hblist(i, 2)) .and. torsion_check_set2) torsion_cos = 0.d0 
      if (hblist(i, 3) == hblist(i, 4) .or. hblist(i, 9) == hblist(i, 4)) torsion_cos = 1.d0
      if (torsion_cos < 0) return
!
! get target angles for hblist(i, 5) - see comment above 
!
      torsion_check      = 0.d0 
      torsion_check_set  = .false.
      torsion_check_set2 = .false. 
      if (nat(hblist(i, 5)) == 8) then ! sulphur?
        if (nrbondsb(i) == 1) then
          angle2_shift = pi 
          angle2_shift_2 = pi/(180.d0/120.d0) 
          torsion_shift = 0.d0
          torsion_check_set2 = .true.! H...O = C -R1 
        else 
          angle2_shift = pi/(180.d0/109.48d0) 
          angle2_shift_2 = angle2_shift 
          torsion_shift = pi/(180.d0/54.74d0) 
        endif
      else if (nat(hblist(i, 5)) == 7) then
        if (nrbondsb(i) == 2) then
          angle2_shift = pi/(180.d0/120.d0)  
          angle2_shift_2 = angle2_shift 
          torsion_shift = 0.d0
        else 
          angle2_shift = pi/(180.d0/109.48d0) 
          angle2_shift_2 = angle2_shift 
          torsion_shift = pi/(180.d0/54.74d0) 
          torsion_check_set = .true.! NR3 group 
        endif
      endif
! extrapolation between tetragonal and planar NR3 group
      if (torsion_check_set) then 
        torsion_check = torsion(hblist(i, 7), hblist(i, 6), hblist(i, 5), hblist(i, 8))  !torsion2_new 
        if (torsion_check <= -pi) torsion_check = torsion_check + 2.d0 * pi 
        if (torsion_check  >  pi) torsion_check = torsion_check - 2.d0 * pi
        if (torsion_check < 0) then
          torsion_check = -pi -torsion_check 
        else 
          torsion_check = pi -torsion_check 
        endif 
        torsion_check_bac = torsion_check ! save sign 
        if (torsion_check < 0) torsion_check = -1.d0 * torsion_check
        torsion_check = torsion_check*180.d0/pi
        torsion_shift = torsion_shift + pi/(180.d0/((54.74d0 -torsion_check)/54.74d0*35.26d0))
        angle2_shift = angle2_shift -pi/(180.d0/((54.74d0 -torsion_check)/54.74d0*19.48d0)) 
        angle2_shift_2 = angle2_shift 
        torsion_check = torsion_check_bac ! restore sign 
      endif
! second angle
      angle2 = angle(hblist(i, 6), hblist(i, 5), hblist(i, 9))   ! angle2_new 
      angle2_cos_new = cos(angle2_shift -angle2) 
      angle2_cos_2_new = cos(angle2_shift_2 -angle2) 
      if (angle2_cos_2_new > angle2_cos_new) angle2_cos_new = angle2_cos_2_new
      if (angle2_cos_new <= 0.d0) return
! torsion angle
      torsion_correct = torsion(hblist(i, 7), hblist(i, 6), hblist(i, 5), hblist(i, 9))   ! torsion1_new   
      if (torsion_correct <= -pi) torsion_correct = torsion_correct  +  2.d0 * pi 
      if (torsion_correct > pi) torsion_correct = torsion_correct - 2.d0 * pi
      if ((.not.torsion_check_set2) .or. (abs(torsion_correct*180.d0/pi) > 90.d0)) then
        if (torsion_correct < 0.d0) then
          torsion_correct = -pi -torsion_correct
        else
          torsion_correct = pi - torsion_correct
        endif
      endif
! correction of NR3 torsion angle for through-bond case
      if (torsion_check < 0.d0) then ! negative torsion angle occupied by -NR3 r3 
        torsion_value = torsion_shift - torsion_correct 
        if (torsion_value <= -pi) torsion_value = torsion_value + 2.d0 * pi
        if (torsion_value  >  pi) torsion_value = torsion_value - 2.d0 * pi
        torsion_cos_new = cos(torsion_value)
      else if (torsion_check > 0) then ! positive torsion angle occupied by -NR3 r3
        torsion_value = -torsion_shift - torsion_correct 
        if (torsion_value <= -pi) torsion_value = torsion_value + 2.d0 * pi
        if (torsion_value  >  pi) torsion_value = torsion_value - 2.d0 * pi 
        torsion_cos_new = cos(torsion_value)
      else ! planar -NR3 or general case
        torsion_value   =  torsion_shift - torsion_correct 
        torsion_value_2 = -torsion_shift - torsion_correct 
        if (torsion_value   <= -pi) torsion_value   = torsion_value   + 2.d0 * pi 
        if (torsion_value    >  pi) torsion_value   = torsion_value   - 2.d0 * pi 
        if (torsion_value_2 <= -pi) torsion_value_2 = torsion_value_2 + 2.d0 * pi 
        if (torsion_value_2  >  pi) torsion_value_2 = torsion_value_2 - 2.d0 * pi
        torsion_cos_new   = cos(torsion_value)
        torsion_cos_2_new = cos(torsion_value_2)
        if (torsion_cos_2_new > torsion_cos_new)torsion_cos_new = torsion_cos_2_new 
      endif
      if (distance(hblist(i, 9), hblist(i, 5)) > distance(hblist(i, 9), hblist(i, 6)) .and. torsion_check_set2) torsion_cos_new = 0.d0 
      if (hblist(i, 7) == hblist(i, 8) .or. hblist(i, 9) == hblist(i, 8)) torsion_cos_new = 1.d0
      if (torsion_cos_new < 0.d0) return
! parameters
      if (nat(hblist(i, 1)) == 7) then ! nitrogen
        if (nrbondsa(i) >= 3) then
          scale_a = scale_nsp3
        else
          scale_a = scale_nsp2
        endif
      else ! oxygen
        if (nrbondsa(i) >= 2) then
          scale_a = scale_osp3
        else
          scale_a = scale_osp2
        endif
      endif ! sulphur?
      if (nat(hblist(i, 5)) == 7) then ! nitrogen
        if (nrbondsb(i) >= 3) then
          scale_b = scale_nsp3
        else
          scale_b = scale_nsp2
        endif
      else ! oxygen
        if (nrbondsb(i) >= 2) then
          scale_b = scale_osp3
        else
          scale_b = scale_osp2
        endif
      endif ! sulphur?
      scale_c = (scale_a + scale_b)/2.d0
!
! damping 1.d0/(1.d0 + exp( -60.d0*(x/2.65d0 - 1.d0)))
!
      ha_dist = distance(hblist(i, 9), hblist(i, 1))
      hb_dist = distance(hblist(i, 9), hblist(i, 5))
      xc_dist = min(ha_dist, hb_dist)

! energies
      if (method_PM7) then
        XY_dist = max(ha_dist, hb_dist) - xc_dist
        if (XY_dist > 0.5d0) then
! 
! Make damping a function of the shorter X - H distance.
! Distances greater than covcut Angstroms reduce damping 
! distances less than covcut are not affected
!
        damping = 1.d0 - 1.d0/(1.d0 + exp( -60.d0*(xc_dist/covcut - 1.0d0)))
      else
!
! In very strong X - H - Y systems the X - Y distance is small, so an increased
! X - H minimum distance is expected, therefore do not dampen the function.
!
        damping = 1.d0
      end if
! y-x damping 
      xc_dist = distance(hblist(i, 1), hblist(i, 5)) 

!
! Make damping a function of the O - O distance
! Damping is decreased as the O - O distances approaches shortcut Angstroms
!
        damping = damping/(1.d0 + exp( -100.d0*(xc_dist/shortcut - 1.d0))) 
!
! At very large O - O distances, make damping go to zero
!
        damping = damping*(1.d0 - 1.d0/(1.d0 + exp( -10.d0*(xc_dist/longcut - 1.d0))))
        XY_dist = distance(hblist(i, 1), hblist(i, 5))
        EH_plus = scale_c/XY_dist**2.d0*angle_cos**2* &
          (1.d0 - (1.d0 - angle2_cos*torsion_cos*angle2_cos_new*torsion_cos_new)**2)*hartree2kcal*damping 
!
!  Add in extra stabilization as the O - O distance approaches 2.41 Angstroms.
!  As a guide, formic acid dimer has O - O: 2.67 Angstroms, stabilization should total 18.61 kcal/mol
!              the H5O2(+) ion has O - O: 2.38 - 2.43 Angstroms
!
        short = - 10.d0*exp(-20.d0*(max(XY_dist - 2.41d0, 0.d0))**2)
        EH_plus = EH_plus + short        
      else
! y-h damping
      damping = 1.d0 - 1.d0/(1.d0 + exp( -60.d0*(xc_dist/covcut - 1.d0)))
! y-x damping 
      xc_dist = distance(hblist(i, 1), hblist(i, 5))

! short range
      damping = damping/(1.d0 + exp( -100.d0*(xc_dist/shortcut - 1.d0))) 
! long range
      damping = damping*(1.d0 - 1.d0/(1.d0 + exp( -10.d0*(xc_dist/longcut - 1.d0))))
        EH_plus = scale_c/distance(hblist(i, 1), hblist(i, 5))**2.d0* &
        angle_cos**2*angle2_cos**2*torsion_cos**2*angle2_cos_new**2*torsion_cos_new**2*hartree2kcal*damping 
      end if
    endif  
    return
  end function EH_plus





