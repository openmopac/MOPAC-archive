!
! dftd3 is the "D3" method of Grimme, et.al, as described in "A consistent and accurate ab initio 
! parametrization of density functional dispersion correction (DFT-D) for the 94 elements H-Pu"
! S. Grimme, J. Antony, S. Ehrlich, H. Krieg, THE JOURNAL OF CHEMICAL PHYSICS 132, 154104 .2010.
!
!  This subroutine is based on materials provided by S. Grimme.
!
!
  subroutine dftd3(disp, l_grad, dxyz)
  use funcon_C, only : a0, fpc_9, fpc_2
  use molkst_C, only : numat
  use common_arrays_C, only: coord, nat 
  implicit none   
  double precision :: disp, dxyz(3, numat)
  logical :: l_grad
  integer :: max_elem, maxc                      
  parameter (max_elem = 94)
  parameter (maxc     = 5)  ! maximum coordination number references per element    
!
! local and dummy variables
!
      integer :: i, iat, jat, mxc(max_elem)
      logical :: first = .true.
      double precision :: x, c6, autokcal, k2, e6, e8, rs6, rs8, s6, s18, &
        alp6, alp8, rs18, alp, rthr = 20000.d0, rthr2 = 1600.d0, ehb, hbscale, dum
      double precision :: &
        c6ab(max_elem, max_elem, maxc, maxc, 3), & ! C6 for all element pairs 
        dxyz_temp(3,numat),                      & ! Contribution to gradient
        xyz(3, numat),                           & ! coordinates in au
        rcov(max_elem),                          & ! covalent radii
        cn(numat),                               & ! coordination numbers of the atoms
        r2r4(max_elem),                          & ! atomic <r^2>/<r^4> values
        r0ab(max_elem, max_elem)                   ! cut - off radii for all element pairs
      parameter (k2 = 4.d0/3.d0)  
      save 
!
! PBE0/def2 - QZVP atomic values 
!
      data r2r4 / &
       8.0589d0,  3.4698d0, 29.0974d0, 14.8517d0, 11.8799d0,  7.8715d0,  5.5588d0, &
       4.7566d0,  3.8025d0,  3.1036d0, 26.1552d0, 17.2304d0, 17.7210d0, 12.7442d0, &
       9.5361d0,  8.1652d0,  6.7463d0,  5.6004d0, 29.2012d0, 22.3934d0, 19.0598d0, &
      16.8590d0, 15.4023d0, 12.5589d0, 13.4788d0, 12.2309d0, 11.2809d0, 10.5569d0, &
      10.1428d0,  9.4907d0, 13.4606d0, 10.8544d0,  8.9386d0,  8.1350d0,  7.1251d0, &
       6.1971d0, 30.0162d0, 24.4103d0, 20.3537d0, 17.4780d0, 13.5528d0, 11.8451d0, &
      11.0355d0, 10.1997d0,  9.5414d0,  9.0061d0,  8.6417d0,  8.9975d0, 14.0834d0, &
      11.8333d0, 10.0179d0,  9.3844d0,  8.4110d0,  7.5152d0, 32.7622d0, 27.5708d0, &
      23.1671d0, 21.6003d0, 20.9615d0, 20.4562d0, 20.1010d0, 19.7475d0, 19.4828d0, &
      15.6013d0, 19.2362d0, 17.4717d0, 17.8321d0, 17.4237d0, 17.1954d0, 17.1631d0, &
      14.5716d0, 15.8758d0, 13.8989d0, 12.4834d0, 11.4421d0, 10.2671d0,  8.3549d0, &
       7.8496d0,  7.3278d0,  7.4820d0, 13.5124d0, 11.6554d0, 10.0959d0,  9.7340d0, &
       8.8584d0,  8.0125d0, 29.8135d0, 26.3157d0, 19.1885d0, 15.8542d0, 16.1305d0, &
      15.6161d0, 15.1226d0, 16.1576d0 /    
!                                   
! covalent radii (taken from Pyykko and Atsumid0, Chem. Eur. J. 15d0, 2009d0, 188 - 197)
! values for metals decreased by 10 %
!
      data rcov/ &
      0.32d0, 0.46d0, 1.20d0, 0.94d0, 0.77d0, 0.75d0, 0.71d0, 0.63d0, 0.64d0, 0.67d0, &
      1.40d0, 1.25d0, 1.13d0, 1.04d0, 1.10d0, 1.02d0, 0.99d0, 0.96d0, 1.76d0, 1.54d0, &
      1.33d0, 1.22d0, 1.21d0, 1.10d0, 1.07d0, 1.04d0, 1.00d0, 0.99d0, 1.01d0, 1.09d0, &
      1.12d0, 1.09d0, 1.15d0, 1.10d0, 1.14d0, 1.17d0, 1.89d0, 1.67d0, 1.47d0, 1.39d0, &
      1.32d0, 1.24d0, 1.15d0, 1.13d0, 1.13d0, 1.08d0, 1.15d0, 1.23d0, 1.28d0, 1.26d0, &
      1.26d0, 1.23d0, 1.32d0, 1.31d0, 2.09d0, 1.76d0, 1.62d0, 1.47d0, 1.58d0, 1.57d0, &
      1.56d0, 1.55d0, 1.51d0, 1.52d0, 1.51d0, 1.50d0, 1.49d0, 1.49d0, 1.48d0, 1.53d0, &
      1.46d0, 1.37d0, 1.31d0, 1.23d0, 1.18d0, 1.16d0, 1.11d0, 1.12d0, 1.13d0, 1.32d0, &
      1.30d0, 1.30d0, 1.36d0, 1.31d0, 1.38d0, 1.42d0, 2.01d0, 1.81d0, 1.67d0, 1.58d0, &
      1.52d0, 1.53d0, 1.54d0, 1.55d0 /
      if (first) then
        first = .false.
        rcov = k2*rcov/a0
        autokcal = fpc_9*fpc_2/a0
        do i = 1, max_elem
          dum = 0.5*r2r4(i)*dfloat(i)**0.5   
! store it as sqrt because the geom. av. is taken
          r2r4(i) = sqrt(dum)                         
        enddo
        rthr = 20000.0d0
        call setr0ab(max_elem, a0, r0ab)
        call copyc6(maxc, max_elem, c6ab, mxc) 
        rthr2 = 1600.0d0
        rthr2 = rthr2/r0ab(1, 1)
        s6   = 1.0d0
        alp  = 14.0d0
        rs18 = 1.0d0
        rs6 = 1.560d0
        s18 = 1.009d0
        hbscale = 1.301d0
        rs8   = rs18       
        alp6  = alp
        alp8  = alp + 2.d0
      end if   
!
!   Switch from MOPAC
!
      xyz = coord/a0

! CNs for output

      call ncoord(numat, rcov, nat, xyz, cn)      
  
      do iat = 1, numat - 1
        do jat = iat + 1, numat
          call getc6(maxc, max_elem, c6ab, mxc, nat(iat), nat(jat), cn(iat), cn(jat), c6)
        enddo
      enddo
      call edisp(max_elem, maxc, numat, xyz, nat, c6ab, mxc, r2r4, r0ab, rcov, rs6, rs8, alp6, alp8, rthr, e6, e8)

!HBOND      
      dxyz_temp = 0.0d0
      call hbsimple(numat, nat, xyz, hbscale, ehb, l_grad, dxyz_temp)
      e6 = e6*s6
      e8 = e8*s18
!HBOND      
      disp  = (- e6 - e8 + ehb)*autokcal
      if(l_grad)then
        call gdisp(max_elem, maxc, numat, xyz, nat, c6ab, mxc, r2r4, r0ab, rcov, s6, s18, rs6, &
          rs8, alp6, alp8, rthr, dxyz_temp, x, rthr2)
        dxyz = dxyz + 2.d0*dxyz_temp*autokcal
      endif
    end subroutine dftd3

