      SUBROUTINE ANAD3  (COORD, D3ENERGY, DERIV, DEBUG, *)
      implicit double precision(a-h,o-z)

C     Analytical derivatives for Grimme's version 3 dispersion ("-D3").
C     Reference:   J. Chem. Phys. 132, 154104 (2010)
C
C     Written by Luke Fiedler, University of Minnesota-Twin Cities (November 2011).
C
C     The COORD handed to this subroutine have been converted from the internal
C     coordinates of the global common variable GEO to Cartesian coordinates (in
C     Angstroms) by a call to GMETRY in COMPFG prior to this subroutine being
C     called.  (The global variable GEO comes from COMMON /GEOM  / GEO(3,NUMATM).)
C     This subroutine is called when the global logical D3METH is flagged by
C     the MOLDAT subroutine upon the appearance of the input file keywords "MNDO-D3"
C     or "AM1-D3".  The global logical DISMET is not flagged (it is only for the
C     original "-D" dispersion methods of Hillier).  If D3METH is .TRUE. this 
C     subroutine is called at the end of the energy calculations. 
C
C        INPUT:
C      COORD    =  Array of all atom's Cartesian coordinates (Angstroms).
C                  (q,n):  q=1(x),2(y),3(z); n=atom serial number
C      DEBUG    =  Integer indicating if and how much intermediate value
C                  information is to be output by this subroutine for debugging.
C                  (0=no printing any information)
C
C        OUTPUT:
C      D3ENERGY =  Total -D3 dispersion energy (kcal/mol).
C      DERIV    =  Dispersion energy derivatives w.r.t. all atoms'
C                  cartesian coordinates.  (kcal/mol/Angstroms)
C
C     As a cautionary note: any dummy atoms "XX" are excluded from the atom list 
C     of COORD and NUMAT, however any point charges (+, ++, -, and --) are 
C     included in the atom list with their own distinct atomic numbers.
C
C     Reference(s):
C        J. Chem. Phys. 132, 154104 (2010); S. Grimme, J. Antony, S. Ehrlich, H. Krieg.
C

      include 'SIZES.i'
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     &                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     &                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /D3PARAM/ C6AB(17,17,5,5,3),R0AB(17,17),D3SR6,D3S8,LSETD3,
     &                 LD3SR6,LD3S8
      LOGICAL          LSETD3, LD3SR6, LD3S8
      COMMON /HPUSED/ HPUSED
      LOGICAL HPUSED
      DIMENSION COORD(3,NUMATM)
      DIMENSION DERIV(3,NUMATM)
      DIMENSION R(NUMATM,NUMATM)
      DIMENSION DR(NUMATM,NUMATM,3,2)
      DIMENSION DCN(numatm,3,NUMATM)
      DIMENSION DTOT6(3,numat)
      DIMENSION DTOT8(3,numat)
      DIMENSION dgtemp(3,numat)
      DIMENSION dlij(3,numat),dtij(3,numat)
      DIMENSION dc6(3,numat),dc8(3,numat)
      DIMENSION dgg(3),dff(3),dc(3)
      DIMENSION dbb(3,2),dcc(3,2),ddd(3,2)
      double precision  cn(numatm)
      integer           debug
      double precision  k1, k3
      data  k1,     k3  / 16.0000d0,         4.0000d0 /
      double precision  k2            ! k2 in inverse Angstroms.
      parameter(k2=4.0d0/3.0d0/0.52917726d0)
      double precision alpha6, alpha8 ! alpha6, alpha8 unitless.
      data alpha6, alpha8 / 14.0000d0,  16.0000d0 /
      double precision s6, s8
      data s6         /  1.0000d0  /  ! s_6 parameter is for all methods.
      double precision sr6, sr8
      data sr8        /  1.0000d0  /  ! s_r,8 for all methods.
c     Parameters for "zero" damping and not using triple-zeta basis set.
c     Slater-Dirac exchange
c      data sr6        /  0.999d0   /
c      data s8         / -1.957d0   /
c      data sr8        /  0.697d0   /
c     BLYP
c      data sr6        /  1.094d0   /
c      data s8         /  1.682d0   /
c     HF (Hartree-Fock)
c      data sr6        /  1.158d0   /
c      data s8         /  1.746d0   /
c     TPSS 
c      data sr6        /  1.166d0   /
c      data s8         /  1.105d0   /
c     M06  
c      data sr6        /  1.325d0   /
c      data s8         /  0.000d0   /
c     PBE  
c      data sr6        /  1.217d0   /
c      data s8         /  0.722d0   /
c     BP   
c      data sr6        /  1.139d0   /
c      data s8         /  1.683d0   /
c     B97D 
c      data sr6        /  0.892d0   /
c      data s8         /  0.909d0   /
c     revPBE
c      data sr6        /  0.923d0   /
c      data s8         /  1.010d0   /
c     PBEsol
      data sr6        /  1.345d0   /
      data s8         /  0.612d0   /
c     rPW86PBE
c      data sr6        /  1.224d0   /
c      data s8         /  0.901d0   /
c     RPBE  
c      data sr6        /  0.872d0   /
c      data s8         /  0.514d0   /
c     B3LYP 
c      data sr6        /  1.261d0   /
c      data s8         /  1.703d0   /
c     PBE0  
c      data sr6        /  1.287d0   /
c      data s8         /  0.928d0   /
c     revPBE38
c      data sr6        /  1.021d0   /
c      data s8         /  0.862d0   /
c     PW6B95
c      data sr6        /  1.532d0   /
c      data s8         /  0.862d0   /
c     TPSS0 
c      data sr6        /  1.252d0   /
c      data s8         /  1.242d0   /
c     B2PLYP
c      data sr6        /  1.427d0   /
c      data s8         /  1.022d0   /
c      data s6         /  0.64d0    /
c     PWPB95
c      data sr6        /  1.557d0   /
c      data s8         /  0.705d0   /
c      data s6         /  0.82d0    /
c     "b2gp-plyp"
c      data sr6        /  1.586d0   /
c      data s8         /  0.760d0   /
c      data s6         /  0.56d0    /
c     "ptpss"
c      data sr6        /  1.541d0   /
c      data s8         /  0.879d0   /
c      data s6         /  0.75d0    /
c     "mpwlyp"
c      data sr6        /  1.239d0   /
c      data s8         /  1.098d0   /
c     "bpbe"
c      data sr6        /  1.087d0   /
c      data s8         /  2.033d0   /
c     "bhlyp"
c      data sr6        /  1.370d0   /
c      data s8         /  1.442d0   /
c     TPSSh 
c      data sr6        /  1.223d0   /
c      data s8         /  1.219d0   /
c     PWB6K 
c      data sr6        /  1.660d0   /
c      data s8         /  0.550d0   /
c     B1B95 
c      data sr6        /  1.613d0   /
c      data s8         /  1.868d0   /
c     BOP   
c      data sr6        /  0.929d0   /
c      data s8         /  1.975d0   /
c     OLYP  
c      data sr6        /  0.806d0   /
c      data s8         /  1.764d0   /
c     OPBE  
c      data sr6        /  0.837d0   /
c      data s8         /  2.055d0   /
c     SSB   
c      data sr6        /  1.215d0   /
c      data s8         /  0.663d0   /
c     revSSB
c      data sr6        /  1.221d0   /
c      data s8         /  0.560d0   /
c     "otpss"
c      data sr6        /  1.128d0   /
c      data s8         /  1.494d0   /
c     B3PW91
c      data sr6        /  1.176d0   /
c      data s8         /  1.775d0   /
c     revPBE0
c      data sr6        /  0.949d0   /
c      data s8         /  0.792d0   /
c     PBE38 
c      data sr6        /  1.333d0   /
c      data s8         /  0.998d0   /
c     MPW1B95
c      data sr6        /  1.605d0   /
c      data s8         /  1.118d0   /
c     MPWB1K
c      data sr6        /  1.671d0   /
c      data s8         /  1.061d0   /
c     BMK   
c      data sr6        /  1.931d0   /
c      data s8         /  2.168d0   /
c     CAM-B3LYP
c      data sr6        /  1.378d0   /
c      data s8         /  1.217d0   /
c     LC-wPBE
c      data sr6        /  1.355d0   /
c      data s8         /  1.279d0   /
c     M05   
c      data sr6        /  1.373d0   /
c      data s8         /  0.595d0   /
c     M05-2X
c      data sr6        /  1.417d0   /
c      data s8         /  0.000d0   /
c     M06L  
c      data sr6        /  1.581d0   /
c      data s8         /  0.000d0   /
c     M06   
c      data sr6        /  1.325d0   /
c      data s8         /  0.000d0   /
c     M06-2X
c      data sr6        /  1.619d0   /
c      data s8         /  0.000d0   /
c     M06HF 
c      data sr6        /  1.446d0   /
c      data s8         /  0.000d0   /
c     DFTB  
c      data sr6        /  1.699d0   /
c      data s8         /  1.504d0   /
c     HCTH/120
c      data sr6        /  1.221d0   /
c      data s8         /  1.206d0   /
      double precision  rcov(120)     ! rcov in Angstroms.
      data  rcov  / 0.3200d0, 0.4600d0, 1.2000d0, 0.9400d0, 0.7700d0,
     &              0.7500d0, 0.7100d0, 0.6300d0, 0.6400d0, 0.6700d0,
     &              1.4000d0, 1.2500d0, 1.1300d0, 1.0400d0, 1.1000d0,
     &              1.0200d0, 0.9900d0, 0.9600d0, 1.7600d0, 1.5400d0,
     &              1.3300d0, 1.2200d0, 1.2100d0, 1.1000d0, 1.0700d0,
     &              1.0400d0, 1.0000d0, 0.9900d0, 1.0100d0, 1.0900d0,
     &              1.1200d0, 1.0900d0, 1.1500d0, 1.1000d0, 1.1400d0,
     &              1.1700d0, 1.8900d0, 1.6700d0, 1.4700d0, 1.3900d0,
     &              1.3200d0, 1.2400d0, 1.1500d0, 1.1300d0, 1.1300d0,
     &              1.0800d0, 1.1500d0, 1.2300d0, 1.2800d0, 1.2600d0,
     &              1.2600d0, 1.2300d0, 1.3200d0, 1.3100d0, 2.0900d0,
     &              1.7600d0, 1.6200d0, 1.4700d0, 1.5800d0, 1.5700d0,
     &              1.5600d0, 1.5500d0, 1.5100d0, 1.5200d0, 1.5100d0,
     &              1.5000d0, 1.4900d0, 1.4900d0, 1.4800d0, 1.5300d0,
     &              1.4600d0, 1.3700d0, 1.3100d0, 1.2300d0, 1.1800d0,
     &              1.1600d0, 1.1100d0, 1.1200d0, 1.1300d0, 1.3200d0,
     &              1.3000d0, 1.3000d0, 1.3600d0, 1.3100d0, 1.3800d0,
     &              1.4200d0, 2.0100d0, 1.8100d0, 1.6700d0, 1.5800d0,
     &              1.5200d0, 1.5300d0, 1.5400d0, 1.5500d0, 26*0.0d0  /
      double precision qqpar(120)       ! Q parameter values for calc C8 from C6.
      double precision qpar(120)        ! Scaled Q param values for C8 from C6 calcs.
      double precision c6(numatm,numatm)
      double precision c8(numatm,numatm)
      double precision lij(numatm,numatm)
      double precision tij(numatm,numatm)
      double precision gtemp
      double precision etemp
      double precision etemp8
      double precision damp6,damp8
      integer          maxcn(120)
      integer          ci, cj
      data  maxcn / 2,1,2,3,5,5,4,3,2,1,
     &              2,3,4,5,4,3,2,   103*0 /
c Eh/(a0**6) -> (kcal/mol)/(Angstrom**6):
      parameter(c6conv=(627.515d0)/(0.52917726d0**6))  ! Conversion factor for C6AB parameters.



C BEGINNING OF RAW QPAR PARAMETERS.
ccc      DATA QPAR  / 2.007348983d0, 4*0.0d0, 3.104928224d0, 0.0d0, 
ccc     &             2.593616798d0, 112*0.0d0/
      DATA QQPAR /  8.0589d0,     3.4698d0,    29.0974d0,    14.8517d0,
     &             11.8799d0,     7.8715d0,     5.5588d0,     4.7566d0,
     &              3.8025d0,     3.1036d0,    26.1552d0,    17.2304d0,
     &             17.7210d0,    12.7442d0,     9.5361d0,     8.1652d0,
     &              6.7463d0,     5.6004d0,    29.2012d0,    22.3934d0,
     &             19.0598d0,    16.8590d0,    15.4023d0,    12.5589d0,
     &             13.4788d0,    12.2309d0,    11.2809d0,    10.5569d0,
     &             10.1428d0,     9.4907d0,    13.4606d0,    10.8544d0,
     &              8.9386d0,     8.1350d0,     7.1251d0,     6.1971d0,
     &             30.0162d0,    24.4103d0,    20.3537d0,    17.4780d0,
     &             13.5528d0,    11.8451d0,    11.0355d0,    10.1997d0,
     &              9.5414d0,     9.0061d0,     8.6417d0,     8.9975d0,
     &             14.0834d0,    11.8333d0,    10.0179d0,     9.3844d0,
     &              8.4110d0,     7.5152d0,    32.7622d0,    27.5708d0,
     &             23.1671d0,    21.6003d0,    20.9615d0,    20.4562d0,
     &             20.1010d0,    19.7475d0,    19.4828d0,    15.6013d0,
     &             19.2362d0,    17.4717d0,    17.8321d0,    17.4237d0,
     &             17.1954d0,    17.1631d0,    14.5716d0,    15.8758d0,
     &             13.8989d0,    12.4834d0,    11.4421d0,    10.2671d0,
     &              8.3549d0,     7.8496d0,     7.3278d0,     7.4820d0,
     &             13.5124d0,    11.6554d0,    10.0959d0,     9.7340d0,
     &              8.8584d0,     8.0125d0,    29.8135d0,    26.3157d0,
     &             19.1885d0,    15.8542d0,    16.1305d0,    15.6161d0,
     &             15.1226d0,    16.1576d0,  26*0.0d0  /

C Set all C6AB and R0AB parameters if not already done.
      if (.not.lsetd3) call setd3(*9999)
C (C6AB PARAMETERS ASSIGNED OUTSIDE OF THIS SUBROUTINE.
C      First index = atomic # of atom 1
C      Second index= atomic # of atom 2
C      Third index = coord # of atom 1
C      Fourth index= coord # of atom 2
C      Fifth index = 1:  C6AB value ( hartree * (bohr)**6 )
C      Fifth index = 2:  coordination number for atom #1 (unitless)
C      Fifth index = 3:  coordination number for atom #2 (unitless)
C  R0AB PARAMETERS ASSIGNED OUTSIDE OF THIS SUBROUTINE.
C      Indicies are atomic numbers of atom pairs.)

C Scale raw QPAR parameters.
      do i=1,94
         qpar(i)=dsqrt(0.5d0*qqpar(i)*dsqrt(dfloat(i)))
      enddo

C Use external parameters if required.
      if (ld3sr6) then
        sr6=d3sr6
      endif
      if (ld3s8) then
        s8=d3s8
      endif

C     Zero out D3 dispersion variables and derivatives.
      D3ENERGY=0.0d0
      DERIV   =0.0d0
      r=   0.0d0
      dr=  0.0d0
      cn=  0.0d0
      dcn= 0.0d0

C     Write out coordinates used in this subroutine.
C     (MOPAC will rotate the initial coordinates user gives the program
C     so that the first atom is at the origin, the second atom is
C     on the x axis, and the third atom is in the xy-plane.)
C     Test with three dummy atoms first in geometry to align MOPAC's
c     coordinates with input file in this order: (0,0,0), (1,0,0), (0,1,0).
      if (debug.gt.10) then
        write(6,'(/5x,a)')
     &         "COORDINATES: (probably are rotated/translated)"
        do i=1,numat
          write(6,'(2x,i3,5x,3(f10.6,2x))') 
     &           nat(i),coord(1,i),coord(2,i),coord(3,i)
        enddo
      endif

C     Get interatomic distances and coordination numbers for all atoms.
c#      if (debug.gt.9) write(6,'(/''ATM   ELEM   COORD NUM (CN)'')')
      dr=0.0d0
      do i=1,numat
         cn(i)=0.0d0
         ni=nat(i)
         if (hpused.and.ni.eq.9) ni=1
         do j=1,numat
            if (i.ne.j) then
               nj=nat(j)
               if (hpused.and.nj.eq.9) nj=1
               r2=(coord(1,i)-coord(1,j))**2 +
     &            (coord(2,i)-coord(2,j))**2 +
     &            (coord(3,i)-coord(3,j))**2
               rij=dsqrt(r2)
               r(i,j)=rij
c              DR(i,j,mx,k)=derivative of r(i,j) wrt k's mx component
               do mx=1,3  ! already have i.ne.j
                 drterm=(coord(mx,i)-coord(mx,j))/rij
                 dr(i,j,mx,1)=dr(i,j,mx,1)+drterm  ! Derivative of r(i,j) wrt i's mx component
                 dr(i,j,mx,2)=dr(i,j,mx,2)-drterm  ! Derivative of r(i,j) wrt j's mx component
               enddo
               cold=cn(i)
c#               write(6,'(//)')
c#               write(6,*) " i,j=",i,j
c#               write(6,*) "Prior cn(i)=",cn(i)
c#               write(6,*) "Prior dcn(i,_,j)=",dcn(i,1,j)
c#               write(6,*) "                 ",dcn(i,2,j)
c#               write(6,*) "                 ",dcn(i,3,j)
c              DCN(i,mm,k)=derivative of CN(i) wrt k's coordinate mm
               hh=  k2*(rcov(ni)+rcov(nj))
               gg=  hh/rij   
               dgg(1)= -hh/r2*dr(i,j,1,1)   ! deriv of gg wrt atom i's x coord
               dgg(2)= -hh/r2*dr(i,j,2,1)   ! deriv of gg wrt atom i's y coord
               dgg(3)= -hh/r2*dr(i,j,3,1)   ! deriv of gg wrt atom i's z coord
               ff=  dexp(-k1*(gg-1.0d0))
               dff(1)= ff*(-k1)*dgg(1)      ! deriv of ff wrt atom i's x coord
               dff(2)= ff*(-k1)*dgg(2)      ! deriv of ff wrt atom i's y coord
               dff(3)= ff*(-k1)*dgg(3)      ! deriv of ff wrt atom i's z coord
               cc=  1.0d0/(1.0d0+ff)
               dc(1)= -(cc*cc)*dff(1)      ! deriv of cc wrt atom i's x coord
               dc(2)= -(cc*cc)*dff(2)      ! deriv of cc wrt atom i's y coord
               dc(3)= -(cc*cc)*dff(3)      ! deriv of cc wrt atom i's z coord
               cn(i)=cn(i)+cc
               dcn(i,1,j)=dcn(i,1,j)-dc(1)   ! summing individ terms of deriv of cn(i) wrt j's x coordinate
               dcn(i,2,j)=dcn(i,2,j)-dc(2)   ! summing individ terms of deriv of cn(i) wrt j's y coordinate
               dcn(i,3,j)=dcn(i,3,j)-dc(3)   ! summing individ terms of deriv of cn(i) wrt j's z coordinate
               dcn(i,1,i)=dcn(i,1,i)+dc(1)   ! summing individ terms of deriv of cn(i) wrt i's x coordinate
               dcn(i,2,i)=dcn(i,2,i)+dc(2)   ! summing individ terms of deriv of cn(i) wrt i's y coordinate
               dcn(i,3,i)=dcn(i,3,i)+dc(3)   ! summing individ terms of deriv of cn(i) wrt i's z coordinate
c#               write(6,*) "i,j,ni,nj=",i,j,ni,nj
c#               write(6,*) "rij=",rij," r(i,j)=",r(i,j)," r2=",r2
c#               write(6,*) "dr(i,j,1,1)=",dr(i,j,1,1)
c#               write(6,*) "dr(i,j,2,1)=",dr(i,j,2,1)
c#               write(6,*) "dr(i,j,3,1)=",dr(i,j,3,1)
c#               write(6,*) "dr(i,j,1,2)=",dr(i,j,1,2)
c#               write(6,*) "dr(i,j,2,2)=",dr(i,j,2,2)
c#               write(6,*) "dr(i,j,3,2)=",dr(i,j,3,2)
c#               write(6,*) "k1=",k1," k2=",k2
c#               write(6,*) "rcov(ni)=",rcov(ni)," rcov(nj)=",rcov(nj)
c#               write(6,*) "hh=",hh," dhh=0.0"
c#               write(6,*) "gg=",gg," dgg(1)=",dgg(1)
c#               write(6,*) "                             dgg(2)=",dgg(2)
c#               write(6,*) "                             dgg(3)=",dgg(3)
c#               write(6,*) "ff=",ff," dff(1)=",dff(1)
c#               write(6,*) "                             dff(2)=",dff(2)
c#               write(6,*) "                             dff(3)=",dff(3)
c#               write(6,*) "cc=",cc," dc(1)=",dc(1)
c#               write(6,*) "                             dc(2)=",dc(2)
c#               write(6,*) "                             dc(3)=",dc(3)
               cnew=cn(i)
c#               write(6,*) "New   cn(i)=",cn(i)
c#               write(6,*) "New   dcn(i,_,j)=",dcn(i,1,j)
c#               write(6,*) "                 ",dcn(i,2,j)
c#               write(6,*) "                 ",dcn(i,3,j)
            endif
         enddo
         if (debug.gt.9) then
            write(6,'(a)') "--------------------------------------"
            write(6,'(a)') "Atm   Elm     Coord # (CN)"
            write(6,'(i3,3x,i3,5x,f21.16)')  i, ni, cn(i)
         endif
         if (debug.gt.9) then
            write(6,'(a)')  "   Derivatives of CN wrt x,y,z:"
            do mi=1,numat
               write(6,'(2x,i3,3x,f20.12,3x,f20.12,3x,f20.12)') 
     &                       mi, dcn(i,1,mi), dcn(i,2,mi), dcn(i,3,mi)
            enddo
         endif
      enddo



C     Get C6 and damping values for all atom pairs and calculate total dispersion energy.
      d3energy=0.0d0
      total6=0.0d0
      total8=0.0d0
      dtot6=0.0d0
      dtot8=0.0d0
      hs6=s6*0.5d0
      hs8=s8*0.5d0
      do i=1,numat
         ni=nat(i)
         if (hpused.and.ni.eq.9) ni=1
         do j=1,numat
            if (i.eq.j) goto 200
            nj=nat(j)
            if (hpused.and.nj.eq.9) nj=1
            rij=r(i,j)
c            open(40,file='rij_compare.txt',access='append')     ! Add to end of existing file, or create new if none yet.
c            open(40,file='rij_compare.txt',status='new')        ! Make sure not to overwrite old file.
c            open(40,file='rij_compare.txt',access='sequential') ! Overwrite everything starting at beginning.
c            write(40,'(3x,i3,2x,i3,9x,2(f10.6,3x),5x,d12.4)')
c     &           i,j,rij,r(i,j),r(i,j)-rij
c            close(40)

            if (r0ab(ni,ni).lt.0.0d0) then
              write(6,'(a,i3,a,i3)')"No -D3 parameters loaded for atom "
     &                              ,i," element ",ni
              return 1
            endif
C           First get r**(-6) damping function.
            damp6=1.0d0/(1.0d0+6.0d0*(rij/(sr6*r0ab(ni,nj)))**(-alpha6))
            aa=1.0d0/(sr6*r0ab(ni,nj))
            bb=r(i,j)*aa
            dbb(1,1)=dr(i,j,1,1)*aa    ! Deriv of bb wrt i's x coord
            dbb(2,1)=dr(i,j,2,1)*aa    ! Deriv of bb wrt i's y coord
            dbb(3,1)=dr(i,j,3,1)*aa    ! Deriv of bb wrt i's z coord
            dbb(1,2)=dr(i,j,1,2)*aa    ! Deriv of bb wrt j's x coord
            dbb(2,2)=dr(i,j,2,2)*aa    ! Deriv of bb wrt j's y coord
            dbb(3,2)=dr(i,j,3,2)*aa    ! Deriv of bb wrt j's z coord
            cc=6.0d0*(bb**(-alpha6))
            hh=cc*(-alpha6/bb)
            dcc(1,1)=dbb(1,1)*hh       ! Deriv of cc wrt i's x
            dcc(2,1)=dbb(2,1)*hh       ! Deriv of cc wrt i's y
            dcc(3,1)=dbb(3,1)*hh       ! Deriv of cc wrt i's z
            dcc(1,2)=dbb(1,2)*hh       ! Deriv of cc wrt j's x
            dcc(2,2)=dbb(2,2)*hh       ! Deriv of cc wrt j's y
            dcc(3,2)=dbb(3,2)*hh       ! Deriv of cc wrt j's z
            dd=1.0d0/(1.0d0+cc)
            ddd(1,1)=-dd*dd*dcc(1,1)   ! Deriv of dd wrt i's x
            ddd(2,1)=-dd*dd*dcc(2,1)   ! Deriv of dd wrt i's y
            ddd(3,1)=-dd*dd*dcc(3,1)   ! Deriv of dd wrt i's z
            ddd(1,2)=-dd*dd*dcc(1,2)   ! Deriv of dd wrt j's x
            ddd(2,2)=-dd*dd*dcc(2,2)   ! Deriv of dd wrt j's y
            ddd(3,2)=-dd*dd*dcc(3,2)   ! Deriv of dd wrt j's z
            damp6=dd
c#            WRITE(6,'(///A,D20.14)') "Damp6 is ",damp6
c#            WRITE(6,'(4x,i3,8x,3(d15.9,2x))') 
c#     &          i,ddd(1,1),ddd(2,1),ddd(3,1)
c#            WRITE(6,'(4x,i3,8x,3(d15.9,2x))') 
c#     &          j,ddd(1,2),ddd(2,2),ddd(3,2)
c#            WRITE(6,'(//)')
            

            

C           Get C6 for atom pair i,j.
            c6(i,j)=0.0d0
            lij(i,j)=0.0d0
            tij(i,j)=0.0d0
            dtij=0.0d0
            dlij=0.0d0
            do ci=1,maxcn(ni)
            do cj=1,maxcn(nj)
              c1=cn(i)-c6ab(ni,nj,ci,cj,2)
              c2=cn(j)-c6ab(ni,nj,ci,cj,3)
              gtemp= exp(-k3*(c1*c1+c2*c2))
              do k=1,numat
               dgtemp(1,k)=-gtemp*2.0d0*k3*(c1*dcn(i,1,k)+c2*dcn(j,1,k))
               dgtemp(2,k)=-gtemp*2.0d0*k3*(c1*dcn(i,2,k)+c2*dcn(j,2,k))
               dgtemp(3,k)=-gtemp*2.0d0*k3*(c1*dcn(i,3,k)+c2*dcn(j,3,k))
              enddo
              lij(i,j)=lij(i,j)+gtemp
              do k=1,numat
               dlij(1,k)=dlij(1,k)+dgtemp(1,k)
               dlij(2,k)=dlij(2,k)+dgtemp(2,k)
               dlij(3,k)=dlij(3,k)+dgtemp(3,k)
              enddo
              tij(i,j)=tij(i,j)+gtemp*c6ab(ni,nj,ci,cj,1)
              do k=1,numat
               dtij(1,k)=dtij(1,k)+dgtemp(1,k)*c6ab(ni,nj,ci,cj,1)
               dtij(2,k)=dtij(2,k)+dgtemp(2,k)*c6ab(ni,nj,ci,cj,1)
               dtij(3,k)=dtij(3,k)+dgtemp(3,k)*c6ab(ni,nj,ci,cj,1)
              enddo
            enddo
            enddo
            c6(i,j)=tij(i,j)/lij(i,j)
            do k=1,numat
cobs              dc6(1,k)=zz*(lij(i,j)*dtij(1,k)-tij(i,j)*dlij(1,k))
cobs              dc6(2,k)=zz*(lij(i,j)*dtij(2,k)-tij(i,j)*dlij(2,k))
cobs              dc6(3,k)=zz*(lij(i,j)*dtij(3,k)-tij(i,j)*dlij(3,k))
              dc6(1,k)=(dtij(1,k)/lij(i,j))-
     &                 (tij(i,j)/lij(i,j)/lij(i,j)*dlij(1,k))
              dc6(2,k)=(dtij(2,k)/lij(i,j))-
     &                 (tij(i,j)/lij(i,j)/lij(i,j)*dlij(2,k))
              dc6(3,k)=(dtij(3,k)/lij(i,j))-
     &                 (tij(i,j)/lij(i,j)/lij(i,j)*dlij(3,k))
            enddo
            if (debug.ge.10) then
              write(6,'(/a,i2,a,i2,a,f20.16)') 
     &                "c6(",i,",",j,",)=",c6(i,j)
              write(6,'(a,i3,'','',i3,a)') 
     &                " Derivative of c6 for atom pair ",i,j," wrt:"
              do kk=1,numat
                write(6,'(3x,i3,10x,3(f20.15,2x))') 
     &                       kk,dc6(1,kk),dc6(2,kk),dc6(3,kk)
              enddo
              write(6,*)
            endif


C           Get r**(-6) dispersion energy contribution for atom pair i,j.
c           etemp=s6*(c6(i,j)/rij**6)*damp6
            rij6=1.0d0/r(i,j)**6
            etemp=s6*(c6(i,j)*rij6)*damp6    ! This atom pair's r**(-6) dispersion energy contribution.
            total6=total6+etemp*0.5d0        ! Sum of r**(-6) contributions only.  Factor of half for double counting.
c#            write(6,'(i2,x,i2,3x,a,d12.6,3x,a,d12.6)') 
c#     &           i,j,"total6=",total6,"etemp=",etemp
            d3energy=d3energy-etemp*0.5d0    ! Sum in r**(-6) dispersion contributions.  Factor of half for double counting.

C           Sum of derivatives of r**(-6) energy contributions.
            hetemp=etemp*0.5d0
            do k=1,numat
             dtot6(1,k)=dtot6(1,k)+hs6*(dc6(1,k)*rij6*damp6)
             dtot6(2,k)=dtot6(2,k)+hs6*(dc6(2,k)*rij6*damp6)
             dtot6(3,k)=dtot6(3,k)+hs6*(dc6(3,k)*rij6*damp6)
             if (k.eq.i) then
              dtot6(1,k)=dtot6(1,k)+hetemp*(-6.0d0/r(i,j))*dr(i,j,1,1)
              dtot6(2,k)=dtot6(2,k)+hetemp*(-6.0d0/r(i,j))*dr(i,j,2,1)
              dtot6(3,k)=dtot6(3,k)+hetemp*(-6.0d0/r(i,j))*dr(i,j,3,1)
              dtot6(1,k)=dtot6(1,k)+hs6*(c6(i,j)*rij6*ddd(1,1))
              dtot6(2,k)=dtot6(2,k)+hs6*(c6(i,j)*rij6*ddd(2,1))
              dtot6(3,k)=dtot6(3,k)+hs6*(c6(i,j)*rij6*ddd(3,1))
             endif
             if (k.eq.j) then
              dtot6(1,k)=dtot6(1,k)+hetemp*(-6.0d0/r(i,j))*dr(i,j,1,2)
              dtot6(2,k)=dtot6(2,k)+hetemp*(-6.0d0/r(i,j))*dr(i,j,2,2)
              dtot6(3,k)=dtot6(3,k)+hetemp*(-6.0d0/r(i,j))*dr(i,j,3,2)
              dtot6(1,k)=dtot6(1,k)+hs6*(c6(i,j)*rij6*ddd(1,2))
              dtot6(2,k)=dtot6(2,k)+hs6*(c6(i,j)*rij6*ddd(2,2))
              dtot6(3,k)=dtot6(3,k)+hs6*(c6(i,j)*rij6*ddd(3,2))
             endif
c             WRITE(6,*)"dc6(1,2), dr(i,j,1,1), ddd(1,1)=",
c     &                  dc6(1,2), dr(i,j,1,1), ddd(1,1)
c             WRITE(6,*)"c6(1,2), rij6, damp6=",
c     &                  c6(1,2), rij6, damp6
            enddo
            
            

C           Get r**(-8) damping function.
c           damp8=1.0d0/(1.0d0+6.0d0*(rij/(sr8*r0ab(ni,nj)))**(-alpha8))
            aa=1.0d0/(sr8*r0ab(ni,nj))
            bb=r(i,j)*aa
            dbb(1,1)=dr(i,j,1,1)*aa    ! Deriv of bb wrt i's x coord
            dbb(2,1)=dr(i,j,2,1)*aa    ! Deriv of bb wrt i's y coord
            dbb(3,1)=dr(i,j,3,1)*aa    ! Deriv of bb wrt i's z coord
            dbb(1,2)=dr(i,j,1,2)*aa    ! Deriv of bb wrt j's x coord
            dbb(2,2)=dr(i,j,2,2)*aa    ! Deriv of bb wrt j's y coord
            dbb(3,2)=dr(i,j,3,2)*aa    ! Deriv of bb wrt j's z coord
            cc=6.0d0*(bb**(-alpha8))
            hh=cc*(-alpha8/bb)
            dcc(1,1)=dbb(1,1)*hh       ! Deriv of cc wrt i's x
            dcc(2,1)=dbb(2,1)*hh       ! Deriv of cc wrt i's y
            dcc(3,1)=dbb(3,1)*hh       ! Deriv of cc wrt i's z
            dcc(1,2)=dbb(1,2)*hh       ! Deriv of cc wrt j's x
            dcc(2,2)=dbb(2,2)*hh       ! Deriv of cc wrt j's y
            dcc(3,2)=dbb(3,2)*hh       ! Deriv of cc wrt j's z
            dd=1.0d0/(1.0d0+cc)
            ddd(1,1)=-dd*dd*dcc(1,1)   ! Deriv of dd wrt i's x
            ddd(2,1)=-dd*dd*dcc(2,1)   ! Deriv of dd wrt i's y
            ddd(3,1)=-dd*dd*dcc(3,1)   ! Deriv of dd wrt i's z
            ddd(1,2)=-dd*dd*dcc(1,2)   ! Deriv of dd wrt j's x
            ddd(2,2)=-dd*dd*dcc(2,2)   ! Deriv of dd wrt j's y
            ddd(3,2)=-dd*dd*dcc(3,2)   ! Deriv of dd wrt j's z
            damp8=dd


C           Get C8 values for the atom pair.
c           c8(i,j)=3.0d0*c6(i,j)*qpar(ni)*qpar(nj)
            fact8=3.0d0*qpar(ni)*qpar(nj)
            c8(i,j)=c6(i,j)*fact8
            do k=1,numat
              dc8(1,k)=dc6(1,k)*fact8
              dc8(2,k)=dc6(2,k)*fact8
              dc8(3,k)=dc6(3,k)*fact8
            enddo


C           Get r**(-8) dispersion energy contribution for atom pair i,j.
c           etemp8=s8*(c8(i,j)/rij**8)*damp8
            rij8=1.0d0/r(i,j)**8
            etemp8=s8*(c8(i,j)*rij8)*damp8    ! This atom pair's r**(-8) dispersion energy contribution.
            total8=total8+etemp8*0.5d0         ! Sum of r**(-8) contributions only.  Factor of half for double counting.
c#            write(6,'(i2,x,i2,3x,a,d12.6,3x,a,d12.6)') 
c#     &           i,j,"total8=",total8,"etemp8=",etemp8
            d3energy=d3energy-etemp8*0.5d0



C           Sum of derivatives of r**(-8) energy contributions.
            hetemp8=etemp8*0.5d0
            do k=1,numat
             dtot8(1,k)=dtot8(1,k)+hs8*(dc8(1,k)*rij8*damp8)
             dtot8(2,k)=dtot8(2,k)+hs8*(dc8(2,k)*rij8*damp8)
             dtot8(3,k)=dtot8(3,k)+hs8*(dc8(3,k)*rij8*damp8)
             if (k.eq.i) then
              dtot8(1,k)=dtot8(1,k)+hetemp8*(-8.0d0/r(i,j))*dr(i,j,1,1)
              dtot8(2,k)=dtot8(2,k)+hetemp8*(-8.0d0/r(i,j))*dr(i,j,2,1)
              dtot8(3,k)=dtot8(3,k)+hetemp8*(-8.0d0/r(i,j))*dr(i,j,3,1)
              dtot8(1,k)=dtot8(1,k)+hs8*(c8(i,j)*rij8*ddd(1,1))
              dtot8(2,k)=dtot8(2,k)+hs8*(c8(i,j)*rij8*ddd(2,1))
              dtot8(3,k)=dtot8(3,k)+hs8*(c8(i,j)*rij8*ddd(3,1))
             endif
             if (k.eq.j) then
              dtot8(1,k)=dtot8(1,k)+hetemp8*(-8.0d0/r(i,j))*dr(i,j,1,2)
              dtot8(2,k)=dtot8(2,k)+hetemp8*(-8.0d0/r(i,j))*dr(i,j,2,2)
              dtot8(3,k)=dtot8(3,k)+hetemp8*(-8.0d0/r(i,j))*dr(i,j,3,2)
              dtot8(1,k)=dtot8(1,k)+hs8*(c8(i,j)*rij8*ddd(1,2))
              dtot8(2,k)=dtot8(2,k)+hs8*(c8(i,j)*rij8*ddd(2,2))
              dtot8(3,k)=dtot8(3,k)+hs8*(c8(i,j)*rij8*ddd(3,2))
             endif
c             WRITE(6,*)"dc8(1,2), dr(i,j,1,1), ddd(1,1)=",
c     &                  dc8(1,2), dr(i,j,1,1), ddd(1,1)
c             WRITE(6,*)"c8(1,2), rij8, damp8=",
c     &                  c8(1,2), rij8, damp8
            enddo
            
            

C           Debugging output.
            if (debug.gt.9) write(6,*) "**",i,j,c6(i,j),lij(i,j)
            if (debug.gt.9)
     &          write(6,'(4(2(i1,x),3x),2(f12.7,2x),2(3x,f12.7))')
     &          i,j,ni,nj,ci,cj,maxcn(ni),maxcn(nj),c6ab(ni,nj,ci,cj,2),
     &          c6ab(ni,nj,ci,cj,3),c6ab(ni,nj,ci,cj,1),gtemp

 200     continue
         enddo
      enddo




C     Output some debugging information.
      if (debug.ne.0) then
        write(6,'(a,f16.10)')  "% E8          =",
     &                                       total8/(-d3energy)*100.d0
        write(6,'(a,2f16.10)') "Final etemp6  =",total6,total6*627.515d0
        write(6,'(a,2f16.10)') "Final etemp8  =",total8,total8*627.515d0
        write(6,'(a,2f16.10)') "Sum etemp6,8  =",(total6+total8),
     *                                         (total6+total8)*627.515d0
        write(6,'(a,f16.10,a)') "Final D3ENERGY=",d3energy," a.u."
      endif



      cq=627.515d0
      if (debug.gt.9) then
        write(6,'(/a,f20.16)') "E6 energy total=",total6*cq
        write(6,'(a,i3,'','',i3,a)') 
     &            " Derivative of total6:"
        do kk=1,numat
          write(6,'(3x,i3,10x,3(f20.15,2x))') 
     &            kk,dtot6(1,kk)*cq,dtot6(2,kk)*cq,dtot6(3,kk)*cq
        enddo
        write(6,*)
        write(6,'(/a,f20.16)') "E8 energy total=",total8*cq
        write(6,'(a,i3,'','',i3,a)') 
     &            " Derivative of total8:"
        do kk=1,numat
          write(6,'(3x,i3,10x,3(f20.15,2x))') 
     &            kk,dtot8(1,kk)*cq,dtot8(2,kk)*cq,dtot8(3,kk)*cq
        enddo
        write(6,*)
      endif


      d3energy=d3energy*627.515d0
      do i=1,numat
        do mi=1,3
           deriv(mi,i)=-dtot6(mi,i)-dtot8(mi,i)
           deriv(mi,i)=deriv(mi,i)*627.515d0
        enddo
      enddo

C#      write(6,'(a,f16.10,a)') "Final D3ENERGY=",d3energy," kcal/mol"
      if (debug.ne.0)
     &  write(6,'(a,f16.10,a)') "Final D3ENERGY=",d3energy," kcal/mol"
      
      if (debug.ne.0) then
        write(6,'(a)') "Final D3ENERGY derivatives, DERIV: (kcal/mol/A)"
        write(6,'(a)') "             X            Y            Z"
        do i=1,numat
          write(6,'(3x,i3,3x,f10.5,2x,f10.5,2x,f10.5)')
     &      i, deriv(1,i), deriv(2,i), deriv(3,i)
        enddo
        write(6,'(//)')
      endif

C     Print out final information.
      if (debug.gt.5) then
         write(6,'(//A)')
     &   "   ATM1 ATM2   ELM1 ELM2   "//
     &   "    X1       Y1       Z1       "//
     &   "    X2       Y2       Z2         DISTANCE      C6"
        DO 110 I=1,NUMAT
           NI=NAT(I)
           if (hpused.and.ni.eq.9) ni=1
           DO 100 J=I+1,NUMAT
              NJ=NAT(J)
              if (hpused.and.nj.eq.9) nj=1
              rij=DSQRT((COORD(1,I)-COORD(1,J))**2+
     &                  (COORD(2,I)-COORD(2,J))**2+
     &                  (COORD(3,I)-COORD(3,J))**2)
              write(6,'(3x,i3,2x,i3,4x,i3,2x,i3,4x,
     &           3(f8.4,x),4x,3(f8.4,x),4x,f10.6,2x,f10.6)') 
     &           i,j,ni,nj,coord(1,i),coord(2,i),coord(3,i),
     &           coord(1,j),coord(2,j),coord(3,j),rij,c6(i,j)
 100       CONTINUE
 110    CONTINUE
      endif


      RETURN
 9999 RETURN 1
      END
