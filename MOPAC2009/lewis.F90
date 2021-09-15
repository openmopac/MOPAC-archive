subroutine lewis (radius)
   !***********************************************************************
   !
   !  LEWIS works out what atom is bonded to what atom.  It does this
   !  using the Cartesian coordinates, COORD, and the covalent radii.
   !
   ! On exit, NBONDS holds the number of bonds to each atom, and
   !          IBONDS contains the atom numbers.
   !
   ! The maximum number of bonded atoms per atom is 4.  More are allowed
   ! for checking purposes, but for a SCF calculation, 4 is the maximum.
   !
   !***********************************************************************
    use molkst_C, only: natoms, numat, l1u, l2u, l3u, id, maxtxt, keywrd
    use chanel_C, only: iw
    use elemts_C, only: elemnt
    use common_arrays_C, only: txtatm, tvec, coord, nat, labels, &
      nbonds, ibonds
    use reada_I
    implicit none
    double precision, dimension (numat), intent (in) :: radius
!
    logical :: debug, let, lbond
    integer :: i, ibad, ii, ik, j, jbad, jk, k, kl, l, l1, l2, l3, m, n
    double precision :: r, rmin, x1, x2, x3, safety
    character :: blank*60 = " "
    double precision, dimension (3) :: coord1
    integer, dimension (:), allocatable :: im, iz
!
    allocate (im(natoms), iz(numat))
    let = (Index (keywrd, " 0SCF") + Index (keywrd, " LET") &
         & + Index (keywrd, " RESEQ") /= 0)
    debug = (Index (keywrd, " LEWIS") /= 0)
    l1 = Min (1, l1u)
    l2 = Min (1, l2u)
    l3 = Min (1, l3u)
    j = 0
    do i = 1, natoms
      if (labels(i) /= 99) then
        j = j + 1
        im(j) = i
      end if
    end do
    nbonds = 0
    ibad = 0
    do i = 1, numat
      if (nat(i) == 1) then
         !
         !  DO H ATOMS FIRST.  ALL H ATOMS ARE ASSUMED TO BE ATTACHED TO THE
         !  NON-H ATOM NEAREST TO THE H ATOM.
         !
        rmin = 1.d15
        x1 = coord(1, i)
        x2 = coord(2, i)
        x3 = coord(3, i)
        iz(i) = 0
        do j = 1, numat
          if (radius(j) >= 0.d0 .and. nat(j) /= 1) then
            if (id == 0) then
              r = (x1-coord(1, j)) ** 2 + (x2-coord(2, j)) ** 2 + &
                   & (x3-coord(3, j)) ** 2
              if (r < rmin) then
                iz(i) = j
                rmin = r
              end if
            else
              do ik = -l1, l1
                do jk = -l2, l2
                  do kl = -l3, l3
                    do l = 1, 3
                      coord1(l) = coord(l, j) + tvec(l, 1) * ik &
                           & + tvec(l, 2) * jk + tvec(l, 3) * kl
                    end do
                    r = (coord1(1)-coord(1, i)) ** 2 &
                         & + (coord1(2)-coord(2, i)) ** 2 &
                         & + (coord1(3)-coord(3, i)) ** 2
                    if (r < rmin) then
                      iz(i) = j
                      rmin = r
                    end if
                  end do
                end do
              end do
            end if
          end if
        end do
        rmin = Sqrt (rmin)
        if (iz(i) == 0) then
          write (iw, "(A,I4,A)") &
               & " Atom", i, " is very far from any non-hydrogen atom!"
          call mopend ("One hydrogen appears to have no bonds")
        end if
        if (rmin > Max (1.3d0, 3.d00*radius(iz(i)))) then
          if (ibad == 0) then
            write (iw,*)
          end if
          ibad = ibad + 1
          if (ibad <= 20) then
            write (iw, "(A,I5,A,F8.3,A)") " Hydrogen atom", i, " is", &
           & rmin, " angstroms from the nearest non-hydrogen atom."
            write (iw, "(A,F8.3,A)") " It should be within", &
           & Max (1.3d0, 2.d00*radius(iz(i))), &
           & " angstroms from the non-hydrogen atom in MOZYME."
          else if (ibad == 21) then
            write (iw,*) " REMAINING ERRORS SUPPRESSED"
          end if
        end if
      end if
    end do
    if (ibad == 1) then
      write (iw,*)
    end if
    if (ibad /= 0 .and. .not. let) then
      call mopend ("A hydrogen atom is badly positioned")
    end if
   !
   !  NOW ALL NON-H ATOMS.  ATOMS ARE ASSUMED ATTACHED IF THEY ARE WITHIN
   !  1.1  TIMES THE SUM OF THEIR COVALENT RADII.
   !
    jbad = ibad
    ibad = 0
    do i = 1, numat
      if (nat(i) == 1) then
        j = iz(i)
        if (j == 0) then
          write (iw,*) " HYDROGEN ATOM", i, &
         & " DOES NOT APPEAR TO BE BONDED TO ANYTHING"
          ibad = 1
        end if
        nbonds(i) = 1
        ibonds(1, i) = j
        nbonds(j) = Min (nbonds(j)+1, 8)
        ibonds(nbonds(j), j) = i
      else
        x1 = coord(1, i)
        x2 = coord(2, i)
        x3 = coord(3, i)
        do j = 1, i - 1
          if (nat(j) /= 1) then
            if (i /= j) then
              rmin = 1.d6
              if (id == 0) then
                rmin = (x1-coord(1, j)) ** 2 + (x2-coord(2, j)) ** 2 + &
               & (x3-coord(3, j)) ** 2
              else
                do ik = -l1, l1
                  do jk = -l2, l2
                    do kl = -l3, l3
                      do l = 1, 3
                        coord1(l) = coord(l, j) + tvec(l, 1) * ik + tvec &
                       & (l, 2) * jk + tvec(l, 3) * kl
                      end do
                      rmin = Min (rmin, (coord1(1)-coord(1, i))**2 &
                           & + (coord1(2)-coord(2, i))**2 &
                           & + (coord1(3)-coord(3, i))**2)
                    end do
                  end do
                end do
              end if
              if(nat(i) == 6 .and. nat(j) == 6) then
                safety = 1.17d0 
              else
                safety = 1.1d0
              end if
              if (Sqrt(rmin) < safety*(radius(i)+radius(j))) then
                nbonds(i) = Min (nbonds(i)+1, 8)
                nbonds(j) = Min (nbonds(j)+1, 8)
                ibonds(nbonds(i), i) = j
                ibonds(nbonds(j), j) = i
              end if
            end if
          end if
        end do
      end if
    end do
   !
   !  Parse CVB(2:3,45:65)
   !
    i = Index (keywrd, " CVB")
    k = 0
    if (i /= 0) then
      k = Index (keywrd(i:), ")") + i
    end if
    if (k == i .and. i /= 0) then
      k = Index (keywrd(i + 3:), " ") + i + 3
      write(iw,"(//10x,a)")" No closing parenthesis for CVB keyword"
      write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(i:k)//'"'
      call mopend ("ERROR IN CVB KEYWORD")
      return
    end if
    if (i /= 0) then
      do
        j = Nint (reada (keywrd(i:k), 1))
        ii = Index (keywrd(i:k), ":") + i
        if (ii == i) then
          write (iw,*)
          write (iw,*) " ERROR IN CVB KEYWORD - ':' EXPECTED BUT NOT FOUND"
          j = Index (keywrd, " CVB")
          write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(j:k)//'"'
          call mopend ("ERROR IN CVB KEYWORD")
          return
        end if
        i = ii
        l = Nint (reada (keywrd(i:k), 1))
        if (j > numat .or. j == 0 .or. j < -numat) then
          write (iw,*) " AT LEAST ONE ATOM DEFINED BY CVB IS FAULTY"
          write (iw,*) " THE FAULTY ATOM NUMBER IS", j
          j = Index (keywrd, " CVB")
          write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(j:k)//'"'
          call mopend ("AT LEAST ONE ATOM DEFINED BY CVB IS FAULTY")
          return
        end if
        if (l > numat .or. l == 0 .or. j < -numat) then
          write (iw,*) " AT LEAST ONE ATOM DEFINED BY CVB IS FAULTY"
          write (iw,*) " THE FAULTY ATOM NUMBER IS", l
          j = Index (keywrd, " CVB")
          write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(j:k)//'"'
          call mopend ("AT LEAST ONE ATOM DEFINED BY CVB IS FAULTY")
          return
        end if
        if (j > 0 .and. l > 0) then
          do ii = 1, nbonds(j)
            if (ibonds(ii, j) == l .and. .not. let) then
              write(iw,"(//a,i5,a,i5,a)")" The bond defined by CVB between atoms",j," and", &
              l, " already exists.  Correct error and re-submit"
              write(iw,"(a)") " (If an extra bond needs to be added, use keyword 'SETPI')"
              call mopend(" Fault in CVB keyword")
              return
            end if
          end do
          nbonds(j) = nbonds(j) + 1
          nbonds(l) = nbonds(l) + 1
          ibonds(nbonds(j), j) = l
          ibonds(nbonds(l), l) = j
        else
!
!  make sure that the faulty bond actually exists.
!
          j = Abs(j)
          l = Abs(l)
          if (j == 0 .or. l == 0) then
            write(iw,"(//10x,a)")" Error detected while reading CVB"
            j = Index (keywrd, " CVB")
            write(iw,'(10x,a)')" CVB keyword ="//'"'//keywrd(j:k)//'"'
            call mopend(" Fault in CVB keyword")
            return    
          end if
          lbond = .false.
          do n = 1, nbonds(j)
            if (ibonds(n,j) == l) then
!
!  Faulty bond exists, therefore delete it.
!          
              do m = n + 1,nbonds(j)
                ibonds(m - 1,j) = ibonds(m,j)
              end do
              nbonds(j) = nbonds(j) - 1
              lbond = .true.
            end if
          end do
          do n = 1, nbonds(l)
            if (ibonds(n,l) == j) then
!
!  Faulty bond exists, therefore delete it.
!          
              do m = n + 1,nbonds(l)
                ibonds(m - 1,l) = ibonds(m,l)
              end do
              nbonds(l) = nbonds(l) - 1
              lbond = .true.
            end if
          end do
          if (.not. lbond) then
            write(iw,"(//a,i5,a,i5,a)")" The bond defined by CVB between atoms",j," and", &
              l, " did not exist.  Correct error and re-submit"
              call mopend(" Fault in CVB keyword")
              return
          end if
        end if        
        j = Index (keywrd(i:k), ",")+Index (keywrd(i:k), ";")
        if (j /= 0) then
          i = i + j
        else
          exit
        end if
      end do
    end if
    if (ibad /= 0 .and. .not. let) then
      write (iw,*) " (HYDROGENS MUST BE BONDED TO NON-HYDROGEN ATOMS)"
      call mopend ("HYDROGENS MUST BE BONDED TO NON-HYDROGEN ATOMS")
    end if
    jbad = jbad + ibad
    ibad = 0
    do i = 1, numat
      if (nat(i) == 1) then
         !
         !  CHECK TO SEE IF ANY HYDROGEN ATOMS ARE BRIDGING HYDROGENS
         !
        rmin = 1.d15
        x1 = coord(1, i)
        x2 = coord(2, i)
        x3 = coord(3, i)
        k = iz(i)
        do j = 1, nbonds(k)
          l = ibonds(j, k)
          if (l /= i .and. nat(l) /= 1) then
            if ((x1-coord(1, l))**2 + (x2-coord(2, l))**2 &
                 & + (x3-coord(3, l))**2 < 1.3d0) then
              write (iw, "(A,I5,A,A2,I5,A,A2,I5)") " HYDROGEN ATOM", im (i), &
                   & " HAS COVALENT BONDS TO ", elemnt (nat(k)), im (k), &
                   & " AND ", elemnt (nat(l)), im(l)
              ibad = 1
            end if
          end if
        end do
      end if
    end do
    if (ibad /= 0 .and. .not. let) then
      call mopend ("A HYDROGEN ATOM IS BRIDGING")
    end if
    jbad = jbad + ibad
    ibad = 0  
   !
   !  Special check for nitro groups
   !
    do i = 1, numat
      if (nat(i) == 7) then
        k = 0
        l = 0
        do j = 1, nbonds(i)
          if (nat(ibonds(j,i)) == 8 .and. nbonds(ibonds(j,i)) == 1) then
            k = ibonds(j,i)
            exit
          end if
        end do
        do j = j + 1, nbonds(i)
          if (nat(ibonds(j,i)) == 8 .and. nbonds(ibonds(j,i)) == 1) then
            l = ibonds(j,i)
            exit
          end if
        end do
       !
       !  Found a system with N bonded to two oxygen atoms (k and l), 
       !  which are not bonded to anything else.
       !
        if (k /= 0 .and. l /= 0) then
         !
         !  Put a bond between the two oxygen atoms
         !
          nbonds(k) = 2
          nbonds(l) = 2
          ibonds(2,k) = l
          ibonds(2,l) = k
        end if
      end if
    end do
   !
   !  End of special check for nitro groups
   !
!
!  Put atoms into order of atomic number
!
    do i = 1, numat
      if (nbonds(i) > 1) then
        n = nbonds(i)
        iz(:n) = ibonds(:n,i)
        do j = 1, n
          k = 0
          do m = 1, n
            if (iz(m) /= 0) then
              if (nat(iz(m)) > k) then
                l = m
                 k = nat(iz(m))
              end if 
            end if
          end do         
          ibonds(j,i) = iz(l)
          iz(l) = 0
        end do        
      end if
    end do
    if (debug .and. jbad /= 0 .and. .not. let) then
      write (iw, "(/,A,/)") "   TOPOGRAPHY OF SYSTEM"
      write (iw,*) "  ATOM No. "//blank(:maxtxt/2 + 1)//"  LABEL  "//blank(:maxtxt/2 + 1)//"Atoms connected to this atom"
      do i = 1, numat
        write (iw, "(I7,9X,A,9I7)") i, elemnt (nat(i)) // " " // txtatm &
       & (im(i)) (:maxtxt) // " ", (ibonds(j, i), j=1, nbonds(i))
      end do
    end if
    if (jbad /= 0 .and. .not. let) then
      call mopend ("Geometry is faulty")
    end if
end subroutine lewis
