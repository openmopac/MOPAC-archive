subroutine geochk ()
   !***********************************************************************
   !                                                                      *
   !  GEOCHK DOES SEVERAL VERY DIFFERENT THINGS:
   !
   !   (A) IT CHECKS THE GEOMETRY TO MAKE SURE IT CONFORMS TO THE LEWIS
   !       STRUCTURE CONVENTIONS.
   !
   !   (B) IT IDENTIFIES ALL IONIZED ATOMS. LATER ON, MAKVEC WILL NEED TO
   !       KNOW WHICH ATOMS ARE IONIZED.
   !
   !   (C) IT CALCULATES THE CHARGE ON THE SYSTEM.
   !
   !   (D) (OPTIONAL) IT RESEQUENCES THE ATOMS SO THAT ALL ATOMS OF EACH
   !       RESIDUE ARE CONTIGUOUS.  IF THIS IS DONE, THE JOB CANNOT BE
   !       CONTINUED, SO THE NEW GEOMETRY IS PRINTED OUT AND THE RUN
   !       STOPPED.
   !
   !                                                                      *
   !***********************************************************************
    use common_arrays_C, only : geo, coord, nfirst, nlast, &
       & labels, nat, na, nb, nc, nbonds, ibonds, txtatm, atmass
    use MOZYME_C, only : ions, icharges, angles, allres, iz, ib, &
    iopt, nres, at_res, res_start, Lewis_tot, Lewis_elem, noccupied, nvirtual
!    
    use molkst_C, only: natoms, numat, nelecs, keywrd, moperr, maxtxt, &
      maxatoms, line, nalpha, nbeta, uhf, nclose, nopen, norbs, id
    use chanel_C, only: iw, iarc, archive_fn
    use mod_atomradii, only: atom_radius_covalent
    use parameters_C, only : ams, natorb, tore, main_group
    use elemts_C, only : elemnt
    use reada_I
    implicit none
!
    character (len=40) :: padding
    character (len=20), allocatable :: Lewis_formatted(:,:)
    character (len=1), dimension (:), allocatable :: atom_charge
    character (len=12) :: ion_names(-6:6)
    character :: num
    double precision, dimension(:), allocatable :: radius
    logical, save :: debug, let, lres, lreseq, times, opend, charges, &
      done, lstart_res(100), neutral(100), lsite
    logical, dimension (:), allocatable :: ioptl
    integer :: i, ibad, ichrge, irefq, ires, ii, jj, m, nfrag, io, kk, &
   & j, jbad, k, l, large, n1, new, alloc_stat, uni_res, mres, near_ions(100), &
     maxtxt_store, nn1
    integer, dimension(:), allocatable ::  mb    
    integer, save :: numbon(3), start_res(100), num_ions(-6:6)
    integer, dimension(:), allocatable :: nnbonds
    integer, dimension(:,:), allocatable :: iibonds
    double precision :: sum, r_ions(100)
    data numbon / 3 * 0 /
    data ion_names / &
      "Less than -5", &
      "Penta-anion ", &
      "Tetra-anion ", &
      "Tri-anion   ", &
      "Di-anion    ", &
      "Anion       ", &
      "(not used)  ", &
      "Cation      ", &
      "Di-cation   ", &
      "Tri-cation  ", &
      "Tetra-cation", &
      "Penta-cation", &
      "More than +5" /
!
    intrinsic Abs, Index, Min, Nint, Allocated
!
    if (allocated(ions))    deallocate (ions)
    if (allocated(iopt))    deallocate(iopt)
    if (allocated(at_res))  deallocate(at_res)
    if (Allocated (iz))     deallocate (iz)
    if (Allocated (ib))     deallocate (ib)
    allocate (ions(maxatoms), iz(maxatoms), ib(maxatoms), mb(maxatoms), at_res(maxatoms), &
            & atom_charge(maxatoms), ioptl(maxatoms), iopt(maxatoms), radius(maxatoms), &
            stat=alloc_stat)
    if (alloc_stat /= 0) then
      call mopend(" Failed to allocate arrays in GEOCHK")
      go to 1100
    end if
    ib(:) = 0
    at_res = 0
    mres = 0
    ions = 0
   !
    done = .false.
    start_res = -2
    lstart_res = .true.
    i = index(keywrd," START_RES")
    if(i /= 0) then
      i = i + 10
      k = index(keywrd(i:),")") + i - 1
      do j = 1, 10
        if (i >= k) exit
        start_res(j) = nint(reada(keywrd, i)) - 1
        l = index(keywrd(i:k), "-") !  Part of a protein
        if (l /= 0) then
          i = i + l 
        else
        l = index(keywrd(i:k), " ") !  Start of a new protein
          if (l /= 0) then
            i = i + l 
            lstart_res(j + 1) = .true.
          end if
        end if
        if (l == 0) exit
      end do     
    end if
!
!   Add or remove hydrogen atoms, as necessary.
!
    i = index(keywrd," SITE")
    neutral = .false.
    lsite = .false.
!
!   Identify sites that either should be ionized or should not be ionized.
!
!   These are:
!
!   1:   -COOH
!   2:   -COO(-)
!   3:   -NH3(+)
!   4:   -NH2
!   5:   -Arg(+)-
!   6:   -Arg-
!   7:   -His(+)-
!   8:   -His-
!
    if (i /= 0) then
       call extvdw_for_MOZYME (radius, atom_radius_covalent)
      call lewis(radius)
      j = index(keywrd(i + 4:)," ") + i + 4
      if (index(keywrd(i:j),"COOH") /= 0) then
        neutral(1) = .true.
      else if (index(keywrd(i:j),"COO") /= 0) then
        neutral(2) = .true.
      end if
      if (index(keywrd(i:j),"NH3") /= 0) then
        neutral(3) = .true.
      else if (index(keywrd(i:j),"NH2") /= 0) then
        neutral(4) = .true.
      end if
      if (index(keywrd(i:j),"ARG(+)") /= 0) then
        neutral(5) = .true.
      else if (index(keywrd(i:j),"ARG") /= 0) then
        neutral(6) = .true.
      end if
      if (index(keywrd(i:j),"HIS(+)") /= 0) then
        neutral(7) = .true.
      else if (index(keywrd(i:j),"HIS") /= 0) then
        neutral(8) = .true.
      end if
      lsite = .true.
      call site(neutral)
    end if
   !
   !  Store charge, if present
   !
    do i = 1,natoms
      atom_charge(i) = txtatm(i)(2:2)
!
!  Prevent atom number being mis-read as a charge.
!
      if (txtatm(i)(2:2) /= "+" .and. txtatm(i)(2:2) /= "-" .and. txtatm(i)(2:2) /= "0") &
      & atom_charge(i) = " "
    end do
   !
   !    ASSIGN LOGICALS USING KEYWRD
   !
    lres = (Index (keywrd, " RESI")+Index (keywrd, " NEWGEO")+ &
   & Index (keywrd, " RESEQ")+Index (keywrd, " PDBOUT") /= 0)
    if (lres) lres = (index(keywrd, " NORES") == 0) 
    lreseq = (Index (keywrd, " RESEQ") /= 0)
    let = (Index (keywrd, " 0SCF")+Index (keywrd, " LET")+ &
   & Index (keywrd, " RESEQ")+Index (keywrd, " GEO-OK") /= 0)
    times = (Index (keywrd, " TIMES") /= 0)
    if (times) then
      call timer (" START OF GEOCHK")
    end if
    debug = (Index (keywrd, " GEOCHK") /= 0)
    if (Index (keywrd, " CHARGE=") /= 0) then
      irefq = Nint (reada (keywrd, Index (keywrd, " CHARGE=")))
    else
      irefq = 0
    end if
    call extvdw_for_MOZYME (radius, atom_radius_covalent)
    if (moperr) return
   !
    if (Index (keywrd, " LARGE") /= 0) then
      large = 1000000
    else
      large = 20
    end if
   !
   !   WORK OUT WHAT ATOMS ARE BONDED TO EACH OTHER.
   !
    call lewis (radius)
    if (moperr) return   
    allocate (nnbonds(numat), iibonds(15,numat), stat=alloc_stat)
    if (alloc_stat /= 0) then
      call memory_error ("geochn")
      go to 1100
    end if
!
!  Store nbonds and ibonds in case they are modified within this subroutine
!
    nnbonds = nbonds
    iibonds = ibonds
    if (moperr) then
      if (Index (keywrd, " GEO-OK") == 0) then
        write (iw,*) " GEOMETRY CONTAINS FAULTS. TO CONTINUE CALCULATION SPECIFY ""GEO-OK"""
        go to 1100
      else
        moperr = .false.
      end if
    end if
   !
   !   ZERO OUT IONS
   !
    if (lres) then
      !
      !  FIND THE NITROGEN ATOM OF THE N END OF THE PROTEIN.
      !
      do i = 1, numat
        ioptl(i) = .false.
      end do
      call findn1 (n1, ioptl, io)
      if (n1 == 0 .and. Index(keywrd, "PDBO") == 0) then
        write (iw,*) " NO PEPTIDE LINKAGES FOUND.  CHECK DATA SET"
        call mopend ("NO PEPTIDE LINKAGES FOUND.  CHECK DATA SET")
        go to 1100
      end if
    end if
    if (lreseq) then
      !
      !   RESEQUENCE THE ATOMS.  WHEN RESEQ IS CALLED, THE SEQUENCE OF
      !   ATOMS IS CHANGED.  A CONSEQUENCE OF THIS IS THAT THE SCF CANNOT
      !   BE RUN.
      !
   !   na(1) = 1
      new = 0
      do
        if (n1 /= 0) call reseq (ioptl, iz, n1, new, io)
        if (moperr) go to 1100
        call findn1 (n1, ioptl, io)
        if (n1 == 0) exit
      end do
      if (new /= numat) then
     !   write (iw,*) "for moiety ", new, numat
        do i = 1, numat
          if (nat(i) /= 1 .and. .not. ioptl(i)) then
               !
               !   IDENTIFY ALL NON-PROTEIN MOLECULES IN THE SYSTEM
               !
            call moiety (ioptl, iz, i, new)
          end if
        end do
        do i = 1, numat
          if (nat(i) == 1 .and. .not. ioptl(i)) then
               !
               !   IDENTIFY ALL HYDROGENS ATTACHED TO RESIDUE-LIKE SPECIES
               !
            new = new + 1
            iz(new) = i
          end if
        end do
        if (new /= numat) then
          write (iw,*) " THERE IS A FAULT IN RESEQ"
          write (iw,*) new, numat
          call mopend (" THERE IS A FAULT IN RESEQ")
          go to 1100
        end if
!
!  Unconditionally, convert geometry into Cartesian coordinates
!
        geo(:,:numat) = coord(:,:numat)
        na = 0
      end if
      l = 1
      do i = 1, numat
        nfirst(i) = l
        j = iz(i)
        mb(j) = i
        do k = 1, 3
          geo(k, i) = coord(k, j)
        end do
        labels(i) = nat(j)
        nlast(i) = nfirst(i) + natorb(labels(i)) - 1
        l = nlast(i) + 1 
      end do
      nat(:numat) = labels(:numat)
      coord(:,:numat) = geo(:,:numat)
      done = .true.
      !
      !   DUMMY ATOMS ARE EXCLUDED, THEREFORE
      !
      natoms = numat
      !
      !
      !   REARRANGE ATOMS TO SUIT THE NEW NUMBERING SYSTEM
      !
      do i = 1, numat
        j = iz(i)
        ib(i) = Min (nbonds(j), 4)
        l = Min (nbonds(i), 4)
        do k = 1, l
          ibonds(k+4, i) = mb(ibonds(k, i))
        end do
      end do
      do i = 1, numat
        nbonds(i) = ib(i)
        do k = 1, nbonds(i)
          ibonds(k, i) = ibonds(k+4, iz(i))
        end do
      end do
    end if
    noccupied = 0  
    if (Index (keywrd, " RESEQ")+Index (keywrd, " SITE") == 0) then
   !
   !  EXAMINE THE GEOMETRY - IDENTIFY THE LEWIS ELEMENTS (SIGMA BONDS,
   !  LONE PAIRS, CATIONS, PI BONDS, ANIONS, OTHER CHARGES)
   !
      numbon = 0
      call chklew (mb, numbon, l, large, debug)
      if (moperr) return
      l = 0
      do i = 1, numat
        l = l + Abs (iz(i))
      end do
      if (l /= 0) then         
        !
        !  THERE ARE IONS.  IDENTIFY THEM.
        !
         call chkion (mb, numbon(2), atom_charge)
        if (lreseq) moperr = .false.
      end if
      do i = 1, numat
        ions(i) = nint(tore(nat(i)))
      end do
      do i = 1, Lewis_tot
        if (Lewis_elem(1,i) > 0) then
          noccupied = noccupied + 1
          j = Lewis_elem(1,i)
          if (Lewis_elem(2,i) > 0) then
            k = Lewis_elem(2,i)
            ions(k) = ions(k) - 1  ! one electron from a bond
            ions(j) = ions(j) - 1  ! one electron from a bond
          else
            ions(j) = ions(j) - 2  ! two electrons from a lone pair
          end if
        end if
      end do
      nvirtual = 0
      do i = 1, Lewis_tot
        if (Lewis_elem(2,i) > 0) nvirtual = nvirtual + 1
      end do

    end if 
!
!   Use the following block to debug the construction of the Lewis structure
!
    if ( .false.) then
!
!  Sanity check - are all atomic orbitals accounted for?
!
      k = abs(norbs - noccupied - nvirtual)
      do i = 1, numat
        if (iz(i) /= 0) then
          k = k + 1
        end if
        if (ib(i) /= 0) then
          k = k + 1
        end if
      end do
      iz = 0
      do i = 1, Lewis_tot
        j = Lewis_elem(1,i)
        if (j > 0) iz(j) = iz(j) + 1
        j = Lewis_elem(2,i)
        if (j > 0) iz(j) = iz(j) + 1
      end do
      do i = 1, numat
        if (iz(i) - natorb(nat(i)) /= 0) then
          k = k + 1
        end if
      end do
      if (k /= 0) then
        write(iw,*)" An error has been detected."
      end if
    end if
   !
   !
    if (lres) then
      txtatm = " "
      maxtxt = -1
      angles = 0.d0
      allres = " "
      if (done) then        
         !
         !   THE EARLIER CALL TO RESEQ MEANS THAT N1 MIGHT HAVE MOVED.
         !   SO FIND N1 AGAIN.
         !
        ioptl(:numat) = .false.
        call findn1 (n1, ioptl, io)
      end if
      ib(:numat) = -100000
      nfrag = 0
      ires = 0
      uni_res = 0
      do
        nfrag = nfrag + 1
        if (start_res(nfrag) /= -2) ires = start_res(nfrag)

        ! General bug-trap: if the number of fragments is unreasonably large,
        ! assume the system is unrecognizable and exit
        if (ires > natoms/4 .or. nfrag > 99) then
          write (iw,*) " STRUCTURE UNRECOGNIZABLE"
          call mopend("Structure Unrecognizable")
          inquire(unit=iarc, opened=opend) 
          if (opend) close(iarc, status = "DELETE")          
          go to 1100
        endif
   !
        call names (ioptl, ib, n1, ires, nfrag, io, uni_res, mres)
   !
        if (nfrag /= 1 .and. lstart_res(nfrag)) then
          do i = n1 - 1, 1, -1
            if (txtatm(i) /= " ") then
              txtatm(i)(7:7) = "+"
              exit
            end if
          end do
        end if     
        nn1 = n1
        call findn1 (n1, ioptl, io)
        if (n1 == 0) exit  
        if (n1 == nn1) ioptl(n1) = .true.                
      end do
!
!  Split up unknowns if they exist
!
      j = 0
      do i = 1, numat 
        j = j + 1
        if (j > numat - 1) exit
        if (txtatm(j)(8:10) == "UNK") then
          if (txtatm(j + 1)(8:10) == "UNK") then
            sum = (coord(1,j) - coord(1, j + 1))**2 + (coord(2,j) - coord(2, j + 1))**2 + (coord(3,j) - coord(3, j + 1))**2 
            if (sum > 100.d0)  then
              txtatm(j)(7:7) = "+"
            end if
          end if          
        end if
      end do
!
!  Re-evaluate all residues
!
      j = 1
      allres(j) = txtatm(1)(8:10)
      do i = 2, natoms
        if (j == 37) then
          j = 37
        end if
        if (txtatm(i) == " ")exit
        if (nat(i) /= 1 .and. txtatm(i)(12:14) /= txtatm(i - 1)(12:14)) then
        j = j + 1
        allres(j) = txtatm(i)(8:10)
        end if
      end do
      ires = j
      if (uni_res > 1) then
        write(iw,"(/10x,a)")"        Ramachandran Angles"
        write(iw,"(10x,a,/)")"    Residue    phi    psi  omega"
      end if
      do i = 1, uni_res
        if (Abs(angles(1,i)) + Abs(angles(3,i))> 1.d-20 .and. res_start(i) > 0) &
        write(iw,"(14x,a,3f7.1)")txtatm(res_start(i))(8:10)//" "//txtatm(res_start(i))(12:14), (angles(j,i),j = 1,3)
      end do
      write(iw,*)" "
      iopt(:natoms) = ib(:natoms)
      !
      !   LABEL THE ATOMS IN ANY NON-PROTEIN MOLECULES IN THE SYSTEM
      !
      nfrag = nfrag + 1
      if (start_res(nfrag) /= -2) ires = start_res(nfrag) ! + 1
      call ligand (ires)
      nres = uni_res
    end if   
   !
   !  MODIFY IONS SO THAT IT REFERS TO ALL ATOMS (REAL AND DUMMY)
   !
    j = 0
    iz = ions
    ions = 0
    do i = 1, natoms
      if (labels(i) == 99) then
        ions(i) = 0       
      else
         j = j + 1
        ions(i) = iz(j)
      end if
    end do
    ibad = 0
    if (lres .and. .not. lreseq) then
      !
      !   CHECK ALL IONS TO SEE IF ANY RESIDUE IS A DI-ION
      !
      outer_loop: do i = 1, numat
        if(ions(i) /= 1 .and. ions(i) /= -1) cycle  ! WARNING
      end do outer_loop
      if (ibad /= 0) then
        write (iw,*)
      end if
      jbad = ibad
      ibad = 0
      ibad = ibad + jbad
    end if
!
!  
!
    charges = (lsite .or. index(keywrd, "CHARGES") /= 0)
    ichrge = -noccupied*2
    do i = 1, numat
      ichrge = ichrge + nint(tore(nat(i)))
    end do
    line = " "
    if (noccupied /= 0 .and. (index(keywrd," LEWIS") > 0 .or. noccupied*2 /= nelecs)) then
      write (iw, "(/,A,/)") "   TOPOGRAPHY OF SYSTEM"
      write (iw,*) "  ATOM No. "//line(:maxtxt/2 + 1)//"  LABEL  "//line(:maxtxt/2 + 1)//"Atoms connected to this atom"
      do i = 1, numat
        write (iw, "(I7,9X,A,9I7)") i, elemnt (nat(i)) // " " // txtatm(i) (:maxtxt) // " ", (ibonds(j, i), j=1, nbonds(i))
      end do
    end if
    if (noccupied /= 0 .and. index(keywrd," LEWIS") > 0) then
      write (iw, "(/37x,A,/)") "   Lewis Structure"
      if (index(keywrd, " LARGE") /= 0)  &
        write (iw,"(23x,a,/)") "  ATOMS IN OCCUPIED LOCALIZED MOLECULAR ORBITALS"
      l = 4
      allocate(Lewis_formatted(Lewis_tot/l + 5, l))
      Lewis_formatted = " "
      k = 0
      j = 0
      do i = 1, Lewis_tot
        if (Lewis_elem(1,i) > 0) j = j + 1
      end do
      m = j/l + 1 
               !    123456789012345678901234567890    
      write(iw,"(4('     LMO  Atom  Atom    '))")  
      ii = 0
      jj = 1  
      do i = 1, Lewis_tot
        if (Lewis_elem(1,i) > 0) then
          k = k + 1
          ii = ii + 1
          if (ii > m) then
            ii = 1
            jj = jj + 1
          end if
          if (Lewis_elem(2,i) > 0) then
            write (Lewis_formatted(ii,jj), "(I8,I6,I6)") k, (Lewis_elem(j,i),j=1,2)
          else
            write (Lewis_formatted(ii,jj), "(I8,I6)") k, Lewis_elem(1,i)
          end if
        end if
      end do
      do i = 1, Lewis_tot
        if (Lewis_formatted(i,1) == " ") exit
        write(iw,"(10a)")(Lewis_formatted(i,j)//"    ", j = 1, l)," "
      end do
      if (index(keywrd, " LARGE") /= 0) then
        write (iw,"(/23x,a,/)") "  ATOMS IN UNOCCUPIED LOCALIZED MOLECULAR ORBITALS"
        Lewis_formatted = " "
        j = 0
        do i = 1, Lewis_tot
          if (Lewis_elem(2,i) > 0) j = j + 1
        end do
        m = j/l + 1
        k = 0
        ii = 0
        jj = 1  
        do i = 1, Lewis_tot
          if (Lewis_elem(2,i) > 0) then
            k = k + 1
            ii = ii + 1
            if (ii > m) then
              ii = 1
              jj = jj + 1
            end if
            if (Lewis_elem(1,i) > 0) then
              write (Lewis_formatted(ii,jj), "(I8,I6,I6)") k, (Lewis_elem(j,i),j=1,2)
            else
              write (Lewis_formatted(ii,jj), "(I8,I6)") k, Lewis_elem(2,i)
            end if
          end if
        end do
        do i = 1, Lewis_tot
          if (Lewis_formatted(i,1) == " ") exit
          write(iw,"(10a)")(Lewis_formatted(i,j)//"    ", j = 1, l)," "
        end do
      end if
      deallocate(Lewis_formatted)
    end if
    num_ions = 0
    do i = 1, numat
      j = (Min(6, Max(-6, ions(i))))
      num_ions(j) = num_ions(j) + 1
    end do
    write (iw,*)
    if (noccupied > 0) write (iw,"(a,/)") "          Type          Number of Lewis structural elements identified"
    if (numbon(1) /= 0) then
      write (iw, "(A,I6)") "         SIGMA BONDS   ", numbon (1)
    end if
    if (numbon(2) /= 0) then
      write (iw, "(A,I6)") "         LONE PAIRS    ", numbon (2)
    end if
    if (numbon(3) /= 0) then
      write (iw, "(A,I6)") "         PI BONDS      ", numbon (3)
    end if
    if (index(keywrd," LEWIS") > 0 .or. (noccupied*2 /= nelecs .and. noccupied /= 0)) then
      write(iw,"(/,a,i6)")" Number of filled levels from atoms and charge:", nelecs/2
      write(iw,"(a,i6)")" Number of filled levels from Lewis structure: ", noccupied
    end if
    l = 0
    m = 0
    num = char(Int(log10(numat + 1.0)) + ichar("1") + 1) 
    do i = 1, numat
      if (.not. main_group(nat(i))) then
!
!  Element is a transition metal.  Work out its formal oxidation state
!
        k = ions(i)
        do j = 1, Lewis_tot
          if (Lewis_elem(1,j) /= 0 .and. Lewis_elem(2,j) /= 0) then
            if (Lewis_elem(1,j) == i) k = k + 1
            if (Lewis_elem(2,j) == i) k = k + 1
          end if
        end do
        if (m == 0) write(iw,*)
        m = 1
        write(iw,"(10x,a,i"//num//",a,sp,i3)")" Formal oxidation state of atom",i,", a "//trim(elemnt(nat(i)))//", is", k
  !      write(iw,"(10x,a,i5,a,sp,i3)")" Formal oxidation state of atom",i,", a "//trim(elemnt(nat(i)))//", is", k
        if (k < 0) l = 1
        if (k > 3) l = 1
      end if
    end do
    if (l == 1) call web_message(iw,"Lewis_structures.html")
    i = 0
    do j = 1,6
      i = i + num_ions(j) + num_ions(-j)
    end do
    if (i > 0) then
      write (iw,*)
      write (iw,"(a,/)") "          Type           Number of charged sites identified"
      do i = 1,6
      if (num_ions(i)  > 0) write (iw, "(9x,a,2x,i6)") ion_names(i) , num_ions(i)
      if (num_ions(-i) > 0) write (iw, "(9x,a,2x,i6)") ion_names(-i), num_ions(-i)
      end do
      i = 0
      do j = 1,6
        i = i + num_ions(j) * j
      end do
      write(iw,"(/,a,i5)")" SUM OF POSITIVE CHARGES", i
      i = 0
      do j = 1,6
        i = i + num_ions(-j) * j
      end do
      write(iw,"(a,i5)")" SUM OF NEGATIVE CHARGES", -i
      padding = " "
      maxtxt_store = maxtxt
      if (maxtxt < 0) maxtxt = 14
      if (maxtxt > 1) then
         write(iw,"(/,a)")"   Ion Atom No.                 Label                Type    Charge"
         l = max(1,(17 - maxtxt/2))
      else
         write(iw,"(/,a)")"   Ion Atom No.  Type    Charge"
      end if
      
      do j = 6, -6, -1
        if (j == 0) cycle
        k = 0
        do i = 1, numat
          if (ions(i) == j) then
            if (k == 0) write(iw,*)
            k = k + 1
            line = " "
            jj = 0
            if (j == 1) then
              do ii = 1, numat
                if (ions(ii) < 0) then
                  sum = (coord(1,i) - coord(1,ii))**2 + (coord(2,i) - coord(2,ii))**2 + (coord(3,i) - coord(3,ii))**2 
                  if (sum < 99.d0) then
                    jj = jj + 1
                    if (jj > 100) exit
                    near_ions(jj) = ii
                    r_ions(jj) = sqrt(sum)
                    line = " Angstroms from anion"
                  end if                  
                end if
              end do
            else if (j == -1) then
              do ii = 1, numat
                if (ions(ii) > 0) then
                  sum = (coord(1,i) - coord(1,ii))**2 + (coord(2,i) - coord(2,ii))**2 + (coord(3,i) - coord(3,ii))**2 
                  if (sum < 99.d0) then
                    jj = jj + 1
                    if (jj > 100) exit
                    near_ions(jj) = ii
                    r_ions(jj) = sqrt(sum)
                    line = " Angstroms from cation"
                  end if                  
                end if
              end do
            end if
            if (jj > 1) then
!
!  Sort near ions into increasing distance
!
              jj = min(jj,100)
              do ii = 1, jj
                do kk = ii + 1, jj
                  if (r_ions(kk) < r_ions(ii)) then
                    sum = r_ions(kk)
                    r_ions(kk) = r_ions(ii)
                    r_ions(ii) = sum
                    m = near_ions(kk)
                    near_ions(kk) = near_ions(ii)
                    near_ions(ii) = m
                  end if
                end do 
              end do
              do ii = 2, jj
                if (r_ions(ii) > 5.d0) then
                  jj = ii - 1
                  exit
                end if
              end do
            end if
            if (maxtxt > 1) then
              if (jj > 0) then
                kk = 0
                if (elemnt(nat(near_ions(1)))(1:1) /= " ") kk = 1
                write(iw,"(i5,3x,i5,3x, a,4x,a2,i9,a,f3.1,a)") &
                  & k, i, padding(:l)//txtatm(i)(1:maxtxt)//padding(:l), elemnt(nat(i)),ions(i), &
                  & "    (",r_ions(1),line(:len_trim(line) + kk)//elemnt(nat(near_ions(1)))// &
                  &": "//line(30:32 - kk)//txtatm(near_ions(1))(:maxtxt)//")"
                if (jj > 1) then
                  do ii = 2, jj
                    kk = 0
                    if (elemnt(nat(near_ions(ii)))(1:1) /= " ") kk = 1
                    write(iw,"(66x, a, f3.1,a)")  "   (",r_ions(ii), &
                    line(:len_trim(line) + kk)//elemnt(nat(near_ions(ii)))// &
                      &": "//line(30:32 - kk)//txtatm(near_ions(ii))(:maxtxt)//")"
                  end do
                end if
              else
                write(iw,"(i5,3x,i5,3x, a,4x,a2,i9,a)") &
                k, i, padding(:l)//txtatm(i)(1:maxtxt)//padding(:l), elemnt(nat(i)),ions(i)
              end if
            else
              write(iw,"(i5,3x,i5,5x,a2,i9)")k, i, elemnt(nat(i)),ions(i)
            end if            
          end if
        end do
      end do
      maxtxt = maxtxt_store
    end if
    write (iw,*)
    if (index(keywrd, " LEWIS") /= 0) then
      if (noccupied*2 == nelecs)write (iw,*) " Run stopped because keyword LEWIS was used"
      call mopend ("Keyword LEWIS used")
    end if
    if (noccupied /= 0 .and. (charges .or. .not. lreseq .and. (ichrge /= 0 .or. irefq /= 0))) then
      write (iw, "(/10x,A,I4)") " COMPUTED CHARGE ON SYSTEM:", ichrge
    end if
   !
    if(maxtxt == -1) then
       txtatm(:) = "("//txtatm(:)(1:14)//")"//txtatm(:)(15:17)
       maxtxt = 16
    end if
    atmass(1:numat) = ams(nat(1:numat))
    if (done .and. .not. lreseq) then
      call xyzint (coord, numat, na, nb, nc, 1.d0, geo)
    end if
    if (lreseq) then
      geo(:,:numat) = coord(:,:numat)
      na = 0
    end if   
    if (lsite) then
      
      inquire(unit=iarc, opened=opend) 
      if ( .not. opend) then
        open(unit=iarc, file=archive_fn, status='UNKNOWN', position='asis') 
        rewind iarc 
      end if
      call geout (iarc)
      if ( index(keywrd," PDBOUT") /= 0) then
        archive_fn = archive_fn(:len_trim(archive_fn) - 3)//"pdb" 
        inquire(unit=iarc, opened=opend) 
        if (opend) close(iarc)
        open(unit=iarc, file=archive_fn, status='UNKNOWN', position='asis') 
        rewind iarc 
        coord(:,:numat) = geo(:,:numat)
        call pdbout(1)
        call pdbout(iarc)
      end if
      call mopend ("Keyword SITE used")
      write(iw,'(//,a)')" Run stopped because SITE used"
      return
    end if
    if (irefq /= ichrge .and. .not. lreseq .or. charges) then
      !
      !  THE CALCULATED CHARGE DOES NOT MATCH THAT DEFINED BY CHARGE=N.
      !  THEREFORE, THE USER HAS MADE A MISTAKE.  WRITE OUT CHARGES
      !  FOUND HERE.
      !
      !
      if (icharges /= 0) then
        write (iw,*)   
      end if
!
!Only print error message if CHARGE= is present
!
      if (irefq /= ichrge .and. index(keywrd," CHARGE=") /= 0) &
        write (iw, "(10x,A,I3,A)") " CHARGE SPECIFIED FOR SYSTEM (", irefq,") IS INCORRECT"  
      if (charges) then
        write (iw,*) " Run stopped because keyword CHARGES or SITE was used"
        call mopend ("Keyword CHARGES or SITE used")
        return
      end if    
      if (index(keywrd, " LEWIS") /= 0) then
        write (iw,*) " Run stopped because keyword LEWIS was used"
        call mopend ("Keyword LEWIS used")
        return
      end if 
      !
      !
      if (Index (keywrd, " 0SCF")+Index (keywrd, " RESEQ") + &
      & Index (keywrd, " LEWIS") == 0) then    
        write(iw,*)" "              
        if (irefq > 9) then
          write (iw, "(10x,A,I2,A)") " IN THE DATA-SET SUPPLIED, THE CHARGE SPECIFIED (+", irefq,") IS INCORRECT."  
        else if (irefq > 0) then
          write (iw, "(10x,A,I1,A)") " IN THE DATA-SET SUPPLIED, THE CHARGE SPECIFIED (+", irefq,") IS INCORRECT."  
        else if (irefq == 0) then
          if (index(keywrd," CHARGE=") /= 0) &
            write (iw, "(10x,A,I2,A)") " IN THE DATA-SET SUPPLIED, THE CHARGE SPECIFIED (0) IS INCORRECT."  
        else
          write (iw, "(10x,A,I2,A)") " IN THE DATA-SET SUPPLIED, THE CHARGE SPECIFIED (", irefq,") IS INCORRECT."  
        end if 
        if (index(keywrd," GEO-OK") /= 0) then
          write (iw, "(10x,A)") " KEYWORD 'GEO-OK' WAS PRESENT, SO THE CHARGE HAS BEEN RESET."
          write (iw, "(10x,A)") " IF THE NEW CHARGE IS INCORRECT, EITHER MODIFY THE STRUCTURE OR "//&
          "USE KEYWORD 'SETPI' TO CORRECT THE LEWIS STRUCTURE." 
        end if     
        nelecs = nelecs + irefq - ichrge     
        nclose = nelecs/2
        nopen = nclose
        nalpha = 0
        nbeta = 0
        uhf = .false.     
        if (irefq /= ichrge) then
          write(iw,"(/10x,a)")" If the Lewis structure used by MOZYME is incorrect, use keywords such as SETPI to correct it"
          call web_message(iw,"setpi.html")
          if (Index(keywrd," GEO-OK") == 0 .and. index(keywrd," CHARGE=") /= 0 ) then  
          write(iw,"(10x,a)")" To over-ride this check, either add 'GEO-OK' to the set of keywords or remove the 'CHARGE' keyword."  
          write(iw,"(10x,a)")" If that is done, then the correct charge will be used."
          call mopend(" Charge specified is incorrect.  Either remove keyword 'CHARGE' or add keyword 'GEO-OK'")
          return  
        else if (id > 0) then
          write(iw,"(10x,a)")" Infinite systems must have a zero charge on the unit cell."
          call mopend(" Unit cell has a charge. Correct fault and re-submit ")
          return
        else 
          call fix_charges(ichrge)  
        end if
      end if
      end if
    end if
    if (done) then
      write (iw, "(//10X,A,//)") " GEOMETRY AFTER RE-SEQUENCING"
      call geout (iw)
      if (index(keywrd, "0SCF") + index(keywrd, " RESEQ") == 0 .or. &
        index(keywrd, " PDBOUT") == 0) then
        inquire(unit=iarc, opened=opend) 
        if ( .not. opend) then
          open(unit=iarc, file=archive_fn, status='UNKNOWN', position='asis') 
          rewind iarc 
        end if
        write (iarc, "(//10X,A,//)") " GEOMETRY AFTER RE-SEQUENCING"
        call geout (iarc)
      end if
      if ( index(keywrd," PDBOUT") /= 0) then
        call pdbout(1)
        call pdbout(iarc)
      end if
      call mopend (" GEOMETRY RESEQUENCED")
      go to 1100
    end if
    if (ibad /= 0 .and. .not. let) then
      call mopend (" ERROR")
      go to 1100
    end if
    if (Index (keywrd, " NEWGEO") /= 0) then
      call newflg ()
    end if
    if (times) then
      call timer (" END OF GEOCHK")
    end if
   !
   !  MODIFY IONS SO THAT IT REFERS TO REAL ATOMS ONLY
   !
    j = 0
    do i = 1, natoms
      if (labels(i) /= 99) then
        j = j + 1
        iz(i) = j
      end if
    end do
    do j = 1, 2
      do i = 1, numat
   !     if (ions(j, i) /= 0) then
      !    ions(j, i) = iz(ions(j, i))
      !  end if
      end do
    end do
!
!  Restore charges, if present
!
    do i = 1, natoms
      if(atom_charge(i) /= " ") txtatm(i)(2:2) = atom_charge(i)
    end do
!
!  Restore nbonds and ibonds in case they are modified within this subroutine
!
    nbonds(:numat) = nnbonds
    ibonds(:,:numat) = iibonds(:,:numat)
1100 continue
    if (Allocated (nnbonds))      deallocate (nnbonds, iibonds)
    if (Allocated (iz))           deallocate (iz)
    if (Allocated (ib))           deallocate (ib)
    if (Allocated (mb))           deallocate (mb)
    if (Allocated (atom_charge))  deallocate (atom_charge)
    if (Allocated (ioptl))        deallocate (ioptl)
    return
end subroutine geochk
subroutine extvdw_for_MOZYME (radius, refvdw)
    use molkst_C, only: numat, keywrd
    use chanel_C, only: iw
    use common_arrays_C, only: nat
    use mod_atomradii, only: is_metal
    use wrdkey_I 
    use reada_I
    implicit none
    double precision, dimension (numat), intent (out) :: radius
    double precision, dimension (102), intent (in) :: refvdw
    integer :: i, j, k
    double precision, dimension (102) :: vdw(102)
    character (len=80) :: txt_rad
    character (len=2), dimension (102) :: elemnt

    data elemnt / "H ", "HE", "LI", "BE", "B ", "C ", "N ", "O ", "F ", "NE", &
   & "NA", "MG", "AL", "SI", "P ", "S ", "CL", "AR", "K ", "CA", "SC", "TI", &
   & "V ", "CR", "MN", "FE", "CO", "NI", "CU", "ZN", "GA", "GE", "AS", "SE", "BR", &
   & "KR", "RB", "SR", "Y ", "ZR", "NB", "MO", "TC", "RU", "RH", "PD", "AG", &
   & "CD", "IN", "SN", "SB", "TE", "I ", "XE", "CS", "BA", "LA", "CE", "PR", &
   & "ND", "PM", "SM", "EU", "GD", "TB", "DY", "HO", "ER", "TM", "YB", "LU", "HF", &
   & "TA", " W", "RE", "OS", "IR", "PT", "AU", "HG", "TL", "PB", "BI", "PO", &
   & "AT", "RN", "FR", "RA", "AC", "TH", "PA", " U", "NP", "PU", "AM", "CM", &
   & "BK", "CF", "XX", "+3", "-3", "CB" /
   !
   !  Modify Van der Waal's radii of various atoms.  The format of
   ! the keyword is: VDW(:chemical symbol=n.nn:chemical symbol=n.nn ...)
   ! e.g. VDWM(:H=1.0:Cl=1.7)
   !
    txt_rad = " "
    i = index(keywrd," METAL")
    if (i /= 0) then
      j = index(keywrd(i:),")") + i
      txt_rad = keywrd(i:j)
      do k = 1,83
        if (.not. is_metal(k)) is_metal(k) = (index(txt_rad,elemnt(k)) /= 0)
      end do
    end if
    i = index(keywrd," VDWM(")
    if (i /= 0) then
      if (keywrd(i+6:i+6) /= ":") keywrd(i+6:) = ":"//keywrd(i+6:)
      j = index(keywrd(i + 6:),")") + i + 6
      do i = i + 6, j
         if (keywrd(i:i) == ":") keywrd(i:i) = ";"
         if (keywrd(i:i) == ",") keywrd(i:i) = ";"
      end do
    end if
    do i = 1, 102
      j = 2
      if (elemnt(i) (2:2) == " ") then
        j = 1
      end if
      vdw(i) = wrdkey (keywrd, "VDWM(", 5, ";"//elemnt(i) (:j)//"=", j+2, refvdw(i))
    end do
    !
    !  Verify that all radii that will be used are, in fact, set correctly
    !
    do i = 1, numat
      if (nat(i) > 102) cycle
      if (vdw(nat(i)) > 900.d0) then
        write (iw,*) "MISSING VAN DER WAALS RADIUS FOR " // elemnt (nat(i))
        j = 2
        if (elemnt(nat(i)) (2:2) == " ") j = 1
        write (iw, "(2x,3a)") "To correct this, add keyword 'VDWM(:", &
       & elemnt (nat(i)) (1:j), "=n.nn)'"
        call mopend ('MISSING VAN DER WAALS RADIUS FOR '//elemnt(nat(i)))
        return
      end if
    end do
   !
   ! Flag which elements are to be treated as metals by giving them negative
   ! atomic radii
    do i = 1, 102
      if (is_metal(i)) vdw(i) = -1.0d0
    end do
    do i = 1, numat
      radius(i) = vdw(nat(i))
    end do
!
!  Set VDW radius for individual atoms to -1
!
    if (txt_rad /= " ") then
      do i = 1, len_trim(txt_rad)
        if (ichar(txt_rad(i:i)) - ichar("0") <= 9 .and. ichar(txt_rad(i:i)) - ichar("0") > 0) then
          j = nint(reada(txt_rad(i:), 1))
          radius(j) = -1.d0
          do j = i, len_trim(txt_rad)
            if (ichar(txt_rad(i:i)) - ichar("0") > 9 .or. &
            ichar(txt_rad(i:i)) - ichar("0") < 0) exit
            txt_rad(i:i) = " "
          end do
        end if
      end do
    end if
    return
end subroutine extvdw_for_MOZYME
subroutine fix_charges(ichrge)  
  use molkst_C, only: refkey, keywrd, line
  implicit none
  integer, intent (in) :: ichrge
  integer :: i, j
!
!  Modify charges in keyword line so that the system will run using MOZYME.
!  Steps:
!     If "old" CHARGE=n keyword exists in the reference keyword line, delete it.
!     Write new CHARGE keyword, if charge is non-zero
!     Rewind the data set file.
!     Write out the new data set.
!     Rewind it again, so it is ready for reading.
!     Rewind the output file, so the user does not see the faulty output.
!
      line = refkey(1)
      call upcase(line,len_trim(line))     
      i = Index(line," CHARGE=")
      if (i /= 0) then
        j = Index(refkey(1)(i+2:)," ")
        refkey(1)(i + 1:) = refkey(1)(i + j + 2:)
      end if
      !                 12345678901 
      i = Index(refkey(1),"            ")
      if (ichrge /= 0) then
        if (ichrge > 99) then
          write(refkey(1)(i:i+11),'(" CHARGE=",i3)')ichrge
        else if (ichrge > 9) then
          write(refkey(1)(i:i+11),'(" CHARGE=",i2)')ichrge
        else if (ichrge > 0) then
          write(refkey(1)(i:i+11),'(" CHARGE=",i1)')ichrge
        else if (ichrge > -10) then
          write(refkey(1)(i:i+11),'(" CHARGE=",i2)')ichrge
        else 
          write(refkey(1)(i:i+11),'(" CHARGE=",i3)')ichrge
        end if
      end if
       i = Index(keywrd," CHARGE=")
      if (i /= 0) then
        j = Index(keywrd(i+2:)," ")
        keywrd(i + 1:) = keywrd(i + j + 2:)
      end if
      !                 12345678901 
      i = Index(keywrd,"            ")
      if (ichrge /= 0) then
        if (ichrge > 99) then
          write(keywrd(i:i+11),'(" CHARGE=",i3)')ichrge
        else if (ichrge > 9) then
          write(keywrd(i:i+11),'(" CHARGE=",i2)')ichrge
        else if (ichrge > 0) then
          write(keywrd(i:i+11),'(" CHARGE=",i1)')ichrge
        else if (ichrge > -10) then
          write(keywrd(i:i+11),'(" CHARGE=",i2)')ichrge
        else 
          write(keywrd(i:i+11),'(" CHARGE=",i3)')ichrge
        end if
      end if
end subroutine fix_charges
subroutine add_sp_H(i1, i, i2)
!
! Given three atoms, i1, i, and i2, put a hydrogen atom at the point 
! of a triangle
!
  use molkst_C, only: natoms, maxatoms
  use common_arrays_C, only : geo, nat, txtatm
  use chanel_C, only: iw
  implicit none
  integer :: i1, i, i2
  logical :: first = .true.
  double precision :: scale
  natoms = natoms + 1
  if (natoms > maxatoms) then
    if (first) then
      write(iw,*)" Too many changes. Re-run using the data set generated by this job"
      first = .false.
    end if
    natoms = natoms - 1
    return
  end if
  geo(:,natoms) = 2.d0*geo(:,i1) - 2.d0*geo(:,i) + geo(:,i2)
  scale = 1.1d0/sqrt( (geo(1,i1) - geo(1,natoms))**2 + &
                      (geo(2,i1) - geo(2,natoms))**2 + &
                      (geo(3,i1) - geo(3,natoms))**2 )
  geo(:,natoms) = geo(:,i1) + scale*(geo(:,natoms) - geo(:,i1))
  nat(natoms) = 1
  txtatm(natoms) = " "
end subroutine add_sp_H
subroutine add_sp2_H(i1, i, i2)
!
! Given three atoms, i1, i, and i2, put a hydrogen atom at the point 
! of a triangle
!
  use molkst_C, only: natoms, maxatoms
  use common_arrays_C, only : geo, nat, txtatm
  use chanel_C, only: iw
  implicit none
  integer :: i1, i, i2
  logical :: first = .true.
  double precision :: scale
  natoms = natoms + 1
  if (natoms > maxatoms) then
    if (first) then
      write(iw,*)" Too many changes. Re-run using the data set generated by this job"
      first = .false.
    end if
    natoms = natoms - 1
    return
  end if
  geo(:,natoms) = 3.d0*geo(:,i) - geo(:,i1) - geo(:,i2)
  scale = 1.1d0/sqrt( (geo(1,i) - geo(1,natoms))**2 + &
                      (geo(2,i) - geo(2,natoms))**2 + &
                      (geo(3,i) - geo(3,natoms))**2 )
  geo(:,natoms) = geo(:,i) + scale*(geo(:,natoms) - geo(:,i))
  nat(natoms) = 1
  txtatm(natoms) = " "
end subroutine add_sp2_H
subroutine add_sp3_H(i1, i, i2, i3)
!
! Given four atoms, i1, i, and i2, put a hydrogen atom at the point 
! of a tetrahedron
!
  use molkst_C, only: natoms, maxatoms
  use common_arrays_C, only : geo, nat, txtatm
  use chanel_C, only: iw
  implicit none
  integer :: i1, i, i2, i3
  logical :: first = .true.
  double precision :: scale
  natoms = natoms + 1
  if (natoms > maxatoms) then
    if (first) then
      write(iw,*)" Too many changes. Re-run using the data set generated by this job"
      first = .false.
    end if
    natoms = natoms - 1
    return
  end if
  geo(:,natoms) = 4.d0*geo(:,i) - geo(:,i1) - geo(:,i2) - geo(:,i3)
  scale = 1.1d0/sqrt( (geo(1,i) - geo(1,natoms))**2 + &
                      (geo(2,i) - geo(2,natoms))**2 + &
                      (geo(3,i) - geo(3,natoms))**2 )
  geo(:,natoms) = geo(:,i) + scale*(geo(:,natoms) - geo(:,i))
  nat(natoms) = 1
  txtatm(natoms) = " "
end subroutine add_sp3_H
subroutine site(neutral)
  use common_arrays_C, only : geo, coord, labels, nat, nbonds, ibonds, &
    atmass, txtatm
  use parameters_C, only : ams
  use molkst_C, only : numat, natoms
  implicit none
  logical, intent(in) :: neutral(8)
  integer :: i_atom
  integer :: i, j, k, l, m, o(2)
  do i_atom = 1, numat
!
!  Deliberately add or remove hydrogen atoms as required by keyword "SITE"
!
    if (neutral(5) .or. neutral(6)) then      !  Must be -Arg(+)- or -Arg-
!
!  Check for Arg or Arg(+)
!
      if (nat(i_atom) == 6 .and. nbonds(i_atom) == 3) then
        if (nat(ibonds(1, i_atom)) == 7 .and. &
            nat(ibonds(2, i_atom)) == 7 .and. &
            nat(ibonds(3, i_atom)) == 7 ) then  ! System is C bonded to three N, check for number of H on N
          i = 0
          do j = 1,3
            k = ibonds(j,i_atom)
            do l = 1, nbonds(k)
              m = ibonds(l,k)
              if (nat(m) == 1) i = i + 1
            end do
          end do
          if (i /= 5 .and. neutral(6) .or. i /= 4 .and. neutral(5)) cycle
          if (neutral(6)) then
!
! Found the system -NH-C(NH2)2, now delete a hydrogen atom
!
            do j = 1, 3
              if (nbonds(ibonds(j, i_atom)) == 3) then
                l = ibonds(j, i_atom)
                k = 0
                if (nat(ibonds(1,l)) == 1) k = k + 1
                if (nat(ibonds(2,l)) == 1) k = k + 1
                if (nat(ibonds(3,l)) == 1) k = k + 1
                if (k == 2) then
                  if (nat(ibonds(1,l)) == 1) then
                    k = ibonds(1,l)
                  else
                    k = ibonds(2,l)
                  end if
                  nat(k) = 99
                  exit
                end if
              end if
            end do
          else
!
! Found the system -NH-C(NH2)(NH), now add a hydrogen atom
!
            do j = 1, 3
              if (nbonds(ibonds(j, i_atom)) == 2) then ! Found a N with two bonds
                l = ibonds(j, i_atom)
                call add_sp2_H(ibonds(1,l), l, ibonds(2,l))                  
                exit
              end if
            end do
          end if
        end if
      end if
    end if
    if (neutral(1)  .or. neutral(2)) then          !  Must be -COOH or -COO(-)
      if (nat(i_atom) == 6 .and. nbonds(i_atom) == 3) then
        k = 0
        do i = 1,3
          j = ibonds(i,i_atom)
          if (nat(j) == 8) then
            if (nbonds(j) == 1) then
              k = k + 1
            else if (nbonds(j) == 2 .and. (nat(ibonds(1,j)) == 1 .or. nat(ibonds(2,j)) == 1)) then
              k = k + 1
            end if
          end if
        end do
        if (k == 2) then
          if(neutral(2)) then
            do i = 1,2
              j = ibonds(i,i_atom)
              if (nat(j) == 8 .and. nbonds(j) == 2 .and. (nat(ibonds(1,j)) == 1 .or. nat(ibonds(2,j)) == 1)) then
                if(nat(ibonds(1,j)) == 1) nat(ibonds(1,j)) = 99
                if(nat(ibonds(2,j)) == 1) nat(ibonds(2,j)) = 99
              end if
            end do
          else
            k = 0
            l = 0
            do i = 1,3
              j = ibonds(i,i_atom)
              if (nat(j) == 8 .and. nbonds(j) == 2 .and. (nat(ibonds(1,j)) == 1 .or. nat(ibonds(2,j)) == 1)) k = k + 1
              if (nat(j) == 8 .and. nbonds(j) == 1) then
                l = l + 1
                o(l) = j
              end if
            end do
            if (k == 0 .and. l == 2) then
!
!  -COO group identified.  Now add a hydrogen
!
              call add_sp_H(o(1), j, o(2))  
            end if
          end if
        end if
      end if    
    end if
    if (neutral(3) .or. neutral(4)) then      !  Must be -NH3(+) or -NH2
      if (nat(i_atom) == 7) then
        i = 0
        l = 0
        do j = 1, nbonds(i_atom)      
          if (nat(ibonds(j,i_atom)) == 1) i = i + 1         
          do k = 1, nbonds(ibonds(j,i_atom))
            m = ibonds(k, ibonds(j,i_atom))
            if (nat(m) == 7 .and. m /= i_atom) l = 1
          end do
        end do
        if (l == 1) cycle
        if (i == 2 .and. neutral(3) .and. nbonds(i_atom) == 3) then
!
!  Nitrogen bonded to two hydrogen atoms, Add a hydrogen atom.
!
          call add_sp3_H(ibonds(1,i_atom), i_atom, ibonds(2,i_atom), ibonds(3,i_atom))
        else if (i == 3 .and. neutral(4) .and. nbonds(i_atom) == 4) then
!
!  Nitrogen bonded to two hydrogen atoms, Add a hydrogen atom.
!
          do i = 1, 4
            if (nat(ibonds(i, i_atom)) == 1) then
              j = ibonds(i, i_atom)
              nat(j) = 99
              exit
            end if
          end do
        end if
      end if
    end if
    if (neutral(7) .or. neutral(8)) then      !  Must be -His(+)- or -His-
!
!  Check for His or His(+)
!
      if (nat(i_atom) == 6 .and. nbonds(i_atom) == 3) then
        j = 0
        k = 0
        do i = 1, 3
          if (nat(ibonds(i, i_atom)) == 7 ) j = j + 1
          if (nat(ibonds(i, i_atom)) == 1 ) k = k + 1
        end do
        if (j == 2 .and. k == 1) then  ! System is C bonded to two N and one H, therefore His
          i = 0
          do j = 1,3
            k = ibonds(j,i_atom)
            do l = 1, nbonds(k)
              m = ibonds(l,k)
              if (nat(m) == 1) i = i + 1
            end do
          end do
          if (i /= 2 .and. neutral(8) .or. i /= 1 .and. neutral(7)) cycle
          if (neutral(8)) then
!
! Found a His(+), now delete a hydrogen atom
!
            outer_loop: do j = 1,3
              k = ibonds(j,i_atom)
              do l = 1, nbonds(k)
                m = ibonds(l,k)
                if (nat(m) == 1) then
                  nat(m) = 99
                  exit outer_loop
                end if
              end do
            end do outer_loop
          else
!
! Found the system -NH-C(NH2)(NH), now add a hydrogen atom
!
            do j = 1, 3
              if (nbonds(ibonds(j, i_atom)) == 2) then ! Found a N with two bonds
                l = ibonds(j, i_atom)
                call add_sp2_H(ibonds(1,l), l, ibonds(2,l))                  
                exit
              end if
            end do
          end if
        end if
      end if
    end if
  end do
  j = 0
  do i = 1, natoms
    if (nat(i) /= 99) then
      j = j + 1
      geo(:,j) = geo(:,i)
      coord(:,j) = geo(:,i)
      nat(j) = nat(i)
      atmass(j) = ams(nat(i))
      txtatm(j) = txtatm(i)
    end if
  end do
  numat = j
  natoms = numat
  labels(:numat) = nat(:numat)
end subroutine site


