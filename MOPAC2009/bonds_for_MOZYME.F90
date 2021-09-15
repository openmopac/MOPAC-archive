   subroutine bonds_for_MOZYME ()
    use molkst_C, only: numat, maxtxt, keywrd, nl_atoms
    use chanel_C, only: iw
    use common_arrays_C, only : p, nat, txtatm, l_atom
    use MOZYME_C, only : iorbs
    use elemts_C, only: elemnt
    implicit none
    character (len=60) :: blank
    logical :: lall, mozyme_style
    integer :: i, io, j, jo, ju, k, kk, kl, ku, l, m, mtx, newl, nper, j_lim
    double precision :: sum, sumlim, valenc
    integer, allocatable, dimension (:) :: ibab
    logical, allocatable, dimension (:) :: l_atom_store
    double precision, allocatable, dimension (:) :: bab
    intrinsic Index, Min, Mod
    integer, external :: ijbo
  !  
    lall = (Index (keywrd, " ALLBOND") /= 0)
    sumlim = 0.01d0                  !  Report all bonds greater than 0.01 not involving hydrogen
    if (lall) sumlim = sumlim*0.1d0  !  Report all bonds greater than 0.001   
    blank = " "
    if (maxtxt /= 0) then
      mtx = maxtxt + 23
      nper = 3
    else
      mtx = 21
      nper = 4
    end if
    mozyme_style = .true.
    if (index(keywrd, " IRC") + index(keywrd, " DRC") + index(keywrd, " MINI") /= 0) mozyme_style = .false.    
    if (mozyme_style) then
      write (iw, "(/25X,A,/)") " BOND ORDERS BETWEEN ATOMS (VALENCIES)"
      allocate (bab(100), ibab(100))
    else
      write (iw, '(1X,2/20X,''BOND ORDERS AND VALENCIES'')') 
      sumlim = -1.d0
      i = (nl_atoms*(nl_atoms + 1))/2
      allocate (bab(i), l_atom_store(nl_atoms))
    end if
  !  call dbreak()
    newl = 0
    l = 0
    lall = .true.
    do i = 1, numat
    if ( .not. mozyme_style .and.  .not. l_atom(i)) cycle
      newl = newl + 1
      valenc = 0.d0
      io = iorbs(i)
      if (mozyme_style) then
          j_lim = numat
          l = 0
        else
          j_lim = i - 1
        end if
      if (nat(i) /= 1 .or. lall) then
        if (io == 1) then
          kk = ijbo (i, i) + 1
          valenc = 2.d0 * p(kk) - p(kk) ** 2
        else
          kk = ijbo (i, i)
          do j = 1, 4
            do k = 1, j
              kk = kk + 1
              valenc = valenc - p(kk) * p(kk)
            end do
            valenc = valenc + 2.d0 * p(kk)
          end do
        end if
        do j = 1, j_lim 
          if ( .not. mozyme_style .and.  .not. l_atom(j)) cycle
          if (nat(j) /= 1 .or. lall) then
            jo = iorbs(j)
            if (i /= j .and. ijbo (i, j) >= 0) then
              kl = ijbo (i, j) + 1
              ku = kl + io * jo - 1
              sum = 0.d0
              do k = kl, ku
                sum = sum + p(k) ** 2
              end do
              if (sum > sumlim) then
                l = l + 1
                bab(l) = sum
                if (mozyme_style) ibab(l) = j
              end if
            else
              if ( .not. mozyme_style) then
                l = l + 1
                bab(l) = 0.d0
              end if
            end if
          end if
        end do
        if (mozyme_style) then         
          if (l /= 0) then
            do j = 1, l
              sum = 0.d0
              do k = j, l
                if (bab(k) > sum) then
                  m = k
                  sum = bab(k) * (1.d0+1.d-4)
                end if
              end do
              k = ibab(j)
              ibab(j) = ibab(m)
              ibab(m) = k
              sum = bab(j)
              bab(j) = bab(m)
              bab(m) = sum
            end do
            ju = Min (nper, l)
            
            if (maxtxt /= 0) then
              write (iw, "(I6,1X,A,A,F5.3,A1,6(I6,A2,F6.3))") i, elemnt (nat(i)) &
             & // " " // txtatm (i) (:maxtxt) // " ", "     (", valenc, ")", &
             & (ibab(j), elemnt(nat(ibab(j))), bab(j), j=1, ju)
            else
              write (iw, "(I6,1X,A,A,F5.3,')',6(I6,1x,A2,F6.3))") i, elemnt(nat(i)), &
              "     (", valenc, (ibab(j), elemnt(nat(ibab(j))), &
             & bab(j), j=1, ju)
            end if
            do m = nper + 1, l, nper
              ju = Min (m+nper-1, l)
              write (iw, "(A,6(I6,A2,F6.3))") blank (1:mtx), (ibab(j), &
             & elemnt(nat(ibab(j))), bab(j), j=m, ju)
            end do
            newl = newl + 1
            if (Mod(newl, 5) == 0) then
              write (iw,*)
            end if
          end if
        else
          l = l + 1
          bab(l) = valenc
        end if
      end if
    end do
    if ( .not. mozyme_style) then
      l_atom_store = l_atom(:nl_atoms)
      l_atom(:nl_atoms) = .true.
      call vecprt (bab, newl) 
      l_atom(:nl_atoms) = l_atom_store
      deallocate (l_atom_store, bab)
    end if
end subroutine bonds_for_MOZYME

