subroutine pdbout (mode1)
    use molkst_C, only: numat, ncomments, keywrd
    use chanel_C, only: iw
    use elemts_C, only: elemnt
    use common_arrays_C, only: txtatm, coord, nat, all_comments
    implicit none
    integer, intent (in) :: mode1
!
    character :: letter, big
    character :: ele_pdb*2, blank*4 = "    ", chains(100)*2
    integer :: i, iprt, j, k, nline, ibig, chain
    logical :: ter, big_number, nores
    intrinsic Abs, Char, Ichar
!
    nline = 0
    if (mode1 == 1) then
      iprt = iw
    else
      iprt = Abs (mode1)
    end if   
    if (ncomments > 0) then
      if (index(all_comments(1),"HEADER") == 0) &
        write (iprt, "(A)") "HEADER"
      write(iprt,"(a)")(all_comments(i)(2:len_trim(all_comments(i))), i = 1, ncomments)
    else
       write (iprt, "(A)") "HEADER"
    end if
    nores = (index(keywrd," NORES") /= 0)
    i = index(keywrd," CHAINS")  
    if (i /= 0) then
      i = i + index(keywrd(i:),"(")
      do j = i, i + 26
        k = ichar(keywrd(j:j))
        if (k < ichar("A") .or. k > ichar("Z")) exit
        chains(j - i + 1) = " "//keywrd(j:j)
      end do
    else
!
! Allow for up to 100 chains
!
      do i = 1, 26
        chains(i) = " "//char(ichar("A") + i - 1)
      end do
      do i = 27, 52
        chains(i) = "A"//char(ichar("A") + i - 27)
      end do
      do i = 53, 78
        chains(i) = "B"//char(ichar("A") + i - 53)
      end do
      do i = 79, 100
        chains(i) = "C"//char(ichar("A") + i - 79)
      end do
    end if
    big = " "
    ibig = 0
    chain = 1
    big_number = .false.
    do i = 1, numat
      if (i > 1 .and. big_number) then
        if (txtatm(i)(12:12) > "0" .and. txtatm(i)(12:12) < "9") then
          big = txtatm(i)(12:12)
        else
        if (txtatm(i)(13:15) == "  0" .and. txtatm(i - 1)(13:15) /= "000") then
          ibig = ibig +1
          big = char(ibig + ichar("0"))          
        end if
        if (ibig > 0) then
          do j = 13, 14
            if (txtatm(i)(j:j) == " ") txtatm(i)(j:j) = "0"
          end do
        end if
        end if
      end if
      big_number  = ( i > 200)
      nline = nline + 1
      letter = txtatm(i) (19:19)
      ter = (txtatm(i) (8:8) == "+")
      if (elemnt(nat(i)) (1:1) == " ") then
        ele_pdb(1:1) = " "
        ele_pdb(2:2) = elemnt(nat(i)) (2:2)
        j = 2
      else
        j = 1
        ele_pdb(1:1) = elemnt(nat(i)) (1:1)
        ele_pdb (2:2) = Char (Ichar ("A")-Ichar ("a")+Ichar (elemnt(nat(i)) (2:2)))
      end if
      if (letter == "J") then
        if (nores) then
          write (iprt, "(A,F12.3,F8.3,F8.3,A,a2)") "HETATM"//txtatm(i)(22:41), &
          & (coord(k, i), k=1, 3), "  1.00  0.00      PROT", ele_pdb
        else
          write (iprt, "(A,I5,A,A3,A,F11.3,F8.3,F8.3,A,a2)") "HETATM", i, blank &
         & (1:j) // ele_pdb (j:2) // "   ", txtatm (i) (9:11), "  " // txtatm (i) &
         & (12:15)//" ", (coord(k, i), k=1, 3), "  1.00  0.00      PROT", ele_pdb
       end if
      else
        if (letter == "V") then
          letter = " "
        end if
        if (nores) then
          write (iprt, "(A,F12.3,F8.3,F8.3,A,a2)") "ATOM"//txtatm(i)(20:41), &
        & (coord(k, i), k=1, 3), "  1.00  0.00      PROT", ele_pdb
        else                
          if (j == 1) then
            write (iprt, "(A,I5,A,A3,A,F11.3,F8.3,F8.3,A,a2)") "ATOM  ", i, &
        & txtatm(i) (17:17)//ele_pdb(1:2)//letter // txtatm(i)(18:18)//" ", &
        & txtatm (i) (9:11), &
        & chains(chain)//big//txtatm(i)(13:15)//" ", &
        & (coord(k, i), k=1, 3), "  1.00  0.00      PROT", ele_pdb
          else
            write (iprt, "(A,I5,A,A3,A,F11.3,F8.3,F8.3,A,a2)") "ATOM  ", i, &
        & " "//txtatm(i) (17:17)//ele_pdb(2:2)//letter // txtatm(i)(18:18)//" ", &
        & txtatm (i) (9:11),  &
        & chains(chain)//big//txtatm(i)(13:15)//" ", &
        & (coord(k, i), k=1, 3), "  1.00  0.00      PROT", ele_pdb
          end if 
        end if        
      end if
      if (ter) then
        write(iprt, "(a)")"TER"  !  Write out the TERMINAL marker
        chain = chain + 1
      end if
    end do
end subroutine pdbout
