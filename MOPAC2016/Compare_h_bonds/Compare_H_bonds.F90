Program Compare_H_bonds
  implicit none
  integer :: i, j, k, i1, j1, iw, ir, numat,ir_1, ir_2, io_stat, num_h(2), loop, n1, n2, ij(2,10000), nbonds
  character :: out_file_1*1000, out_file_2*1000, line*1000, txtatm(10000)*26
  real :: Rij, sum, ave, ase, energy(10000), del_Rij(10000), store_energy(10000)
  real, allocatable :: bonds(:,:,:)
  external :: getarg
  double precision, external :: reada
  iw = 26
  ir_1 = 24
  ir_2 = 25
  open(unit=iw, file="h_bonds.out")
  call getarg (1, out_file_1)  
  i = len_trim(out_file_1) - 3
  if (out_file_1(i:i) /= ".")out_file_1 = trim(out_file_1)//".out"
  i = len_trim(out_file_2) - 3
  if (out_file_2(i:i) /= ".")out_file_2 = trim(out_file_2)//".out"
  write(iw,'(a,a,a)')"First  output file name: '",trim(out_file_1),"'"
  call getarg (2, out_file_2)  
  write(iw,'(a,a,a)')"Second output file name: '",trim(out_file_2),"'"
  open(ir_1, file = trim(out_file_1), iostat = io_stat)
  if (io_stat /= 0) then
    write(*,'(//10x,a)')" Problem opening: '"// trim(out_file_1)//"'"
    stop
  end if
  open(ir_2, file = trim(out_file_2), iostat = io_stat)
  if (io_stat /= 0) then
    write(*,'(//10x,a)')" Problem opening: '"// trim(out_file_2)//"'"
    stop
  end if
  rewind(ir_1)
  rewind(ir_2)
  do 
    read(ir_1,'(a1000)', iostat = io_stat) line
    if (io_stat /= 0) then
      if (ir_1 == 24) write(*,'(//10x,a)')" Problem reading: '"// trim(out_file_1)//"'"
      if (ir_1 == 25) write(*,'(//10x,a)')" Problem reading: '"// trim(out_file_2)//"'"
    stop
  end if
    if (index(line, "Empirical Formula:") /= 0) exit
  end do
  numat = reada(line, index(line,"="))
  allocate (bonds(4, numat, numat))
  bonds = 0.d0
  ir = ir_1
  do loop = 1,2
    rewind (ir)
    do
      read(ir,'(a1000)', iostat = io_stat) line
      if (index(line, "Donor") > 0 .and. index(line, "Acceptor") > 0 .and. index(line, "R(D-H)") > 0) exit  
    end do
    read(ir,'(a1000)', iostat = io_stat) line
    num_h(loop) = 0
    do
      read(ir,'(a1000)', iostat = io_stat) line
      if (index(line,'"') == 0) exit
      i = nint(reada(line,16))
      j = nint(reada(line,55))
      txtatm(i) = line(10:)
      if (txtatm(i)(18:20) == "HOH") cycle
      txtatm(j) = line(49:)
      if (txtatm(j)(18:20) == "HOH") cycle
      num_h(loop) = num_h(loop) + 1
      Rij = reada(line,39)
      sum = reada(line,112)
      bonds(loop,i,j) = sum
      bonds(loop,j,i) = sum
      bonds(loop + 2,i,j) = Rij
      bonds(loop + 2,j,i) = Rij
    end do
    ir = ir_2
  end do
!
!  
!
  nbonds = 0
  do i = 1, numat
    do j = 1, i
      if (bonds(1,i,j) < -0.1d0 .and. bonds(2,i,j) < -0.1d0) then
        nbonds = nbonds + 1
        ij(1,nbonds) = i
        ij(2,nbonds) = j
        energy(nbonds) = bonds(1,i,j) - bonds(2,i,j)
        del_Rij(nbonds) = (bonds(3,i,j) - bonds(4,i,j))
      end if
    end do
  end do
  k = 0
  write(iw,'(//19x,a)') "Difference between hydrogen bond energies in the out files"
  write(iw,'(/75x,a, 4x,a)')"Hydrogen-bond Energy","Hydrogen-Bond Lengths"
  write(iw,'(5x,a,15x,a, 2x,a,2x,a, a,3x,a,3x,a,2x,a)')"     Atom in first output", "Atom in second output", &
  "       Diff.", "First", "  Second", " Delta", "First", "Second"
  write(iw,'(49x,a,4x,a,4x,a,4x,a)')
  store_energy(:numat) = energy(:numat)
  do 
    sum = 0.d0
    do i = 1, nbonds
      if (sum < abs(energy(i))) then
        sum = abs(energy(i))
        j = i
      end if
    end do
    if (sum < 0.1d0) exit
    k = k + 1
    i1 = ij(1,j)
    j1 = ij(2,j)
    write(iw,'(i4, 3x, a, 5x, a, f10.3, f8.3, f7.3,f9.3,f8.3,f8.3)')k, """"//txtatm(i1)(1:)//"""", &
    "  """//txtatm(j1)(1:)//"""", energy(j), bonds(1,i1,j1), bonds(2,i1,j1), (bonds(3,i1,j1) - bonds(4,i1,j1)), &
    bonds(3,i1,j1), bonds(4,i1,j1)
    energy(j) = 0.d0
  end do
  energy(:numat) = store_energy(:numat)
  write(iw,'(//30x,a)') "Difference between hydrogen bond lengths in the out files"
  write(iw,'(/77x,a,4x,a)')"Hydrogen-bond Lengths", "Hydrogen-bond Energy"
  write(iw,'(11x,a,15x,a, 2x,a,2x,a, a,3x,a,3x,a,2x,a)')"Atom in first output", "Atom in second output", &
  "        Delta.", "First", "  Second", " Diff", "First", "Second"
  write(iw,'(49x,a,4x,a,4x,a,4x,a)')
  k = 0
  ave = 0.d0
  ase = 0.d0
  do 
    sum = 0.d0
    do i = 1, nbonds
      if (sum < abs(del_Rij(i))) then
        sum = abs(del_Rij(i))
        j = i
      end if
    end do
    if (sum < 0.0001d0) exit
    k = k + 1
    i1 = ij(1,j)
    j1 = ij(2,j)
    energy(j) = 0.d0
    write(iw,'(i4, 3x, a, 7x, a, f13.3, f8.3, f7.3,f9.3,f8.3,f7.3)')k, """"//txtatm(i1)(1:)//"""", &
    """"//txtatm(j1)(1:)//"""", del_Rij(j), bonds(3,i1,j1), bonds(4,i1,j1), (bonds(1,i1,j1) - bonds(2,i1,j1)), &
    bonds(1,i1,j1), bonds(2,i1,j1)
    ase = ase + del_Rij(j)
    del_Rij(j) = 0.d0
    ave = ave + sum
  end do
  write(iw,'(//10x, a, f6.3, a)')"Average unsigned difference in hydrogen-bond lengths:", ave/k, "  Angstroms"
  write(iw,'(//10x, a, f6.3, a)')"Average   signed difference in hydrogen-bond lengths:", ase/k, "  Angstroms"  
  close(iw)
  continue  
end program Compare_H_bonds
  