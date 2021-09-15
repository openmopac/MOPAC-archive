  Program Make_MOPAC_Makefile
  implicit none
  integer :: i, ii, j, ir = 5, nfiles, ndep(1000), nd, iw = 6, it = 7
  character :: dependencies(1000,60)*30, files(1000)*30, line*200, blank*80 = " "
  open(unit=ir,file="mopac.dsp")
  open(unit=iw,file="Makefile.txt")
  do 
  read(ir,'(a)')line
  if (index(line, "Begin Source File") /= 0) exit
  end do
!
!  Read in all files plus their dependencies
!
  nfiles = 0
  do
    read(ir,'(a)', iostat = i)line
    if (i /= 0) exit
    if (index(line, "SOURCE") == 0) cycle
    i = index(line, ".F90")
    do j = i, 1, -1
      if (line(j:j) == "\") exit
    end do
    nfiles = nfiles + 1
    files(nfiles) = line(j + 1: i - 1)  
    nd = 0  
    do
      read(ir,'(a)', iostat = i)line
      if (line == " ") exit
      if (i /= 0) exit
      i = index(line,".mod")
      if (i == 0) cycle
      do j = i, 1, -1
        if (line(j:j) == "\") exit
      end do
      nd = nd + 1
      dependencies(nfiles, nd) = line(j + 1: i - 1)  
    end do
    ndep(nfiles) = nd 
  end do
  write(iw,'(a)')"MOPAC_OBJECTS = \"
  do i = 1, nfiles - 2, 3
    j = min(i + 2, nfiles - 2)
    write(iw,'(6a)')("$(OBJ)/"//trim(files(nd))//".$(O)"//blank(:30 - len_trim(files(nd))), nd = i, j)," \"
  end do
  j = min(i + 2, nfiles)
  write(iw,'(6a)')("$(OBJ)/"//trim(files(nd))//".$(O)"//blank(:30 - len_trim(files(nd))), nd = i, j)
!
!  Write out objects for PARAM
!
  open(unit = it, file = "PARAM_objects.txt")
  do
    read(it,'(a)', iostat = ii) line
    if (ii /= 0) exit
    write(iw,'(a)')trim(line)
  end do  
  close(it)
!
!  Write out targets for MOPAC
!
  open(unit = it, file = "Targets.txt")
  do
    read(it,'(a)', iostat = ii) line
    if (ii /= 0) exit
    write(iw,'(a)')trim(line)
  end do 
  close(it)
  do i = 1, nfiles
    write(iw,'(6a)')"$(OBJ)/"//trim(files(i))//".$(O): \"
    do j = 1, ndep(i)
      write(iw,'(6a)')"  $(OBJ)/"//trim(dependencies(i, j))//".$(O)  \"      
    end do
    write(iw,'(6a)')"  $(MOPAC_SRC)/"//trim(files(i))//".F90"
    write(iw,'(6a)')char(9)//" $(FORTRAN_COMPILER)  $(MOPAC_SRC)/"//trim(files(i))//".F90"
    write(iw,*) " "
  end do
!
!  Write out dependencies for param
!
  open(unit = it, file = "PARAM_Dependencies.txt")
  do
    read(it,'(a)', iostat = ii) line
    if (ii /= 0) exit
    write(iw,'(a)')trim(line)
  end do 
  close(it)
  end program make_MOPAC_Makefile