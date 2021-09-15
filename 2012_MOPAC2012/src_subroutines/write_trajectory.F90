subroutine write_trajectory(xyz, mode, charge, escf, ekin, time, xtot)
  use molkst_C, only : step_num, numat, jloop => itemp_1, line, nl_atoms
  use chanel_C, only : ixyz, xyz_fn
  USE elemts_C, only : elemnt 
  use common_arrays_C, only : nat, l_atom
  implicit none
  double precision, optional :: escf, ekin, time, xtot
  double precision, intent (in) :: xyz(3,numat)
  double precision, optional :: charge(numat)
  integer, intent (in) :: mode
  integer :: icalcn = 0, i, k, j, io_stat
  character :: dummy_char*1
  character, dimension (:,:), allocatable :: store_path*48
  save :: icalcn
  if (icalcn /= step_num) then
    open(unit=ixyz, file=xyz_fn)
    icalcn = step_num
  end if
  select case (mode)  
  case (1)  !  write out a trajectory
!
!  Write out "xyz" file
!
    write(ixyz,"(i6,a)") nl_atoms," "
    write(ixyz,*)"DRC "
    do i = 1, numat
      if (l_atom(i)) write(ixyz,"(3x,a2,3f15.5)")elemnt(nat(i)), (xyz(j,i),j=1,3)
    end do
    if (mod(jloop,10) == 0) then  ! write out every 10'th iteration.
      if (time > 1.d-6) then
        write (line, "(a7,i6,a8,f17.5,a8,f9.4,a7,f9.4,a7,f9.3)") " CYCLE:", jloop, &
         & "  Pot.E:", escf, "  Kin.E:", ekin, "  Move:", xtot,"  Time:", Min(time,99999.999d0)
       else    
          write (line, "(a7,i6,a19,f17.5,a8,f9.4,a7,f9.4,a7,f9.3)") " CYCLE:", jloop, &
     & "  Potential energy:", escf, " Diff.:", ekin, "  Move:", xtot
      end if
      call to_screen(line)
    endif
    if (charge(1) > -100.d0) return  
    case (2) !  Reverse the path
    close (ixyz)
    open(ixyz, file = xyz_fn)
    rewind(ixyz)
    allocate (store_path(nl_atoms, jloop))
!
!  Read in the path already generated
!
    do i = 1, jloop
      read(ixyz,*, iostat=io_stat)dummy_char
      if (dummy_char =="0" .and. i < 0) return ! dummy use of dummy_char
      read(ixyz,*, iostat=io_stat)dummy_char
      if (dummy_char =="0" .and. i < 0) return ! dummy use of dummy_char
      read(ixyz,"(a)", iostat=io_stat) &
        (store_path(k,i),k=1,nl_atoms)
      if (io_stat /= 0) exit
    end do
    close (ixyz)
    open(ixyz, file = xyz_fn)
    rewind(ixyz)
!
! Write out the path already generated, in reverse
!
     jloop = i - 1
     do i = jloop, 1, -1
       write(ixyz,"(i6,a)") nl_atoms," "
       write(ixyz,*)" reversed "
       do k = 1, nl_atoms
          write(ixyz,"(a)") trim(store_path(k,i))
       end do
     end do        
   end select
   deallocate (store_path)
   
 end subroutine write_trajectory

