subroutine write_trajectory(xyz, mode, charge, escf, ekin, time, xtot)
  use molkst_C, only : numcal, numat, jloop => itemp_1, line
  USE vast_kind_param, ONLY:  double 
  use chanel_C, only : ixyz, xyz_fn
  USE elemts_C, only : elemnt 
  use common_arrays_C, only : nat
  implicit none
  double precision, intent (in) :: escf, ekin, time, xtot
  double precision, intent (in) :: xyz(3,numat), charge(numat)
  integer, intent (in) :: mode
  integer :: icalcn = 0, i, k, j, ii, io_stat, this_point = 0, size_of_block
  character :: dummy_char*1
  real(double), dimension (:,:,:), allocatable :: store_path
  real(double), dimension (:,:), allocatable :: store_coord
  character (len=80), dimension(:), allocatable :: all_text
  if (icalcn /= numcal) then
    open(unit=ixyz, file=xyz_fn)
    icalcn = numcal
  end if
  select case (mode)  
  case (1)  !  write out a trajectory
    this_point = this_point + 1
    call ntm_wrtmap(xyz, charge, this_point, escf, ekin, escf+ekin, time, xtot)
    if (mod(this_point,10) == 0) then  ! write out every 10'th iteration.
      if (time > 1.d-6) then
        write (line, "(a7,i6,a8,f14.4,a8,f9.4,a7,f9.4,a7,f9.3)") " CYCLE:", this_point, &
         & "  Pot.E:", escf, "  Kin.E:", ekin, "  Move:", xtot,"  Time:", Min(time,99999.999d0)
       else    
          write (line, "(a7,i6,a19,f14.4,a8,f9.4,a7,f9.4,a7,f9.3)") " CYCLE:", this_point, &
     & "  Potential energy:", escf, " Diff.:", ekin, "  Move:", xtot
      end if
      call to_screen(line)
    endif
  case (2) !  Reverse the path
    call to_screen("Reversing first half of IRC curve")
    open (unit = ixyz, file = 'xyzmap', status = 'unknown')
    do size_of_block = 1,10000
      read(ixyz,"(a80)", iostat=j) line
      if (j < 0) return
      if (index(line,"EndOfBlock") /= 0) exit
    end do
    allocate (all_text(size_of_block*jloop))
    rewind (ixyz)
    do i = 1, size_of_block*this_point
      read (ixyz,"(a80), iostat = ii") all_text(i)
      if (ii < 0) exit
    end do
    if (ii >= 0) then
      rewind (ixyz)
      do ii = this_point - 1, 0, -1
        i = ii*size_of_block
        do j = 1, size_of_block
          write (ixyz,"(a)") Trim(all_text(i + j))
        end do
      end do
    end if
    deallocate (all_text)
    call to_screen("Second half of IRC curve")
    this_point = 0
   end select    
 end subroutine write_trajectory

