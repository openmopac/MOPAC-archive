subroutine set_up_rapid ()
  use common_arrays_C, only : xparam, grad, dxyz, coord
  use molkst_C, only : escf, step_num, iflepo, moperr
  use MOZYME_C, only : mode
  implicit none
!
!  When RAPID is used, do a full SCF + derivatives.  Then
!  identify those atoms that move.  Subsequent SCFs and derivatives
!  only involve those atoms that move.
!
!  First, run a normal, single-point SCF calculation.  This fills all
!  the arrays that will be used in future SCF calculations.
!  In particular, the arrays P, H, F, and dxyz are filled.
!
  mode = 0
  call compfg (xparam, .true., escf, .true., grad, .true.)
!  From here on, any call to COMPFG will use the subset of atoms.
!
  call pinout (1)
!
!  Identify all the atoms that move.  The number of moving atoms is
!  NUMRED
!
  call picopt (1)
!
  iflepo = 1
!
!  Increment step_num: this indicates that startup in the SCF should
!  be re-done.
!
  step_num = step_num + 1
!
!  The RAPID technique uses "depleted arrays"  - normal arrays that
!  would be used in a full SCF, but with all terms that refer to the
!  moving atoms deleted.  This is done in the following steps:
! 1.  The array is multipled by -1.
! 2.  The terms arising from the moving atoms are added to the array.
! 3.  The resulting array is again multiplied by -1.
!
!  That is, each array = the normal array - terms due to the moving atoms.
!  Each "depleted array" has the same "part<x>" where <x> refers to the
!  name of the normal array.  
!
!  Second, recalculate the one-electron matrix, but this time build the array parth
!
  mode = -1
  call hcore_for_MOZYME () 
  if (moperr) return  
!  Build the depleted array part_dxyz
  call dcart(coord, dxyz)
!  Finally, set mode to +1.  The first time COMPFG is run, the following arrays will be built:
!    partp
!    partf
!
  mode = 1
end subroutine set_up_rapid
