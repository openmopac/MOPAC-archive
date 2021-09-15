      subroutine wrttxt(iprt) 
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:05:05  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      use molkst_C, only : koment, title, refkey 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iprt 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i 
!-----------------------------------------------
      do i = 1, 6
        if (index(refkey(i), " NULL") /= 0) exit
        write(iprt,'(a)')refkey(i)(:len_trim(refkey(i)))
      end do
      if (index(koment,' NULL ') == 0) write (iprt, '(A)') koment(:len_trim(koment))  
      if (index(title,' NULL ') == 0) write (iprt, '(A)') title(:len_trim(title)) 
      return  
      end subroutine wrttxt 
