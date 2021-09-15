subroutine mopac(license)
  use common_arrays_C, only : p, c, cb, pa, pb, geo, coord, nat, &
  nfirst, nlast, eigs, eigb, h, q, w
  use molkst_C, only : numat, mpack, norbs, keywrd, line, site_no, &
  verson, mozyme, n_screen, academic, ijulian
  use maps_C, only : latom
  use chanel_C, only : iw, iw0
  use MOZYME_C, only : icocc, icvir, ncocc, ncvir, nvirtual, noccupied, &
      & nnce, nncf, cocc, cvir, ncf, nce,  cocc_dim, &
      & cvir_dim, icocc_dim, icvir_dim
  implicit none
  integer :: i, license
  double precision, dimension (:), allocatable :: bond_orders, store, mullik_vecs
  integer , dimension(:), allocatable :: ifact 
  iw0 = 0
  latom = 0
  academic = .false.
  ijulian = 100000
  site_no = 38
  call getdatestamp(line, verson) 
  verson(7:7) = "W"
  if (license <= 0) then
    site_no = 1
  end if
  call to_screen("MOPAC started ")
  ! call dbreak()
  call run_mopac
  open(unit=24,file="keywords")
  write(24,"(a)")"("//verson//")"//keywrd(:len_trim(keywrd))
  if ( norbs > 0 .and. allocated(w)) then
    if (allocated(q))           deallocate(q)
    if (latom == 0 .and. index(keywrd," IRC") + index(keywrd, "DRC") == 0) then
        allocate(ifact(norbs + 1),  store(mpack), mullik_vecs(norbs**2))
        allocate (bond_orders(numat*(numat+1)/2), q(numat))
        do i = 1, norbs 
           ifact(i) = (i*(i - 1))/2 
        end do 
        ifact(norbs+1) = (norbs*(norbs + 1))/2 
        if (mozyme)  then
          if (allocated(c)) deallocate (c)
          allocate (c(norbs, norbs), stat=i)
          if (i /= 0) then
            call memory_error ("mopac")
            call dbreak()
            return
          end if
   !         call dbreak()
  !
  ! Convert LMOs to Eigenvectors
  !
          call lmo_to_eigenvectors(noccupied, ncf, nncf, ncocc, noccupied, &
           & icocc, icocc_dim, cocc, cocc_dim, eigs, c)
          call lmo_to_eigenvectors(nvirtual, nce, nnce, ncvir, nvirtual, icvir, &
           & icvir_dim, cvir, cvir_dim, eigs(noccupied + 1), c(1, noccupied + 1))          
! 
!  At this point, do all necessary conversions to go from MOZYME to conventional
!
!
!              Convert "h" from MOZYME form to lower-half-triangle
!
          mpack = (norbs*(norbs + 1))/2  
          allocate(pb(mpack))
          call convert_mat_packed_to_triangle(h, pb) 
          deallocate(h)        !   "h" must be deallocated because it might be smaller than mpack
          allocate(h(mpack))   !   Re-allocate "h"
          h(:mpack) = pb(:mpack) 
        
            call mullik () 
!
!              Convert "p" from MOZYME form to lower-half-triangle
!
          call convert_mat_packed_to_triangle(p, pb)
          deallocate(p)
          allocate(p(mpack))
          p(:mpack) = pb(:mpack)
         deallocate (w, store)
         allocate (w(norbs*norbs), store(mpack))         
        end if
        call cache_bonds (p, w, c,bond_orders, cb, pa, pb)
        call cache_muln (c, cb, eigs, eigb, q, &
         & h, w, norbs, mullik_vecs, geo, coord, store, &
         & nat, nfirst, nlast, ifact(1:norbs), ifact(2:),bond_orders , &
         & numat*(numat+1)/2)
        deallocate (bond_orders)
    end if
  end if
  close (iw)
!  call dbreak()
  n_screen = 8
  call to_screen("Calculation done.")       
end subroutine mopac
