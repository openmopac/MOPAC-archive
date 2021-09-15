subroutine run_mopac 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
!
      use common_arrays_C, only : nfirst, nlast, nat, xparam, grad, nw, &
      p, pa, pb, na, labels
!
      USE molkst_C, only : gnorm, natoms, numat, nvar, numcal, nscf, &
        escf, iflepo, iscf, keywrd, last, moperr, maxatoms, ncomments, &
        time0, atheat, errtxt, isok, mpack, gui, line, na1, site_no, &
        press, mozyme, step_num, jobnam, nelecs, stress, E_disp, E_hb, no_pKa, &
        MM_corrections, lxfac, in_house_only,  param_constant, trunc_1, trunc_2
!
      USE parameters_C, only : tore, ios, iop, iod, eisol, eheat, zs, &
      & eheat_sparkles
!
      use cosmo_C, only : iseps, useps, lpka, solv_energy
!
      USE funcon_C, only : fpc_9, fpc
!
      USE maps_C, only : latom, react, rxn_coord
!
      use symmetry_C, only : state_Irred_Rep
!
      USE chanel_C, only : ir, iw, iarc, output_fn, end_fn, iend, input_fn, &
        archive_fn
!
      use MOZYME_C, only : rapid, nres
!
      use meci_C, only : nmos
!
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use second_I  
      use geout_I 
      use wrttxt_I 
      use geoutg_I 
      use datin_I 
      use fbx_I 
      use fordd_I 
      use calpar_I  
      use compfg_I 
      use react1_I 
      use grid_I 
      use paths_I 
      use pathk_I 
      use force_I 
      use drc_I 
      use nllsq_I 
      use powsq_I 
      use ef_I 
      use flepo_I 
      use writmo_I 
      use polar_I 
      use pmep_I 
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i, l
      real(double) :: time00, eat,  tim
      logical :: exists, opend
      double precision, external :: C_triple_bond_C
!----------------------------------------------- 
      tore = ios + iop + iod 
      call fbx                            ! Factorials and Pascal's triangle (pure constants)
      call fordd                          ! More constants, for use by MNDO-d
      param_constant=1.0d0
      if (param_constant < -1.d5) stop
      trunc_1 = 7.0d0    ! Beyond 7.0 Angstroms, use exact point-charge  
      trunc_2 = 0.22d0   ! Multiplier in Gaussian: exp(-trunc_2*(trunc_1 - Rab)^2)
!
! Read in all data; put it into a scratch file, "ir"
!
      call getdat(ir,iw)
      call to_screen("To_file: Start of reading in data")
      if (natoms == 0 .or. moperr) return
!
!   CLOSE UNIT IW IN CASE IT WAS ALREADY PRE-ASSIGNED
!
      close(iw) 
      l = 0
 11   open(unit=iw, file=output_fn, status='UNKNOWN', position='asis', iostat = i)  
      if (i /= 0) then
        l = l + 1
        write(0,"(i3,3a)")21 - l," File """,output_fn(:len_trim(output_fn)),""" is unavailable for use"
        write(0,"(a)")"    Correct fault (probably the output file is in use elsewhere)"
        call sleep(5)
        if (l < 20) goto 11
        write(0,"(a)") "Job abandoned due to output file being unavailable"
        call mopend("Job abandoned due to output file being unavailable")
        call sleep(5)
        goto 101
      end if
      if (l > 0) then
       write(0,"(a)")" Fault successfully corrected.  Job continuing"
      end if
      rewind iw  
      isok = .TRUE. 
      errtxt = 'Job stopped by operator' 
      time00 = second(1)
      if (moperr) goto 101    
!
! Set up essential arrays, these are the arrays that are needed for reading in the data
!
      natoms = natoms + 200
      call setup_mopac_arrays(natoms, 1)
      maxatoms = natoms
      inquire (file ="S:\Survey_Of_Solids\bits.txt", exist = in_house_only)
      if (.not. in_house_only) inquire (file ="/Users/jstewart/fa3207.cif", exist = in_house_only)
   10 continue 
      numcal = numcal + 1      ! A new calculation
      step_num = step_num + 1  ! New electronic structure, therefore increment step_num
      moperr = .FALSE. 
      gnorm  = 0.D0 
      press  = 0.d0
      E_disp = 0.d0
      E_hb   = 0.d0
      solv_energy = 0.d0
      nres = 0
      nscf = 0
      nmos = 0
      na1 = 0
      lpka = .false.
      stress = 0.d0
      no_pKa = 0
      time0 = second(1)
      MM_corrections = .false.
      state_Irred_Rep = " " 
      if (numcal > 1) call to_screen("To_file: Leaving MOPAC")
!
!    Read in all the data for the current job
!
      call readmo 
      if (.not. gui .and. numcal == 1 .and. natoms == 0) then
        write(line,'(2a)')" Data set exists, but does not contain any atoms."
        write(0,'(//10x,a,//)')trim(line)
        call mopend(trim(line))
        write(0,'(5x,a)')" (Check the first few lines of the data-set for an extra blank line."
        write(0,'(5x,a)')"  If there is an extra line, delete it and re-submit.)"
        stop
      end if
      if (numcal == 1 .and. moperr .or. natoms == 0) then
!
!   Check for spurious "extra" data
!
        do i = 1,15
          read(ir,*, iostat=l)line
          if (l /= 0) exit
        end do
        if (i > 14) then
          write(iw,"(//10x,a)") " WARNING: There are extra data at the end of the input data set." 
          write(iw,"(10x,a,/)")"          There might be an error in the data set." 
        end if
        goto 101
      end if
      if (site_no == -1) then
        l = 0
        do i = 1, natoms
          if (labels(i) < 99) l = l + 1
        end do
        select case (l)
          case(1:12, 50:60, 110:120)
          case default
            write(iw,'(//10x,a)')" The demo version of MOPAC2009 is limited to systems with 1-12, 50-60, or 110-120 atoms"
            call web_message(iw,"public.html")
            if (.not. gui) write(*,'(//10x,a)')" Demo version limited to systems with 1-12, 50-60, or 110-120 atoms."
            if (.not. gui) write(*,'(10x,a)')" For more information, read the output file from this job."
            call mopend(" Demo version limited to systems with 1-12, 50-60, or 110-120 atoms")
            write(line,'(a,i5,a)')" The system supplied has",l," atoms"
            call mopend(trim(line))
            if (.not. gui) call sleep(5) 
            return
        end select
      end if
      if (moperr) go to 10 
      call to_screen(" Keywords used: "//keywrd(:len_trim(keywrd)))
       lxfac = (index(keywrd," XFAC") /= 0)
      
!
! Load in parameters for the method to be used
!
      call switch 
      if (index(keywrd,' EXTERNAL') /= 0) call datin (iw) 
      if (moperr) go to 100 
      do i = 57,71
        if (zs(i) < 0.1d0) tore(i) = 3.d0
      end do
!
! Set up all the data for the molecule
!           
      call moldat (0)  ! data dependent on the system
      call calpar      ! Calculate derived parameters   
       
  
      if (moperr) goto 100
      call to_screen("To_file: Data read in")
!
!  If no SCF calculations are needed, output geometry and quit
!      
      if (index(keywrd,' 0SCF') + index(keywrd, " RESEQ") /= 0 ) then 
        inquire(unit=iarc, opened=opend) 
        if (opend) close (iarc)
        open(unit=iarc, file=archive_fn, status='UNKNOWN', position='asis') 
        rewind iarc 
        if (index(keywrd,' XYZ') /= 0) then 
          line = ' GEOMETRY IN CARTESIAN COORDINATES' 
        else if (index(keywrd,' INT') /= 0) then 
          line = ' GEOMETRY IN MOPAC Z-MATRIX FORMAT' 
        else 
          line = ' GEOMETRY OF SYSTEM SUPPLIED' 
        end if
        write (iw, '(A)') line(:len_trim(line))
        xparam(1) = -1.D0 
        if (index(keywrd," OLDEN") /= 0) then
!
! read in density so that charges can be calculated
!
          if (mozyme) then
            call set_up_MOZYME_arrays()  
          else
            if (allocated(p))      deallocate (p)
            if (allocated(pa))     deallocate (pa)
            if (allocated(pb))     deallocate (pb)
            allocate(p(mpack), pa(mpack), pb(mpack))
          end if      
          call den_in_out(0)
          if (moperr) return
          if (mozyme) call density_for_MOZYME (p, 0, nelecs/2, pa)
        end if
        call geout (iw) 
        if (index(keywrd,' AIGOUT') /= 0) then 
          write (iw, '(2/,A)') '  GEOMETRY IN GAUSSIAN Z-MATRIX FORMAT' 
          call wrttxt (iw) 
          call geoutg (iw) 
          write (iarc, '(2/,A)') '  GEOMETRY IN GAUSSIAN Z-MATRIX FORMAT' 
          call wrttxt (iarc) 
          call geoutg (iarc) 
        else if (mozyme .or. (index(keywrd," PDBOUT") + index(keywrd," RESEQ") /= 0)) then
          if (index(keywrd," NORES") == 0) call geochk()
          if (index(keywrd," RESEQ") /= 0) then
            moperr = .false.
            goto 10
          end if
          if (moperr) goto 101
          if ( index(keywrd," PDBOUT") /= 0) then
            call pdbout(iarc)
          else
            call geout (iarc)
          end if
        else
         call geout (iarc) 
        end if
        go to 100 
      endif 
!
!  If any special work is done, do it here
!
  !    call special
   !   go to 10
!
! Everything is ready - now set up the arrays used by the SCF, etc.
!    
      useps = .FALSE.     
      iseps = (index(keywrd,' EPS=') + index(keywrd," PKA") /= 0) 
      call setup_mopac_arrays(1,2)
      iseps = (index(keywrd,' EPS=') /= 0) 
      if (moperr) goto 100     
      if (allocated(nw)) deallocate(nw)
      allocate(nw(numat))
      l = 1 
      do i = 1, numat 
        nw(i) = l 
        l = l + ((nlast(i)-nfirst(i)+1)*(nlast(i)-nfirst(i)+2))/2 
      end do 
!
!  CALCULATE THE ATOMIC ENERGY
!
      eat = 0.D0 
      if (index(keywrd, " SPARKL") /= 0) then
        atheat = 0.d0
        do i = 1, numat
          if  (nat(i) > 56 .and. nat(i) < 72 .and. zs(nat(i)) < 0.1d0) then
            atheat = atheat + eheat_sparkles(nat(i))
          else
            atheat = atheat + eheat(nat(i))
          end if
        end do
      else
        atheat = sum(eheat(nat(:numat))) 
      end if
      eat = sum(eisol(nat(:numat))) 
      atheat = atheat - eat*fpc_9 
      atheat = atheat + C_triple_bond_C()
      rxn_coord = 1.d9
!
!  All data for the current job are now read in, and all parameters are
!  available in the arrays. 
!  Now decide what type of calculation is to be done.
!
      if (mozyme) then
        call geochk()
        if (moperr) goto 10  
        rapid = (index(keywrd, " RAPID") /= 0)
        call set_up_MOZYME_arrays()     
        if (moperr) goto 101    
        call picopt(-1)       
        if (rapid) call set_up_rapid()
      end if
      if (index(keywrd,' SADDLE') /= 0) then
        call to_screen(" Transition state geometry calculated using SADDLE")
        call react1 () 
      else if (index(keywrd,' STEP1') /= 0) then 
        call to_screen(" Grid calculation")
        call grid () 
        iflepo = -1  !  Prevent printing of results
      else if (latom /= 0) then
        call to_screen(" Path calculation")
        if (index(keywrd,' STEP')==0 .or. index(keywrd,' POINT')==0) then 
          call paths () 
        else 
          call pathk () 
        end if
        iflepo = -1  
      else if (index(keywrd,' FORCE') + index(keywrd,' IRC=') + &
        index(keywrd,' THERM') + index(keywrd,' DFORCE') /= 0) then
        call to_screen(" Force constant calculation")
        last = 1
        call force ()
        iflepo = -1  
      else if (index(keywrd,' DRC') + index(keywrd,' IRC') /= 0) then 
        call to_screen(" Reaction coordinate calculation")
        na = 0
        if (.not. allocated(react)) then
          allocate(react(3*numat))
          react = 0.d0
        end if
        call drc (react, react)
        iflepo = -1  
      else if (index(keywrd,' NLLSQ') /= 0) then
        write(iw,'(5x,a)')" Transition state refinement using NLLSQ"
        call to_screen(" Transition state refinement using NLLSQ")
        call nllsq () 
      else if (index(keywrd,' SIGMA') /= 0) then 
        write(iw,'(5x,a)')" Transition state refinement using SIGMA"
        call to_screen(" Transition state refinement using SIGMA")
        call powsq ()       
      elseif (index(keywrd,' 1SCF') /= 0 .or. nvar == 0) then 
        iflepo = 1 
        iscf = 1 
        last = 1 
        i = index(keywrd,' GRAD') 
        grad(:nvar) = 0.D0 
        numcal = numcal + 1
        call to_screen(" Single point calculation")
        time00 = second(2)
        call compfg (xparam, .TRUE., escf, .TRUE., grad, i /= 0) 
      else if (index(keywrd,' DFP') + index(keywrd,' FLEPO') + &
        index(keywrd,' BFGS')/=0 .or. nvar == 1) then
        write(iw,'(5x,a)')" Geometry optimization using BFGS"
        call to_screen(" Geometry optimization using BFGS")
        call flepo (xparam, nvar, escf) 
      else if (index(keywrd,' TS')/=0) then
        write(iw,'(5x,a)')" Transition state refinement using EF"
        call to_screen(" Transition state refinement using EF")
        call ef (xparam, escf) 
      else if (index(keywrd,' LBFGS') + index(keywrd, " GEO_REF") /=0 .or. &
        nvar > 2000 .and. index(keywrd,' EF') ==0) then
        write(iw,'(5x,a)')" Geometry optimization using L-BFGS"
        call to_screen(" Geometry optimization using L-BFGS")
        call lbfgs (xparam, escf) 
      else
        write(iw,'(5x,a)')" Geometry optimization using EF"
        call to_screen(" Geometry optimization using EF")
        call ef (xparam, escf) 
      end if
!
!  Calculation done, now print results
!
      if (moperr) go to 100 
      last = 1 
      if (iflepo >= 0) then 
        call writmo
        if (moperr) go to 100 
        if (index(keywrd,' POLAR') /= 0) then 
          call polar () 
          if (moperr) go to 100 
        endif 
        if (index(keywrd,' STATIC') /= 0) then 
          numcal = numcal + 1 !  In case POLAR was also used
          call static_polarizability 
          if (moperr) go to 100 
        endif 
        if (index(keywrd,'PMEP') /= 0) call pmep () 
        if (moperr) go to 100 
        if (index(keywrd,' ESP') /= 0) then 
          call esp () 
          if (moperr) go to 100 
        endif 
      endif 
  100 continue 
      tim = second(2) - time00
      if (tim > 1.d7) tim = tim - 1.d7 
      write (iw, '(3/,'' TOTAL CPU TIME: '',F16.2,'' SECONDS'')') tim 
      write (iw, '(/,'' == MOPAC DONE =='')') 
      if ( .not. gui) then
        if (allocated(p)) deallocate(p)
        if (allocated(react)) deallocate(react)
        inquire (file = end_fn, exist = exists)
        if (exists) then
          open(unit=iend, file=end_fn, status='UNKNOWN', position='asis', iostat=i) 
          close(iend, status = 'delete', iostat=i) 
        end if
        ncomments = 0
        go to 10 
      end if
!
! Carefully delete all arrays created using "allocate"
!
  101 if ( .not. gui) then
        call setup_mopac_arrays(0,0) 
        call delete_MOZYME_arrays()
      end if
!
!  Delete files that are definitely not wanted
!
      inquire (file = end_fn, exist = exists)
      if (exists) then
        open(unit=iend, file=end_fn, status='UNKNOWN', position='asis', iostat=i) 
        close(iend, status = 'delete', iostat=i) 
      end if
      inquire (file = input_fn, exist = exists)
      if (exists) then
        open(unit=ir, file=input_fn, status='UNKNOWN', position='asis', iostat=i) 
        close(ir, status = 'delete', iostat=i)    
      end if
      jobnam = " "
      return
end subroutine run_mopac
subroutine special
!
!  Use this subroutine for any special work.
!  for example, to print a MOPAC data-set.
!
  use molkst_C, only : jobnam, refkey, line
  use upcase_I
  implicit none
  integer :: iprt = 33, i, j, k, len_key
  open(unit=iprt, file=jobnam(:len_trim(jobnam) - 0)//"_(PM6).arc", status='UNKNOWN', position='asis', iostat = i) 
  do i = 1, 6
    if (index(refkey(i), " NULL") /= 0) exit
    line = refkey(i)
    len_key = len_trim(refkey(i))
    call upcase(line, len_key)
!
!  Put all changes in keywords here
!
    j = index(line, " 1SCF") 
    if (j /= 0) refkey(i)(j:j + 4) = " "
    j = index(line, " PM6") 
    if (j /= 0) refkey(i)(j:j + 3) = " "
    j = index(line, " DENOUT") 
    if (j /= 0) refkey(i)(j:j + 7) = " "
    j = index(line, " GRADIENTS") 
    if (j /= 0) refkey(i)(j:j + 9) = " "
    j = index(line, " GNORM=") 
    if (j == 0) then
      j = index(line, "        ")
      refkey(i)(j:j+8) = " GNORM=4"
    end if
!
!  Remove all extra blank spaces
!
    len_key = len_trim(refkey(i))
    refkey(i)(len_key + 1:len_key + 1) = "@"
    do j = 1, len_key
      do k = 1,10
        if (refkey(i)(j:j + 1) == "  ") refkey(i)(j:) = refkey(i)(j + 1:)
      end do
    end do
    j = index(refkey(i), "@")
    refkey(i)(j:) = " "
    j = index(refkey(i),"     ")
    refkey(i)(j:) = " PM6"
  end do
  write(iprt,"(a)")"  MOPAC2009"
  write(iprt,"(a)")" FINAL GEOMETRY OBTAINED"
  call geout (iprt) 
end subroutine special

 