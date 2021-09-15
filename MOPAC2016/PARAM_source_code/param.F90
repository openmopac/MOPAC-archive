program  param
    use param_global_C, only : numvar, large, power, ifiles_8, &
    & contrl, fnsnew, nfns, xparamp, diffns, fns, penalty, &
    save_parameters
!
    use molkst_C, only : tdump, maxatoms, is_PARAM, numat, jobnam, &
     moperr, method_mndo, method_am1, method_pm3, method_mndod, &
     koment, norbs, mpack, nvar, n2elec, keywrd, uhf, l123, run, &
     method_pm6, lm61, program_name, gui, method_rm1, tleft, num_threads, &
     method_pm6_dh_plus, method_pm6_dh2, method_pm7, param_constant, trunc_1, trunc_2
    use cosmo_C, only : iseps, nspa
!
    use common_arrays_C, only:  atmass, na, nb, nc, geoa, p, nw
!
    use chanel_C, only : ir, iw, ilog, job_fn, input, iarc, ifiles_1
    use funcon_C, only : fpc_1, fpc_2, a0, ev, fpc_6, fpc_7, fpc_8, &
    & fpc_9, fpc_10, fpc_5
    use conref_C, only : fpcref 
    use meci_C, only : nmos
    use parameters_C, only: f0sd, g2sd, tore, ios, iop, iod, f0sd_store, g2sd_store, &
      uss, upp, zs
    USE journal_references_C, only : allref
    implicit none
    logical :: opend, exists
    integer :: i, j, l, k, loop, maxcyc, n9, n4, n1
    double precision :: sum
    character (len=120) :: text
    character (len=24) :: idate
  !
  !
  !.. External Calls ..
    external finish, getdat, fdate
  !
  !.. External Functions ..
    double precision, external :: Reada
    logical, external :: date_okay
  !
  !.. Intrinsic Functions ..
    intrinsic Index
    integer, external :: mkl_get_max_threads
  !
  ! ... Executable Statements ...
  !
  !
    num_threads = mkl_get_max_threads()
    call mkl_set_num_threads(num_threads)
    program_name = "Standalone MOPAC "
    if ( .not. date_okay()) then
      write(*,'(//11x,a)')"This copy of PARAM is now out-of-date"
      call web_message(0,"param.html")
    end if
    gui = .false.
    ifiles_8 = 8
    ifiles_1 = ifiles_8
    power = 2.0d0
    tore = ios + iop + iod
    is_PARAM = .true.
    run = 1
      call fbx   ! Factorials and Pascal's triangle (pure constants)
      call fordd ! More constats, for use by MNDO-d
  !
  !  one-time construction of arrays in setalp
  !
  !  call setalp_init
  !  time0 = second (1)
  !
  !  Disable MOPAC test for the presence of parameters
  !
    do i = 1, 107
      do j = 1, 5
        allref (i, j) (1:1) = " "
      end do
    end do
  !
  ! Read in all the data in the file "<filename>.dat", and put it
  ! in an internal file. 
  !
    call getdat(input, ifiles_8)
      if (moperr) then
      call finish
    end if
    job_fn = Trim (jobnam) // ".mop"
    inquire (file=job_fn, exist=exists)
    if (exists) then
      write(ifiles_8,*)"File ",job_fn," exists, therefore job cannot be run."
      call finish
    end if
  !
  !  Open output results
  !
    i = Index (jobnam, " ") - 1
    k = Index (contrl, "OUT=")
    if (k /= 0) then
      j = Index (contrl(k+4:), " ")
      text = contrl (k+4:k+j+2) // jobnam (:i) // ".out"
    else
      text = jobnam (:i) // ".out"
    end if
  97  open (unit=ifiles_8, file=text, status="UNKNOWN", iostat=i)
    if (i /= 0) then
      write(0,*)" Output file '"//trim(text)//"' is busy.  Correct the fault or kill this job"
      call sleep (10)
      go to 97
    end if
    goto 100
  !
  !  Open input data set
  !
100 rewind (input)
    i = 0
    do
      read (input, "(A120)") text
      i = i + 1
      if (text(1:1) /= "*") exit
      write (ifiles_8,"(a)") Trim(text)
    end do
  !  close (input, status="keep")
    close (ir)
  !  i = Index (jobnam, " ") - 1
  !  open (unit=ir, file=jobnam(:i)//".temp", status="UNKNOWN",err=99)
    ir = input
    rewind (ir)
  !
  !  Read in keywords for PARAM control
  !
    read (ir, "(A241)",end = 99, err = 98) contrl
    i = Index(contrl," +")
    if ( i /= 0) then
      contrl(i:i) = " "
      read (ir, "(A241)",end = 99, err = 98) koment
      contrl(len_trim(contrl)+1:) = " "//koment 
      i = Index(contrl,"+")
      if (i /= 0) then
        koment(i:i) = " "
        read (ir, "(A241)",end = 99, err = 98) koment
        contrl(len_trim(contrl)+1:) = " "//koment 
      end if
    end if
  !
  !  Force character 1 to be a space
  !
    if (contrl(1:1) /= " ") then
      koment = " " // trim(contrl (1:240))
      contrl = koment
    else
      koment = trim(contrl)
    end if
    call upcase (contrl, 241)
    j = 1
    do i = 1, 20
      l = Index (contrl(j:), "=") + j
      if (l /= j) then
        k = Index (contrl(l:), " ") + l - 2
        contrl(l:k) = koment(l:k)
        j = k
      end if
    end do
    if (index(contrl, "PKA") /= 0) then
      if (index(contrl," 1SCF") == 0) then
        i = index(contrl,"      ")
        contrl(i:i+5) = " 1SCF"
      end if
      iseps = .true.
      nspa = 42
    end if
    param_constant = 1.d0
    trunc_1 = 7.0d0
    trunc_2 = 0.22d0
    if (index(contrl, "CONST") /= 0) then
      i = index(contrl, "CONST")
      param_constant = reada(contrl,i)
    end if
!
!   HERE IS WHERE THE MAXIMUM NUMBER OF ATOMS IN ANY SYSTEM IS SET
!
    if (Index(contrl," EPS") /= 0) then
      n1 = 20             !  Number of atoms with 1 basis function
      n4 = 20             !  Number of atoms with 4 basis functions
      n9 = 10             !  Number of atoms with 9 basis functions
      iseps = .true.
      nspa = 42
      lm61 = 45*n9 + 10*n4 + n1
    else
      n1 = 200            !  Number of atoms with 1 basis function
      n4 = 304            !  Number of atoms with 4 basis functions
      n9 = 100            !  Number of atoms with 9 basis functions
      lm61 = 0
      iseps = .false. 
    end if
    
!
!
    maxatoms = n1 + n4 + n9
    call setup_mopac_arrays(maxatoms, 1)
    allocate(nw(maxatoms))
    norbs = 9*n9 + 4*n4 + n1
    mpack = (norbs*(norbs + 1))/2
    numat = maxatoms
    nvar = 3*maxatoms
    n2elec = 2025*n9+100*n4+n1+2025*(n9*(n9-1))/2+450*n9*n4+45*n9*n1+&
     & 100*(n4*(n4-1))/2+10*n4*n1+ (n1*(n1-1))/2+10
   
    nmos = 12
    l123 = 1000
    uhf = .true.
    call setup_mopac_arrays(1,2)
    allocate(geoa(3,maxatoms))
    na(:) = 0
    nb(:) = 0
    nc(:) = 0
    atmass = 1.d0
    if (Index(contrl, "OLDFPC") == 0)then
  !
  ! Load in the modern (CODATA) fundamental constants
  !
    fpc_1 = fpcref(1, 1)
    fpc_2 = fpcref(1, 2)
    a0 = fpcref(1, 3)
    ev = fpcref(1, 4)
    fpc_5 = fpcref(1, 5)
    fpc_6 = fpcref(1, 6)
    fpc_7 = fpcref(1, 7)
    fpc_8 = fpcref(1, 8)
    fpc_9 = fpcref(1, 9)
    fpc_10 = fpcref(1, 10)
else
  !
  ! Load in the old (CODATA) fundamental constants
  !
    fpc_1 = fpcref(2, 1)
    fpc_2 = fpcref(2, 2)
    a0 = fpcref(2, 3)
    ev = fpcref(2, 4)
    fpc_5 = fpcref(2, 5)
    fpc_6 = fpcref(2, 6)
    fpc_7 = fpcref(2, 7)
    fpc_8 = fpcref(2, 8)
    fpc_9 = fpcref(2, 9)
    fpc_10 = fpcref(2, 10)
endif
   i = Index(contrl," POWER")
   if (i /= 0) power = Reada(contrl, i)
      method_am1         = (index(contrl,' AM1') /= 0)
      method_pm3         = (index(contrl,' PM3') /= 0)
      method_pm6_dh2     = (index(contrl,' PM6-DH2')  /= 0)
      method_pm6_dh_plus = (index(contrl,' PM6-DH+')  /= 0)
      method_mndo        = (index(contrl,' MNDO ') /= 0)
      method_mndod       = (index(contrl,' MNDOD') /= 0)
      method_rm1         = (index(contrl,' RM1') /= 0)
      method_pm6         = (index(contrl,' PM6') /= 0)
      method_pm7         = (index(contrl,' PM7') /= 0)
      if (.not. method_pm7) method_pm7   = (.not.(method_am1 .or. method_pm3 .or. method_mndod .or. &
      method_mndo .or. method_rm1 .or. method_pm6 .or. method_pm6_dh2 .or. method_pm6_dh_plus))
      tdump = 1.d8 ! (a long time)
      tleft=1.d6
  !
  !  Write out output banner
  !
    write (ifiles_8, "(1x,15('*****')//18x,'PARAMETRIZATION CALCULATION',' RESULTS'///1x,15('*****'))")
    idate = " "
    call fdate (idate)
    write (ifiles_8, "(55x,A24)") idate
    keywrd = contrl
    call parkey (contrl)
    write (ifiles_8, "(1X,15('*****'))")
    call switch
    if (index(contrl, "PKA") /= 0) call Parameters_for_PKA(uss(99), upp(99), zs(99))
!
!
    penalty = 10000.d0  ! Contribution to SSQ = penalty*error**2
!
!
  !
  ! Read in all data relating to parameters
  !
    j = iw
    iw = ifiles_8
    if (index(keywrd, " PARAMS") /= 0) call datin(ifiles_8)
    iw = j
    if (moperr) stop
    call getpar
    if ((Index (contrl, " CHKPAR") /= 0) .and. numvar == 0) then
      write(ifiles_8,*)" No parameters therefore parameter ", &
      &" independence cannot be determined"
      call finish
    end if
    do i = 1,90
    if (f0sd_store(i) < 1.d-5) f0sd_store(i) = f0sd(i)
    if (g2sd_store(i) < 1.d-5) g2sd_store(i) = g2sd(i)
    end do
    do i = 57, 71
      if (zs(i) < 0.1d0) tore(i) = 3.d0
    end do
    call fractional_metal_ion
    call calpar
    
    if (Index (contrl, " CC") /= 0) then
  !    call output_cc_rep_fn
    end if
    save_parameters = (Index (contrl, " NOSAVEP") == 0)
  !
  ! Read in all data relating to reference data
  !
    i = size(p)
    deallocate(p)
    call datinp()
    text = trim(jobnam)//".F90" 
    j = iw
    iw = ifiles_8
    ! call create_parameters_for_PMx_C(text, "7") 
    iw = j
    allocate(p(i))
    call fractional_metal_ion
    large = (Index (contrl, " LARGE") /= 0)
!
!  Unconditionally, do not generate MOPAC log files
!
      inquire (unit=ilog, opened=opend)
      if (opend) then
        close (unit=ilog, status="DELETE")
      end if
      open (unit=ilog, status="SCRATCH", form="FORMATTED")
!
!  Do NOT generate normal MOPAC output, unless "LARGE" is present.
!
      if (large) then
        inquire (unit=iw, opened=opend)
        if (opend) then
          close (unit=iw, status="DELETE")
        end if
        i = Index (jobnam, " ") - 1
        inquire (unit=iarc, opened=opend)
        if (opend) close(iarc)
        open (unit=iw, file=jobnam(:len_trim(jobnam))//".arc", status="UNKNOWN")
        rewind (iw)
      else
        open (unit=iw, status="SCRATCH", form="FORMATTED")
      end if     
  !***********************************************************************
  !
  !   Different functions within PARAM
  !
  !***********************************************************************
  if (Index (contrl, " CHKPAR") /= 0) then
    !
    ! Check the parameter set to see if it is well-defined
    !
      call direct(1)
      call chkpar
      write (ifiles_8, "(//20X,A)") " PARAM FINISHED"
      call finish
      stop
    end if
    if (Index (contrl, " SURVEY") /= 0) then
    !
    !   Generate publication-quality tables
    !
      call partab 
      stop
    end if
    if (numvar < 1) then
    !
    ! Compute the error function, but do not optimize any parameters.
    !
      call direct (1)
      stop
    end if
    if (Index (contrl, " CYCLES") /= 0) then
      maxcyc = Nint(Reada(contrl,Index (contrl, " CYCLES")))
    else
      maxcyc = 600
    end if
  !
  ! Optimize the parameters
  !
    fnsnew(1) = -1.d7
    do loop = 1, maxcyc
      call direct(loop)
      if (.not. large) then
!
!   Delete scatch files to save storage space
!
        close (iw, iostat=i)
        open (unit=iw, status="SCRATCH", form="FORMATTED", iostat=i)
      end if   
      call rapid0 (loop)
      call pparsav(save_parameters)
    !
    !  Calculate the predicted value of the new error function
    !
      do i = 1, nfns
        sum = 0.d0
        do j = 1, numvar
          sum = sum + xparamp(j) * diffns(j, i)
        end do
        fnsnew(i) = fns(i) - sum
      end do      
    end do
    call finish
 99 write(*,*)" Data file '",jobnam(:i),".dat' does not exist!"
    call finish
 98 write(*,*)" Data file '",jobnam(:i),".dat' is locked!"
    call finish
end program param
logical function date_okay()
! use dfport
  implicit none
!  character  :: julian*8
  ! character :: verson*24
  ! character (len=8), external :: jdate   ! Use this line if a Linux or Mac version, and comment out the line "use dfport"
  
! call getdatestamp(line, verson)
!  julian = jdate()
!
!  Set date_created to the date of creation of the executable
!
! date_created    =   (ichar(verson(2:2)) - ichar('0'))*365 + &
!                      (ichar(verson(4:4)) - ichar('0'))*100 + &
!                      (ichar(verson(5:5)) - ichar('0'))*10 + &
!                      (ichar(verson(6:6)) - ichar('0')) + 183 
 
!
!  todays_date =   ((ichar(julian(2:2)) - ichar('0'))*365 + &
!                   (ichar(julian(3:3)) - ichar('0'))*100 + &
!                   (ichar(julian(4:4)) - ichar('0'))*10 + &
!                   (ichar(julian(5:5)) - ichar('0'))) 
 ! write(*,'(10x,a,i4,a)')" This copy of PARAM has", date_created - todays_date ," days left"
  date_okay = .true. ! (date_created > todays_date)

  end function date_okay
  
