      subroutine pathk() 
!-----------------------------------------------
!   Follow a reaction path in which the step-size is a constant,
!   Keywords STEP and POINTS are used
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : iflepo, numat, keywrd, nvar, tleft, escf, &
      line, norbs, numcal, id, natoms, nl_atoms
      use maps_C, only : rxn_coord, lparam, react, latom, kloop
      use common_arrays_C, only : geo, xparam, profil, na, l_atom, coord, nat, &
        loc, grad
      use chanel_C, only : iw, ires, iarc, restart_fn, archive_fn, iw0, &
        ixyz, xyz_fn
      use elemts_C, only : elemnt
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:32  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use dfpsav_I 
      use second_I 
      use ef_I 
      use geout_I 
      use wrttxt_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(20) :: mdfp 
      integer :: npts, maxcyc, i, lloop, iloop, l, m, k, iw00, percent = 0
      real(double), dimension(3*numat) :: gd, xlast 
      real(double), dimension(20) :: xdfp 
      real(double) :: step, degree, c1, cputot, cpu1, cpu2, cpu3, stepc1, factor
      logical :: use_lbfgs, opend, scale, debug
!-----------------------------------------------
      use_lbfgs = (index(keywrd,' LBFGS')/=0 .or. nvar > 2000 .and. index(keywrd,' EF') == 0)
      step = reada(keywrd,index(keywrd,'STEP') + 5) 
      debug = (index(keywrd, " DEBUG") /= 0)
      npts = nint(reada(keywrd,index(keywrd,'POINT') + 6)) 
!
!  THE SMALLEST VALUE IN THE PATH IS
!      REACT(1) DEGREE OR GEO(LPARAM,LATOM) RADIANS
!
      degree = 57.29577951308232D0 
      if (lparam /= 1 .and. na(latom) /= 0) then 
        step = step/degree 
        c1 = degree 
      else 
        c1 = 1.D0 
      endif 
      if (index(keywrd, " MINI") /= 0) then
        loc = 0
        nvar = 0
        do i = 1, natoms 
          do l = 1, 3 
            if (lparam /= l .or. latom /= i) then
              nvar = nvar + 1 
              loc(1,nvar) = i 
              loc(2,nvar) = l 
              xparam(nvar) = geo(l,i) 
            end if
          end do 
        end do
        if (allocated(grad)) deallocate(grad)
        allocate(grad(nvar))
      end if
      open(unit=ixyz, file=xyz_fn)
!
      kloop = 1 
      maxcyc = 100000 
      if (index(keywrd,' BIGCYCLES') /= 0) maxcyc = nint(reada(keywrd,index(&
        keywrd,' BIGCYCLES'))) 
      cputot = 0.0D0 
      rxn_coord = geo(lparam,latom)
      if (allocated(profil)) deallocate(profil)
      if (allocated(react)) deallocate(react)
      allocate(profil(npts + 1), react(npts + 1))
      profil(1) = 0.D0 
      react(1) = geo(lparam,latom)
      if (use_lbfgs) then 
        write (iw, '(''  ABOUT TO ENTER L-BFGS FROM PATHK'')') 
        if (index(keywrd,'RESTAR') /= 0) then 
          mdfp(9) = 0 !   This section is almost certainly faulty!
          gd = 0.d0
          xlast = 0.d0
          xdfp = 0.d0
          call dfpsav (cputot, xparam, gd, xlast, escf, mdfp, xdfp) 
          write (iw, '(2/10X,'' RESTARTING AT POINT'',I3)') kloop 
        endif 
      else
        write (iw, '(''  ABOUT TO ENTER EF FROM PATHK'')') 
        if (index(keywrd,'RESTAR') /= 0) then 
          open(unit=ires, file=restart_fn, status='UNKNOWN', form=&
            'UNFORMATTED', position='asis') 
          rewind ires 
          read (ires, end=60, err=60) i, l
          if (norbs /= l .or. numat /= i) then
              write(iw,*)" Restart file read in does not match current data set"
              call mopend (" Restart file read in does not match current data set")
              goto 99
          end if
          read (ires, err=60) kloop 
          read (ires, err=60) rxn_coord 
          read (ires, err=60) (profil(i),i=1,kloop) 
          write (iw, '(2/10X,'' RESTARTING AT POINT'',I3)') kloop 
        endif 
      endif 
!
      geo(lparam,latom) = rxn_coord 
      lloop = kloop 
      scale = .false.
      if (id == 1) then
        do i = 1, natoms
          if (na(i) /= 0) exit
        end do
        scale = (i > natoms .and.latom == natoms)
      end if
      iw00 = iw0
      if (.not. debug) iw0 = -1     
      do iloop = kloop, npts 
        if (iloop - lloop >= maxcyc) tleft = -100.D0 
        cpu1 = second(2) 
        if (iloop > 1 .and. scale) then
          factor = geo(lparam,latom)/rxn_coord
          do i = 1, latom - 1
            xparam((i - 1)*3 +lparam) = xparam((i - 1)*3 +lparam)*factor
          end do
        end if
        rxn_coord = geo(lparam,latom)         
        numcal = numcal + 1
        if (use_lbfgs) then
          write(iw,'(5x,a)')" Geometry optimization using L-BFGS"
          if (iloop == 1 .and.iw00 > -1) then
            if (.not. debug) iw0 = 0 ! Temporarily allow writing to screen
            call to_screen(" Geometry optimization using L-BFGS")
            if (.not. debug) iw0 = -1
          end if
          call lbfgs (xparam, escf) 
        else
          write(iw,'(5x,a)')" Geometry optimization using EF"
          if (iloop == 1 .and.iw00 > -1) then
            if (.not. debug) iw0 = 0 ! Temporarily allow writing to screen
            call to_screen(" Geometry optimization using EF")
            if (.not. debug) iw0 = -1
          end if         
          call ef (xparam, escf) 
        endif 
        i = index(keywrd,'RESTAR')
        if (i /= 0) keywrd(i:i+6) = " "
        i = index(keywrd,'OLDENS')
        if (i /= 0) keywrd(i:i+5) = " "
        if (iflepo == (-1)) goto 99         
        kloop = kloop + 1 
        cpu2 = second(2) 
        cpu3 = cpu2 - cpu1 
        cputot = cputot + cpu3 
        profil(iloop) = escf 
        write (iw, '(/''          VARIABLE        FUNCTION'')') 
        write (iw, '('' :'',F16.5,F16.6)') geo(lparam,latom)*c1, escf 
        if (iw00 > -1) then
          if (.not. debug) iw0 = 0 ! Temporarily allow writing to screen
          i = nint((100.0*iloop)/npts)
          if (i /= percent) then
            percent = i
            write(line,"('' :'',F16.4,F16.4,i9,a)")geo(lparam,latom)*c1, escf, &
              percent, "% of Reaction Coordinate done"
          else
            write(line,"('' :'',F16.4,F16.4,i9,a)")geo(lparam,latom)*c1, escf
          end if
          call to_screen(line)
        end if
        call geout (iw) 
        geo(lparam,latom) = geo(lparam,latom) + step 
!
!  Write out "xyz" file
!
        write(ixyz,"(i6,a)") nl_atoms," "
        write(ixyz,*)"PATH "
        do i = 1, numat
          if (l_atom(i)) write(ixyz,"(3x,a2,3f15.5)")elemnt(nat(i)), (coord(l,i),l=1,3)
        end do
        if (.not. debug) iw0 = -1
      end do 
      if (cputot > 1.d7) cputot = cputot - 1.d7
      react(1) = react(1)*c1 
      stepc1 = step*c1 
      do i = 2, npts 
        react(i) = react(i-1) + stepc1 
      end do 
      write (iw, &
      '(/16X,''POINTS ON REACTION PATH '',/16X,''AND CORRESPONDING HEATS'',2/)') 
      inquire(unit=iarc, opened=opend) 
      if (opend) close(unit=iarc, status='KEEP') 
      open(unit=iarc, file=archive_fn, status='UNKNOWN', position=&
        'asis') 
      write (iarc, 30) 
      call wrttxt (iarc) 
   30 format(' ARCHIVE FILE FOR PATH CALCULATION'/,&
        'A PROFILE OF COORDINATES - HEATS'/) 
      write (iarc, '(/'' TOTAL CPU TIME : '',F10.3/)') cputot 
!
      l = npts/8 
      m = npts - l*8  
      if (l >= 1) then 
        do k = 0, l - 1 
          write (iw, '(9F17.8)') (react(i),i=k*8 + 1,k*8 + 8) 
          write (iw, '(9F17.8,/)') (profil(i),i=k*8 + 1,k*8 + 8) 
        end do 
      endif 
      if (m > 0) then 
        write (iw, '(9F17.8)') (react(i),i=l*8 + 1,l*8 + m)  
        write (iw, '(9F17.8,/)') (profil(i),i=l*8 + 1,l*8 + m) 
      endif 
      do i = 1, npts
        write(iarc,'(2f17.8)')react(i), profil(i)
      end do
      if (.not. debug) iw0 = iw00
      goto 99  
   60 continue 
      write (iw, '(A,I3,A)') ' ERROR DETECTED DURING READ FROM CHANNEL', ires, &
        ' IN SUBROUTINE PATHK' 
      call mopend ('ERROR DETECTED DURING READ IN SUBROUTINE PATHK') 
  99  if (allocated(profil)) deallocate(profil)
      if (allocated(react)) deallocate(react)
      return  
      end subroutine pathk 
