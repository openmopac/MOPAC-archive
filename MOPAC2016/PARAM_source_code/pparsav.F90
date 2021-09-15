  subroutine pparsav(save_parameters)
    use param_global_C, only : valvar, valold, toplim, &
    botlim, numvar, ifiles_8, locvar, contrl, fnsnew, penalty
!
    use chanel_C, only : iext
    use molkst_C, only : jobnam, line
    use parameters_C, only : partyp
    use elemts_C, only : elemnt
    implicit none
    logical :: save_parameters
!-----------------------------------------------------------------------
    integer :: i, j, k, j1
    double precision :: penalty_fn, sum
    character (len=80) :: filename
    intrinsic Index
    character (len=2) elemnt2
!-----------------------------------------------------------------------
    if(save_parameters) then
      if (numvar == 0) return
      k = Index (jobnam, " ") - 1
      i = Index(contrl, "NEW_RP=")
      if (i /= 0) then
      j = Index(contrl(i:)," ") + i - 2
      if(contrl(j:j) /= "/") then
        j = j + 1
        contrl(j:j) = "/"
      end if
      filename = contrl(i+13:j)//jobnam (:k) // "." // "rp"
      else
      filename = jobnam (:k) // "." // "rp"
      end if
      open (unit=iext, form="FORMATTED", status="UNKNOWN", file=filename)
      rewind (iext)
      write(ifiles_8,"(2a)")' Parameter    Element     New value   ',&
      & '    Change    Limits: Low    High     Penalty    Gradient'
      do i = 1, numvar
        j = locvar(2, i)/200
        if(j /= 0) then
          elemnt2 = elemnt(j)
           if(elemnt2(1:1) == " ")elemnt2 = elemnt2(2:2)
          j = locvar(2,i) - j*200
        else
          elemnt2 = " "
          j = locvar(2,i)
        endif
        k = i
        call lockit(valvar(i), k)
        if (valvar(i)-toplim(i) > 0.d0 .or. valvar(i)-botlim(i) < 0.d0) then
          penalty_fn = penalty*((Max(0.d0, valvar(i)-toplim(i))+Min(0.d0, valvar(i)-botlim(i))))**2
          write (line, "(4X,A7,4X,A2,3X,2F15.8,F12.2,F8.2,f12.2,f14.2)") partyp(locvar(1, i))//elemnt2, &
          & elemnt(j), valvar(i), valvar(i) - valold(i), botlim(i), toplim(i), &
          & penalty_fn,fnsnew(i)
        else
          write (line, "(4X,A7,4X,A2,3X,2F15.8,32x,f14.2)") partyp (locvar(1, i))//elemnt2, &
           & elemnt(j), valvar(i), valvar(i) - valold(i),fnsnew(i)
        end if
        if (j /= 99 .or. locvar(1, i) < 22 .or. locvar(1, i) > 33) then
          write (iext, "(a)") trim(line)
        else
          j1 = locvar(1, i) - 21               
          if (j1 < 10) then
            write(iext,"(4x,'PAR',i1,11x,a)") j1, trim(line(20:))
          else
            write(iext,"(4x,'PAR',i2,10x,a)") j1, trim(line(20:))
          end if
        end if
      end do
    end if
!
!  For parameters that should always be a maximum or a minimum, modify
!  the limits to impose a mild force on the parameter
!
    do i = 1, numvar
      if (botlim(i) < -1.d3) then
!
! This parameter should be as negative as possible.  Set the upper bound
! slightly lower than the actual parameter. (difference is such that the force = botlim*0.0001)
!
        toplim(i) = valvar(i) - 0.000158114*sqrt(-botlim(i))
      end if
      if (toplim(i) > 1.d3) then
!
! This parameter should be as positive as possible.  Set the lower bound
! slightly higher than the actual parameter.
!
        botlim(i) = valvar(i) + 0.000158114*sqrt(toplim(i))
      end if
    end do
    sum = 0.d0
    do i = 1, numvar
      j = locvar(2, i)/200
      if(j /= 0) then
        elemnt2 = elemnt(j)
        if(elemnt2(1:1) == " ")elemnt2 = elemnt2(2:2)
        j = locvar(2,i) - j*200
      else
         elemnt2 = " "
         j = locvar(2,i)
      endif
      k = i
      call lockit(valvar(i), k)
      if (valvar(i)-toplim(i) > 0.d0 .or. valvar(i)-botlim(i) < 0.d0) then
        penalty_fn = penalty*((Max(0.d0, valvar(i)-toplim(i))+Min(0.d0, valvar(i)-botlim(i))))**2
        write (line, "(4X,A7,4X,A2,3X,2F15.8,F12.2,F8.2,2f12.2)") partyp(locvar(1, i))//elemnt2, &
        & elemnt(j), valvar(i), valvar(i) - valold(i), botlim(i), toplim(i), &
        & penalty_fn,fnsnew(i)
        sum = sum + penalty_fn
      else
        write (line, "(4X,A7,4X,A2,3X,2F15.8,32x,f12.2)") partyp (locvar(1, i))//elemnt2, &
       & elemnt(j), valvar(i), valvar(i) - valold(i),fnsnew(i)
      end if
      if (j /= 99 .and. j /= 98 .or. locvar(1, i) < 22 .or. locvar(1, i) > 33) then
          write (ifiles_8, "(a)") trim(line)
        else
          j1 = locvar(1, i) - 21      
          if (j == 98) j1 = j1 + 12         
          if (j1 < 10) then
            write(ifiles_8,"(4x,'PAR',i1,11x,a)") j1, trim(line(20:))
          else
            write(ifiles_8,"(4x,'PAR',i2,10x,a)") j1, trim(line(20:))
          end if
        end if
    end do
    do i = 1, numvar
      valold(i) = valvar(i)
    end do
    close (iext)
    if (sum > 1.d-6) write(ifiles_8,"(a,f12.2)")' Total penalty function:', sum
    if(save_parameters) write (ifiles_8,*) " PARAMETERS DUMPED O.K."
  end subroutine pparsav
