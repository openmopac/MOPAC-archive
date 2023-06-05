      double precision function SECOND()
C
      double precision aux
      logical first
      SAVE first, aux                                                   CSAV
      DATA first /.TRUE./
C
      if ( first ) then
        aux=SECOND1()
        second=0.0
        first= .FALSE.
      else
        second=SECOND1() - aux
      endif
      return
      END
