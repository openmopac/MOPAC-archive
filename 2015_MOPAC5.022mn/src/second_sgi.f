C	SECOND for SGI
C
      double precision function SECOND()
C
      double precision aux
      logical first
      SAVE first, aux                                                   CSAV
      DATA first /.TRUE./
c
      if ( first ) then
         aux=SECNDS(0.0)
         second=0.0
         first= .FALSE.
      else
         second=SECNDS(0.0) - aux
      endif
      return
      END
