      SUBROUTINE GETCHG(Q,FLGEXT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'KEYS.i'
         INCLUDE 'SIZES.i'
C******************************************************************************
C
C  THIS SUBROUTINE CALCULATES THE PARTIAL ATOMIC CHARGES BASED ON THE CURRENT
C  CHARGE MODEL.
C
C  FLGEXT = IF FALSE, IGNORE EXTERNAL CHARGE MODEL CHARGES.
C  CORE = VALENCE NUCLEAR CHARGE BY ELEMENT
C  Q = ATOMIC NET CHARGES
C
C  CREATED BY DJG 0995
C  ADAPTATED TO NEW CHGMP1 ROUTINE BY DL 0397
C******************************************************************************
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /CMCOM/  ECMCG(NUMATM)
      COMMON /CORE  / CORE(107)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      DIMENSION Q(*)
      LOGICAL FLGEXT
      IF (IEXTCM.NE.0.AND.FLGEXT) THEN
         CALL SCOPY(NUMAT,ECMCG,1,Q,1)
      ELSE
         IF (ICHMOD.EQ.1) THEN
            CALL CHGMP1(P,Q)
         ELSE IF (ICHMOD.EQ.2) THEN                                     GDH0997
           CALL CHGMP2(P,Q)                                             GDH0997
         ELSE
            CALL CHRGE(P,Q)
            DO 50 I=1,NUMAT
               Q(I)=CORE(NAT(I))-Q(I)
50          CONTINUE
         ENDIF
      ENDIF
      END
