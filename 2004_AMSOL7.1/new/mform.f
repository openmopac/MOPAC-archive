      SUBROUTINE MFORM(NFILE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
C******************************************************************************
C
C   FIGURES OUT AND PRINTS THE MOLECULAR FORMULA
C
C   ON INPUT
C      NFILE = OUTPUT FILE
C
C   CREATED BY DJG 0995 FROM EXISTING LINES IN WRITES
C
C******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /ELEMTS/ ELEMNT(107)
      DIMENSION NELEMT(107), IDIGT1(107), IDIGT2(107)
      CHARACTER*2 ELEMNT, IELEMT(20), NUMBRS(11)*1
      DATA NUMBRS /'0','1','2','3','4','5','6','7','8','9',' '/
C
      DO 150 I=1,107
  150 NELEMT(I)=0
      DO 160 I=1,NUMAT
      IGO=NAT(I)
      IF (IGO.GT.107) GO TO 160
      NELEMT(IGO)=NELEMT(IGO)+1
  160 CONTINUE
      ICHFOR=0
      IF (NELEMT(6).EQ.0) GO TO 170
      ICHFOR=1
      IELEMT(1)=ELEMNT(6)
      NZS=NELEMT(6)
      IF (NZS.LT.10) THEN
         IF (NZS.EQ.1) THEN
            IDIGT1(1)=11                                                DJG0295
         ELSE
            IDIGT1(1)=NZS+1                                             DJG0295
         ENDIF
         IDIGT2(1)=11                                                   DJG0295
      ELSE
         KFRST=NZS/10
         KSEC=NZS-(10*KFRST)
         IDIGT1(1)=KFRST+1                                              DJG0295
         IDIGT2(1)=KSEC+1                                               DJG0295
      ENDIF
  170 NELEMT(6)=0
      DO 180 I=1,107
      IF (NELEMT(I).EQ.0) GO TO 180
      ICHFOR=ICHFOR+1
      IELEMT(ICHFOR)=ELEMNT(I)
      NZS=NELEMT(I)
      IF (NZS.LT.10) THEN
         IF (NZS.EQ.1) THEN
            IDIGT1(ICHFOR)=11                                           DJG0295
         ELSE
            IDIGT1(ICHFOR)=NZS+1                                        DJG0295
         ENDIF
         IDIGT2(ICHFOR)=11                                              DJG0295
      ELSE
         KFRST=NZS/10
         KSEC=NZS-(10*KFRST)
         IDIGT1(ICHFOR)=KFRST+1                                         DJG0295
         IDIGT2(ICHFOR)=KSEC+1                                          DJG0295
      ENDIF
  180 CONTINUE
      WRITE(NFILE,2000)(IELEMT(I),NUMBRS(IDIGT1(I)),NUMBRS(IDIGT2(I)),  DJG0995
     1      I=1,ICHFOR)
2000  FORMAT (/,1X,17(A2,A1,A1,1X))                                     DJG0295
      RETURN
      END
