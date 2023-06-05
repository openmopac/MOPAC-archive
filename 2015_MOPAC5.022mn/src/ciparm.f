      SUBROUTINE CIPARM
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
C******************************************************************************
C
C  THIS SUBROUTINE IS TAKEN FROM AMSOL 7.1 
C  THIS SUBROUTINE SETS UP THE CI PARAMETERS FOR THE SUBROUTINE WRITES.
C
C   NMOS IS NO. OF M.O.S USED IN C.I.
C   NCIS IS CHANGE IN SPIN, OR NUMBER OF STATES
C
C  CREATED BY DJG 0995 FROM EXISTING LINES IN WRITES
C
C******************************************************************************
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /SCRAH2/ PQKL(NRELAX*MORB2+1)
      COMMON /CIDATA/ VECTCI(MAXCI),XXDUM,NCI1,NCI2,NCI3                DL0397
     1               ,NCIDUM((1+NMECI*2)*MAXCI)                         DL0397
      COMMON /OPTIMI / IMP,IMP0
      LOGICAL PRINT1,LGRAD                                              DJG0995
C     WRITE(JOUT,100)                                                   DJG0995
      NMOS=0
      NCIS=0
      IF(ICI.NE.0) NMOS=IICI                                            DJG0995
      IF(NMOS.EQ.0) NMOS=NOPEN-NCLOSE
      IF(NCIS.EQ.0) THEN
         IF(ITRIPL.NE.0.OR.IQUART.NE.0) NCIS=1                          DJG0995
         IF(IQUINT+ISEXTE.NE.0) NCIS=2                                  DJG0995
      ENDIF
C                                                                       DJG0995
C  LGRAD=FALSE TELLS MECI TO RUN OVER ONLY CI-ACTIVE MO'S               DJG0995
C  PRINT1=TRUE TURNS ON SOME PRINTING OPTIONS IN MECI                   DJG0995
C                                                                       DJG0995
      LGRAD=.FALSE.                                                     DJG0995
      PRINT1=.TRUE.                                                     DJG0995
C     X=MECI(EIGS,C,CBETA,EIGB, NORBS,NMOS,NCIS,PRINT1,LGRAD)
      CALL MECI(EIGS,C,CBETA,EIGB,RMECI,NORBS,NMOS,NCIS,.FALSE.,
     &          LGRAD)
      CALL MECIP (P,C,CBETA,NORBS,PQKL,NCI2)
100   FORMAT(/,10X,'Multi-electron configuration ',                     DJG0995
     1       'interaction calculation')                                 DJG0995
      RETURN
      END
