      SUBROUTINE FORSAV(TIME,DELDIP,IPT,N3,FMATRX, COORD,NVAR,REFH,
     1                  EVECS,JSTART,FCONST,*)                          CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION FMATRX(*), DELDIP(3,*), COORD(*), EVECS(*), FCONST(*)
************************************************************************
*
*  FORSAV SAVES AND RESTORES DATA USED IN THE FORCE CALCULATION.
*
* ON INPUT TIME = TOTAL TIME ELAPSED SINCE THE START OF THE CALCULATION.
*          IPT  = LINE OF FORCE MATRIX REACHED, IF IN WRITE MODE,
*               = 0 IF IN READ MODE.
*        FMATRX = FORCE MATRIX
************************************************************************
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
CSAV         SAVE                                                           GL0892
      IF (DOPRNT) THEN                                                  CIO
       OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',FORM='UNFORMATTED')
       REWIND 9
       OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTED')
       REWIND 10
      ENDIF                                                             CIO
      IR=9
      IW=9
      IF( IPT .EQ. 0 ) THEN
C
C   READ IN FORCE DATA
C
         IF (DOPRNT) READ(IR,END=20,ERR=20)TIME,IPT,REFH                CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF (DOPRNT) READ(IR)(COORD(I),I=1,NVAR)                        CIO
         IF (DOPRNT) READ(IR,END=10,ERR=10)(FMATRX(I),I=1,LINEAR)       CIO
         IF (DOPRNT) READ(IR)((DELDIP(J,I),J=1,3),I=1,IPT)              CIO
         N33=NVAR*NVAR
         IF (DOPRNT) READ(IR)(EVECS(I),I=1,N33)                         CIO
         IF (DOPRNT) READ(IR)JSTART,(FCONST(I),I=1,NVAR)                CIO
         RETURN
      ELSE
C
C    WRITE FORCE DATA
C
         IF (DOPRNT) REWIND IW                                          CIO
         IF(TIME.GT.1.D6)TIME=TIME-1.D6
         IF (DOPRNT) WRITE(IW)TIME,IPT,REFH                             CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF (DOPRNT) WRITE(IW)(COORD(I),I=1,NVAR)                       CIO
         IF (DOPRNT) WRITE(IW)(FMATRX(I),I=1,LINEAR)                    CIO
         IF (DOPRNT) WRITE(IW)((DELDIP(J,I),J=1,3),I=1,IPT)             CIO
         N33=NVAR*NVAR
         IF (DOPRNT) WRITE(IR)(EVECS(I),I=1,N33)                        CIO
         IF (DOPRNT) WRITE(IR)JSTART,(FCONST(I),I=1,NVAR)               CIO
         LINEAR=(NORBS*(NORBS+1))/2
         IF (DOPRNT) WRITE(10)(PA(I),I=1,LINEAR)                        CIO
         IF (DOPRNT.AND.(NALPHA.NE.0)) WRITE(10)(PB(I),I=1,LINEAR)      CIO
         IF (DOPRNT) CLOSE(9)                                           CIO
         IF (DOPRNT) CLOSE(10)                                          CIO
      ENDIF
      RETURN
   10 IF (DOPRNT) THEN                                                  CIO
        WRITE(6,'(''INSUFFICIENT DATA ON DISK FILES FOR A FORCE '',
     1''CALCULATION'',/10X,''RESTART. PERHAPS THIS STARTED OF AS A '',
     2''FORCE CALCULATION '')')
        WRITE(6,'(10X,''BUT THE GEOMETRY HAD TO BE OPTIMIZED FIRST, '',
     1''IN WHICH CASE '',/10X,''REMOVE THE KEY-WORD "FORCE".'')')
      ENDIF                                                             CIO
      RETURN 1                                                          CSTP (stop)
   20 IF (DOPRNT) WRITE(6,'(//10X,''NO RESTART FILE EXISTS!'')')        CIO
      RETURN 1                                                          CSTP (stop)
 9999 RETURN 1                                                          CSTP
      END
