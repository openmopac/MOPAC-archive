      SUBROUTINE EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,IL,JL,BMAT,IPOW,
     &                 *)                                               CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      CHARACTER ELEMNT*2, KEYWRD*160
      DIMENSION HESS(MAXPAR,*),GRAD(*),BMAT(MAXPAR,*),IPOW(9),
     1 XPARAM(*), PMAT(*)
**********************************************************************
*
* EFSAV STORES AND RETRIEVE DATA USED IN THE EF GEOMETRY
*        OPTIMIZATION. VERY SIMILAR TO POWSAV.
*
*  ON INPUT HESS   = HESSIAN MATRIX, PARTIAL OR WHOLE.
*           GRAD   = GRADIENTS.
*           XPARAM = CURRENT STATE OF PARAMETERS.
*           IL     = INDEX OF HESSIAN,
*           JL     = CYCLE NUMBER REACHED SO-FAR.
*           BMAT   = "B" MATRIX!
*           IPOW   = INDICES AND FLAGS.
*           IPOW(9)= 0 FOR RESTORE, 1 FOR DUMP, 2 FOR SILENT DUMP
*
**********************************************************************
      COMMON /GEOVAR/ XDUMMY(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0494
      COMMON /ELEMTS/ ELEMNT(120)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,rmin,rmax,omin,xlamd,xlamd0,skal,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /LOCVAR/ LOCVAR(2,MAXPAR)
      COMMON /NUMSCF/ NSCF
      COMMON /KEYWRD/ KEYWRD
      COMMON /VALVAR/ VALVAR(MAXPAR),NUMVAR
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, JLOOP
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      COMMON /PATHI / LATOM,LPARAM                                      IR0494
      IF (DOPRNT) THEN                                                  CIO
       OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',FORM='UNFORMATTED')
       REWIND 9
       OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTED')
       REWIND 10
      ENDIF                                                             CIO
      IR=9
      IF(IPOW(9) .EQ. 1 .OR. IPOW(9) .EQ. 2) THEN
         FUNCT1=SQRT(DDOT(NVAR,GRAD,1,GRAD,1))                          IR0494
         IF(IPOW(9).EQ.1)THEN
            IF (DOPRNT) WRITE(6,'(//10X,                                CIO
     &                           ''CURRENT VALUE OF GRADIENT NORM =''   CIO
     &                           ,F12.6)')FUNCT1                        CIO
            IF(DOPRNT)WRITE(6,'(/10X,''CURRENT VALUE OF GEOMETRY'',/)') CIO
            CALL GEOUT(*9999)                                           IR0494 CSTP(call)
         ENDIF
C
C  IPOW(1) AND IPOW(9) ARE USED ALREADY, THE REST ARE FREE FOR USE
C  
         IPOW(8)=NSCF
         IF (DOPRNT) THEN                                               CIO
         WRITE(IR)IPOW,IL,JL,FUNCT,TT0
         WRITE(IR)(XPARAM(I),I=1,NVAR)
         WRITE(IR)(  GRAD(I),I=1,NVAR)
         WRITE(IR)((HESS(J,I),J=1,NVAR),I=1,NVAR)
         WRITE(IR)((BMAT(J,I),J=1,NVAR),I=1,NVAR)
         WRITE(IR)(OLDF(I),I=1,NVAR),(D(I),I=1,NVAR),(VMODE(I),I=1,NVAR)
         WRITE(IR)DD,MODE,NSTEP,NEGREQ
         ENDIF                                                          CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF (DOPRNT) WRITE(IR)(PMAT(I),I=1,LINEAR)                      CIO
         LINEAR=(NORBS*(NORBS+1))/2
         IF (DOPRNT) WRITE(10)(PA(I),I=1,LINEAR)                        CIO
         IF(DOPRNT.AND.(NALPHA.NE.0)) WRITE(10)(PB(I),I=1,LINEAR)       CIO
         IF(DOPRNT.AND.(LATOM .NE. 0)) THEN                             CIO
            WRITE(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
            WRITE(IR)JLOOP,X0, X1, X2
         ENDIF
         IF (DOPRNT) THEN                                               CIO
           CLOSE(9)
           CLOSE(10)
         ENDIF                                                          CIO
         RETURN
      ELSE
C#         WRITE(6,'(//10X,'' READING DATA FROM DISK''/)')
         IF (DOPRNT) READ(IR,END=10,ERR=10)IPOW,IL,JL,FUNCT,TT0         CIO
         NSCF=IPOW(8)
         I=TT0/1000000
         TT0=TT0-I*1000000
         IF (DOPRNT) THEN                                               CIO
          WRITE(6,'(//10X,''TOTAL TIME USED SO FAR:'',
     1     F13.2,'' SECONDS'')')TT0
          WRITE(6,'(  10X,''              FUNCTION:'',F17.6)')FUNCT
          READ(IR)(XPARAM(I),I=1,NVAR)
          READ(IR)(  GRAD(I),I=1,NVAR)
          READ(IR)((HESS(J,I),J=1,NVAR),I=1,NVAR)
          READ(IR)((BMAT(J,I),J=1,NVAR),I=1,NVAR)
          READ(IR)(OLDF(I),I=1,NVAR),(D(I),I=1,NVAR),(VMODE(I),I=1,NVAR)
          READ(IR)DD,MODE,NSTEP,NEGREQ
         ENDIF                                                          CIO
         LINEAR=(NVAR*(NVAR+1))/2
         IF (DOPRNT) READ(IR)(PMAT(I),I=1,LINEAR)                       CIO
         LINEAR=(NORBS*(NORBS+1))/2
C        READ DENSITY MATRIX
         IF (DOPRNT) READ(10)(PA(I),I=1,LINEAR)                         CIO
         IF(DOPRNT.AND.(NALPHA.NE.0)) READ(10)(PB(I),I=1,LINEAR)        CIO
         IF(LATOM.NE.0) THEN
            IF (DOPRNT) THEN                                            CIO
             READ(IR)((ALPARM(J,I),J=1,3),I=1,NVAR)
             READ(IR)JLOOP,X0, X1, X2
            ENDIF                                                       CIO
            IL=IL+1
         ENDIF
         IF (DOPRNT) THEN                                               CIO
            CLOSE(9)
            CLOSE(10)
         ENDIF                                                          CIO
         RETURN
   10    IF (DOPRNT) WRITE(6,'(//10X,''NO RESTART FILE EXISTS!'')')     CIO
      RETURN 1                                                           CSTP (stop)
      ENDIF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
