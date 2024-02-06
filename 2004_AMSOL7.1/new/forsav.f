      SUBROUTINE FORSAV(TIME,DELDIP,IPT,N3,FMATRX, COORD,NVAR,REFH, 
     1                  EVECS,JSTART,FCONST) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
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
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
      DIMENSION FMATRX(*), DELDIP(3,*), COORD(*), EVECS(*), FCONST(*) 
       SAVE
      OPEN(JRES,STATUS='UNKNOWN',FORM='UNFORMATTED')                    GDH1095
      REWIND JRES                                                       GDH1095
      OPEN(JDEN,STATUS='UNKNOWN',FORM='UNFORMATTED')                    GDH1095
      REWIND JDEN                                                       GDH1095
      IF( IPT .EQ. 0 ) THEN 
C 
C   READ IN FORCE DATA 
C 
         READ(JRES)TIME,IPT,REFH                                        GDH1095
         LINEAR=(NVAR*(NVAR+1))/2 
         READ(JRES)(COORD(I),I=1,NVAR)                                  GDH1095
         READ(JRES,END=10,ERR=10)(FMATRX(I),I=1,LINEAR)                 GDH1095
         READ(JRES)((DELDIP(J,I),J=1,3),I=1,IPT)                        GDH1095
         N33=NVAR*NVAR 
         READ(JRES)(EVECS(I),I=1,N33)                                   GDH1095
         READ(JRES)JSTART,(FCONST(I),I=1,NVAR)                          GDH1095
         RETURN 
      ELSE 
C 
C    WRITE FORCE DATA 
C 
         REWIND JRES                                                    GDH1095
         IF(TIME.GT.1.D6)TIME=TIME-1.D6 
         WRITE(JRES)TIME,IPT,REFH                                       GDH1095
         LINEAR=(NVAR*(NVAR+1))/2 
         WRITE(JRES)(COORD(I),I=1,NVAR)                                 GDH1095
         WRITE(JRES)(FMATRX(I),I=1,LINEAR)                              GDH1095
         WRITE(JRES)((DELDIP(J,I),J=1,3),I=1,IPT)                       GDH1095
         N33=NVAR*NVAR 
         WRITE(JRES)(EVECS(I),I=1,N33)                                  GDH1095
         WRITE(JRES)JSTART,(FCONST(I),I=1,NVAR)                         GDH1095
         LINEAR=(NORBS*(NORBS+1))/2 
         WRITE(JDEN)(PA(I),I=1,LINEAR)                                  GDH1095
         IF(NALPHA.NE.0)WRITE(JDEN)(PB(I),I=1,LINEAR) 
         IF(IPT.EQ.N3) THEN 
            WRITE(JOUT,'(//10X,''FORCE MATRIX WRITTEN TO DISK'')')      GDH1095
         ELSE 
      ISTOP=1                                                           GDH1095
      IWHERE=71                                                         GDH1095
      RETURN                                                            GDH1095
         ENDIF 
      ENDIF 
      RETURN 
   10 WRITE(JOUT,20)                                                    GDH1095
   20 FORMAT(//,10X, 
     1'INSUFFICIENT DATA ON DISK FILES FOR A FORCE CALCULATION',/10X, 
     2'RESTART. PERHAPS THIS STARTED OF AS A FORCE CALCULATION ',/10X, 
     3'BUT THE GEOMETRY HAD TO BE OPTIMIZED FIRST, IN WHICH CASE ',/10X, 
     4'REMOVE THE KEY-WORD "FORCE".') 
      ISTOP=1                                                           GDH1095
      IWHERE=72                                                         GDH1095
      RETURN                                                            GDH1095
      END 
