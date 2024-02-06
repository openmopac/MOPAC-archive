      SUBROUTINE JCARIN (COORD,XPARAM,FORWRD,B,NCOL) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C***********************************************************************
C     JACOBIAN dCARTESIAN/dINTERNAL, WORKED OUT BY FINITE DIFFERENCE. 
C  INPUT 
C     XPARAM(*) : INTERNAL COORDINATES 
C     FORWRD    : .TRUE. IF FORWARD FINITE DIFFERENCES TO BE USED,
C                 .FALSE. IF CENTRAL FINITE DIFFERENCES IN USE.
C  OUTPUT 
C     B(NVAR,NCOL) : JACOBIAN MATRIX B(I,J)=dCARTESIAN(J)/dINTERNAL(I).
C
C     /SCRACH/ HAS BEEN CONVERTED TO /SCRCHR/ AND /SCRCHI/ FOR AMSOL    
C     revised May 1996 by D. Liotard (stepsize, forwrd)
C***********************************************************************
      COMMON /SCRCHR/ COOLD(3,NUMATM*27),                               GCL0892
     1                DUM3(6*MAXPAR**2+1-3*NUMATM*27)                   GCL0393
     2       /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR) 3GL3092
     3       /MLKSTI/ NUMAT, NDUMMY(4*NUMATM + 7)                       3GL3092
     4       /EULER / TVEC(3,3),ID 
     5       /UCELL / L1L,L2L,L3L,L1U,L2U,L3U,KDUM(6) 
     6       /GEOM  / GEO(3,NUMATM) 
     7       /GEOVAR/ DUMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY             3GL3092
      DIMENSION COORD(3,*),XPARAM(*),B(NVAR,*) 
      LOGICAL FORWRD                                                    DL0496
       SAVE
C     JACOBIAN STEP SIZE:                                               DL0496
      IF (FORWRD) THEN                                                  DL0496
         STEP=1.D-7                                                     DL0496
      ELSE                                                              DL0496
         STEP=1.D-6                                                     DL0496
      ENDIF                                                             DL0496
C     NUMBER OF COLUMNS                                                 DL0496
      NCOL=3*NUMAT 
      IF(ID.NE.0) NCOL=NCOL*(L1U-L1L+1)*(L2U-L2L+1)*(L3U-L3L+1)         DL0496
C 
C     INTERNAL OF CENTRAL POINT 
      DO 10 IVAR=1,NVAR 
   10 GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR) 
C 
      IF (ID.EQ.0) THEN 
C 
C        MOLECULAR SYSTEM 
C        ---------------- 
         DO 30 IVAR=1,NVAR 
C        STEP FORWARD 
         GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)+STEP 
         IF(NDEP.NE.0) CALL SYMTRY 
         CALL GMETRY (GEO,COORD) 
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         DO 20 J=1,NCOL 
   20    B(IVAR,J)=COORD(J,1) 
   30    GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR) 
C        FOLLOWING IF/THEN/ELSE STATEMENT REWORKED BY DL0496
         IF (FORWRD) THEN
C           CENTRAL POINT USED
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            DO 40 IVAR=1,NVAR
            DO 40 J=1,NCOL
   40       B(IVAR,J)=B(IVAR,J)-COORD(J,1)
         ELSE
            DO 60 IVAR=1,NVAR
C           STEP BACKWARD
            GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)-STEP
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            DO 50 J=1,NCOL
   50       B(IVAR,J)=B(IVAR,J)-COORD(J,1)
   60       GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)
         ENDIF
      ELSE 
C 
C        SOLID STATE 
C        ----------- 
         DO 130 IVAR=1,NVAR 
C        STEP FORWARD 
         GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)+STEP 
         IF(NDEP.NE.0) CALL SYMTRY 
         CALL GMETRY (GEO,COORD) 
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         IJ=0 
         DO 120 II=1,NUMAT 
         DO 120 IL=L1L,L1U 
         DO 120 JL=L2L,L2U 
         DO 120 KL=L3L,L3U 
         DO 120 LL=1,3 
         IJ=IJ+1 
  120    B(IVAR,IJ)=COORD(LL,II) 
     .            +TVEC(LL,1)*IL+TVEC(LL,2)*JL+TVEC(LL,3)*KL 
  130    GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR) 
C        FOLLOWING IF/THEN/ELSE STATEMENT REWORKED BY DL0496
         IF (FORWRD) THEN
C           CENTRAL POINT USED
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            IJ=0
            DO 140 II=1,NUMAT
            DO 140 IL=L1L,L1U
            DO 140 JL=L2L,L2U
            DO 140 KL=L3L,L3U
            IJ=IJ+1
            DO 140 LL=1,3
  140       COOLD(LL,IJ)=COORD(LL,II)
     .                  +TVEC(LL,1)*IL+TVEC(LL,2)*JL+TVEC(LL,3)*KL
            DO 150 IVAR=1,NVAR
            DO 150 IJ=1,NCOL
  150       B(IVAR,IJ)=B(IVAR,IJ)-COOLD(IJ,1)
         ELSE
            DO 170 IVAR=1,NVAR
C           STEP BACKWARD
            GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)-STEP
            IF(NDEP.NE.0) CALL SYMTRY
            CALL GMETRY (GEO,COORD)
            IJ=0
            DO 160 II=1,NUMAT
            DO 160 IL=L1L,L1U
            DO 160 JL=L2L,L2U
            DO 160 KL=L3L,L3U
            DO 160 LL=1,3
            IJ=IJ+1
  160       B(IVAR,IJ)=B(IVAR,IJ)-COORD(LL,II)
     .                -TVEC(LL,1)*IL-TVEC(LL,2)*JL-TVEC(LL,3)*KL
  170       GEO(LOC(2,IVAR),LOC(1,IVAR))=XPARAM(IVAR)
         ENDIF
      ENDIF 
      IF (FORWRD) THEN                                                   DL0496
         STEP=1.0D0/STEP                                                 DL0496
      ELSE                                                               DL0496
         STEP=0.5D0/STEP                                                 DL0496
      ENDIF                                                              DL0496
      CALL DSCAL (NCOL*NVAR,STEP,B,1)                                    DJG0496
      RETURN 
      END 
