      SUBROUTINE GMETRY(GEO,COORD) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM), 
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM) 
     2       /EULER / TVEC(3,3), ID 
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),COLCST 
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION GEO(3,*),COORD(3,*) 
      LOGICAL FIRST
      CHARACTER *15 NDIMEN(4) 
       SAVE
      DATA NDIMEN/' MOLECULE     ',' POLYMER       ', 
     1'LAYER STRUCTURE',' SOLID         '/ 
C*********************************************************************** 
C 
C    GMETRY  COMPUTES COORDINATES FROM BOND-ANGLES AND LENGTHS. 
C 
C     THREE SEPARATE OPTIONS EXIST WITHIN GMETRY. THESE ARE: 
C    (A) IF NA(1) IS EQUAL TO 99 (IMPOSSIBLE UNDER NORMAL CIRCUMSTANCES) 
C        THEN GEO IS ASSUMED TO BE IN CARTESIAN RATHER THAN INTERNAL 
C        COORDINATES, AND COORD IS THEN SET EQUAL TO GEO. 
C    (B) IF STEP IS NON-ZERO (THIS IS THE CASE WHEN "SADDLE" IS USED) 
C        THEN GEO IS FIRST MODIFIED BY SHIFTING THE INTERNAL COORDINATES 
C        ALONG A RADIUS FROM GEOA TO PLACE GEO AT ADISTANCESTEPFROMGEOA. 
C    (C) NORMAL CONVERSION FROM INTERNAL TO CARTESIAN COORDINATES, 
C        REMOVING THE DUMMY ATOMS 
C 
C  ON INPUT: 
C         GEO    = ARRAY OF INTERNAL COORDINATES. 
C         NATOMS = NUMBER OF ATOMS, INCLUDING DUMMIES. 
C         NA     = ARRAY OF ATOM LABELS FOR BOND LENGTHS. 
C         NB     =   "          "           ANGLES WITH NA. 
C         NC     =   "          "           DIHEDRALS WITH NB,NA 
C 
C  ON OUTPUT: 
C         COORD  = ARRAY OF CARTESIAN COORDINATES, WITHOUT DUMMIES. 
C 
C*********************************************************************** 
C                                     OPTION (A) 
      IF (ICONTR(36).EQ.1) THEN                                         GDH0195
         ICONTR(36)=2                                                   GDH0195
         FIRST=.TRUE.                                                   GDH0195
      ENDIF                                                             GDH0195
      IF(NA(1).EQ.99) THEN 
         DO 10 I=1,3 
            DO 10 J=1,NATOMS 
   10    COORD(I,J)=GEO(I,J) 
         GOTO 110 
      ENDIF 
C                                     OPTION (B) 
      IF(ABS(STEP) .GT. 1.D-4) THEN 
         SUM=0.D0 
         DO 20 I=1,NATOMS 
            DO 20 J=1,3 
               GEOVEC(J,I)=GEO(J,I)-GEOA(J,I) 
   20    SUM=SUM+GEOVEC(J,I)**2 
         SUM=SQRT(SUM) 
         ERROR=(SUM-STEP)/SUM 
         DO 30 I=1,NATOMS 
         DO 30 J=1,3 
   30    GEO(J,I)=GEO(J,I)-ERROR*GEOVEC(J,I) 
      ENDIF 
C                                     OPTION (C) 
      CALL INTCAR (GEO,COORD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
C 
C *** NOW REMOVE THE TRANSLATION VECTORS, IF ANY, FROM THE ARRAY COOR 
C 
  110 CONTINUE 
      K=NATOMS 
  120 IF(LABELS(K).NE.107) GOTO 130 
      K=K-1 
      GOTO 120 
  130 K=K+1 
      IF(K.GT.NATOMS) GOTO 180 
C 
C   SYSTEM IS A SOLID, OF DIMENSION NATOMS+1-K 
C 
      L=0 
      DO 140 I=K,NATOMS 
         L=L+1 
         MC=NA(I) 
         DO 140 J=1,3 
            TVEC(J,L)=COORD(J,I)-COORD(J,MC) 
  140 CONTINUE 
      ID=L 
      IF (FIRST) THEN  
         FIRST=.FALSE.
         WRITE(JOUT,150)NDIMEN(ID+1) 
  150    FORMAT(/10X,'    THE SYSTEM IS A ',A15,/) 
         IF(ID.EQ.0) GOTO 180 
         WRITE(JOUT,160) 
         WRITE(JOUT,170)(I,(TVEC(J,I),J=1,3),I=1,ID) 
  160    FORMAT(/,'                UNIT CELL TRANSLATION VECTORS',/ 
     1/,'              X              Y              Z') 
  170    FORMAT('    T',I1,' = ',F11.7,'    ',F11.7,'    ',F11.7) 
      ENDIF 
  180 CONTINUE 
C 
C *** REMOVE TRANSLATION VECTORS AND DUMMY ATOMS 
C 
      J=0 
      DO 200 I=1,NATOMS 
         IF (LABELS(I).EQ.99.OR.LABELS(I).EQ.107) GO TO 200 
         J=J+1 
         DO 190 K=1,3 
  190    COORD(K,J)=COORD(K,I) 
  200 CONTINUE 
      CUTOFF=200.D0 
      RETURN 
      END 
