      SUBROUTINE INTCAR (GEO,COORD) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM) 
     .               ,NA(NUMATM),NB(NUMATM),NC(NUMATM) 
      DIMENSION GEO(3,*),COORD(3,*) 
C*********************************************************************** 
C 
C    INTCAR  COMPUTES COORDINATES FROM BOND-ANGLES AND LENGTHS. 
C *** IT IS ADAPTED FROM THE PROGRAM WRITTEN BY M.J.S. DEWAR. 
C 
C 
C  ON INPUT: 
C         GEO    = ARRAY OF INTERNAL COORDINATES. 
C         NATOMS = NUMBER OF ATOMS, INCLUDING DUMMIES. 
C         NA     = ARRAY OF ATOM LABELS FOR BOND LENGTHS. 
C         NB     =   "          "           ANGLES. 
C         NC     =   "          "           DIHEDRALS. 
C 
C  ON OUTPUT: 
C         COORD  = ARRAY OF CARTESIAN COORDINATES, INCLUDING DUMMIES. 
C 
C  CONVENTION FOR ORIENTATION OF DIHEDRALS: 
C         KLYNE, PRELOG, EXPERIENCA VOL. 16, PP 521 (1960). 
C  NOTE ... THE SAME CONVENTION IS USED IN THE FOLLOWING ROUTINES: 
C  XYZINT, JINCAR 
C 
C*********************************************************************** 
       SAVE
      COORD(1,1)=0.0D00 
      COORD(2,1)=0.0D00 
      COORD(3,1)=0.0D00 
      COORD(1,2)=GEO(1,2) 
      COORD(2,2)=0.0D00 
      COORD(3,2)=0.0D00 
      IF(NATOMS.EQ.2) RETURN 
      CCOS=COS(GEO(2,3)) 
      IF(NA(3).EQ.1)THEN 
         COORD(1,3)=COORD(1,1)+GEO(1,3)*CCOS 
      ELSE 
         COORD(1,3)=COORD(1,2)-GEO(1,3)*CCOS 
      ENDIF 
      COORD(2,3)=GEO(1,3)*SIN(GEO(2,3)) 
      COORD(3,3)=0.0D00 
      DO 100 I=4,NATOMS 
         COSA=COS(GEO(2,I)) 
         MB=NB(I) 
         MC=NA(I) 
         XB=COORD(1,MB)-COORD(1,MC) 
         YB=COORD(2,MB)-COORD(2,MC) 
         ZB=COORD(3,MB)-COORD(3,MC) 
         RBC=1.0D00/SQRT(XB*XB+YB*YB+ZB*ZB) 
         IF (ABS(COSA).LT.0.99999999991D00) GO TO 40 
C 
C     ATOMS MC, MB, AND (I) ARE COLLINEAR 
C 
         RBC=GEO(1,I)*RBC*COSA 
         COORD(1,I)=COORD(1,MC)+XB*RBC 
         COORD(2,I)=COORD(2,MC)+YB*RBC 
         COORD(3,I)=COORD(3,MC)+ZB*RBC 
         GO TO 100 
C 
C     THE ATOMS ARE NOT COLLINEAR 
C 
   40    MA=NC(I) 
         XA=COORD(1,MA)-COORD(1,MC) 
         YA=COORD(2,MA)-COORD(2,MC) 
         ZA=COORD(3,MA)-COORD(3,MC) 
C 
C     ROTATE ABOUT THE Z-AXIS TO MAKE YB=0, AND XB POSITIVE.  IF XYB IS 
C     TOO SMALL, FIRST ROTATE THE Y-AXIS BY 90 DEGREES. 
C 
         XYB=SQRT(XB*XB+YB*YB) 
         K=-1 
         IF (XYB.GT.0.1D00) GO TO 50 
         XPA=ZA 
         ZA=-XA 
         XA=XPA 
         XPB=ZB 
         ZB=-XB 
         XB=XPB 
         XYB=SQRT(XB*XB+YB*YB) 
         K=+1 
C 
C     ROTATE ABOUT THE Y-AXIS TO MAKE ZB VANISH 
C 
   50    COSTH=XB/XYB 
         SINTH=YB/XYB 
         XPA=XA*COSTH+YA*SINTH 
         YPA=YA*COSTH-XA*SINTH 
         SINPH=ZB*RBC 
         COSPH=SQRT(ABS(1.D00-SINPH*SINPH)) 
         XQA=XPA*COSPH+ZA*SINPH 
         ZQA=ZA*COSPH-XPA*SINPH 
C 
C     ROTATE ABOUT THE X-AXIS TO MAKE ZA=0, AND YA POSITIVE. 
C 
         YZA=SQRT(YPA**2+ZQA**2) 
         IF(YZA.LT.2.D-2 )THEN 
            IF(YZA.LT.1.D-4)GOTO 70 
            WRITE(JOUT                                                  GDH1095
     .     ,'(//20X,'' CALCULATION ABANDONED AT THIS POINT'')') 
            WRITE(JOUT,                                                 GDH1095
     .   '(//10X,'' THREE ATOMS BEING USED TO DEFINE THE'',/ 
     110X,'' COORDINATES OF A FOURTH ATOM, WHOSE BOND-ANGLE IS'')') 
            WRITE(JOUT,'(10X,'' NOT ZERO OR 180 DEGREEES, ARE '',       GDH1095
     1''IN AN ALMOST STRAIGHT'',/ 
     210X,'' LINE.  THERE IS A HIGH PROBABILITY THAT THE'',/ 
     310X,'' COORDINATES OF THE ATOM WILL BE INCORRECT.'')') 
            WRITE(JOUT,'(//20X,''THE FAULTY ATOM IS ATOM NUMBER'',I4)')IGDH1095
            CALL GEOUT 
            WRITE(JOUT,                                                 GDH1095
     .        '(//20X,''CARTESIAN COORDINATES UP TO FAULTY ATOM'') 
     1') 
            WRITE(JOUT,'(//5X,''I'',12X,''X'',12X,''Y'',12X,''Z'')')    GDH1095
            DO 60 J=1,I 
   60       WRITE(JOUT,'(I6,F16.5,2F13.5)')J,(COORD(K,J),K=1,3)         GDH1095
            WRITE(JOUT,'(//6X,'' ATOMS'',I3,'','',I3,'', AND'',I3,      GDH1095
     1'' ARE WITHIN'',F7.4,'' ANGSTROMS OF A STRAIGHT LINE'')') 
     2MC,MB,MA,YZA 
         ISTOP=1                                                        GDH1095
         IWHERE=163                                                     GDH1095
         RETURN                                                         GDH1095
         ENDIF 
         COSKH=YPA/YZA 
         SINKH=ZQA/YZA 
         GOTO 80 
   70    CONTINUE 
C 
C   ANGLE TOO SMALL TO BE IMPORTANT 
C 
         COSKH=1.D0 
         SINKH=0.D0 
   80    CONTINUE 
C 
C     COORDINATES :-   A=(XQA,YZA,0),   B=(RBC,0,0),  C=(0,0,0) 
C     NONE ARE NEGATIVE. 
C     THE COORDINATES OF I ARE EVALUATED IN THE NEW FRAME. 
C 
         SINA=SIN(GEO(2,I)) 
         SIND=-SIN(GEO(3,I)) 
         COSD=COS(GEO(3,I)) 
         XD=GEO(1,I)*COSA 
         YD=GEO(1,I)*SINA*COSD 
         ZD=GEO(1,I)*SINA*SIND 
C 
C     TRANSFORM THE COORDINATES BACK TO THE ORIGINAL SYSTEM. 
C 
         YPD=YD*COSKH-ZD*SINKH 
         ZPD=ZD*COSKH+YD*SINKH 
         XPD=XD*COSPH-ZPD*SINPH 
         ZQD=ZPD*COSPH+XD*SINPH 
         XQD=XPD*COSTH-YPD*SINTH 
         YQD=YPD*COSTH+XPD*SINTH 
         IF (K.LT.1) GO TO 90 
         XRD=-ZQD 
         ZQD=XQD 
         XQD=XRD 
   90    COORD(1,I)=XQD+COORD(1,MC) 
         COORD(2,I)=YQD+COORD(2,MC) 
         COORD(3,I)=ZQD+COORD(3,MC) 
  100 CONTINUE 
      RETURN 
      END 
