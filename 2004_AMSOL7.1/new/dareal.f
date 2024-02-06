      SUBROUTINE DAREAL (RAD,NUMAT,AREA,DAREA,NCROSS,NC,K,LGRX
     .                  ,DAREAR,LGRR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'SIZES2.i'
      PARAMETER (MXSS=2*NUMATM+1)
C ---------------------------------------------------------------------+
C     ACCESSIBLE SOLID ANGLE OF SPHERES (RADIANS). ANALYTICAL.
C     ALLOW MULTIPLE CONNECTED COMPONENTS ON A SPHERE.
C     REGULARIZE RARE EVENTS (POINTS SHARED BY MORE THAN 3 SPHERES)
C     BY A SMALL PERTURBATION ON THE RADIUS.
C     NOTE THAT THE STRICT AREA IS ONLY A (C0) FUNCTION.
C     AREA .  .  .  .  .  .  .  .  .  .  .  . D. LIOTARD, DECEMBER 1992.
C     CARTESIAN DERIVATIVES  .  .  .  .  .  . D. RINALDI,      MAY 1994.
C     DERIVATIVE WITH RESPECT TO RAD(K)  .  . D. LIOTARD,  OCTOBER 1997.
C INPUT
C     RAD(NUMAT)       : RADII.
C     NUMAT            : NUMBER OF SPHERES.
C     K                : NO OF THE SPHERE STUDIED.
C     LGRX             : .TRUE. TO CALCULATE CARTESIAN DERIVATIVES.
C     LGRR             : .TRUE. TO CALCULATE DERIVATIVE VS RAD(K).
C     RLIO(NUMAT,NUMAT): INTERCENTRE DISTANCES,
C     URLIO(3,i,j)     : ASSOCIATED UNIT VECTORS: FROM i TO j.
C OUTPUT
C     AREA          : ACCESSIBLE SOLID ANGLE FOR SPHERE NO K.
C     NCROSS        : NUMBER OF SPHERES OVERLAPPING K.
C     NC            : INDICE OF SPHERES CONNECTED TO K.
C   AND IF LGRX = .TRUE. :
C     DAREA         : DERIVATIVES OF THE ACCESSIBLE SOLID ANGLE WITH
C                     RESPECT TO THE CARTESIAN COORDINATES.
C     THAT IS: DAREA(I,J) = dAREA(K)/dCOORD(I,NC(J)) ... J=0,NCROSS.
C   AND IF LGRR = .TRUE. :
C     DAREAR        : DERIVATIVE OF THE ACCESSIBLE SOLID ANGLE WITH
C                     RESPECT TO RAD(K).
C MODIFICATION NOTES
C     IMPLEMENTATION IN AMPAC-6.56,                           DL, MAR 2K
C     MIGRATED TO F90, REMOVING /SCRACH,/SCRAH2/,             DL, NOV 02
C     REGULARIZATION (C2) OF CUSP ON SPHERICAL SLICE,         DL, MAR 03
C ---------------------------------------------------------------------+
      LOGICAL FREEIJ,FREEJI,CONECT,LPOLY,LGRX,LGRR,FIRST
      DIMENSION RAD(*),NC(0:*),DAREA(3,0:NUMATM)
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)
     2       /SCRCHI/ LAB(NUMATM),NCNCT(MXSS,NUMATM)
     3               ,CONECT(NUMATM,NUMATM)
      COMMON /SCRCHR/ CTHETA(NUMATM,NUMATM),STHETA(NUMATM),SIT(NUMATM)
     1,DCSIT(3,NUMATM),DJCOSN(3,3,NUMATM,NUMATM),COSN(3,NUMATM,NUMATM)
     2,DSTETA(3,NUMATM),DCTETA(3,NUMATM,NUMATM),DCOSN(3,3,NUMATM,NUMATM)
     3,WORK(MXSS),DIWORK(3,NUMATM),DJWORK(3,NUMATM),DKWORK(3,NUMATM)
     4,D0WORK(3,NUMATM),DWORKR(NUMATM),DCAODD(3,0:NUMATM),DSITR(NUMATM)
     5,DCAPLY(3,0:NUMATM),DICOSN(3,3,NUMATM,NUMATM)
     6,DCTETR(NUMATM),DSTETR(NUMATM),DCOSNR(3,NUMATM,NUMATM),DCNIJR(3)
     7,DVIJR(3),DCNIKR(3),DCC2I(3),DKCNIK(3,3),DAPHI(3),DBPHI(3),VIJ(3)
     8,DICOS(3,3),DJCOS(3,3),VN(3),DIVN(3,3),DJVN(3,3),DIVIJ(3,3)
     9,DJVIJ(3,3),DIWIJ(3,3),DJWIJ(3,3),DIAIJ(3),DJAIJ(3),DIBIJ(3)
     A,DJBIJ(3),DICIJ(3),DJCIJ(3),CNIJ(3),DICNIJ(3,3),DJCNIJ(3,3)
     B,CNIK(3),DICNIK(3,3)
C     EPSI:THRESHOLD TO AVOID DRAMATIC CONSEQUENCES OF ROUND-OFF ERRORS,
C     MUST BE GREATER THAN THE POOREST OF RELATIVE ERRORS ON FUNCTIONS:
C     SQRT, ATAN.
C
C     EPSC: SIZE OF INTERVAL OF REGULARIZATION OF SS CUSP.
C     R(i,k) REPLACED WITH R(i,k)+W. W = POLYNOME OF ORDER 4 IN R(I,K),
C     SUCH THAT (C2) AT LIMITS OF THE INTERVAL (CUSP-EPSC, CUSP+EPSC).
C     W EVOLVES BETWEEN 0 AND (+-)EPSC, SO AS TO "ELONGATE" THE CUSP,
C     MAKING ITS 1ST AND 2ND ORDER DERIVATIVES ZEROED AT THE "CUSP".
C     GENERAL FORMALISM KEPT UNCHANGED, BUT DISTORTED BY W.
C     LET R = CUSP+u (u INFINITELY SMALL). TAYLOR EXPANSION READS:
C      CTHETA(I,I)      --> (+-)1 (u**3)    STHETA(I) --> 0 (u**3/2)
C     dCTHETA(I,I)/dR   --> 0     (u**2)   dSTHETA(I) --> 0 (u**1/2)
C     d2CTHETA(I,I)/dR2 --> 0     (u**1)   d2STHETA(I) -> infinite
C     NOTE. STHETA AND dSTHETA ARE UNLIKELY TO BE USEFUL: ONE MUST HAVE
C           SEVERAL CUSPS OCCURING SIMULTANEOUSLY AT THE SAME POINT,
C           AND SPHERES NOT BURRIED BY THEMSELVES!!!
C
      SAVE FIRST,FOURPI,W0,W1,W2,W4
      DATA EPSI/1D-11/, EPSC /2D-3/, FIRST /.TRUE./
      DATA TWOPI /6.283185307179586476925286766558 D0/
      IF (FIRST) THEN
         FIRST=.FALSE.
         PI=TWOPI*0.5D0
         FOURPI=TWOPI+TWOPI
         W0=3D0*EPSC/16D0
         W1=-0.5D0
         W2=3D0/(8D0*EPSC)
         W4=-1D0/(16D0*EPSC**3)
      ENDIF
      RK=RAD(K)
      NC(0)=K
      IF (LGRX) THEN
          DAREA(1,0)=0D0
          DAREA(2,0)=0D0
          DAREA(3,0)=0D0
      ENDIF
      IF (LGRR) DAREAR=0D0
      IF (RK.LE.0D0) THEN
         NCROSS=0
         AREA=0D0
         GOTO 300
      ELSE
C        PRESET ACCESSIBLE SOLID ANGLES TO "FREE" SPHERES.
         AREA=FOURPI
      ENDIF
   10 NCROSS=0
      EPSK=EPSI*RK
C     CHECK IF SPHERE K NOT BURIED; SET UP FLAG FOR OVERLAP OF K.
C     -----------------------------------------------------------
      DO 20 I=1,NUMAT
      IF (I.EQ.K.OR.RAD(I).LE.0D0) GOTO 20
      IF (ABS(RK+RLIO(I,K)-RAD(I)).LT.EPSC) THEN
C        CUSP CASE 1: RAD(K)+R(I,K)=RAD(I), (+- EPSC)
         NCROSS=NCROSS+1
         NC(NCROSS)=I
         LAB(NCROSS)=1
      ELSE IF (ABS(RLIO(I,K)+RAD(I)-RK).LT.EPSC) THEN
C        CUSP CASE 2: RAD(I)+RLIO(I,K)=RAD(K), (+- EPSC)
         NCROSS=NCROSS+1
         NC(NCROSS)=I
         LAB(NCROSS)=2
      ELSE IF (ABS(RLIO(I,K)-RAD(I)-RK).LT.EPSC) THEN
C        CUSP CASE 3: RAD(I)+RAD(K)=R(I,K), (+- EPSC)
         NCROSS=NCROSS+1
         NC(NCROSS)=I
         LAB(NCROSS)=3
      ELSE
C        OTHER REGULAR CASES (FAR AWAYS FROM A CUSP).
         IF (RK+RAD(I)-RLIO(I,K).LT.EPSK) GOTO 20
         IF (RLIO(I,K)-ABS(RK-RAD(I)).LT.EPSK) THEN
            IF (RK.LE.RAD(I)) THEN
C              SPHERE K IMBEDDED IN SPHERE I
               AREA=0D0
               NCROSS=0
               GOTO 300
            ENDIF
         ELSE
            NCROSS=NCROSS+1
            NC(NCROSS)=I
            LAB(NCROSS)=0
         ENDIF
      ENDIF
   20 CONTINUE
      IF (NCROSS.EQ.0) THEN
C        THE SPHERE K IS FREE.
         GOTO 300
      ENDIF
C     SET UP DATA FOR SPHERICAL SEGMENTS (SS).
C     ----------------------------------------
      RKINV=0.5D0/RK
      RK2=RK**2
      DO 39 I=1,NCROSS
      NCNCT(I,1)=NC(I)
      LI=NCNCT(I,1)
      CONECT(I,I)=.FALSE.
C     DIRECTOR COSINES OF VECTOR KI.
      COSN(1,I,I)=-URLIO(1,LI,K)
      COSN(2,I,I)=-URLIO(2,LI,K)
      COSN(3,I,I)=-URLIO(3,LI,K)
C     SINE, COSINE OF HALF-ANGLE OF CONE. PLUS DERIVATIVES
      IF (LAB(I).EQ.0) THEN
         RIKINV=1D0/RLIO(LI,K)
         CTHETA(I,I)=RKINV*(RLIO(LI,K)+(RK2-RAD(LI)**2)*RIKINV)
         STHETA(I)=SQRT(1D0-CTHETA(I,I)**2)
         IF (LGRX.OR.LGRR) THEN
            X=-CTHETA(I,I)/STHETA(I)
            DRCTHT=RKINV*(1D0-(RK2-RAD(LI)**2)*RIKINV**2)
            IF (LGRX) THEN
               DO 31 ICOR=1,3
               DCTETA(ICOR,I,I)=DRCTHT*COSN(ICOR,I,I)
               DSTETA(ICOR,I)=X*DCTETA(ICOR,I,I)
               COSNI=COSN(ICOR,I,I)*RIKINV
               DO 30 JCOR=1,ICOR
               DCOSN(ICOR,JCOR,I,I)=-COSNI*COSN(JCOR,I,I)
               DCOSN(JCOR,ICOR,I,I)=DCOSN(ICOR,JCOR,I,I)
   30          CONTINUE
               DCOSN(ICOR,ICOR,I,I)=DCOSN(ICOR,ICOR,I,I)+RIKINV
   31          CONTINUE
            ENDIF
            IF (LGRR) THEN
               DCTETR(I)=RIKINV-CTHETA(I,I)/RK
               DSTETR(I)=X*DCTETR(I)
            ENDIF
         ENDIF
      ELSE IF (LAB(I).EQ.1) THEN
         WX=RLIO(LI,K)-RAD(LI)+RK
         CUSP=((W4*WX**2+W2)*WX+W1)*WX+W0+RLIO(LI,K)
         RIKINV=1D0/CUSP
         GAMMA=
     .   MAX(1D-16,((RK+RAD(LI))*RIKINV+1D0)*(CUSP+RK-RAD(LI))*RKINV)
         CTHETA(I,I)=GAMMA-1D0
         STHETA(I)=SQRT(GAMMA*(2D0-GAMMA))
         IF (LGRX.OR.LGRR) THEN
            X=-CTHETA(I,I)/STHETA(I)
            DCTHT=RKINV*(1D0-(RK2-RAD(LI)**2)*RIKINV**2)
            DCUSP=(4D0*W4*WX**2+2D0*W2)*WX+W1
            IF (LGRX) THEN
               DRCTHT=DCTHT+DCUSP*DCTHT
               DO 33 ICOR=1,3
               DCTETA(ICOR,I,I)=DRCTHT*COSN(ICOR,I,I)
               DSTETA(ICOR,I)=X*DCTETA(ICOR,I,I)
               COSNI=COSN(ICOR,I,I)*RIKINV
               DO 32 JCOR=1,ICOR
               DCOSN(ICOR,JCOR,I,I)=-COSNI*COSN(JCOR,I,I)
               DCOSN(JCOR,ICOR,I,I)=DCOSN(ICOR,JCOR,I,I)
   32          CONTINUE
               DCOSN(ICOR,ICOR,I,I)=DCOSN(ICOR,ICOR,I,I)+RIKINV
   33          CONTINUE
            ENDIF
            IF (LGRR) THEN
               DCTETR(I)=RIKINV-CTHETA(I,I)/RK+DCUSP*DCTHT
               DSTETR(I)=X*DCTETR(I)
            ENDIF
         ENDIF
      ELSE IF (LAB(I).EQ.2) THEN
         WX=RLIO(LI,K)+RAD(LI)-RK
         CUSP=((W4*WX**2+W2)*WX+W1)*WX+W0+RLIO(LI,K)
         RIKINV=1D0/CUSP
         GAMMA=
     .   MAX(1D-16,((RK+RAD(LI))*RIKINV-1D0)*(CUSP-RK+RAD(LI))*RKINV)
         CTHETA(I,I)=1D0-GAMMA
         STHETA(I)=SQRT(GAMMA*(2D0-GAMMA))
         IF (LGRX.OR.LGRR) THEN
            X=-CTHETA(I,I)/STHETA(I)
            DCTHT=RKINV*(1D0-(RK2-RAD(LI)**2)*RIKINV**2)
            DCUSP=-(4D0*W4*WX**2+2D0*W2)*WX-W1
            IF (LGRX) THEN
               DRCTHT=DCTHT-DCUSP*DCTHT
               DO 35 ICOR=1,3
               DCTETA(ICOR,I,I)=DRCTHT*COSN(ICOR,I,I)
               DSTETA(ICOR,I)=X*DCTETA(ICOR,I,I)
               COSNI=COSN(ICOR,I,I)*RIKINV
               DO 34 JCOR=1,ICOR
               DCOSN(ICOR,JCOR,I,I)=-COSNI*COSN(JCOR,I,I)
               DCOSN(JCOR,ICOR,I,I)=DCOSN(ICOR,JCOR,I,I)
   34          CONTINUE
               DCOSN(ICOR,ICOR,I,I)=DCOSN(ICOR,ICOR,I,I)+RIKINV
   35          CONTINUE
            ENDIF
            IF (LGRR) THEN
               DCTETR(I)=RIKINV-CTHETA(I,I)/RK+DCUSP*DCTHT
               DSTETR(I)=X*DCTETR(I)
            ENDIF
         ENDIF
      ELSE
         WX=RLIO(LI,K)-RAD(LI)-RK
         CUSP=((-W4*WX**2-W2)*WX+W1)*WX-W0+RLIO(LI,K)
         RIKINV=1D0/CUSP
         GAMMA=
     .   MAX(1D-16,((RK+RAD(LI))*RIKINV-1D0)*(CUSP-RK+RAD(LI))*RKINV)
         CTHETA(I,I)=1D0-GAMMA
         STHETA(I)=SQRT(GAMMA*(2D0-GAMMA))
         IF (LGRX.OR.LGRR) THEN
            X=-CTHETA(I,I)/STHETA(I)
            DCTHT=RKINV*(1D0-(RK2-RAD(LI)**2)*RIKINV**2)
            DCUSP=(4D0*W4*WX**2+2D0*W2)*WX-W1
            IF (LGRX) THEN
               DRCTHT=DCTHT-DCUSP*DCTHT
               DO 37 ICOR=1,3
               DCTETA(ICOR,I,I)=DRCTHT*COSN(ICOR,I,I)
               DSTETA(ICOR,I)=X*DCTETA(ICOR,I,I)
               COSNI=COSN(ICOR,I,I)*RIKINV
               DO 36 JCOR=1,ICOR
               DCOSN(ICOR,JCOR,I,I)=-COSNI*COSN(JCOR,I,I)
               DCOSN(JCOR,ICOR,I,I)=DCOSN(ICOR,JCOR,I,I)
   36          CONTINUE
               DCOSN(ICOR,ICOR,I,I)=DCOSN(ICOR,ICOR,I,I)+RIKINV
   37          CONTINUE
            ENDIF
            IF (LGRR) THEN
               DCTETR(I)=RIKINV-CTHETA(I,I)/RK+DCUSP*DCTHT
               DSTETR(I)=X*DCTETR(I)
            ENDIF
         ENDIF
      ENDIF
   39 CONTINUE

C     CONNECTIVITY MATRIX BETWEEN SS.
C     -------------------------------
C     TRUE IIF CONNECTED. DIAGONAL=TRUE IIF IMBEDDED.
      DO 42 I=2,NCROSS
      DO 41 J=1,I-1
      IF (CONECT(J,J)) GOTO 41
      CISJ=CTHETA(I,I)*STHETA(J)
      SICJ=STHETA(I)*CTHETA(J,J)
      SISJ=STHETA(I)*STHETA(J)
C     COSINES OF ANGLES BETWEEN VECTOR KI AND KJ.
      CTHETA(J,I)=DOT(COSN(1,I,I),COSN(1,J,J),3)
      IF (LGRX) THEN
         DO 40 ICOR=1,3
         DCTETA(ICOR,I,J)=DOT(DCOSN(1,ICOR,I,I),COSN(1,J,J),3)
         DCTETA(ICOR,J,I)=DOT(COSN(1,I,I),DCOSN(1,ICOR,J,J),3)
   40    CONTINUE
      ENDIF
      TIJ=CTHETA(J,I)-CTHETA(I,I)*CTHETA(J,J)
      IF (TIJ.GT.SISJ-EPSI*ABS(CISJ-SICJ)) THEN
         IF (CTHETA(J,J).GT.CTHETA(I,I)) THEN
C           SS J IMBEDDED IN SS I
            CONECT(J,J)=.TRUE.
         ELSE
C           SS I IMBEDDED IN SS J
            CONECT(I,I)=.TRUE.
            GOTO 42
         ENDIF
      ELSE
         EPSIJ=EPSI*(SICJ+CISJ)
         IF (SICJ+CISJ.GE.0D0) THEN
            CONECT(J,I)=TIJ.GT.EPSIJ-SISJ
         ELSE
            IF (TIJ.LE.-SISJ-EPSIJ) THEN
C              SPHERE K FULLY COVERED BY SS I PLUS SS J
               NCROSS=0
               AREA=0D0
               GOTO 300
            ELSE
               CONECT(J,I)=.TRUE.
            ENDIF
         ENDIF
         CONECT(I,J)=CONECT(J,I)
      ENDIF
   41 CONTINUE
   42 CONTINUE
C     SUM THE CONTRIBUTIONS FROM ISOLATED SS.
C     ---------------------------------------
      ASLICE=0D0
      IF (LGRX) THEN
         DO J=0,NCROSS
         DAREA(1,J)=0D0
         DAREA(2,J)=0D0
         DAREA(3,J)=0D0
         ENDDO
      ENDIF
      IF (LGRR) DASLIR=0D0
      NCLUST=0
      DO 60 I=1,NCROSS
      IF (CONECT(I,I)) GOTO 60
      DO 50 J=1,NCROSS
      IF (CONECT(J,J)) GOTO 50
      IF (CONECT(J,I)) THEN
         NCLUST=NCLUST+1
C        LABEL OF NON-ISOLATED SS: LAB
         LAB(NCLUST)=I
C        PREPARE DATA FOR FURTHER USE
         WORK(NCLUST)=CTHETA(I,I)+EPSI*STHETA(I)
         WORK(NCLUST+NCROSS)=CTHETA(I,I)-EPSI*STHETA(I)
         GOTO 60
      ENDIF
   50 CONTINUE
      ASLICE=ASLICE+1D0-CTHETA(I,I)
      IF (LGRX) THEN
         DO 55 ICOR=1,3
         DAREA(ICOR,I)=DAREA(ICOR,I)-DCTETA(ICOR,I,I)
         DAREA(ICOR,0)=DAREA(ICOR,0)+DCTETA(ICOR,I,I)
   55    CONTINUE
      ENDIF
      IF (LGRR) DASLIR=DASLIR-DCTETR(I)
   60 CONTINUE
      ASLICE=TWOPI*ASLICE
      IF (LGRX) CALL DSCAL (3*(NCROSS+1),TWOPI,DAREA(1,0),1)
      IF (LGRR) DASLIR=DASLIR*TWOPI
      IF (NCLUST.EQ.0) THEN
C        NO CLUSTER OF SS REMAINS ON SPHERE K.
         AREA=FOURPI-ASLICE
         IF (LGRX) THEN
            DO 61 I=0,NCROSS
            DAREA(1,I)=-DAREA(1,I)
            DAREA(2,I)=-DAREA(2,I)
            DAREA(3,I)=-DAREA(3,I)
   61       CONTINUE
         ENDIF
         IF (LGRR) DAREAR=-DASLIR
         GOTO 300
      ENDIF
C     "FREE" INTERSECTIONS BETWEEN CLUSTERED SS.
C     ------------------------------------------
      NFREE=0
      NCNCT(MXSS,LAB(1))=0
      DO 80 I=2,NCLUST
      LI=LAB(I)
      NCNCT(MXSS,LI)=0
      DO 80 J=1,I-1
      LJ=LAB(J)
      IF (CONECT(LJ,LI)) THEN
C        DIRECTOR COSINES OF INTERSECTION POINTS BETWEEN SS
         SIN2IJ=1D0/(1D0-CTHETA(LJ,LI)**2)
         AIJ=(CTHETA(LI,LI)-CTHETA(LJ,LJ)*CTHETA(LJ,LI))*SIN2IJ
         BIJ=(CTHETA(LJ,LJ)-CTHETA(LI,LI)*CTHETA(LJ,LI))*SIN2IJ
         CIJ=SQRT((1D0-AIJ*CTHETA(LI,LI)-BIJ*CTHETA(LJ,LJ))*SIN2IJ)
C        COMPUTE  VN = COSN(LI,LI) X COSN(LJ,LJ).
         VN(1)=COSN(2,LI,LI)*COSN(3,LJ,LJ)-COSN(3,LI,LI)*COSN(2,LJ,LJ)
         VN(2)=COSN(3,LI,LI)*COSN(1,LJ,LJ)-COSN(1,LI,LI)*COSN(3,LJ,LJ)
         VN(3)=COSN(1,LI,LI)*COSN(2,LJ,LJ)-COSN(2,LI,LI)*COSN(1,LJ,LJ)
         IF (LGRX.OR.LGRR) THEN
            DCIJ=0.5D0/CIJ
            IF (LGRX) THEN
               DO 62 ICOR=1,3
               DIVN(1,ICOR)=DCOSN(2,ICOR,LI,LI)*COSN(3,LJ,LJ)
     .                     -DCOSN(3,ICOR,LI,LI)*COSN(2,LJ,LJ)
               DIVN(2,ICOR)=DCOSN(3,ICOR,LI,LI)*COSN(1,LJ,LJ)
     .                     -DCOSN(1,ICOR,LI,LI)*COSN(3,LJ,LJ)
               DIVN(3,ICOR)=DCOSN(1,ICOR,LI,LI)*COSN(2,LJ,LJ)
     .                     -DCOSN(2,ICOR,LI,LI)*COSN(1,LJ,LJ)
               DJVN(1,ICOR)=COSN(2,LI,LI)*DCOSN(3,ICOR,LJ,LJ)
     .                     -COSN(3,LI,LI)*DCOSN(2,ICOR,LJ,LJ)
               DJVN(2,ICOR)=COSN(3,LI,LI)*DCOSN(1,ICOR,LJ,LJ)
     .                     -COSN(1,LI,LI)*DCOSN(3,ICOR,LJ,LJ)
               DJVN(3,ICOR)=COSN(1,LI,LI)*DCOSN(2,ICOR,LJ,LJ)
     .                     -COSN(2,LI,LI)*DCOSN(1,ICOR,LJ,LJ)
   62          CONTINUE
               DSN2IJ=2D0*CTHETA(LJ,LI)*SIN2IJ
            ENDIF
            IF (LGRR) THEN
               DAIJR=(DCTETR(LI)-DCTETR(LJ)*CTHETA(LJ,LI))*SIN2IJ
               DBIJR=(DCTETR(LJ)-DCTETR(LI)*CTHETA(LJ,LI))*SIN2IJ
               DCIJR=-SIN2IJ*(DAIJR*CTHETA(LI,LI)+AIJ*DCTETR(LI)
     .         +DBIJR*CTHETA(LJ,LJ)+BIJ*DCTETR(LJ))*DCIJ
            ENDIF
         ENDIF
         DO 63 ICOR=1,3
C        INTERSECTION PIJ
         COSN(ICOR,LI,LJ)=AIJ*COSN(ICOR,LI,LI)+BIJ*COSN(ICOR,LJ,LJ)
     .   +CIJ*VN(ICOR)
C        INTERSECTION PJI
         COSN(ICOR,LJ,LI)=AIJ*COSN(ICOR,LI,LI)+BIJ*COSN(ICOR,LJ,LJ)
     .   -CIJ*VN(ICOR)
         IF (LGRX) THEN
            DIS2IJ=DSN2IJ*DCTETA(ICOR,LI,LJ)
            DJS2IJ=DSN2IJ*DCTETA(ICOR,LJ,LI)
            DIAIJ(ICOR) =
     .      (DCTETA(ICOR,LI,LI)-CTHETA(LJ,LJ)*DCTETA(ICOR,LI,LJ))
     .      * SIN2IJ + AIJ*DIS2IJ
            DJAIJ(ICOR) =
     .      (-DCTETA(ICOR,LJ,LJ)*CTHETA(LJ,LI)-CTHETA(LJ,LJ) *
     .      DCTETA(ICOR,LJ,LI))*SIN2IJ + AIJ*DJS2IJ
            DJBIJ(ICOR) =
     .      (DCTETA(ICOR,LJ,LJ)-CTHETA(LI,LI)*DCTETA(ICOR,LJ,LI))
     .      * SIN2IJ + BIJ*DJS2IJ
            DIBIJ(ICOR) =
     .      (-DCTETA(ICOR,LI,LI)*CTHETA(LJ,LI)-CTHETA(LI,LI) *
     .      DCTETA(ICOR,LI,LJ))*SIN2IJ + BIJ*DIS2IJ
            DICIJ(ICOR) =
     .      -DCIJ*((DIAIJ(ICOR)*CTHETA(LI,LI)+AIJ*DCTETA(ICOR,LI,LI)
     .      + DIBIJ(ICOR)*CTHETA(LJ,LJ))*SIN2IJ) +
     .      0.5D0*CIJ*DIS2IJ
            DJCIJ(ICOR) =
     .      -DCIJ*((DJBIJ(ICOR)*CTHETA(LJ,LJ)+BIJ*DCTETA(ICOR,LJ,LJ)
     .      + DJAIJ(ICOR)*CTHETA(LI,LI))*SIN2IJ) +
     .      0.5D0*CIJ*DJS2IJ
         ENDIF
         IF (LGRR) THEN
            DCOSNR(ICOR,LI,LJ)=DAIJR*COSN(ICOR,LI,LI)
     .      +DBIJR*COSN(ICOR,LJ,LJ)+DCIJR*VN(ICOR)
            DCOSNR(ICOR,LJ,LI)=DAIJR*COSN(ICOR,LI,LI)
     .      +DBIJR*COSN(ICOR,LJ,LJ)-DCIJR*VN(ICOR)
         ENDIF
   63    CONTINUE
         IF (LGRX) THEN
            DO 64 ICOR=1,3
            DO 64 JCOR=1,3
            DICOS(ICOR,JCOR)=DIAIJ(JCOR)*COSN(ICOR,LI,LI) +
     .      AIJ*DCOSN(ICOR,JCOR,LI,LI)+DIBIJ(JCOR)*COSN(ICOR,LJ,LJ)
            DJCOS(ICOR,JCOR)=DJAIJ(JCOR)*COSN(ICOR,LI,LI) +
     .      DJBIJ(JCOR)*COSN(ICOR,LJ,LJ)+BIJ*DCOSN(ICOR,JCOR,LJ,LJ)
            DIWIJ(ICOR,JCOR)=DICIJ(JCOR)*VN(ICOR)+CIJ*DIVN(ICOR,JCOR)
            DJWIJ(ICOR,JCOR)=DJCIJ(JCOR)*VN(ICOR)+CIJ*DJVN(ICOR,JCOR)
            DICOSN(ICOR,JCOR,LI,LJ)=DICOS(ICOR,JCOR)+DIWIJ(ICOR,JCOR)
            DJCOSN(ICOR,JCOR,LI,LJ)=DJCOS(ICOR,JCOR)+DJWIJ(ICOR,JCOR)
            DICOSN(ICOR,JCOR,LJ,LI)=DICOS(ICOR,JCOR)-DIWIJ(ICOR,JCOR)
            DJCOSN(ICOR,JCOR,LJ,LI)=DJCOS(ICOR,JCOR)-DJWIJ(ICOR,JCOR)
   64       CONTINUE
         ENDIF
C        INTERSECTION PIJ
         FREEIJ=.TRUE.
C        INTERSECTION PJI
         FREEJI=.TRUE.
C        SET UP TABLE OF FREE INTERSECTIONS BETWEEN SS.
C        HERE IS THE L**3 PART OF THE CODE (L= MEAN VALUE OF NEIGHBORS)
         DO 70 L=1,NCLUST
         IF (L.EQ.I.OR.L.EQ.J) GOTO 70
         LL=LAB(L)
         IF (CONECT(LL,LI).AND.CONECT(LL,LJ)) THEN
            IF (FREEJI) THEN
               CHEK=DOT(COSN(1,LJ,LI),COSN(1,LL,LL),3)
               IF (CHEK.GT.WORK(L)) THEN
                  FREEJI=.FALSE.
               ELSE
                  IF (CHEK.GE.WORK(L+NCROSS)) THEN
C                    SPHERES K, LAB(I), LAB(J), LAB(L) SHARE A POINT (AT
C                    THRESHOLD EPSI IN ANGLE). INCREASE THE RADIUS OF
C                    SPHERE K AND RESTART STUDY OF SPHERE K.
                     RK=RK*(1D0+4D0*EPSI)
                     GOTO 10
                  ENDIF
               ENDIF
            ENDIF
            IF (FREEIJ) THEN
               CHEK=DOT(COSN(1,LI,LJ),COSN(1,LL,LL),3)
               IF (CHEK.GT.WORK(L)) THEN
                  FREEIJ=.FALSE.
               ELSE IF (CHEK.GE.WORK(L+NCROSS)) THEN
                  RK=RK*(1D0+4D0*EPSI)
                  GOTO 10
               ENDIF
            ENDIF
            IF (.NOT.(FREEIJ.OR.FREEJI)) GOTO 80
         ENDIF
   70    CONTINUE
C        ONE OR TWO FREE INTERSECTIONS FOUND. UPDATE TABLE.
         CTHETA(LI,LJ)=CTHETA(LJ,LI)
         IF (FREEJI) THEN
            NFREE=NFREE+1
            M=NCNCT(MXSS,LI)+1
            NCNCT(M,LI)=LJ
            NCNCT(MXSS,LI)=M
            M=NCNCT(MXSS,LJ)+1
            NCNCT(M,LJ)=-LI
            NCNCT(MXSS,LJ)=M
         ENDIF
         IF (FREEIJ) THEN
            NFREE=NFREE+1
            M=NCNCT(MXSS,LI)+1
            NCNCT(M,LI)=-LJ
            NCNCT(MXSS,LI)=M
            M=NCNCT(MXSS,LJ)+1
            NCNCT(M,LJ)=LI
            NCNCT(MXSS,LJ)=M
         ENDIF
      ENDIF
   80 CONTINUE
      IF (NFREE.EQ.0) THEN
C        SPHERE K BURIED BY A CLUSTER OF SS
         NCROSS=0
         AREA=0D0
         GOTO 300
      ENDIF
C     ORIENTED ANGLES OF SLICES OF SS.
C     --------------------------------
      APOLY=0D0
      IF (LGRX) THEN
         DO J=0,NCROSS
         DCAPLY(1,J)=0D0
         DCAPLY(2,J)=0D0
         DCAPLY(3,J)=0D0
         ENDDO
      ENDIF
      IF (LGRR) DAPOLR=0D0
      DO 130 I=1,NCLUST
      IF (LGRX) THEN
         DO J=0,NCROSS
         DCAODD(1,J)=0D0
         DCAODD(2,J)=0D0
         DCAODD(3,J)=0D0
         ENDDO
      ENDIF
      LI=LAB(I)
      NPHI=NCNCT(MXSS,LI)
      IF (NPHI.EQ.0) GOTO 130
      LJ=NCNCT(1,LI)
      IF (LJ.GT.0) THEN
         DO 83 ICOR=1,3
         CNIJ(ICOR) = COSN(ICOR,LJ,LI)
         IF (LGRX.AND.LI.GE.LJ) THEN
            DO 81 JCOR=1,3
            DICNIJ(ICOR,JCOR)=DICOSN(ICOR,JCOR,LJ,LI)
            DJCNIJ(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LJ,LI)
  81        CONTINUE
         ELSE IF (LGRX) THEN
            DO 82 JCOR=1,3
            DICNIJ(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LJ,LI)
            DJCNIJ(ICOR,JCOR)=DICOSN(ICOR,JCOR,LJ,LI)
  82        CONTINUE
         ENDIF
         IF (LGRR) DCNIJR(ICOR)=DCOSNR(ICOR,LJ,LI)
  83     CONTINUE
      ELSE
         LJ=-LJ
         DO 86 ICOR=1,3
         CNIJ(ICOR)=COSN(ICOR,LI,LJ)
         IF (LGRX.AND.LI.GE.LJ) THEN
            DO 84 JCOR=1,3
            DICNIJ(ICOR,JCOR)=DICOSN(ICOR,JCOR,LI,LJ)
            DJCNIJ(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LI,LJ)
  84        CONTINUE
         ELSE IF (LGRX) THEN
            DO 85 JCOR=1,3
            DICNIJ(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LI,LJ)
            DJCNIJ(ICOR,JCOR)=DICOSN(ICOR,JCOR,LI,LJ)
  85        CONTINUE
         ENDIF
         IF (LGRR) DCNIJR(ICOR)=DCOSNR(ICOR,LI,LJ)
  86     CONTINUE
         NCNCT(1,LI)=LJ
      ENDIF
      SIT(LI)=1D0/STHETA(LI)
      C2I=CTHETA(LI,LI)**2
      VIJ(1)=COSN(2,LI,LI)*CNIJ(3)-COSN(3,LI,LI)*CNIJ(2)
      VIJ(2)=COSN(3,LI,LI)*CNIJ(1)-COSN(1,LI,LI)*CNIJ(3)
      VIJ(3)=COSN(1,LI,LI)*CNIJ(2)-COSN(2,LI,LI)*CNIJ(1)
      LPOLY =DOT(VIJ,COSN(1,LJ,LJ),3).GT.0D0
      IF (LGRX.OR.LGRR) THEN
         X=2D0*CTHETA(LI,LI)
         Y=-SIT(LI)**2
         IF (LGRX) THEN
            DO 87 ICOR=1,3
            DCSIT(ICOR,LI)=Y*DSTETA(ICOR,LI)
            DCC2I(ICOR)=X*DCTETA(ICOR,LI,LI)
            DIVIJ(1,ICOR)=
     .      DCOSN(2,ICOR,LI,LI)*CNIJ(3)-DCOSN(3,ICOR,LI,LI)*CNIJ(2)
     .      +COSN(2,LI,LI)*DICNIJ(3,ICOR)-COSN(3,LI,LI)*DICNIJ(2,ICOR)
            DIVIJ(2,ICOR)=
     .      DCOSN(3,ICOR,LI,LI)*CNIJ(1)-DCOSN(1,ICOR,LI,LI)*CNIJ(3)
     .      +COSN(3,LI,LI)*DICNIJ(1,ICOR)-COSN(1,LI,LI)*DICNIJ(3,ICOR)
            DIVIJ(3,ICOR)=
     .      DCOSN(1,ICOR,LI,LI)*CNIJ(2)-DCOSN(2,ICOR,LI,LI)*CNIJ(1)
     .      +COSN(1,LI,LI)*DICNIJ(2,ICOR)-COSN(2,LI,LI)*DICNIJ(1,ICOR)
            DJVIJ(1,ICOR)=
     .      COSN(2,LI,LI)*DJCNIJ(3,ICOR)-COSN(3,LI,LI)*DJCNIJ(2,ICOR)
            DJVIJ(2,ICOR)=
     .      COSN(3,LI,LI)*DJCNIJ(1,ICOR)-COSN(1,LI,LI)*DJCNIJ(3,ICOR)
            DJVIJ(3,ICOR)=
     .      COSN(1,LI,LI)*DJCNIJ(2,ICOR)-COSN(2,LI,LI)*DJCNIJ(1,ICOR)
  87        CONTINUE
         ENDIF
         IF (LGRR) THEN
            DSITR(LI)=Y*DSTETR(LI)
            DC2IR=X*DCTETR(LI)
            DVIJR(1)=COSN(2,LI,LI)*DCNIJR(3)-COSN(3,LI,LI)*DCNIJR(2)
            DVIJR(2)=COSN(3,LI,LI)*DCNIJR(1)-COSN(1,LI,LI)*DCNIJR(3)
            DVIJR(3)=COSN(1,LI,LI)*DCNIJR(2)-COSN(2,LI,LI)*DCNIJR(1)
         ENDIF
      ENDIF
C
C     MAKE A LOOP OVER THE SS I, COMPUTE DIHEDRALS BETWEEN 0 AND TWOPI
      DO 95 J=2,NPHI
      LJ=NCNCT(J,LI)
      IF (LJ.GT.0) THEN
         DO 90 ICOR=1,3
         CNIK(ICOR) = COSN(ICOR,LJ,LI)
         IF (LGRX.AND.LI.GE.LJ) THEN
            DO 88 JCOR=1,3
            DICNIK(ICOR,JCOR)=DICOSN(ICOR,JCOR,LJ,LI)
            DKCNIK(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LJ,LI)
   88       CONTINUE
         ELSE IF (LGRX) THEN
            DO 89 JCOR=1,3
            DICNIK(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LJ,LI)
            DKCNIK(ICOR,JCOR)=DICOSN(ICOR,JCOR,LJ,LI)
   89       CONTINUE
         ENDIF
         IF (LGRR) DCNIKR(ICOR)=DCOSNR(ICOR,LJ,LI)
   90    CONTINUE
      ELSE
         LJ =-LJ
         DO 93 ICOR=1,3
         CNIK(ICOR)=COSN(ICOR,LI,LJ)
         IF (LGRX.AND.LI.GE.LJ) THEN
            DO 91 JCOR=1,3
            DICNIK(ICOR,JCOR)=DICOSN(ICOR,JCOR,LI,LJ)
            DKCNIK(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LI,LJ)
   91       CONTINUE
         ELSE IF (LGRX) THEN
            DO 92 JCOR=1,3
            DICNIK(ICOR,JCOR)=DJCOSN(ICOR,JCOR,LI,LJ)
            DKCNIK(ICOR,JCOR)=DICOSN(ICOR,JCOR,LI,LJ)
   92       CONTINUE
         ENDIF
         IF (LGRR) DCNIKR(ICOR)=DCOSNR(ICOR,LI,LJ)
   93    CONTINUE
         NCNCT(J,LI)=LJ
      ENDIF
C     NOTE: ACCURACY IS CRUCIAL HERE (USE ATAN2, NOT ACOS + ASIN)
      X=DOT(VIJ,CNIK,3)
      Y=DOT(CNIK,CNIJ,3)-C2I
      WORK(J-1)=ATAN2(X,Y)
      IF (WORK(J-1).LE.0D0) WORK(J-1)=WORK(J-1)+TWOPI
      IF (LGRX.OR.LGRR) THEN
         DX=-X/(X**2+Y**2)
         DY= Y/(X**2+Y**2)
         IF (LGRX) THEN
C           CARTESIAN DERIVATIVES OF WORK
            DO 94 ICOR=1,3
            DIX=DOT(DIVIJ(1,ICOR),CNIK,3) +DOT(VIJ,DICNIK(1,ICOR),3)
            DIY=DOT(DICNIK(1,ICOR),CNIJ,3)+DOT(CNIK,DICNIJ(1,ICOR),3)
     .      -DCC2I(ICOR)
            DJX=DOT(DJVIJ(1,ICOR),CNIK,3)
            DJY=DOT(CNIK,DJCNIJ(1,ICOR),3)
            DKX=DOT(VIJ,DKCNIK(1,ICOR),3)
            DKY=DOT(DKCNIK(1,ICOR),CNIJ,3)
            DIWORK(ICOR,J-1)= DY*DIX+DX*DIY
            DJWORK(ICOR,J-1)= DY*DJX+DX*DJY
            DKWORK(ICOR,J-1)= DY*DKX+DX*DKY
            D0WORK(ICOR,J-1)=-DIWORK(ICOR,J-1)-DJWORK(ICOR,J-1)
     .                       -DKWORK(ICOR,J-1)
   94       CONTINUE
         ENDIF
         IF (LGRR) THEN
C           DERIVATIVE OF WORK VS RAD(K)
            DXR=DOT(DVIJR,CNIK,3)+DOT(VIJ,DCNIKR,3)
            DYR=DOT(DCNIKR,CNIJ,3)+DOT(CNIK,DCNIJR,3)-DC2IR
            DWORKR(J-1)=DY*DXR+DX*DYR
         ENDIF
      ENDIF
   95 CONTINUE
      IF (NPHI.EQ.2) THEN
C        TRIVIAL CASE, NO SORT NEEDED
         AODD=WORK(1)
         IF (LGRX) THEN
            LJ=NCNCT(1,LI)
            LK=NCNCT(2,LI)
            DO 96 ICOR=1,3
            DCAODD(ICOR,LI)=DCAODD(ICOR,LI)+DIWORK(ICOR,1)
            DCAODD(ICOR,LJ)=DCAODD(ICOR,LJ)+DJWORK(ICOR,1)
            DCAODD(ICOR,LK)=DCAODD(ICOR,LK)+DKWORK(ICOR,1)
            DCAODD(ICOR,0 )=DCAODD(ICOR,0 )+D0WORK(ICOR,1)
   96       CONTINUE
         ENDIF
         IF (LGRR) DAODDR=DWORKR(1)
      ELSE
C        SORT THE DIHEDRALS OF SS NO I IN ASCENDING ORDER
         DO 100 J=1,NPHI-2
         DO 100 L=J+1,NPHI-1
         IF (WORK(J).GT.WORK(L)) THEN
            WBUF=WORK(L)
            WORK(L)=WORK(J)
            WORK(J)=WBUF
            M=NCNCT(L+1,LI)
            NCNCT(L+1,LI)=NCNCT(J+1,LI)
            NCNCT(J+1,LI)=M
            IF (LGRX) THEN
               CALL DSWAP (3,DIWORK(1,L),1,DIWORK(1,J),1)
               CALL DSWAP (3,DJWORK(1,L),1,DJWORK(1,J),1)
               CALL DSWAP (3,DKWORK(1,L),1,DKWORK(1,J),1)
               CALL DSWAP (3,D0WORK(1,L),1,D0WORK(1,J),1)
            ENDIF
            IF (LGRR) THEN
               WBUF=DWORKR(L)
               DWORKR(L)=DWORKR(J)
               DWORKR(J)=WBUF
            ENDIF
         ENDIF
  100    CONTINUE
C        COMPUTE THE SUM OF DIHEDRALS OF ORDERED SLICES (EVEN IN NUMBER)
C        ---------------------------------------------------------------
         AODD=WORK(1)
         IF (LGRX) THEN
            LJ=NCNCT(1,LI)
            LK=NCNCT(2,LI)
            DO 101 ICOR=1,3
            DCAODD(ICOR,LI)=DCAODD(ICOR,LI)+DIWORK(ICOR,1)
            DCAODD(ICOR,LJ)=DCAODD(ICOR,LJ)+DJWORK(ICOR,1)
            DCAODD(ICOR,LK)=DCAODD(ICOR,LK)+DKWORK(ICOR,1)
            DCAODD(ICOR,0 )=DCAODD(ICOR,0 )+D0WORK(ICOR,1)
  101       CONTINUE
         ENDIF
         IF (LGRR) DAODDR=DWORKR(1)
         DO 110 J=3,NPHI-1,2
         AODD=AODD+WORK(J)-WORK(J-1)
         IF (LGRX) THEN
            LK=NCNCT(J,LI)
            LL=NCNCT(J+1,LI)
            DO 102 ICOR=1,3
            DCAODD(ICOR,LI)=DCAODD(ICOR,LI)
     .                     +DIWORK(ICOR,J )-DIWORK(ICOR,J-1)
            DCAODD(ICOR,LJ)=DCAODD(ICOR,LJ)
     .                     +DJWORK(ICOR,J )-DJWORK(ICOR,J-1)
            DCAODD(ICOR,LK)=DCAODD(ICOR,LK)-DKWORK(ICOR,J-1)
            DCAODD(ICOR,LL)=DCAODD(ICOR,LL)+DKWORK(ICOR,J)
            DCAODD(ICOR,0 )=DCAODD(ICOR,0 )
     .                     +D0WORK(ICOR,J )-D0WORK(ICOR,J-1)
  102       CONTINUE
         ENDIF
         IF (LGRR) DAODDR=DAODDR+DWORKR(J)-DWORKR(J-1)
  110    CONTINUE
      ENDIF
      AEVEN=TWOPI-AODD
      X=1D0-CTHETA(LI,LI)
      IF (LPOLY) THEN
C        ODD DIHEDRALS ARE VERTICE OF POLYGONS
         APOLY=APOLY+AODD
         ASLICE=ASLICE+AEVEN*X
         IF (LGRX) THEN
            DO 111 LT=0,NCROSS
            DCAPLY(1,LT)=DCAPLY(1,LT)+DCAODD(1,LT)
            DAREA(1,LT)=DAREA(1,LT)-DCAODD(1,LT)*X
            DCAPLY(2,LT)=DCAPLY(2,LT)+DCAODD(2,LT)
            DAREA(2,LT)=DAREA(2,LT)-DCAODD(2,LT)*X
            DCAPLY(3,LT)=DCAPLY(3,LT)+DCAODD(3,LT)
            DAREA(3,LT)=DAREA(3,LT)-DCAODD(3,LT)*X
  111       CONTINUE
            DAREA(1,LI)=DAREA(1,LI)-AEVEN*DCTETA(1,LI,LI)
            DAREA(1,0 )=DAREA(1,0 )+AEVEN*DCTETA(1,LI,LI)
            DAREA(2,LI)=DAREA(2,LI)-AEVEN*DCTETA(2,LI,LI)
            DAREA(2,0 )=DAREA(2,0 )+AEVEN*DCTETA(2,LI,LI)
            DAREA(3,LI)=DAREA(3,LI)-AEVEN*DCTETA(3,LI,LI)
            DAREA(3,0 )=DAREA(3,0 )+AEVEN*DCTETA(3,LI,LI)
         ENDIF
         IF (LGRR) THEN
            DAPOLR=DAPOLR+DAODDR
            DASLIR=DASLIR-DAODDR*X-AEVEN*DCTETR(LI)
         ENDIF
      ELSE
C        EVEN DIHEDRALS ARE VERTICE OF POLYGONS
         APOLY=APOLY+AEVEN
         ASLICE=ASLICE+AODD*X
         IF (LGRX) THEN
            DO 112 LT=0,NCROSS
            DCAPLY(1,LT)=DCAPLY(1,LT)-DCAODD(1,LT)
            DAREA(1,LT)=DAREA(1,LT)+DCAODD(1,LT)*X
            DCAPLY(2,LT)=DCAPLY(2,LT)-DCAODD(2,LT)
            DAREA(2,LT)=DAREA(2,LT)+DCAODD(2,LT)*X
            DCAPLY(3,LT)=DCAPLY(3,LT)-DCAODD(3,LT)
            DAREA(3,LT)=DAREA(3,LT)+DCAODD(3,LT)*X
  112       CONTINUE
            DAREA(1,LI)=DAREA(1,LI)-AODD*DCTETA(1,LI,LI)
            DAREA(1,0 )=DAREA(1,0 )+AODD*DCTETA(1,LI,LI)
            DAREA(2,LI)=DAREA(2,LI)-AODD*DCTETA(2,LI,LI)
            DAREA(2,0 )=DAREA(2,0 )+AODD*DCTETA(2,LI,LI)
            DAREA(3,LI)=DAREA(3,LI)-AODD*DCTETA(3,LI,LI)
            DAREA(3,0 )=DAREA(3,0 )+AODD*DCTETA(3,LI,LI)
         ENDIF
         IF (LGRR) THEN
            DAPOLR=DAPOLR-DAODDR
            DASLIR=DASLIR+DAODDR*X-AODD*DCTETR(LI)
         ENDIF
C        REORDER LABELS SO THAT POLYGON'S VERTICE APPEAR AT ODD RANKS
         M=NCNCT(1,LI)
         DO 120 J=1,NPHI-1
         NCNCT(J,LI)=NCNCT(J+1,LI)
  120    CONTINUE
         NCNCT(NPHI,LI)=M
      ENDIF
  130 CONTINUE
C     NOW TO COUNT THE NUMBER OF POLYGONS AND TO SUM INTERSECTION ANGLES
C     ------------------------------------------------------------------
      NPOLY=0
      DO 170 I=1,NCLUST
      LI=LAB(I)
      DO 160 J=2,NCNCT(MXSS,LI),2
      IF (NCNCT(J,LI).NE.0) THEN
C        DEFINE THE "FIRST" VERTEX OF A POLYGON
         IA=NCNCT(J-1,LI)
         IB=LI
         NCNCT(J,LI)=0
         PHI1=CTHETA(IA,IB)-CTHETA(IA,IA)*CTHETA(IB,IB)
         PHI2=SIT(IA)*SIT(IB)
         PHI=ACOS(PHI1*PHI2)
         APOLY=APOLY+PHI
         IF (LGRX.OR.LGRR) THEN
            S1NPHI=1D0/SIN(PHI)
            PHI1=S1NPHI*PHI1
            PHI2=S1NPHI*PHI2
            IF (LGRX) THEN
               DO 131 ICOR=1,3
               DAPHI(ICOR)=(DCTETA(ICOR,IA,IB)-CTHETA(IB,IB)
     .         *DCTETA(ICOR,IA,IA))*PHI2+PHI1*DCSIT(ICOR,IA)*SIT(IB)
               DBPHI(ICOR)=(DCTETA(ICOR,IB,IA)-CTHETA(IA,IA)
     .         *DCTETA(ICOR,IB,IB))*PHI2+PHI1*SIT(IA)*DCSIT(ICOR,IB)
               DCAPLY(ICOR,IA)=DCAPLY(ICOR,IA)-DAPHI(ICOR)
               DCAPLY(ICOR,IB)=DCAPLY(ICOR,IB)-DBPHI(ICOR)
               DCAPLY(ICOR,0 )=DCAPLY(ICOR,0 )+DAPHI(ICOR)+DBPHI(ICOR)
  131          CONTINUE
            ENDIF
            IF (LGRR) THEN
               DPHIR=
     .         (DCTETR(IA)*CTHETA(IB,IB)+CTHETA(IA,IA)*DCTETR(IB))*PHI2
     .         -PHI1*(DSITR(IA)*SIT(IB)+SIT(IA)*DSITR(IB))
               DAPOLR=DAPOLR+DPHIR
            ENDIF
         ENDIF
C
C        FOLLOW AND ERASE VERTICES OF THE CURRENT POLYGON
  140    L=2
  141    IF (L.GT.NCNCT(MXSS,IA)) GOTO 150
         IF (NCNCT(L,IA).EQ.IB) THEN
            IBOLD=IB
            NCNCT(L,IA)=0
            IB=IA
            IA=NCNCT(L-1,IB)
            IF (IA.NE.IBOLD) THEN
               PHI1=CTHETA(IA,IB)-CTHETA(IA,IA)*CTHETA(IB,IB)
               PHI2=SIT(IA)*SIT(IB)
               PHI=ACOS(PHI1*PHI2)
               IF (LGRX.OR.LGRR) THEN
                  S1NPHI=1D0/SIN(PHI)
                  PHI1=S1NPHI*PHI1
                  PHI2=S1NPHI*PHI2
                  IF (LGRX) THEN
                     DO 142 ICOR=1,3
                     DAPHI(ICOR)=(DCTETA(ICOR,IA,IB)
     .               -CTHETA(IB,IB)*DCTETA(ICOR,IA,IA))*PHI2
     .               +PHI1*DCSIT(ICOR,IA)*SIT(IB)
                     DBPHI(ICOR)=(DCTETA(ICOR,IB,IA)
     .               -CTHETA(IA,IA)*DCTETA(ICOR,IB,IB))*PHI2
     .               +PHI1*SIT(IA)*DCSIT(ICOR,IB)
  142                CONTINUE
                  ENDIF
                  IF (LGRR) THEN
                     DPHIR=(DCTETR(IA)*CTHETA(IB,IB)+
     .               CTHETA(IA,IA)*DCTETR(IB))*PHI2
     .               -PHI1*(DSITR(IA)*SIT(IB)+SIT(IA)*DSITR(IB))
                  ENDIF
               ENDIF
            ELSE
               IB=LI
               IA=NCNCT(J-1,LI)
            ENDIF
            APOLY=APOLY+PHI
            IF (LGRX) THEN
               DO 143 ICOR=1,3
               DCAPLY(ICOR,IA)=DCAPLY(ICOR,IA)-DAPHI(ICOR)
               DCAPLY(ICOR,IB)=DCAPLY(ICOR,IB)-DBPHI(ICOR)
               DCAPLY(ICOR,0 )=DCAPLY(ICOR,0 )+DAPHI(ICOR)+DBPHI(ICOR)
  143          CONTINUE
            ENDIF
            IF (LGRR) DAPOLR=DAPOLR+DPHIR
            GOTO 140
         ENDIF
         L=L+2
         GOTO 141
C        ORIENTED LOOP OVER CURRENT POLYGON COMPLETED
  150    NPOLY=NPOLY+1
      ENDIF
  160 CONTINUE
  170 CONTINUE
C     ACCESSIBLE SOLID ANGLE FOR SPHERE K.
C     ------------------------------------
C     POLYGONS IMBEDDED IN OTHERS TAKEN INTO ACCOUNT BY MODULO(4PI)
      APOLY=APOLY+(NPOLY-NFREE)*TWOPI
      AREA=FOURPI-ASLICE-MOD(APOLY,FOURPI)
      IF (LGRX) THEN
         DO 200 I=0,NCROSS
         DAREA(1,I)=-DAREA(1,I)-DCAPLY(1,I)
         DAREA(2,I)=-DAREA(2,I)-DCAPLY(2,I)
         DAREA(3,I)=-DAREA(3,I)-DCAPLY(3,I)
  200    CONTINUE
      ENDIF
      IF (LGRR) DAREAR=-DASLIR-DAPOLR
  300 RETURN
      END
