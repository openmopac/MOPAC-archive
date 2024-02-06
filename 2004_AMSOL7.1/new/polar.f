      SUBROUTINE POLAR 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
*********************************************************************** 
* 
*   POLAR CALCULATES THE POLARIZATION VOLUME IN CUBIC ANGSTROMS 
*         OF A MOLECULE. 
*         A SHAPED ELECTRIC FIELD IS CONSTRUCTED, THE FIELD IS PRODUCED 
*         BY POINT CHARGES IN THE LAYOUT 
* 
*    CHARGE   LOCATION 
* 
*     +Q       A  Z          THIS IS THE LAYOUT FOR THE DIAGONAL TERMS, 
*     +Q/2     AB Z          THE OFF-DIAGONALS, OF COURSE, CONTAIN 
*     -Q/2    -AB Z          TWICE AS MANY TERMS. THE RESULTING FIELD 
*     -Q      -A  Z          IS RECTILINEAR TO A GOOD APPROXIMATION 
*                            IN THE VOLUME OF ALL "NORMAL" MOLECULES 
*  "A" IS PROGRAM-DEFINED AND IS HERE SET TO 160 ANGSTROMS 
*  "B" IS THE INVERSE CUBE ROOT OF TWO. 
*********************************************************************** 
      COMMON /TITLES/ KOMENT,TITLE 
     1       /POLVOL/ POLVOL(107) 
     3       /GEOKST/ NATOMS,LABELS(NUMATM) 
     4               ,NA(NUMATM),NB(NUMATM),NC(NUMATM) 
     5       /TIMECM  / TIME0                                           3GL3092
     6       /CORE  / CORE(107) 
     7       /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR) 
     8       /WMATRX/ WDUMMY(N2ELEC*3),KDUMMY,NBAND(NUMATM) 
C    5       /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY,XPARAM(MAXPAR) 
C    6       /TIME  / TIME0 
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /GEOM  / GEO(3,NUMATM) 
     4       /LAST  / LAST 
     5       /COORD / COORD(3,NUMATM) 
      DIMENSION GRAD(MAXPAR), HEATS(5), PHASE(2,4), 
     1          POLMAT(6), VECTRS(9), EIGS(3) 
      CHARACTER  TYPE*7, KOMENT*80, TITLE*80                            DJG0995
      LOGICAL HYPER,FAIL 
      SAVE
      DATA PHASE/3*1.D0,4*-1.D0,1.D0/ 
      TYPE=' MNDO  ' 
      HYPER=(IHYPER.NE.0)                                               DJG0995
      FACTA=300 
C#      READ(7,*)FACTA 
      IF(IMINDO.NE.0) TYPE='MINDO/3'                                    DJG0995
      IF(IAM1.NE.0)   TYPE='  AM1  '                                    DJG0995
      CALL GMETRY(GEO,COORD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      LAST=1 
      NA(1)=99 
C 
C  SET UP THE VARIABLES IN XPARAM AND LOC, THESE ARE IN CARTESIAN 
C  COORDINATES. 
C 
      NDEP=0 
C 
C   XX ANGSTROMS WAS FOUND TO BE THE BEST DISTANCE. 
C 
      FACT1=0.5D0**(1.D0/3.D0) 
      FACT3=1.D0-FACT1 
      FACT2=1.D0/FACT3**2 
C 
C   2 * PI * E(0) / (23.061 * (Q=CHARGE ON THE ELECTRON)) =0.0015056931 
C   IN CUBIC ANGSTROMS, Q   = 1.60219D-19 COULOMBS 
C                       E(0)=8.854188D-12(JOULES**(-2).C**(-2).M**(-1)) 
C                       E(0)=8.854188D-22 (CONVERTED TO ANGSTROMS) 
      NUMAT=0 
      SUMX=0.D0 
      SUMY=0.D0 
      SUMZ=0.D0 
      DO 20 I=1,NATOMS 
         IF(LABELS(I).NE.99) THEN 
            NUMAT=NUMAT+1 
            LABELS(NUMAT)=LABELS(I) 
            SUMX=SUMX+COORD(1,NUMAT) 
            SUMY=SUMY+COORD(2,NUMAT) 
            SUMZ=SUMZ+COORD(3,NUMAT) 
            DO 10 J=1,3 
   10       GEO(J,NUMAT)=COORD(J,NUMAT) 
         ENDIF 
   20 CONTINUE 
      SUMX=SUMX/NUMAT 
      SUMY=SUMY/NUMAT 
      SUMZ=SUMZ/NUMAT 
      SUMMAX=0.D0 
      ATPOL=0.D0 
      DO 30 I=1,NUMAT 
         ATPOL=ATPOL+POLVOL(NAT(I)) 
         GEO(1,I)=GEO(1,I)-SUMX 
         IF(SUMMAX.LT.ABS(GEO(1,I))) SUMMAX=ABS(GEO(1,I)) 
         GEO(2,I)=GEO(2,I)-SUMY 
         IF(SUMMAX.LT.ABS(GEO(2,I))) SUMMAX=ABS(GEO(2,I)) 
         GEO(3,I)=GEO(3,I)-SUMZ 
         IF(SUMMAX.LT.ABS(GEO(3,I))) SUMMAX=ABS(GEO(3,I)) 
   30 CONTINUE 
C 
C   THE ELECTRIC FIELD ACROSS ANY MOLECULE SHOULD BE ROUGHLY THE SAME, 
C   THEREFORE: 
      DELTA=25*SUMMAX 
      IF(DELTA.LT.160)DELTA=160 
      CONST=DELTA**4*0.0015056931D0 
      CONST=CONST*FACT2 
C 
C  INCREASE THE CHARGE ON THE SPARKLES 
C 
      CORE(105)= FACTA 
      CORE(106)=-CORE(105) 
      CORE(103)= CORE(105)*0.5D0 
      CORE(104)=-CORE(103) 
      CONST=CONST/CORE(105)**2 
      LABELS(NUMAT+1)=105 
      LABELS(NUMAT+2)=104 
      LABELS(NUMAT+3)=103 
      LABELS(NUMAT+4)=106 
      NAT(NUMAT+1)=105 
      NAT(NUMAT+2)=104 
      NAT(NUMAT+3)=103 
      NAT(NUMAT+4)=106 
      NFIRST(NUMAT+1)=NORBS+4 
      NFIRST(NUMAT+2)=NORBS+4 
      NFIRST(NUMAT+3)=NORBS+4 
      NFIRST(NUMAT+4)=NORBS+4 
      NMIDLE(NUMAT+1)=NORBS+3 
      NMIDLE(NUMAT+2)=NORBS+3 
      NMIDLE(NUMAT+3)=NORBS+3 
      NMIDLE(NUMAT+4)=NORBS+3 
      NLAST(NUMAT+1)=NORBS 
      NLAST(NUMAT+2)=NORBS 
      NLAST(NUMAT+3)=NORBS 
      NLAST(NUMAT+4)=NORBS 
      NBAND(NUMAT+1)=0 
      NBAND(NUMAT+2)=0 
      NBAND(NUMAT+3)=0 
      NBAND(NUMAT+4)=0 
      NVAR=0 
      NUMAT=NUMAT+4 
      NATOMS=NUMAT 
      NUMA1=NUMAT-3 
      NUMA2=NUMAT-2 
      NUMA3=NUMAT-1 
      GEO(1,NUMA1)=DELTA 
      GEO(2,NUMA1)=0.D0 
      GEO(3,NUMA1)=1.D8 
      GEO(1,NUMA2)=DELTA*FACT1 
      GEO(2,NUMA2)=0.D0 
      GEO(3,NUMA2)=1.D8 
      GEO(1,NUMA3)=-DELTA*FACT1 
      GEO(2,NUMA3)=0.D0 
      GEO(3,NUMA3)=1.D8 
      GEO(1,NUMAT)=-DELTA 
      GEO(2,NUMAT)=0.D0 
      GEO(3,NUMAT)=1.D8 
C#      I=NVAR 
C#      NVAR=0 
      CALL COMPFG(GEO,HEATS(5),FAIL, GRAD, .FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=21                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
C#      NVAR=I 
      IJ=0 
      DO 70 I=1,3 
         IM1=I-1 
         DO 50 J=1,IM1 
            IJ=IJ+1 
            K=6-I-J 
            L=0 
            DO 40 LL=1,4 
               L=L+1 
               GEO(I,NUMA1)= DELTA*PHASE(1,LL) 
               GEO(I,NUMA2)= DELTA*PHASE(1,LL)*FACT1 
               GEO(I,NUMA3)=-DELTA*PHASE(1,LL)*FACT1 
               GEO(I,NUMAT)=-DELTA*PHASE(1,LL) 
               GEO(J,NUMA1)= DELTA*PHASE(2,LL) 
               GEO(J,NUMA2)= DELTA*PHASE(2,LL)*FACT1 
               GEO(J,NUMA3)=-DELTA*PHASE(2,LL)*FACT1 
               GEO(J,NUMAT)=-DELTA*PHASE(2,LL) 
               GEO(K,NUMA1)= 0.D0 
               GEO(K,NUMA2)= 0.D0 
               GEO(K,NUMA3)= 0.D0 
               GEO(K,NUMAT)= 0.D0 
               CALL COMPFG(GEO,HEATS(L),FAIL, GRAD, .FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=22                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
   40       CONTINUE 
            POLMAT(IJ)=(HEATS(2)+HEATS(4)-HEATS(1)-HEATS(3))*CONST 
   50    CONTINUE 
         IJ=(I*(I+1))/2 
         DO 60 K=NUMA1,NUMAT 
            DO 60 J=1,3 
   60    GEO(J,K)= 0.D0 
         GEO(I,NUMA1)= DELTA 
         GEO(I,NUMA2)= DELTA*FACT1 
         GEO(I,NUMA3)=-DELTA*FACT1 
         GEO(I,NUMAT)=-DELTA 
         CALL COMPFG(GEO,HEATS(2),FAIL, GRAD, .FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=23                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
         GEO(I,NUMA1)=-DELTA 
         GEO(I,NUMA2)=-DELTA*FACT1 
         GEO(I,NUMA3)= DELTA*FACT1 
         GEO(I,NUMAT)= DELTA 
         CALL COMPFG(GEO,HEATS(3),FAIL, GRAD, .FALSE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=24                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
         POLMAT(IJ)=0.5D0*(HEATS(5)+HEATS(5)-HEATS(2)-HEATS(3))*CONST+AT 
     1POL 
   70 CONTINUE 
      WRITE(JOUT,'(A)')KOMENT, TITLE 
      WRITE(JOUT,'(//10X,A,'' POLARIZATION MATRIX, FIELD='',F10.4, 
     1'' VOLTS PER ANGSTROM'')') 
     2TYPE,CORE(105)*2.D0*FACT3*14.399/DELTA**2 
C
C     PASSING A CONSTANT WHICH MAY BE MODIFIED IS NOT STANDARD F77      !JT1001
C
C     CALL VECPRT(POLMAT,3)                                             GCL0393
      ITEMP=3                                                           !JT1001
      CALL VECPRT(POLMAT,ITEMP)                                         !JT1001
      CALL HQRII(POLMAT,3,3,EIGS,VECTRS)                                GDH0793
      WRITE(JOUT,
     .'(//4X,''  POLARIZATION VOLUMES (IN CUBIC ANGSTROMS)'', 
     1'' AND VECTORS, AVERAGE='',F10.3)')(EIGS(1)+EIGS(2)+EIGS(3))/3.D0 
C     
C     PASSING A CONSTANT WHICH MAY BE MODIFIED IS NOT STANDARD F77      !JT1001
C
      ITEMP=3                                                           !JT1001
      CALL MATOUT (VECTRS,EIGS,ITEMP,ITEMP,ITEMP)                       !JT1001
C     CALL MATOUT(VECTRS,EIGS,3,3,3) 
      RETURN 
      END 
