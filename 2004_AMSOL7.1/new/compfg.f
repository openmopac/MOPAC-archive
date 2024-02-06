      SUBROUTINE COMPFG(XPARAM,ESCF,FAIL,GRAD,LGRAD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      DIMENSION XPARAM(*),GRAD(*)
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR),IDUMY,DUMY(MAXPAR)
      COMMON /GEOVAR/ DUMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY             3GL3092
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /ATHEAT/ ATHEAT
      COMMON /WMATRX/ WJ(N2ELEC),WK(N2ELEC*2),NWDUM(NUMATM+1)
      COMMON /ENUCLR/ ENUCLR
      COMMON /ELECT / ELECT
      COMMON /HMATRX/ H(MPACK)
      COMMON /MESAGE/ IFLEP,ISCF                                        DJG0995
C     COMMON /OPTIM / IMP,IMP0,LEC,IPRT
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /CYCLCM/ PCMIN, NGEOM, NSOLPR, NSCFS, JPCNT                GDH0893
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
C***********************************************************************
C
C   COMPFG CALCULATES (A) THE HEAT OF FORMATION OF THE SYSTEM, AND
C                     (B) THE GRADIENTS, IF LGRAD IS .TRUE.
C
C   ON INPUT  XPARAM = ARRAY OF PARAMETERS (INTERNAL OR CARTESIAN COORD)
C             LGRAD  = .TRUE. IF GRADIENTS ARE NEEDED, .FALSE. OTHERWISE
C             FAIL   = .TRUE. IF DENSITY MATRIX TO BE RESTORED WITH
C                             STANDARD DIAGONAL ONES,  .FALSE. OTHERWISE
C
C   ON OUTPUT ESCF   = HEAT OF FORMATION.
C             FAIL   =.TRUE. IF SCF NOT CONVERGED,     .FALSE. OTHERWISE
C             GRAD   = ARRAY OF GRADIENTS, IF REQUIRED BY LGRAD = .TRUE.
C
C***********************************************************************
      LOGICAL DEBUG, PRINT, FAIL, RESTOR, LGRAD
      DIMENSION COORD(3,NUMATM), W(1)
      EQUIVALENCE (W,WJ)
       SAVE

      IF (ICONTR(14).EQ.1) THEN                                         GDH0195
         ICONTR(14)=2                                                   GDH0195
         PRINT=(ICOMPF.NE.0)                                            DJG0995
         DEBUG=(IDEBUG.NE.0.AND.PRINT)                                  DJG0995
      ENDIF
C
C SET UP COORDINATES FOR CURRENT CALCULATION
C
C       PLACE THE NEW VALUES OF THE VARIABLES IN THE ARRAY GEO.
C       MAKE CHANGES IN THE GEOMETRY.
      NGEOM=NGEOM+1
      DO 10 I=1,NVAR
      K=LOC(1,I)
      L=LOC(2,I)
   10 GEO(L,K)=XPARAM(I)
C     IMPOSE THE SYMMETRY CONDITIONS + COMPUTE THE DEPENDENT-PARAMETERS
      IF(NDEP.NE.0) CALL SYMTRY
C     NOW COMPUTE THE ATOMIC COORDINATES.
      IF( DEBUG ) THEN
         WRITE(JOUT,FMT='('' INTERNAL COORDS'',/100(/,3F12.6))')
     1                ((GEO(J,I),J=1,3),I=1,NVAR)
      ENDIF
      CALL GMETRY(GEO,COORD)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF( DEBUG ) THEN
         PRINT *, 'NUMATM = ',NUMATM
         WRITE(JOUT,FMT='('' CARTESIAN COORDS'',/100(/,3F12.6))')
     1                ((COORD(J,I),J=1,3),I=1,NVAR)
      ENDIF
      CALL HCORE(COORD, H, W, WJ, WK, ENUCLR)
C
C COMPUTE THE HEAT OF FORMATION.
C
      RESTOR=FAIL.OR.IFAIL.NE.0                                         DJG0995
      CALL ITER(H, W, WJ, WK, ELECT, LGRAD, RESTOR)
      ESCF=(ELECT+ENUCLR)*23.061D0+ATHEAT
      FAIL=ISCF.NE.1                                                    DJG0995
      IF(FAIL)  WRITE(JOUT,FMT='('' * * * *** WARNING *** * * * SCF''
     *          ,'' NOT CONVERGED IN COMPFG'')')
      IF(PRINT.OR.FAIL.OR.DEBUG) THEN
         WRITE(JOUT,FMT='('' COMPFG : HEAT OF FORMATION'',G30.17)')ESCF
         WRITE(JOUT,FMT='('' PARAMETERS     '',8F8.4,(/10F8.4))')
     *                (XPARAM(I),I=1,NVAR)
      ENDIF
C
C FIND DERIVATIVES IF DESIRED
C

      IF (LGRAD.AND..NOT.FAIL) THEN
         CALL DERIV(GRAD,FAIL)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
         IF(FAIL) WRITE(JOUT,FMT='('' * * * *** WARNING *** * * * SCF'',
     *            '' NOT CONVERGED IN DERIV'')')
         IF(PRINT.OR.DEBUG.OR.FAIL)
     *   WRITE(JOUT,FMT='('' GRADIENT       '',8F8.2,(/10F8.2))')
     *                (GRAD(I),I=1,NVAR)
         IF (ISTOP.NE.0) RETURN                                         GDH1095
      ENDIF
      RETURN
      END
