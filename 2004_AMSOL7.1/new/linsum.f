      SUBROUTINE LINSUM(FUNCT,SUMW,DIP,SZ,SS2,KCHRGE,TIME0,TSOLVE,NFILE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C******************************************************************************
C
C  THIS SUBROUTINE PRINTS OUT THE LINES OF SUMMARY INFORMATION WHICH
C  INCLUDE THE HEAT OF FORMATION (+ DELTA-G SOLVATION), CORE-CORE
C  REPULSION, HOMO ENERGY, ETC.
C
C  ON INPUT
C     FUNCT = HEAT OF FORMATION
C     DIP = UNDEFINED (FOR .OUT FILE) DIPOLE MOMENT (FOR .ARC FILE)
C     KCHRGE = INTEGER CHARGE ON THE MOLECULE
C     TIME0 = TIME ZERO
C     SZ = UNDEFINED (FOR .OUT FILE) Z-COMPONENT OF THE SPIN (FOR .ARC FILE)
C     SSZ = UNDEFINED (FOR .OUT FILE) S^2 VALUE (FOR .ARC FILE)
C     TSOLVE = UNDEFINED (FOR .OUT FILE) TOTAL SOLUTION ENERGY (FOR .ARC FILE)
C     NFILE = OUTPUT FILE NUMBER
C
C  ON OUTPUT
C     SUMW = MOLECULAR WEIGHT
C     
C
C  CREATED BY DJG 0995 FROM EXISTING LINES IN WRITES
C
C******************************************************************************
      COMMON /ENUCLR/ ENUCLR
      COMMON /ELECT / ELECT
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY
      COMMON /REPATH/ REACT(100),LATOM,LPARAM
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /VECTOR/ C(MORB2),EIGS(MAXORB),CBETA(MORB2),EIGB(MAXORB)
      COMMON /SCRAH2/ PQKL(NRELAX*MORB2+1)
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /ONESCM/ ICONTR(100)
      COMMON /GRADNT/ GRAD(MAXPAR), GNORM
      COMMON /MODLCM/ SMODEL                                            DJG0496
      DIMENSION GCOORD(MAXPAR), W(N2ELEC)
      CHARACTER GRTYPE*14
      LOGICAL FAIL,ARCFIL,AQPOL,FARCE,SMODEL                            DJG0496
      SAVE
      IF (ICONTR(47).EQ.1) THEN
         FARCE=(IFORCE+ITHERM.NE.0)
         FAIL=.FALSE.
         ICONTR(47)=2
         AQPOL=SMODEL                                                   DJG0496
         AQPOL=AQPOL.AND.INOPOL.EQ.0.AND.ISM50.EQ.0                     GDH1197
         DEGREE=57.29577951D0
         IF(NA(1).EQ.99) DEGREE=1.D0
C  
C        CALCULATE IONIZATION ENERGY
C
         IF(NALPHA.GT.0)THEN
            EIONIS=-MAX(EIGS(NALPHA), EIGB(NBETA))
         ELSEIF(NELECS.EQ.1)THEN
            EIONIS=-EIGS(1)
         ELSEIF(NELECS.GT.1) THEN
            EIONIS=-MAX(EIGS(NCLOSE), EIGS(NOPEN))
         ELSE
            EIONIS=0.D0
         ENDIF
         NOPN=NOPEN-NCLOSE
C        CORRECTION TO I.P. OF DOUBLETS
         IF(NOPN.EQ.1)THEN
C           NOTE ... IJKL USES /SCRACH/ AND PQKL AS WORKING ARRAYS.
            CALL IJKL (C,NORBS,NOPEN,1,W,PQKL,NRELAX*MORB2,XIIII,1
     .                 ,PQKL,.FALSE.)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            EIONIS=EIONIS+0.5D0*XIIII
         ENDIF
C
C        WE NEED TO CALCULATE THE REACTION COORDINATE GRADIENT.
C
         GNORM=0.D0 
         IF(NVAR.NE.0)GNORM=SQRT(DOT(GRAD,GRAD,NVAR)) 
         IF(LATOM.NE.0) THEN
            MVAR=NVAR
            LOC11=LOC(1,1)
            LOC21=LOC(2,1)
            NVAR=1
            LOC(1,1)=LATOM
            LOC(2,1)=LPARAM
            XREACT=GEO(LPARAM,LATOM)
            CALL DERIV(GCOORD,FAIL)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
            NVAR=MVAR
            LOC(1,1)=LOC11
            LOC(2,1)=LOC21
            IF(NA(1).NE.99) THEN                                        DJG0995
               GRTYPE='kcal/RU   '                                      DJG0995
            ELSE                                                        DJG0995
               GRTYPE='kcal/angstrom '                                  GL0592
            ENDIF                                                       DJG0995
            IF(FAIL) WRITE(NFILE,'('' * * * *** WARNING *** * * * '',   DJG0995
     1         ''SCF DIVERGENCE IN GRADIENT COMPUTATION''/              DJg0995
     2         '' WHEN DERIV CALLED BY WRITE ... '',                    DJG0995
     3         ''SOME COMPONENTS MEANINGLESS'')')                       DJG0995
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         ENDIF
      ENDIF
      ARCFIL=NFILE.EQ.JARC
      IF(AQPOL) THEN                                                    DJG0995
         WRITE(NFILE, 1000) FUNCT                                       DJG0995
         IF (ARCFIL.AND.ITRUES.NE.0) WRITE(NFILE,1100) TSOLVE           DJG0995
         IF (IHF.NE.1) FIHF=FUNCT                                       GDH0196
         WRITE(NFILE, 1001) ELECT                                       DJG0995
      ELSE                                                              CC7-91
         WRITE(NFILE, 1002) FUNCT                                       DJG0995
         IF (IHF.NE.1) FIHF=FUNCT                                       GDH0196
         WRITE(NFILE, 1003) ELECT                                       DJG0995
      ENDIF                                                             CC7-91
      WRITE (NFILE, 1004) ENUCLR                                        DJG0995
      IF(AQPOL) WRITE (NFILE, 1005) (ELECT + ENUCLR)                    DJG0995
      IF(NVAR.NE.0) WRITE(NFILE, 1006) GNORM                            DJG0995
      IF(LATOM.NE.0) THEN
         IF(LPARAM.EQ.1)THEN
            WRITE(NFILE,1007) XREACT                                    DJG0995
         ELSE
            WRITE(NFILE,1008) XREACT*DEGREE                             DJG0995
         ENDIF
         WRITE(NFILE,1009) GCOORD(1), GRTYPE                            DJG0995
      ENDIF
      IF(ARCFIL.AND.(.NOT.FARCE.OR.KCHRGE.EQ.0)) THEN                   DJG0995
         IF (IEXTCM.NE.0) THEN                                          DJG0995
            WRITE(NFILE ,'(1X,''Dipole moment from EXTCM point'',       DJG0995
     1            '' charges '',T44,''='',T47,F12.5,T61,''debye'')')DIP DJG0995
         ELSE IF (ICHMOD.EQ.1) THEN                                     DJG0995
            WRITE(NFILE ,'(1X,''Dipole moment from CM1 point'',         DJG0195
     1            '' charges '',T44,''='',T47,F12.5,T61,''debye'')')DIP DJG0195
         ELSE IF (ICHMOD.EQ.2) THEN                                     GDH0997
            WRITE(NFILE ,'(1X,''Dipole moment from CM2 point'',         GDH0997
     1            '' charges '',T44,''='',T47,F12.5,T61,''debye'')')DIP GDH0997
         ELSE IF (ICHMOD.EQ.3) THEN                                     !JT0802
            WRITE(NFILE ,'(1X,''Dipole moment from CM3 point'',         !JT0802
     1            '' charges '',T44,''='',T47,F12.5,T61,''debye'')')DIP !JT0802
         ELSE                                                           GDH0997
            WRITE(NFILE ,'(1X,''Dipole moment from the wavefunction'',  DJG0195
     1            T44,''='',T47,F12.5,T61,''debye'')')DIP               DJG0195
         ENDIF                                                          DJG0195
      ENDIF                                                             DJG0195
      IF (ARCFIL) THEN                                                  DJG0995
         IF(ICI.NE.0) WRITE(NFILE,2010)                                 DJG0995
         IF(KCHRGE.NE.0) WRITE(NFILE,2010) KCHRGE                       DJG0995
      ENDIF                                                             DJG0995
C GDH0397      IF(AQPOL) THEN                                           DJG0995
C GDH0397         WRITE(NFILE,1010) -EIONIS                             DJG0995
C GDH0397      ELSE                                                     CC7-91
C GDH0397         WRITE(NFILE,1011) EIONIS                              DJG0995
C GDH0397      ENDIF                                                    CC7-91
      IF(IUHF.NE.0) THEN                                                DJG0995
         IF (ARCFIL) THEN                                               DJG0995
            WRITE(NFILE,2020) SZ                                        DJG0995
            WRITE(NFILE,2030) SS2                                       DJG0995
         ENDIF                                                          DJG0995
         WRITE(NFILE,1012)  NALPHA                                      DJG0995
         WRITE(NFILE,1013)  NBETA                                       DJG0995
      ELSE
         WRITE(NFILE,1014) NCLOSE                                       DJG0995
         IF(NOPN.NE.0) WRITE(NFILE,1015) NOPN                           DJG0995
      ENDIF
      SUMW=0.0D0                                                        DJG0995
      DO 10 I=1,NUMAT
   10 SUMW=SUMW+ATMASS(I)
      IF(SUMW.GT.0.1D0) WRITE(NFILE,1016) SUMW                          DJG0995
      CALL PORCPU (TFLY)                                                GL0492
      TIM=TFLY-TIME0
      I=TIM*0.000001D0
      TIM=TIM-I*1000000
      WRITE(NFILE,1018) TIM                                             DJG0995
1000  FORMAT(/,1X,'Heat of formation + Delta-G solvation', T44, '=',    GL0492
     1       T47, F13.6, T61, 'kcal')                                   GL0492
1001  FORMAT(1X,'Electronic energy + Delta-G solvation',T44,'=',        GL0492
     1       T47, F13.6, T61, 'eV')                                     GL0492
1002  FORMAT(/,1X,'Final heat of formation', T44, '=', T47, F13.6,      GL0492
     1       T61, 'kcal')                                               GL0492
1003  FORMAT(1X,'Electronic energy', T44, '=', T47, F13.6, T61, 'eV')   GL0492
1004  FORMAT(1X,'Core-core repulsion', T44, '=', T47, F13.6, T61,'eV')  GL0492
1005  FORMAT(1X,'Total energy + Delta-G solvation', T44, '=', T47,      GL0492
     1       F13.6, T61, 'eV')                                          GL0492
1006  FORMAT(1X,'Gradient norm', T44, '=', T47, F13.6, T61, 'kcal/RU')  DJG0495
1007  FORMAT(1X,'For reaction coordinate', T44, '=', T47, F13.6,        GL0592
     1        T61,'angstroms')                                          GL0592
1008  FORMAT(1X,'For reaction coordinate', T44, '=', T47, F13.6,        GL0592
     1       T61, 'degrees')                                            GL0592
1009  FORMAT(1X,'Reaction gradient', T44, '=', T47, F13.6, T61, A14)    GL0592
1010  FORMAT(1X,'HOMO Energy', T44, '=', T47, F13.6, T61, 'eV')         GL0592
1011  FORMAT(1X,'Ionization potential', T44, '=', T47, F13.6, T61,'eV') GL0592
1012  FORMAT(1X,'No. of alpha electrons', T44, '=', T47, I6)            GL0592
1013  FORMAT(1X,'No. of beta  electrons', T44, '=', T47, I6)            GL0592
1014  FORMAT(1X,'No. of doubly occupied orbitals', T44, '=', T47, I6)   GDH1093
1015  FORMAT(1X,'and no. of open levels', T44, '=', T47, I6)            GL0592
1016  FORMAT(1X,'Molecular weight',/,'  (most abundant/longest-lived',  GDH1093
     1       ' isotopes)', T44, '=',T47, F10.3, T61,' amu')             GDH0794
1018  FORMAT(1X,'Computer time', T44, '=', T46, F10.2, T61, 'seconds')  GL0492
1100  FORMAT(1X,'True solvation energy',T44,'=',T49,F7.2,T61,'kcal')    DJG0995
2000  FORMAT(1X,'Configuration interaction was used')                   DJG0995
2010  FORMAT(1X,'Charge on system',T44,'=',T47,I6)                      DJG0995
2020  FORMAT(1X,'(SZ)',T44,'=',T47,F13.6)                               DJG0995
2030  FORMAT(1X,'(S**2)',T44,'=',T47,F13.6)                             DJG0995
      RETURN
      END
