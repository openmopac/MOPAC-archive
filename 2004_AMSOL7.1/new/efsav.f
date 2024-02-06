      SUBROUTINE EFSAV(TT0,HESS,FUNCT,GRAD,XPARAM,PMAT,IL,JL,BMAT,IPOW
     1                 ,NHFLAG)                                         DJG0495
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES.i'
      INCLUDE 'FFILES.i'                                                GDH1095
      CHARACTER ELEMNT*2                                                DJG0995
      DIMENSION HESS(MAXPAR,*),GRAD(*),BMAT(MAXPAR,*),IPOW(9),
     1 XPARAM(*), PMAT(*)
**********************************************************************
*
* EFSAV STORES AND RETRIEVE DATA USED IN THE EF GEOMETRY
*        OPTIMISATION. VERY SIMILAR TO POWSAV.
*
*  ON INPUT HESS   = HESSIAN MATRIX, PARTIAL OR WHOLE.
*           GRAD   = GRADIENTS.
*           XPARAM = CURRENT STATE OF PARAMETERS.
*           IL     = INDEX OF HESSIAN,
*           JL     = CYCLE NUMBER REACHED SO-FAR.
*           BMAT   = "B" MATRIX!
*           IPOW   = INDICES AND FLAGS.
*           IPOW(9)= 0 FOR RESTORE, 1 FOR DUMP, 2 FOR SILENT DUMP
*           NHFLAG = 1 IF HESSIAN IS TO BE RECALCULATED FIRST CYCLE     DJG0495
*                  = 0 IF NOT                                           DJG0495
*
**********************************************************************
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),
     1                     LOCDEP(MAXPAR)
C
C      CHANGED FROM AMPAC AND MOPAC TO AMSOL
C
C WARNING : XPARAM IS PASSED BOTH THROUGH COMMON GEOVAR AND THE
C           ARGUMENT LIST , CHANGED TO XDUMMY IN THE COMMON BLOCK, 
C           IVAN 2/94
C
      COMMON /GEOVAR/ XDUMMY(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY
C
C   THE COMMON BLOCK MOLKST HAS BEEN CONVERTED TO MLKSTI AND MLKSTR
C           ONLY MLKSTI USED
C
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM), 
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        
     2                NCLOSE,NOPEN,NDUMY                                
C
C      CHANGED FROM MOPAC TO AMSOL
C     COMMON /PATH  / LATOM,LPARAM,REACT(200)
C
      COMMON /REPATH/ REACT(100),LATOM,LPARAM
C EF COMMONS
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON/OPTEF/OLDF(MAXPAR),D(MAXPAR),VMODE(MAXPAR),
     $U(MAXPAR,MAXPAR),DD,RMIN,RMAX,OMIN,XLAMD,XLAMD0,SKAL,
     $MODE,NSTEP,NEGREQ,IPRNT
      COMMON /LOCVAR/ LOCVAR(2,MAXPAR)
      COMMON /NUMSCF/ NSCF, FROZEN
      COMMON /VALVAR/ VALVAR(MAXPAR),NUMVAR
      COMMON /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
      COMMON /ALPARM/ ALPARM(3,MAXPAR),X0, X1, X2, JLOOP
      COMMON /TRADCM/ ENGAS, IRAD, ICR, ICHMOD, ICHGM, NUMSLV           GDH0897
      LOGICAL FROZEN
       SAVE
      OPEN(JRES,STATUS='UNKNOWN',FORM='UNFORMATTED')                    GDH1095
      REWIND JRES                                                       GDH1095
      OPEN(JDEN,STATUS='UNKNOWN',FORM='UNFORMATTED')                    GDH1095
      REWIND JDEN                                                       GDH1095
      IF(IPOW(9).GT.0) THEN                                             DJG0495
         FUNCT1=SQRT(DOT(GRAD,GRAD,NVAR))
         IF(IPOW(9).EQ.1)THEN
            WRITE(JOUT,'(//10X,''CURRENT VALUE OF GRADIENT NORM =''
     1  ,F12.6)')FUNCT1
            WRITE(JOUT,'(/10X,''CURRENT VALUE OF GEOMETRY'',/)')
            CALL GEOUT
         ENDIF
C
C  IPOW(1) AND IPOW(9) ARE USED ALREADY, THE REST ARE FREE FOR USE
C  
         IPOW(8)=NSCF
         WRITE(JRES)IPOW,IL,JL,FUNCT,TT0
         WRITE(JRES)(XPARAM(I),I=1,NVAR)
         WRITE(JRES)(  GRAD(I),I=1,NVAR)
         WRITE(JRES)((HESS(J,I),J=1,NVAR),I=1,NVAR)
         WRITE(JRES)((BMAT(J,I),J=1,NVAR),I=1,NVAR)
         WRITE(JRES)
     .      (OLDF(I),I=1,NVAR),(D(I),I=1,NVAR),(VMODE(I),I=1,NVAR)
         WRITE(JRES)DD,MODE,NSTEP,NEGREQ
         LINEAR=(NVAR*(NVAR+1))/2
         WRITE(JRES)(PMAT(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
         WRITE(JDEN)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)WRITE(10)(PB(I),I=1,LINEAR)
         IF(LATOM .NE. 0) THEN
            WRITE(JRES)((ALPARM(J,I),J=1,3),I=1,NVAR)
            WRITE(JRES)JLOOP,X0, X1, X2
         ENDIF
         WRITE(JRES) NHFLAG                                             DJG0495
C         CLOSE(JRES)                             commented out by      BJL1003
C         CLOSE(JDEN)                             commented out by      BJL1003
         RETURN
      ELSE
C#         WRITE(JOUT,'(//10X,'' READING DATA FROM DISK''/)')
         READ(JRES,END=10,ERR=10)IPOW,IL,JL,FUNCT,TT0
         NSCF=IPOW(8)
         I=TT0/1000000
         TT0=TT0-I*1000000
         IF (ICR.NE.2) THEN                                             DJG0995
            WRITE(JOUT,'(//10X,''TOTAL TIME USED SO FAR:'',
     1            F13.2,'' SECONDS'')')TT0
          WRITE(JOUT,'(  10X,''              FUNCTION:'',F17.6)')FUNCT
         ENDIF
         READ(JRES)(XPARAM(I),I=1,NVAR)
         READ(JRES)(  GRAD(I),I=1,NVAR)
         READ(JRES)((HESS(J,I),J=1,NVAR),I=1,NVAR)
         READ(JRES)((BMAT(J,I),J=1,NVAR),I=1,NVAR)
         READ(JRES)
     .     (OLDF(I),I=1,NVAR),(D(I),I=1,NVAR),(VMODE(I),I=1,NVAR)
         READ(JRES)DD,MODE,NSTEP,NEGREQ
         LINEAR=(NVAR*(NVAR+1))/2
         READ(JRES)(PMAT(I),I=1,LINEAR)
         LINEAR=(NORBS*(NORBS+1))/2
C        READ DENSITY MATRIX
         READ(JDEN)(PA(I),I=1,LINEAR)
         IF(NALPHA.NE.0)READ(JDEN)(PB(I),I=1,LINEAR)
         IF(LATOM.NE.0) THEN
            READ(JRES)((ALPARM(J,I),J=1,3),I=1,NVAR)
            READ(JRES)JLOOP,X0, X1, X2
            IL=IL+1
         ENDIF
         READ(JRES) NHFLAG                                              DJG0495
C         CLOSE(JDEN)                             commented out by      BJL1003
C         CLOSE(JDEN)                             commented out by      BJL1003
         RETURN
   10    WRITE(JOUT,'(//10X,''NO RESTART FILE EXISTS!'')')
      ISTOP=1                                                           GDH1095
      IWHERE=34                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF
      RETURN
      END
