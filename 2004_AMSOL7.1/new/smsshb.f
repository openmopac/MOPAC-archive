      SUBROUTINE SMSSHB(IAT,ARED3)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
         INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'
       INCLUDE 'KEYS.i'
C******************************************************************************
C
C   THIS SUBROUTINE CALCULATES SURFACE TENSIONS WHICH ARE LATER
C   MULTIPLIED TIMES SASA TO GET THE CDS TERM.
C
C   CREATED BY GDH 0296 FROM EXISTING LINES IN SM5HB
C
C******************************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS, NALPHA, NBETA,
     2                NCLOSE,NOPEN,NDUMY
      COMMON /VOLCOM/ RLIO(NUMATM,NUMATM),RMAX(NUMATM)
     1               ,URLIO(3,NUMATM,NUMATM)                            DL0397
      COMMON /HBORDS/ HBORD(NUMATM), HB(NUMATM)
      COMMON /WRITCM/ STS0(100),STS1(100)
      COMMON /PDCOM/  SMXPD(100), SPSRFT(10)                            GDH1095
      SAVE
            SSST=SPSRFT(9)
            RTKK=0.0D0
            RECCC=2.75D0
            DELTAR=0.3D0
            BCCC=1.0D0
            DO 550 J=1,NUMAT
               IF(J.EQ.IAT)GOTO 550
               IF(NAT(J).EQ.16)THEN
                  CUTOF1=RECCC+DELTAR
                  IF(RLIO(IAT,J).LT.CUTOF1)THEN
                     RTKK=EXP(1.0D0-(BCCC/
     X                  (1.0D0-((RLIO(IAT,J)-RECCC)/DELTAR))))
     X                  /(EXP(1.0D0))
                  ENDIF
                  HB(IAT)=HB(IAT)+RTKK*SSST*ARED3
                  STS1(3)=STS1(3)+RTKK*ARED3
               ENDIF
550         CONTINUE
       RETURN
       END
