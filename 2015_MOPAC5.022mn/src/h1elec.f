      SUBROUTINE H1ELEC(NI,NJ,XI,XJ,SMAT,*)                             CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XI(3),XJ(3),SMAT(9,9), BI(9), BJ(9)
C***********************************************************************
C
C  H1ELEC FORMS THE ONE-ELECTRON MATRIX BETWEEN TWO ATOMS.
C
C   ON INPUT    NI   = ATOMIC NO. OF FIRST ATOM.
C               NJ   = ATOMIC NO. OF SECOND ATOM.
C               XI   = COORDINATES OF FIRST ATOM.
C               XJ   = COORDINATES OF SECOND ATOM.
C
C   ON OUTPUT   SMAT = MATRIX OF ONE-ELECTRON INTERACTIONS.
C
C***********************************************************************
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1110
      COMMON /BETAS / BETAS(120),BETAP(120),BETAD(120)
      COMMON /BETA3 / BETA3(153)
      COMMON /KEYWRD/ KEYWRD
      COMMON /EULER / TVEC(3,3), ID
      COMMON /VSIPS / VS(120),VP(120),VD(120)
      COMMON /NATORB/ NATORB(120)
      COMMON /UCELL / L1L,L2L,L3L,L1U,L2U,L3U
      COMMON /NUMCAL/ NUMCAL
      COMMON /SRPL/ ISSRP                                               IR0494
      DIMENSION SBITS(9,9), LIMS(3,2), XJUC(3)
      CHARACTER*160 KEYWRD
      DOUBLE PRECISION BSS, BSP, BPP                                    LF0211

      include 'corgen.f'                                                LF0211
      LOGICAL ISSRP                                                     IR0494
      EQUIVALENCE (L1L,LIMS(1,1))
CSAV         SAVE                                                           GL0892
      DATA ITYPE /1/
      DATA ICALCN /0/
CSTP  Return variable from subroutine GETBET, formerly a function
      DOUBLE PRECISION  RGETBT                                          CSTP
      IF(NI.EQ.102.OR.NJ.EQ.102)THEN
         IF(SQRT((XI(1)-XJ(1))**2+
     1        (XI(2)-XJ(2))**2+
     2        (XI(3)-XJ(3))**2) .GT.1.8)THEN
            DO 10 I=1,9
               DO 10 J=1,9
   10       SMAT(I,J)=0.D0
            RETURN
         ENDIF
      ENDIF
      IF(ID.EQ.0) THEN
         CALL DIAT(NI,NJ,XI,XJ,SMAT,*9999)                               CSTP(call)
      ELSE
         IF (ICALCN .NE. NUMCAL) THEN
            ICALCN = NUMCAL
            DO 20 I=1,ID
               LIMS(I,1)=-1
   20       LIMS(I,2)= 1
            DO 30 I=ID+1,3
               LIMS(I,1)=0
   30       LIMS(I,2)=0
         ENDIF
         DO 40 I=1,9
            DO 40 J=1,9
   40    SMAT(I,J)=0.D0
         DO 70 I=L1L,L1U
            DO 70 J=L2L,L2U
               DO 70 K=L3L,L3U
                  DO 50 L=1,3
   50             XJUC(L)=XJ(L)+TVEC(L,1)*I+TVEC(L,2)*J+TVEC(L,3)*K
                  CALL DIAT(NI,NJ,XI,XJUC,SBITS,*9999)                   CSTP(call)
                  DO 60 L=1,9
                     DO 60 M=1,9
   60             SMAT(L,M)=SMAT(L,M)+SBITS(L,M)
   70    CONTINUE
      ENDIF
   80 GOTO (90,100,110) ITYPE
   90 IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         ITYPE=2
      ELSE
         ITYPE=3
      ENDIF
      GOTO 80
  100 CONTINUE
      II=MAX(NI,NJ)
      NBOND=(II*(II-1))/2+NI+NJ-II
      IF(NBOND.GT.153)GOTO 120
      BI(1)=BETA3(NBOND)*VS(NI)
      BI(2)=BETA3(NBOND)*VP(NI)
      BI(3)=BI(2)
      BI(4)=BI(2)
      BJ(1)=BETA3(NBOND)*VS(NJ)
      BJ(2)=BETA3(NBOND)*VP(NJ)
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
      GOTO 120
  110 CONTINUE
      BI(1)=BETAS(NI)*0.5D0
      BI(2)=BETAP(NI)*0.5D0
      BI(3)=BI(2)
      BI(4)=BI(2)
      BI(5)=BETAD(NI)*0.5D0
      BI(6)=BI(5)
      BI(7)=BI(5)
      BI(8)=BI(5)
      BI(9)=BI(5)
      BJ(1)=BETAS(NJ)*0.5D0
      BJ(2)=BETAP(NJ)*0.5D0
      BJ(3)=BJ(2)
      BJ(4)=BJ(2)
      BJ(5)=BETAD(NJ)*0.5D0
      BJ(6)=BJ(5)
      BJ(7)=BJ(5)
      BJ(8)=BJ(5)
      BJ(9)=BJ(5)
  120 CONTINUE
      NORBI=NATORB(NI)
      NORBJ=NATORB(NJ)
      if (pmods(7)) goto 800   ! LF0211
      IF(NORBI.EQ.9.OR.NORBJ.EQ.9) THEN
C
C    IN THE CALCULATION OF THE ONE-ELECTRON TERMS THE GEOMETRIC MEAN
C    OF THE TWO BETA VALUES IS BEING USED IF ONE OF THE ATOMS
C    CONTAINS D-ORBITALS.
         DO 130 J=1,NORBJ
            DO 130 I=1,NORBI
  130    SMAT(I,J)=-2.0D0*SMAT(I,J)*SQRT(BI(I)*BJ(J))
      ELSE IF (ISSRP) THEN                                              IR0494
C
C ...USE SPECIAL SRP BETAs   (Ivan Rossi - April '94)
C
c#         write(6,*)"H1ELEC:"
c#         write(6,*)"NI   = ",NI,  " NJ=    ",NJ
         IF(NORBI .gt. 1 .and. norbj .gt. 1) THEN
           CALL GETBET(NI,NJ,'BETPP',RGETBT,*9999)                      CSTP CSTP(call)
           BPP=RGETBT                                                   CSTP
c#           write(6,*)"Pairwise PP beta turns out to be: ",BPP   
CSTP       BPP=GETBET( NI, NJ, 'BETPP')
           DO 132 JBET=2,4
             DO 132 IBET=2,4
  132          SMAT(IBET,JBET)=SMAT(IBET,JBET)*BPP
         ENDIF
         IF (NORBI .gt. 1 ) THEN
            CALL GETBET(NJ,NI,'BETSP',RGETBT,*9999)                     CSTP CSTP(call)
            BSP=RGETBT                                                  CSTP
c#            write(6,*)"Pairwise PS beta turns out to be: ",BSP   
CSTP        BSP=GETBET( NJ, NI, 'BETSP')
            DO 134 IBET=2,4
  134          SMAT(IBET,1)=SMAT(IBET,1)*BSP
         ENDIF
         IF (NORBJ .gt. 1 ) THEN
            CALL GETBET(NI,NJ,'BETSP',RGETBT,*9999)                     CSTP CSTP(call)
            BSP=RGETBT                                                  CSTP
c#            write(6,*)"Pairwise SP beta turns out to be: ",BSP
CSTP        BSP=GETBET( NI, NJ, 'BETSP')
            DO 136 JBET=2,4
  136          SMAT(1,JBET)=SMAT(1,JBET)*BSP
         ENDIF
         CALL GETBET(NJ,NI,'BETSS',RGETBT,*9999)                        CSTP CSTP(call)
         BSS=RGETBT
c#         write(6,*)"Pairwise SS beta turns out to be: ",BSS
         SMAT(1,1)=SMAT(1,1)*BSS                                        CSTP
CSTP     SMAT(1,1)=SMAT(1,1)*GETBET( NJ, NI, 'BETSS')
c#         write(6,*)"BETAS(NI)=",BETAS(NI)," BETAP(NI)=",BETAP(NI)
c#         write(6,*)"BETAS(NJ)=",BETAS(NJ)," BETAP(NJ)=",BETAP(NJ)
C end of SRP beta
      ELSE
         DO 140 J=1,NORBJ
            DO 140 I=1,NORBI
c#         write(6,*)"H1ELEC:"
c#         write(6,*)"NI= ",NI," NJ=",NJ," I=",I," J=",J
c#         write(6,*)"Pairwise beta turns out to be: ",BI(I)+BJ(J)
c#         write(6,*)"BI(I)=",BI(I)," BJ(J)=",BJ(J)
C     Replacing the old combining rule (arithmetic mean of single-element beta parameters) with my new pairwise parameters.
C    For atoms NA and NB: NA is always no less than NB.
c         if (NI.GE.NJ) then                                             LJF
c           na=ni
c           nb=nj
c           orba=i
c           orbb=j
c         else                                                           LJF
c           na=nj
c           nb=ni
c           orba=j
c           orbb=i
c         endif                                                          LJF
c         npos=((na*(na-1)/2)+nb)*4-3
c         if (orba.EQ.1) then                                            LJF
c           if (orbb.EQ.1) then                                          LJF
c             npos=npos+0                                                LJF
c           elseif (orbb.LE.4) then                                      LJF
c             npos=npos+1                                                LJF
c           endif                                                        LJF
c         elseif (orba.LE.4) then                                        LJF
c           if (orbb.EQ.1) then                                          LJF
c             npos=npos+2                                                LJF
c           elseif (orbb.LE.4) then                                      LJF
c             npos=npos+3                                                LJF
c           endif                                                        LJF
c         endif                                                          LJF
c#         write(6,*) "H1ELEC:"
c#         write(6,*) "NI=",NI," NJ=",NJ," I=",I," J=",J
c#         write(6,*) "NPOS=",NPOS," PRBETA(NPOS)=",PRBETA(NPOS)
c       Old SMAT assignment line that I replace:                         LJF
c  140    SMAT(I,J)=SMAT(I,J)*(BI(I)+BJ(J))
c#         write(6,'(A,I2,A,I2,A,I2,A,I2,A,F16.10,A,I6)')                 LJF
c#     &              "PRBETA of ",ni,",",nj," norbs: ",i,",",j," is ",   LJF
c#     &              prbeta(npos), " with npos=",npos                    LJF
c  140    SMAT(I,J)=SMAT(I,J)*PRBETA(npos)                               LJF
  140    SMAT(I,J)=SMAT(I,J)*(BI(I)+BJ(J))
      ENDIF
  800 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
