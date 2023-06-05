      SUBROUTINE CALPAR(*)                                              CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /ONELEC/ USS(120),UPP(120),UDD(120)
     1       /ATOMIC/ EISOL(120),EHEAT(120)
     2       /ALPHA / ALP(120)
     3       /EXPONT/ ZS(120),ZP(120),ZD(120)
     4       /GAUSSB/ FN1(120),FN2(120)
     5       /BETAS / BETAS(120),BETAP(120),BETAD(120)
     6       /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120),
     7                GSD(120),GPD(120),GDD(120)
     8       /IDEAS / GAUSS1(120,10), GAUSS2(120,10), GAUSS3(120,10)
      COMMON /MNDO/  USSM(120), UPPM(120), UDDM(120), ZSM(120),ZPM(120),
     1ZDM(120), BETASM(120), BETAPM(120), BETADM(120), ALPM(120),
     2eISOLM(120), DDM(120), QQM(120), AMM(120), ADM(120), AQM(120)
     3,GSSM(120),GSPM(120),GPPM(120),GP2M(120),HSPM(120), POLVOM(120)
      COMMON /KEYWRD/ KEYWRD
      COMMON/MULTIP/ DD(120),QQ(120),AM(120),AD(120),AQ(120)
      COMMON /HPUSED/ HPUSED                                            LF0210
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      LOGICAL HPUSED                                                    LF0210
      DIMENSION NSPQN(120)
      CHARACTER KEYWRD*160
      DIMENSION USSC(120), UPPC(120), GSSC(120), GSPC(120), HSPC(120),
     1GP2C(120), GPPC(120), UDDC(120), GSDC(120), GDDC(120)
CSAV         SAVE                                                           GL0892
      DATA NSPQN/2*1,8*2,8*3,18*4,18*5,32*6,21*0,
     &           13*0/                                                  LF0710
C
C THE CONTINUATION LINES INDICATE THE PRINCIPAL QUANTUM NUMBER.
C
      DATA USSC/
     11.D0,                                                      0.D0,
     21.D0,                                               6*2.D0,0.D0,
     31.D0,                                               6*2.D0,0.D0,
     41.D0,4*2.D0,1.D0,4*2.D0,1.D0,                       6*2.D0,0.D0,
     51.D0,3*2.D0,2*1.D0,2.D0,2*1.D0,0.D0,1.D0,           6*2.D0,0.D0,
     61.D0,22*2.D0,1.D0,1.D0,                             6*2.D0,0.D0,
     721*0.D0,
     &13*0.D0/                                                          LF0710
      DATA  UPPC/
     1 2*0.D0,
     2 2*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     3 2*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     412*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     512*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     626*0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,
     721*0.D0,
     &13*0.D0/                                                          LF0710
      DATA UDDC/18*0.D0,
     1 2*0.D0,1.D0,2.D0,3.D0,5.D0,5.D0,6.D0,7.D0,8.D0,1.D1,1.D1,6*0.D0,
     2 2*0.D0,1.D0,2.D0,4.D0,5.D0,5.D0,7.D0,8.D0,1.D1,1.D1,1.D1,6*0.D0,
     3 2*0.D0,1.D0,6*0.D0,1.D0,6*0.D0,1.D0,2.D0,3.D0,4.D0,
     4                           5.D0,6.D0,7.D0,9.D0,1.D1,1.D1,6*0.D0,
     5 21*0.D0,
     & 13*0.D0/                                                         LF0710
      DATA GSSC/2*0.D0,
     1 0.D0,6*1.D0,0.D0,
     2 0.D0,6*1.D0,0.D0,
     3 0.D0,4*1.D0,0.D0,4*1.D0,0.D0,6*1.D0,0.D0,
     4 0.D0,3*1.D0,7*0.D0,6*1.D0,0.D0,
     5 0.D0,22*1.D0,2*1.D0,6*1.D0,0.D0,
     6 21*0.D0,
     & 13*0.D0/                                                          LF0710
      DATA GSPC/2*0.D0,
     1 2*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     2 2*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     312*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     412*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     526*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,0.D0,
     621*0.D0,
     &13*0.D0/                                                          LF0710
      DATA HSPC/2*0.D0,
     1 2*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     2 2*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     312*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     412*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     526*0.D0,-1.D0,-2.D0,-3.D0,-4.D0,-5.D0,0.D0,
     621*0.D0,
     &13*0.D0/                                                          LF0710
      DATA GP2C/2*0.D0,
     1 3*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     2 3*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     313*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     413*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     527*0.D0,1.5D0,4.5D0,6.5D0,10.D0,0.D0,
     621*0.D0,
     &13*0.D0/                                                          LF0710
      DATA GPPC/2*0.D0,
     1 3*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     2 3*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     313*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     413*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     527*0.D0,-0.5D0,-1.5D0,-0.5D0,2*0.D0,
     621*0.D0,
     &13*0.D0/                                                          LF0710
     7GSDC/18*0.D0,
     8 2*0.D0,2.D0,4.D0,6.D0,5.D0,10.D0,12.D0,14.D0,16.D0,10.D0,7*0.D0,
     9 2*0.D0,2.D0,4.D0,4.D0,5.D0,6.D0,7.D0,8.D0,0.D0,1.D1,7*0.D0,
     1 2*0.D0,2.D0,6*0.D0,2.D0,6*0.D0,2.D0,4.D0,6.D0,8.D0,10.D0,12.D0,
     2                                         14.D0,9.D0,10.D0,7*0.D0,
     321*0.D0,
     &13*0.D0/                                                          LF0710
      DATA GDDC/18*0.D0,
     1 3*0.D0,1.D0,3.D0,10.D0,10.D0,15.D0,21.D0,28.D0,8*0.D0,
     2 3*0.D0,1.D0,6.D0,10.D0,15.D0,21.D0,28.D0,45.D0,8*0.D0,
     317*0.D0,1.D0,3.D0, 6.D0,10.D0,15.D0,21.D0,36.D0,8*0.D0,
     421*0.D0,
     &13*0.D0/                                                          LF0710
C
C  The DATA block shown above is derived from the ground-state atomic
C  configuration of the elements.  In checking it, pay careful attention
C  to the actual ground-state configuration. Note also that there are no
C  configurations which have both p and d electrons in the valence shell

C  LF0210: When p orbital-containg hydrogen atom symbols ("Hp") are used
C          they are reassigned to atomic number 9 and fluorines are 
C          substituted over.  
C          The USSC, UPPC, etc. constants indicate the degree of occupation
C          of the various atomic orbitals for the isolated atom and
C          are used to calculate the isolated atom energy, EISOL.  These
C          should be set to those of hydrogen for the fluorine atom when
C          Hp atom symbols are used.
      IF (HPUSED) THEN                                                  LF0210
         NSPQN(9)=2
         USSC(9)=USSC(1)
         UPPC(9)=UPPC(1)
         UDDC(9)=UDDC(1)
         GSSC(9)=GSSC(1)
         GSPC(9)=GSPC(1)
         HSPC(9)=HSPC(1)
         GP2C(9)=GP2C(1)
         GPPC(9)=GPPC(1)
         GSDC(9)=GSDC(1)
         GDDC(9)=GDDC(1)
      ENDIF
C
C
C     SET SCALING PARAMETER.
      P=2.D0
      P2=P*P
      P4=P**4
      DO 30 I=2,108
         IF (I.GT.98.AND.I.NE.108) GOTO 30                              LF1110
         IF(ZP(I).LT.1.D-4.AND.ZS(I).LT.1.D-4)GOTO 30
**********************************************************************
*
*   CONSTRAINTS ON THE POSSIBLE VALUES OF PARAMETERS
*
**********************************************************************
         IF(ZP(I).LT.0.3D0) ZP(I)=0.3D0
C  PUT IN ANY CONSTRAINTS AT THIS POINT
**********************************************************************
         HPP=0.5D0*(GPP(I)-GP2(I))
         HPP=MAX(0.1D0,HPP)
         HSP(I)=MAX(1.D-7,HSP(I))
         EISOL(I)=USS(I)*USSC(I)+UPP(I)*UPPC(I)+UDD(I)*UDDC(I)+
     1         GSS(I)*GSSC(I)+GPP(I)*GPPC(I)+GSP(I)*GSPC(I)+
     2         GP2(I)*GP2C(I)+HSP(I)*HSPC(I)+GSD(I)*GSDC(I)+
     3         GDD(I)*GDDC(I)
         QN=NSPQN(I)
         DD(I)=(2.D0*QN+1)*(4.D0*ZS(I)*ZP(I))**(QN+0.5D0)/(ZS(I)+ZP(I))
     1**(2.D0*QN+2.D0)/SQRT(3.D0)
         IF (HPUSED.AND.I.EQ.9) DD(I)=32.D0*(ZP(I)*ZS(I))**(1.5D0)*ZP(I) LF0410
     &                                / (ZS(I)+ZP(I))**5                 LF0410
         DDM(I)=DD(I)
         QQ(I)=SQRT((4.D0*QN*QN+6.D0*QN+2.D0)/20.D0)/ZP(I)
         QQM(I)=QQ(I)
C     CALCULATE ADDITIVE TERMS, IN ATOMIC UNITS.
         JMAX=5
         GDD1= (P2*HSP(I)/(27.2111D0* 4.D0*DD(I)**2))**(1.D0/3.D0)
         GQQ= (P4*HPP/(27.2111D0*48.D0*QQ(I)**4))**0.2D0
         D1=GDD1
         D2=GDD1+0.04D0
         Q1=GQQ
         Q2=GQQ+0.04D0
         DO 10 J=1,JMAX
            DF=D2-D1
            HSP1= 2.D0*D1 - 2.D0/SQRT(4.D0*DD(I)**2+1.D0/D1**2)
            HSP2= 2.D0*D2 - 2.D0/SQRT(4.D0*DD(I)**2+1.D0/D2**2)
            HSP1= HSP1/P2
            HSP2= HSP2/P2
            D3= D1 + DF*(HSP(I)/27.2111D0-HSP1)/(HSP2-HSP1)
            D1= D2
            D2= D3
   10    CONTINUE
         DO 20 J=1,JMAX
            QF=Q2-Q1
            HPP1= 4.D0*Q1 - 8.D0/SQRT(4.D0*QQ(I)**2+1.D0/Q1**2)
     1            + 4.D0/SQRT(8.D0*QQ(I)**2+1.D0/Q1**2)
            HPP2= 4.D0*Q2 - 8.D0/SQRT(4.D0*QQ(I)**2+1.D0/Q2**2)
     1            + 4.D0/SQRT(8.D0*QQ(I)**2+1.D0/Q2**2)
            HPP1= HPP1/P4
            HPP2= HPP2/P4
            Q3= Q1 + QF*(HPP/27.2111D0-HPP1)/(HPP2-HPP1)
            Q1= Q2
            Q2= Q3
   20    CONTINUE
         AM(I)= GSS(I)/27.2111D0
         AD(I)= D2
         AQ(I)= Q2
         AMM(I)=AM(I)
         ADM(I)=AD(I)
         AQM(I)=AQ(I)
   30 CONTINUE
      EISOL(1)=USS(1)
      AM(1)=GSS(1)/27.2111D0
      AD(1)=AM(1)
      AQ(1)=AM(1)
      AMM(1)=AM(1)
      ADM(1)=AD(1)
      AQM(1)=AQ(1)
C
C     DEBUG PRINTING.
C     THIS IS FORMATTED FOR DIRECT INSERTION INTO 'PARAM'
C
      IF(INDEX(KEYWRD,'DEP').EQ.0) RETURN
      IF (DOPRNT) WRITE(6,50)                                           CIO
      IF (.NOT.DOPRNT) GOTO 61                                          CIO
      DO 60 I=1,120
         IF(ZS(I).EQ.0) GOTO 60
         IF (DOPRNT) WRITE(6,'(''C'',20X,''DATA FOR ELEMENT'',I3)')I    
         IF (DOPRNT) WRITE(6,'(6X,''DATA USS   ('',I3,'')/'',F16.7,     
     &                       ''D0/'')') I,USS(I)                        
         IF(UPP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA UPP   ('',I3,'')/'',F16.7,''D0/'')')I,UPP(I)  
         IF(UDD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA UDD   ('',I3,'')/'',F16.7,''D0/'')')I,UDD(I)  
         IF(BETAS(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA BETAS ('',I3,'')/'',F16.7,''D0/'')')
     2I,BETAS(I)
         IF(BETAP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA BETAP ('',I3,'')/'',F16.7,''D0/'')')
     2I,BETAP(I)
         IF(BETAD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA BETAD ('',I3,'')/'',F16.7,''D0/'')')
     2I,BETAD(I)
         WRITE(6,'(6X,''DATA ZS    ('',I3,'')/'',F16.7,''D0/'')')
     1I,ZS(I)
         IF(ZP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA ZP    ('',I3,'')/'',F16.7,''D0/'')')I,ZP(I)
         IF(ZD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA ZD    ('',I3,'')/'',F16.7,''D0/'')')I,ZD(I)
         WRITE(6,'(6X,''DATA ALP   ('',I3,'')/'',F16.7,''D0/'')')
     1I,ALP(I)
         WRITE(6,'(6X,''DATA EISOL ('',I3,'')/'',F16.7,''D0/'')')
     1I,EISOL(I)
         IF(GSS(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GSS   ('',I3,'')/'',F16.7,''D0/'')')
     2I,GSS(I)
         IF(GSP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GSP   ('',I3,'')/'',F16.7,''D0/'')')
     2I,GSP(I)
         IF(GPP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GPP   ('',I3,'')/'',F16.7,''D0/'')')
     2I,GPP(I)
         IF(GP2(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA GP2   ('',I3,'')/'',F16.7,''D0/'')')
     2I,GP2(I)
         IF(HSP(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA HSP   ('',I3,'')/'',F16.7,''D0/'')')
     2I,HSP(I)
         IF(DD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA DD    ('',I3,'')/'',F16.7,''D0/'')')I,DD(I)
         IF(QQ(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA QQ    ('',I3,'')/'',F16.7,''D0/'')')I,QQ(I)
         WRITE(6,'(6X,''DATA AM    ('',I3,'')/'',F16.7,''D0/'')')
     1I,AM(I)
         IF(AD(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA AD    ('',I3,'')/'',F16.7,''D0/'')')I,AD(I)
         IF(AQ(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA AQ    ('',I3,'')/'',F16.7,''D0/'')')I,AQ(I)
         IF(FN1(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA FN1   ('',I3,'')/'',F16.7,''D0/'')')I,FN1(I)
         IF(FN2(I) .NE. 0.D0)
     1WRITE(6,'(6X,''DATA FN2   ('',I3,'')/'',F16.7,''D0/'')')I,FN2(I)
         DO 40 J=1,10
            IF(GAUSS1(I,J) .EQ.0.D0) GOTO 40
            WRITE(6,'(6X,''DATA GAUSS1('',I3,'','',I1,'')/'',
     1F16.7,''D0/'')')I,J,GAUSS1(I,J)
            WRITE(6,'(6X,''DATA GAUSS2('',I3,'','',I1,'')/'',
     1F16.7,''D0/'')')I,J,GAUSS2(I,J)
            WRITE(6,'(6X,''DATA GAUSS3('',I3,'','',I1,'')/'',
     1F16.7,''D0/'')')I,J,GAUSS3(I,J)
   40    CONTINUE
   50    FORMAT(1H ,1X,'OUTPUT INCLUDES DEBUG INFORMATION',//)
   60 CONTINUE
   61 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
