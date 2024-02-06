      SUBROUTINE DIJKL2 (DC,N,NA,DIJKL,WIJKL,NMECI) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
      EXTERNAL SDOT                                                     GL0492
C-------------------------------------------------------------------- 
C     RELAXATION OF 2-ELECTRONS INTEGRALS IN M.O BASIS. 
C 
C   INPUT 
C     DC(N,NA) : C.I-ACTIVE M.O DERIVATIVES IN M.O BASIS, IN COLUMN. 
C     N        : TOTAL NUMBER OF M.O. 
C     NA       : NUMBER OF C.I-ACTIVE M.O. 
C     DIJKL(I,J,KL) : <I(1),J(1)!K(2),L(2)> WITH 
C                     I              OVER     ALL    M.O. 
C                     J,KL CANONICAL OVER C.I-ACTIVE M.O. 
C     NMECI    : MAX. SIZE OF WIJKL. (NA <= NMECI). 
C   OUTPUT 
C     WIJKL(I,J,K,L)= d< I(1),J(1) ! K(2),L(2) > 
C                   = <dI,J!K,L> + <I,dJ!K,L> + <I,J!dK,L> + <I,J!K,dL> 
C                     WITH I,J,K,L OVER ALL C.I-ACTIVE M.O. 
C     D.L. (DEWAR GROUP) 1986 
C--------------------------------------------------------------------- 
      DIMENSION DC(N,*),WIJKL(NMECI,NMECI,NMECI,NMECI) 
      DIMENSION DIJKL(N,NA,*) 
      LOGICAL LIJ,LKL 
       SAVE
C 
      IJ=0 
      DO 10 I=1,NA 
      DO 10 J=1,I 
      IJ=IJ+1 
      LIJ=I.EQ.J 
      KL=0 
      DO 10 K=1,I 
      IF(K.EQ.I) THEN 
         LL=J 
      ELSE 
         LL=K 
      ENDIF 
      DO 10 L=1,LL 
      KL=KL+1 
      LKL=K.EQ.L 
      VAL=               SDOT(N,DC(1,I),1,DIJKL(1,J,KL),1) 
      IF(LIJ.AND.LKL.AND.J.EQ.K) THEN 
         VAL=VAL*4.D0 
      ELSE 
         IF(LIJ) THEN 
            VAL=VAL*2.D0 
         ELSE 
            VAL=VAL+     SDOT(N,DC(1,J),1,DIJKL(1,I,KL),1) 
         ENDIF 
         VAL2=           SDOT(N,DC(1,K),1,DIJKL(1,L,IJ),1) 
         IF(LKL) THEN 
            VAL=VAL+VAL2*2.D0 
         ELSE 
            VAL=VAL+VAL2+SDOT(N,DC(1,L),1,DIJKL(1,K,IJ),1) 
         ENDIF 
      ENDIF 
      WIJKL(I,J,K,L)=VAL 
      WIJKL(I,J,L,K)=VAL 
      WIJKL(J,I,K,L)=VAL 
      WIJKL(J,I,L,K)=VAL 
      WIJKL(K,L,I,J)=VAL 
      WIJKL(K,L,J,I)=VAL 
      WIJKL(L,K,I,J)=VAL 
   10 WIJKL(L,K,J,I)=VAL 
      RETURN 
      END 
