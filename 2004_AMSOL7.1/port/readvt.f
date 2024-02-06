      SUBROUTINE READVT (LEC,IPRT,VECT,NDIM) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
C     UNFORMATED READ OF THE VECTOR VECT OF LENGTH NDIM ON UNIT # LEC. 
C     ERROR MESSAGES PRINTED ON UNIT IPRT. 
C     THE DATA MUST BE SEPARATED BY AT LEAST ONE OF THESE CHARACTERS : 
C     BLANK   COMMA   SEMI-COLUMN   SLASH, 
C     AND A VALUE MUST NOT STAND ON MORE THAN ONE 'CARD'. 
C     NOTE ... A BLANK 'CARD' WILL NOT BE INTERPRETED AS ZERO, 
C                                  BUT 
C              'nn*vv' IS ALLOWED, FULFILLING THE VECTOR VECT WITH 
C              nn CONSECUTIVE VALUE vv. 
      DIMENSION VECT(*) 
      CHARACTER LINE*80,SPACE*1,COMMA*1,SCOLUM*1,SLASH*1,STAR*1 
      LOGICAL LEADSP,FLAG 
      SAVE
      DATA COMMA,SPACE,SCOLUM,SLASH,STAR 
     ./     ',' , ' ' , ';'  , '/' , '*'                       / 
C 
      IPOS=0 
   10 READ (LEC,'(A)',END=40,ERR=50) LINE 
C     CONVERT ALL ALLOWED SEPARATOR INTO SPACE 
      DO 20 I=1,80 
      IF(LINE(I:I).EQ.COMMA .OR. LINE(I:I).EQ.SCOLUM .OR. 
     .   LINE(I:I).EQ.SLASH                           ) LINE(I:I)=SPACE 
   20 CONTINUE 
C     FULFIL THE VECTOR VECT 
      LEADSP=.TRUE. 
      FLAG=.FALSE. 
      DO 30 I=1,80 
      LEADSP=LEADSP.AND.LINE(I:I).NE.SPACE.AND.LINE(I:I).NE.STAR 
      IF(LEADSP) THEN 
         IPOS=IPOS+1 
         VECT(IPOS)=READIF(LINE,I) 
         IF(FLAG) THEN 
            FLAG=.FALSE. 
            VAL=VECT(IPOS) 
            MULT=VECT(IPOS-1) 
            IPOS=IPOS-2 
            DO 25 J=1,MULT 
            IPOS=IPOS+1 
   25       VECT(IPOS)=VAL 
         ENDIF 
      ENDIF 
      FLAG=  LINE(I:I).EQ.STAR  .OR. FLAG 
   30 LEADSP=LINE(I:I).EQ.SPACE .OR. FLAG 
      IF(IPOS.LT.NDIM) GO TO 10 
C     DEBUGG 
      IF(IPOS.EQ.NDIM) RETURN 
      WRITE(IPRT,'('' DATA VECTOR OF LENGTH'',I4,'' GREATER THAN'',I4)') 
     .           IPOS,NDIM 
      GO TO 50 
   40 WRITE(IPRT,'('' UNEXPECTED END OF DATA ...'')') 
   50 WRITE(IPRT,'('' READ ERROR ON UNIT'',I3,'' THE DATA ARE :'',A)') 
     .           LEC,LINE 
      WRITE(IPRT,'('' THUS THE RUN STOPPED AT THIS POINT IN ROUTINE'', 
     .             '' READVT'')') 
         ISTOP=1                                                        GDH1095
         IWHERE=164                                                     GDH1095
         RETURN                                                         GDH1095
      END 
