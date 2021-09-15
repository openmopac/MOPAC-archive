
     SUBROUTINE VECPRT (A,NUMB)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
     INCLUDE 'SIZES/NOLIST'
     DIMENSION  A(*)
     COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
    +                NCLOSE,NOPEN
********************************************************************** 
                                                                       
  VECPRT PRINTS A LOWER-HALF TRIANGLE OF A SQUARE MATRIX, THE          
         LOWER-HALF TRIANGLE BEING STORED IN PACKED FORM IN THE        
         ARRAY "A"                                                     
                                                                       
 ON INPUT:                                                             
      A      = ARRAY TO BE PRINTED                                     
      NUMB   = SIZE OF ARRAY TO BE PRINTED                             
(REF) NUMAT  = NUMBER OF ATOMS IN THE MOLECULE (THIS IS NEEDED TO      
               DECIDE IF AN ATOMIC ARRAY OR ATOMIC ORBITAL ARRAY IS    
               TO BE PRINTED                                           
(REF) NAT    = LIST OF ATOMIC NUMBERS                                  
(REF) NFIRST = LIST OF ORBITAL COUNTERS                                
(REF) NLAST  = LIST OF ORBITAL COUNTERS                                
                                                                       
  NONE OF THE ARGUMENTS ARE ALTERED BY THE CALL OF VECPRT              
                                                                       
*********************************************************************  
     DIMENSION NATOM(MAXORB)
     CHARACTER * 6 LINE(21)                                            
     CHARACTER*2 ELEMNT(99),ATORBS(9), ITEXT(MAXORB),JTEXT(MAXORB)
     DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/           
     DATA ELEMNT/'H','He',
    2 'Li','Be','B','C','N','O','F','Ne',
    3 'Na','Mg','Al','Si','P','S','Cl','Ar',
    4 'K','Ca','Sc','Ti','V','Cr','Mn','Fe','Co','Ni','Cu',
    4 'Zn','Ga','Ge','As','Se','Br','Kr',
    5 'Rb','Sr','Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag',
    5 'Cd','In','Sn','Sb','Te','I','Xe',
    6 'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
    6 'Ho','Er','Tm','Yb','Lu','Hf','Ta','W','Re','Os','Ir','Pt',
    6 'Au','Hg','Tl','Pb','Bi','Po','At','Rn',
    7 'Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk','Cf','XX'/
     IF(NUMAT.EQ.NUMB) THEN
                                                                       
    PRINT OVER ATOM COUNT                                              
                                                                       
         DO 103 I=1,NUMAT                                                  
             ITEXT(I)='  '                                                     
             JTEXT(I)=ELEMNT(NAT(I))                                            
             NATOM(I)=I                                                        
 103     CONTINUE                                                          
         ELSE
         IF (NLAST(NUMAT) .EQ. NUMB) THEN
             DO 101 I=1,NUMAT                                                  
                 JLO=NFIRST(I)                                                     
                 JHI=NLAST(I)                                                      
                 L=NAT(I)                                                          
                 K=0                                                               
                 DO 102 J=JLO,JHI                                                  
                     K=K+1                                                             
                     ITEXT(J)=ATORBS(K)                                                  
                     JTEXT(J)=ELEMNT(L)                                                 
                     NATOM(J)=I                                                        
 102             CONTINUE                                                          
 101         CONTINUE                                                          
             ELSE
             DO 104 I=1,NUMB
             ITEXT(I) = '  '
             JTEXT(I) = '  '
 104         NATOM(I)=I                                                        
             ENDIF
         END IF 
     DO 10 I=1,21                                                      
  10     LINE(I)='------'                                                  
     LIMIT=(NUMB*(NUMB+1))/2                                           
     KK=8                                                              
     NA=1                                                              
  20 LL=0                                                              
     M=MIN0((NUMB+1-NA),6)                                             
     MA=2*M+1                                                          
     M=NA+M-1                                                          
     WRITE(6,60)(ITEXT(I),JTEXT(I),NATOM(I),I=NA,M)                    
     WRITE (6,70) (LINE(K),K=1,MA)                                     
     DO 40 I=NA,NUMB                                                   
        LL=LL+1                                                        
        K=(I*(I-1))/2                                                  
        L=MIN0((K+M),(K+I))                                            
        K=K+NA                                                         
        IF ((KK+LL).LE.50) GO TO 30                                    
        WRITE (6,80)                                                   
        WRITE (6,60) (ITEXT(N),JTEXT(N),NATOM(N),N=NA,M)               
        WRITE (6,70) (LINE(N),N=1,MA)                                  
        KK=4                                                           
        LL=0                                                           
  30    WRITE (6,90) ITEXT(I),JTEXT(I),NATOM(I),(A(N),N=K,L)           
  40 CONTINUE                                                          
     IF (L.GE.LIMIT) GO TO 50                                          
     KK=KK+LL+4                                                        
     NA=M+1                                                            
     IF ((KK+NUMB+1-NA).LE.50) GO TO 20                                
     KK=4                                                              
     WRITE (6,80)                                                      
     GO TO 20                                                          
  50 RETURN                                                            
                                                                       
  60 FORMAT (1H0/9X,10(2X,A2,1X,A2,I3,1X))                               
  70 FORMAT (1H ,21A6)                                                 
  80 FORMAT (1H1)                                                      
  90 FORMAT (1H ,A2,1X,A2,I3,10F11.6)                                   
                                                                       
     END                                                               