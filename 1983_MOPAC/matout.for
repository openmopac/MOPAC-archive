
     SUBROUTINE MATOUT (A,B,NC,NR,NDIM)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
     INCLUDE 'SIZES/NOLIST'
     DIMENSION A(NDIM,NDIM), B(NDIM)
     COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
    1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
    +                NCLOSE,NOPEN
********************************************************************** 
                                                                       
      MATOUT PRINTS A SQUARE MATRIX OF EIGENVECTORS AND EIGENVALUES    
                                                                       
    ON INPUT A CONTAINS THE MATRIX TO BE PRINTED.                      
             B CONTAINS THE EIGENVALUES.                               
             NC NUMBER OF MOLECULAR ORBITALS TO BE PRINTED.            
             NR IS THE SIZE OF THE SQUARE ARRAY TO BE PRINTED.         
             NDIM IS THE ACTUAL SIZE OF THE SQUARE ARRAY "A".          
             NFIRST AND NLAST CONTAIN ATOM ORBITAL COUNTERS.           
             NAT = ARRAY OF ATOMIC NUMBERS OF ATOMS.                   
                                                                       
                                                                       
***********************************************************************
     CHARACTER*2 ELEMNT(99), ATORBS(9), ITEXT(MAXORB), JTEXT(MAXORB)
     DIMENSION NATOM(MAXORB)
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
     IF(NLAST(NUMAT).NE.NR) GOTO 103
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
 102 CONTINUE                                                          
 101 CONTINUE                                                          
     GOTO 104                                                          
 103 CONTINUE                                                          
     DO 105 I=1,NR                                                     
     ITEXT(I)='  '                                                     
     JTEXT(I)='  '                                                     
 105 NATOM(I)=I                                                        
 104 CONTINUE                                                          
     KA=1                                                              
     KC=6                                                              
  10 KB=MIN0(KC,NC)                                                    
     WRITE (6,50) (I,I=KA,KB)                                          
     WRITE (6,60) (B(I),I=KA,KB)                                       
     WRITE (6,70)                                                      
     LA=1                                                              
     LC=40                                                             
  20 LB=MIN0(LC,NR)                                                    
     DO 30 I=LA,LB                                                     
     IF(ITEXT(I).EQ.' S')WRITE(6,70)                                   
        WRITE (6,80) ITEXT(I),JTEXT(I),NATOM(I),(A(I,J),J=KA,KB)       
  30 CONTINUE                                                          
     IF (LB.EQ.NR) GO TO 40                                            
     LA=LC+1                                                           
     LC=LC+40                                                          
     WRITE (6,90)                                                      
     GO TO 20                                                          
  40 IF (KB.EQ.NC) RETURN                                              
     KA=KC+1                                                           
     KC=KC+6                                                           
     IF (NR.GT.25) WRITE (6,90)                                        
     GO TO 10                                                          
                                                                       
  50 FORMAT (////,3X,9H ROOT NO.,I5,9I12)                              
  60 FORMAT (/8X,10F12.6)                                              
  70 FORMAT (2H  )                                                     
  80 FORMAT (1H ,2(1X,A2),I3,F10.6,10F12.6)                                 
  90 FORMAT (1H1)                                                      
                                                                       
     END                                                               