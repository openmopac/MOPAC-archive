      SUBROUTINE MXT(A,B,NSZ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES.i'                                                GDH1294
      DIMENSION A(NSZ,NSZ),B(NSZ,NSZ)                                   GDH1294
      SAVE                                                              GDH1294
      DO 10 J=1,NSZ                                                     GDH1294
         DO 20 I=1,NSZ                                                  GDH1294
   20     B(J,I)=A(I,J)                                                 GDH1294
   10 CONTINUE                                                          GDH1294
      RETURN 
      END
