      SUBROUTINE HQRII(A,N,M,E,V,*)                                     CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
************************************************************************
      DIMENSION A(*), E(N), V(N,M)
*************************************************************
*
* HQRII IS A DIAGONALISATION ROUTINE, WRITTEN BY YOSHITAKA BEPPU OF
*       NAGOYA UNIVERSITY, JAPAN.
*       FOR DETAILS SEE 'COMPUTERS & CHEMISTRY' VOL.6 1982. PAGE 000.
*
* ON INPUT    A       = MATRIX TO BE DIAGONALIZED
*             N       = SIZE OF MATRIX TO BE DIAGONALIZED.
*             M       = NUMBER OF EIGENVECTORS NEEDED.
*             E       = ARRAY OF SIZE AT LEAST N
*             V       = ARRAY OF SIZE AT LEAST NMAX*M
*
* ON OUTPUT   E       = EIGENVALUES
*             V       = EIGENVECTORS IN ARRAY OF SIZE NMAX*M
*
************************************************************************
      parameter(isize=3*numatm)
      dimension fv1(isize),sqa(isize*isize)
         SAVE                                                           GL0892
      l=1
      do 5 j=1,n
      do 5 i=1,j
      sqa(j+(i-1)*n)=a(l)
      l=l+1
5     continue
      call tred3(n,isize*isize,sqa,e,fv1,v,*9999)                        CSTP(call) / LF0610
c#      write(6,*) "HQRII calling TQL2."
      call tql2(n,n,e,fv1,v,ierr,*9999)                                  CSTP(call)
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY HQRII_INIT                                                  CSAV
               FV1 = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
              IERR = 0                                                  CSAV
                 J = 0                                                  CSAV
                 L = 0                                                  CSAV
               SQA = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
