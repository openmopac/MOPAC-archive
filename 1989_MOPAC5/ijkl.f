      SUBROUTINE IJKL(I1, I2, J1, J2, ELEM, A1, MDIM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION A1(MDIM,NMECI)
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
************************************************************************
*
*   IJKL FILLS THE TWO-ELECTRON MATRIX XY WITH REPULSION INTEGRALS.
*        XY(I,J,K,L) IS THE REPULSION BETWEEN ONE ELECTRON IN
*        M.O.S I AND J AND AN ELECTRON IN M.O.S K AND L.
*        <I1(1),J1(1)/I2(2),J2(2)>
************************************************************************
      COMMON /WMATRX/ WJ(N2ELEC), WK(N2ELEC)
      DIMENSION W(N2ELEC)
      EQUIVALENCE (W,WJ)
      REAL WJ, WK
      IF (XY(I1,J1,I2,J2).EQ.100.0D0) THEN
         X= SPCG(A1(1,I1),A1(1,J1),A1(1,I2),A1(1,J2),W,WJ)
C#          WRITE(6,'(4I6,F13.6)')I1,J1,I2,J2,X
         XY(I1,J1,I2,J2)=X
         XY(I1,J1,J2,I2)=X
         XY(J1,I1,I2,J2)=X
         XY(J1,I1,J2,I2)=X
         XY(I2,J2,I1,J1)=X
         XY(I2,J2,J1,I1)=X
         XY(J2,I2,I1,J1)=X
         XY(J2,I2,J1,I1)=X
      ENDIF
      IF (XY(I1,I2,J1,J2).EQ.100.0D0) THEN
         Z= SPCG(A1(1,I1),A1(1,I2),A1(1,J1),A1(1,J2),W,WJ)
C#          WRITE(6,'(4I6,F13.6)')I1,I2,J1,J2,Z
         XY(I1,I2,J1,J2)=Z
         XY(I1,I2,J2,J1)=Z
         XY(I2,I1,J1,J2)=Z
         XY(I2,I1,J2,J1)=Z
         XY(J1,J2,I1,I2)=Z
         XY(J1,J2,I2,I1)=Z
         XY(J2,J1,I1,I2)=Z
         XY(J2,J1,I2,I1)=Z
      ENDIF
      IF (XY(I1,J2,I2,J1).EQ.100.0D0) THEN
         Y= SPCG(A1(1,I1),A1(1,J2),A1(1,I2),A1(1,J1),W,WJ)
C#          WRITE(6,'(4I6,F13.6)')I1,J2,I2,J1,Y
         XY(I1,J2,I2,J1)=Y
         XY(I1,J2,J1,I2)=Y
         XY(J2,I1,I2,J1)=Y
         XY(J2,I1,J1,I2)=Y
         XY(I2,J1,I1,J2)=Y
         XY(I2,J1,J2,I1)=Y
         XY(J1,I2,I1,J2)=Y
         XY(J1,I2,J2,I1)=Y
      ENDIF
      X=XY(I1,J1,I2,J2)
      Y=XY(I1,J2,J1,I2)
      ELEM=X-Y
      RETURN
      END
