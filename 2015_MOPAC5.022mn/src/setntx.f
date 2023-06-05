      SUBROUTINE SETNTX
      IMPLICIT NONE
      COMMON /INTEXT/ KEYTXT, COMTXT, TLETXT, SPCTXT(80),
     &                CINTXT, FINTXT,
     &                LINTXT
      CHARACTER*160 KEYTXT
      CHARACTER*80  COMTXT, TLETXT, SPCTXT
      INTEGER       CINTXT, FINTXT
      LOGICAL       LINTXT

C ***********************************************************************
C
C  Luke Fiedler 07/2010:
C
C  Subroutine
C    SETNTX    This subroutine sets all the virtual input file text and
C              values (other than LINTXT) for the common block /INTEXT/
C              which contains all of the keywords, comment lines, and
C              molecular/job specification information.
C
C
C ***********************************************************************

C     Redundantly set LINTXT to .true. if using this subroutine.
      LINTXT=.TRUE.

C    The following is a sample of input lines that can be used by this
C    subroutine:
C     Set up keyword, comment, and title lines here.
      KEYTXT="AM1 C.I.=(3,1) CHARGE=1 MECI PRECISE GRAD VECTORS EF"
      COMTXT="Allyl cation.  Using virtual input file."
      TLETXT="Final Heat of formation should be 219.415 KCAL/MOL"

C    The following is a sample of input lines that can be used by this
C    subroutine:
C     Set up molecular specifications and any other lines necessary to
C     fully specify job (e.g. symmetry specifications or velocity vectors).

c1       FINTXT=2
c1       SPCTXT(1)=" H        "
c1       SPCTXT(2)=" H  1.0  1"

C    The following is another sample of input lines that can be used by this
C    subroutine:
      FINTXT=8
      SPCTXT(1)=
     &" C  0.0000000  0    0.000000  0     0.000000  0  0  0  0  0.0000"
      SPCTXT(2)=
     &" C  1.3737000  1    0.000000  0     0.000000  0  1  0  0  0.0000"
      SPCTXT(3)=
     &" C  1.3733831  1  123.624115  1     0.000000  0  2  1  0  0.0000"
      SPCTXT(4)=
     &" H  1.0949514  1  121.609884  1  -179.932414  1  1  2  3  0.0000"
      SPCTXT(5)=
     &" H  1.0956356  1  122.024766  1     0.067849  1  1  2  3  0.0000"
      SPCTXT(6)=
     &" H  1.1067076  1  118.172070  1  -179.982383  1  2  1  3  0.0000"
      SPCTXT(7)=
     &" H  1.0948917  1  121.588030  1  -179.938569  1  3  2  1  0.0000"
      SPCTXT(8)=
     &" H  1.0955037  1  122.019347  1     0.067854  1  3  2  1  0.0000"


      RETURN
      END
