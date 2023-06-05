      subroutine nextwd (txt, ftxt)
C     Useful function to clip off next nonspace/nontab character substring
C     out of a given text string and return this fragment.
C     Example:
C
C         If given " AM1  GNORM=0.001  POLAR " as txt it would return:
C         "  GNORM=0.001 POLAR " as txt and
C         " AM1" as ftxt.
C
        implicit double precision (a-h,o-z)
        character   txt*(*), ftxt*(*)
        parameter (itab=9)

      ll=len(txt)
      do 5 I=1,ll
    5   if (ichar(txt(i:i)).eq.itab) txt(i:i)=' '

      if (txt(1:1).ne.' ') then 
        n=1
        goto 20
      endif
      n=0
   10 n=n+1
      if (n.gt.len(txt)) return                                         LF0311
      if (txt(n:n).eq.' ') goto 10

   20 continue
      m=n-1
   30 m=m+1
      if (txt(m:m).ne.' ') goto 30
      m=m-1
      ftxt=txt(n:m)
      txt=txt(m+1:)

      return
      end
  


C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
