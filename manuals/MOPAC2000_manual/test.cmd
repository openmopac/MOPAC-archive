REM #!/bin/csh
REM #
REM #   Test a single part of the MOPAC manual
REM #
if EXIST test.tex rm test.tex
cp test_header.tex test.tex
cat %1.tex >> test.tex
echo "\end{document}" >> test.tex
latex test
yap  test.dvi

