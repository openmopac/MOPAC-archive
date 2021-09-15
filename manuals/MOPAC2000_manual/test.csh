#!/bin/csh
#
#   Test a single part of the MOPAC manual
#
if -e test.tex rm test.tex
cp test_header.tex test.tex
cat $1.tex >> test.tex
echo "\end{document}" >> test.tex
latex test
yap test.dvi

