REM
REM   Make the MOPAC manual "from scratch"
REM
latex mopac
makeindex mopac.idx
bibtex mopac
latex mopac
latex mopac
yap mopac.dvi
