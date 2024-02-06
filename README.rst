=================================
MOPAC historical codebase archive
=================================

This archive contains the original source code and manuals for many old versions of MOPAC that
were primarily developed by Dr. James (Jimmy) J. P. Stewart. There were several notable forks
that did not directly involve Dr. Stewart, which are noted at the bottom.

The first set of major releases were open-source software distributed through the Quantum
Chemistry Program Exchange (QCPE) as QCPE program #455 for MOPAC through MOPAC6 and #688 for MOPAC7:

1. MOPAC (1983)
2. MOPAC3 (1985)
3. MOPAC4 (1987)
4. MOPAC5 (1989)
5. MOPAC6 (1990)
6. MOPAC7 (1993)

In 1993, Dr. Stewart began consulting for Fujitsu, and new versions of MOPAC were released as
commercial software distributed by Fujitsu:

7. MOPAC93 (1993)
8. MOPAC97 (1997)
9. MOPAC 2000 (2000)

The commercial distribution of MOPAC93 specifically was coordinated with QCPE as program #689.
In 2001, Dr. Stewart left Fujitsu and formed his own company, Stewart Computational Chemistry,
to continue the development of MOPAC, which was initiated by updating the last open-source
version of MOPAC into a more modern open-source version (7.1) before continuing its commercial
development:

10. MOPAC 7.1 (2006)
11. MOPAC 2007 (2007)
12. MOPAC 2009 (2009)
13. MOPAC 2012 (2012)
14. MOPAC 2016 (2016)

In addition to these versions of MOPAC, other groups forked their own versions of MOPAC to continue
its development in other directions. Fujitsu released two other MOPAC versions (MOPAC 2002 and
MOPAC 2006) after Dr. Stewart left and continues to sell commercial versions of MOPAC as a component
of SCIGRESS [https://www.fqs.pl/en/chemistry/products/scigress]. When Dr. Stewart left the Dewar group
to work at the Frank J. Seiler Research Laboratory in the mid-1980's, the rest of the Dewar group continued
to develop their own version of MOPAC called AMPAC, later commercially distributed by SemiChem
[http://www.semichem.com]. Two notable forks of AMPAC exist: vector computer support was added by Timothy
Clark's group in the mid-1980's and released commercially as VAMP (Vectorized AMPAC), and implicit solvent
models were added by Donald Truhlar's group in the early-1990's and released as AMSOL (QCPE #606). Donald
Truhlar's group also forked MOPAC 5 in the early-1990's to support Cray computers and add new features, and
the latest release of their fork is MOPAC 5.022mn from 2015. AMSOL 7.1 and MOPAC 5.022mn are released under
an Apache license (and their manuals under CC-BY-4.0), and copies are included in this archive with the same licenses.

The data directory contains the reference data sets used for testing and training the PM6 and PM7
models in MOPAC. These sets are the best available proxies for the actual data used in fitting these
models and should be quite accurate, although the precise sets and the PARAM input files used in the
fitting process were not retained for posterity.
