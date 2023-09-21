=================================
MOPAC historical codebase archive
=================================

This archive contains the original source code and manuals for many old versions of MOPAC that
were primarily developed by Dr. James (Jimmy) J. P. Stewart. There were several notable forks
that did not directly involve Dr. Stewart, which are noted at the bottom.

The first set of major releases were open-source software disstributed through the Quantum
Chemistry Program Exchange (QCPE):

1. MOPAC 1 (1983)
2. MOPAC 3 (1985)
3. MOPAC 4 (1987)
4. MOPAC 5 (1989)
5. MOPAC 6 (1990)
6. MOPAC 7 (1993)

In 1993, Dr. Stewart began consulting for Fujitsu, and new versions of MOPAC were released as
commercial software distributed by Fujitsu:

7. MOPAC 93 (1993)
8. MOPAC 97 (1997)
9. MOPAC 2000 (2000)

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
MOPAC 2006) after Dr. Stewart left. When Dr. Stewart left the Dewar group to work at the Frank J.
Seiler Research Laboratory in the mid-1980's, the rest of the Dewar group continued to develop
their own version of MOPAC called AMPAC, later commercially distributed by SemiChem
[http://www.semichem.com]. A fork of AMPAC to support vector computers was developed by Timothy
Clark's group in the mid-1980's and released as VAMP (Vectorized AMPAC). Donald Truhlar's group
forked MOPAC 5 in the early-1990's to support Cray computers and add new features, and the latest
release of their fork is MOPAC 5.022mn from 2015. MOPAC 5.022mn was released under an Apache license,
and a copy is included in this archive with the same license.

The data directory contains the reference data sets used for testing and training the PM6 and PM7
models in MOPAC. These sets are the best available proxies for the actual data used in fitting these
models and should be quite accurate, although the precise sets and the PARAM input files used in the
fitting process were not retained for posterity.
