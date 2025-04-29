# FPEMglobal.aux

This package provides functions for working with files and objects created by,
and associated with, the [FPEMglobal](https://github.com/FPcounts/FPEMglobal)
package.

FPEMglobal.aux was developed by the [Population
Division](https://www.un.org/en/development/desa/population/theme/making-family-planning-count/index.asp)
of the United Nations Dept. of Economic and Social Affairs. It is based on
earlier work by the _Family Planning Research_ (FPR) Group which is comprised of
teams from the [Alkema Lab](https://leontinealkema.github.io/alkema_lab/) at the
University of Massachusetts, Amherst, and [Track20](http://www.track20.org/) at
Avenir Health. It was supported, in part, by grants nos. OPP1110679, OPP1183453, and INV-033016, Making Family
Planning Count 1.0--3.0, from the Bill & Melinda Gates Foundation. 

**Disclaimer:** The views expressed herein are those of the authors and do not necessarily reflect the views of the United Nations.


## Installation

1. You must first install the FPEMglobal package. See instructions [here](https://github.com/FPcounts/FPEMglobal).

2. Repeat the installation steps you took for FPEMglobal, replacing "FPEMglobal" with "FPEMglobal.aux". For example, using the "Straight from GitHub" method, 

    i.  Make sure you have installed the _R_ package [remotes](https://cran.r-project.org/package=remotes). 
    
    ii.  In _R_, type:
  
         ```
         remotes::install_github(repo = "https://github.com/FPcounts/FPEMglobal.aux",
                          ref = remotes::github_release(),
                          build_manual = TRUE, build_vignettes = TRUE, dependencies = TRUE)
         ```

         `repo` is the only mandatory argument. If you encounter any problems you can try omitting any, or all, of the others. If you do not supply the `ref` argument, or if you change it, you may end up installing an old release or one that is in development and not fully tested. 
