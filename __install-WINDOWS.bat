@echo off

ECHO.
ECHO.

ECHO. ================================================================================
ECHO. DATA
ECHO. ================================================================================

Rscript -e "setwd('data-raw'); example(source); sourceDir('.')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. DOCUMENT
ECHO. ================================================================================

Rscript -e "devtools::document()"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. INSTALL
ECHO. ================================================================================

Rscript -e "devtools::install(build = TRUE, build_vignettes = TRUE, upgrade = 'never')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. TESTS
ECHO. ================================================================================

Rscript -e "testthat::test_package('FPEMglobal.aux')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
PAUSE
