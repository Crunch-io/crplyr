## Test environments
* local macOS R installation, R 4.0.2
* ubuntu 18.04 (on github actions), R-devel, current and previous release
* win-builder (Release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* Makes `vdiffr` package optional during tests and `httptest` a required package
  so that `crplyr` passes R CMD check with only depended packages. (Tested on 
  R-devel with `_R_CHECK_DEPENDS_ONLY_`)
