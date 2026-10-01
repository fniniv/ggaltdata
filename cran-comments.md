## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release. The only NOTE is "New submission" from the CRAN incoming feasibility check.

## Test environments

* local: Windows 11, R 4.5.2, `R CMD check --as-cran` with the PDF manual and HTML validation;
  also with only Depends/Imports installed (Suggests absent: the tests that need them are skipped).
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1).
* win-builder: R-devel (2026-09-30 r90605 ucrt) and R-release (4.6.1); 1 NOTE (new submission) on both.

## Notes for the reviewers

* The package has no compiled code and no network access. Examples, tests and the vignette write
  only to `tempdir()`.
