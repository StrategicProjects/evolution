## Submission summary

This is a minor update (0.1.0 -> 0.1.1) that fixes a portability bug and
improves input handling, documentation and test coverage.

Main changes (see NEWS.md for the full list):

* Fixed a bug where the package could fail to load on R 4.2-4.3: the
  null-coalescing operator `%||%` is only part of base R since 4.4.0, but
  the package declares `R (>= 4.2.0)`. It is now defined internally.
* Recipient phone numbers are normalised before the request, and the
  media helper no longer treats a mistyped file path as base64.
* Corrected the copyright holder in the LICENSE file.
* Added a testthat suite covering the offline helpers and argument
  validation.

## Test environments

* local macOS, R 4.6.0
* GitHub Actions (R-CMD-check):
  * ubuntu-latest, R devel
  * ubuntu-latest, R release
  * ubuntu-latest, R oldrel-1
  * macos-latest, R release
  * windows-latest, R release

## R CMD check results

0 errors | 0 warnings | 0 notes

On all GitHub Actions environments the check is clean. On the local macOS
machine two additional NOTEs appear that are environment-specific and do
not reflect the package:

* "Skipping checking HTML validation: 'tidy' doesn't look like recent
  enough HTML Tidy." -- the local HTML Tidy binary is outdated.
* A `.DS_Store` file in the check directory -- created by macOS Finder
  inside the `.Rcheck` working directory during the run; it is not part
  of the built tarball.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
