#!/bin/bash
set -ev
R CMD INSTALL . ## Package needs to be installed, apparently
Rscript -e 'pkgdown::build_site()'
git clone --branch src https://${GITHUB_PAT}@github.com/Crunch-io/ta-da.git ../ta-da
rm -rf ../ta-da/static/r/crplyr
cp -r docs/. ../ta-da/static/r/crplyr
cd ../ta-da
git add .
git commit -m "Updating crplyr pkgdown site (release ${RELEASE_VERSION})" || true
git push origin src || true

