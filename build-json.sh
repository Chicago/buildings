#!/bin/bash
R CMD BATCH '--args 1' R/build-json.R
R CMD BATCH '--args 2' R/build-json.R
R CMD BATCH '--args 3' R/build-json.R
R CMD BATCH '--args 4' R/build-json.R
R CMD BATCH '--args 5' R/build-json.R
R CMD BATCH '--args 6' R/build-json.R
R CMD BATCH '--args 7' R/build-json.R
R CMD BATCH '--args 8' R/build-json.R
R CMD BATCH '--args 9' R/build-json.R
exit 0
