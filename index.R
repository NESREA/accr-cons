# index.R

# A starter script for non-interactive sessions

##
## WARNING: DO NOT MOVE OR MODIFY THIS FILE UNLESS YOU KNOW WHAT YOU'RE DOING!
##


if (!interactive()) {
  if (!require(rprojroot)) {
    install.packages('rprojroot', repos = 'https://cran.rstudio.com')
  }
  cat("Starting up...\n")
  source('scripts/attach.R')
} else {
  cat("This file is to be sourced in non-interactive sessions only")
}