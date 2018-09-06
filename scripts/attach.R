# attach.R

# Script for attaching packages

lapply(c(
  'RSQLite',
  'dplyr',
  'docxtractr',
  'purrr',
  'rprojroot',
  'stringr'
), function(x) {
  if (!require(x, character.only = TRUE, quietly = TRUE)) {
    install.packages(x, repos = 'https://cran.rstudio.com')
    suppressPackageStartupMessages(library(x, character.only = TRUE))
  }
})
criterion <- has_file('accr-cons.Rproj')
source(find_root_file('scripts', 'helpers.R', criterion = criterion))

if (!interactive())
  source(find_root_file('scripts', 'extract-word.R', criterion = criterion))

# If sourced/run interactively, now source '{PROJROOT}/scripts/extract-word.R'