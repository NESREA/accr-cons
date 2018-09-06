# extract-word.R

# Extract data from Microsoft Word documents

## Ensure dependencies are available
if (interactive()) {
  if (!exists('criterion')) 
    criterion <- rprojroot::has_file('accr-cons.Rproj')
  source(rprojroot::find_root_file('scripts', 'attach.R', criterion = criterion))
}

## Import a 'docx' file
docpath <- find_root_file('docs', criterion = criterion)

## Extract and merge all tables from all documents and
## return a list, the elements of which are a data frame
## consisting of merged tables in each document.
dfList <-
  sapply(list.files(docpath, '.docx$', full.names = TRUE), function(x) {
    if (grepl('~\\$', x))
      return(NULL)
    dc <- read_docx(x)
    tbls <- docx_extract_all_tbls(dc)
    
    ## We assume that tables in same document
    ## have same row/column configuration
    ## Adjust the headers, fixing the commonly
    ## merged cells for 'Areas of Competence'
    tbls <- lapply(tbls, adjust_hdr)
    if (docx_tbl_count(dc) > 1)
      tbls <- bind_rows(tbls)
    as.data.frame(tbls)
  })
rm(docpath)

dfList <-
  dfList[-which(sapply(dfList, is.null))]  # remove NULL elements

## Email and phone details were placed in the samme column. Additionally, some
## of the records have more than one of such entries. We will split this column
## into several - one for each entry and append it at right end of the table.
## We will map an anonymous function to every table obtained from each
## document. Definition of helper functions are in 'helpers.R'.
## Column names to match original tables
newCols <-
  c(
    'cert.no',
    'consultant',
    'rep',
    'emails',
    'competence.EA',
    'competence.EMS',
    'competence.ES',
    'competence.ET',
    'competence.LAB',
    'competence.WM',
    'cac.no',
    'remarks'
  )

dfAll <-
  map(dfList,
      function(df) {
        df %>%
          remove_column(regex_pats$serialno) %>%
          remove_column('expiry') %>%
          process_header(newCols, regex_pats$orig.hdrs) %>%
          fix_cac_column() %>%
          set_column_order(newCols)
      }) %>%
  bind_rows() %>%
  extract_col_str(old = 'emails',
                  new = 'email',
                  pattern = regex_pats$email) %>%
  extract_col_str(old = 'emails',
                  new = 'phone',
                  pattern = regex_pats$phone) %>%
  select(-emails) 

rm(dfList)

## Check for data directory
datPth <- find_root_file('data', criterion = criterion)
if (!dir.exists(datPth)) {
  if (!dir.create(datPth))
    stop("creation of 'data' directory failed")
}

## Write to a 'csv' file
write.csv(as.matrix(dfAll), file.path(datPth, 'consultants.csv'))

## Write to an SQLite database
# dcon <- dbConnect(SQLite(), file.path(datPth, 'consultants.db'))
# dbWriteTable(dcon, 'main', dfAll, overwrite = TRUE)
# dbDisconnect(dcon)

rm(dfAll, regex_pats, criterion, newCols, datPth)
cat('Finished.\n')
