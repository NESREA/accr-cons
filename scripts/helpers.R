#
# helpers.R
#

#
# HELPER DATA STRUCTURES
#

# ===
## A decision was made to bring all the regular expression used in
## this project into a single data structure to simplify maintenance.
## We create a list that has 2 sublists for header and value (in-data frame)
## regular expressions.
# ===

hdr_regex <-
  list(
    serialno = '^s(\\.|/)n',
    email = '^e(\\s|-|\\.)?mail',
    cert = '^cert',
    consult = '.*consult',
    rep = '^rep',
    ea = 'ea|.+audit',
    em = '^(env)?.*mgt|manage',
    es = '.+studies',
    et = '.+tech',
    lab = '^lab.*',
    wm = '^waste',
    cac.no = '^(cac|rc)', 
    rmk = '^remark',
    exp = '^expiry'
  )

val_regex <-
  list(
    cac = '(RC|KN)\\s.+\\d{3,}$',
    email = '[[:alnum:]]+@[[:alnum:]]+\\.(com|org|net|ng|uk|edu|biz)',
    phone = '0[7-9][0-1][0-9]{8}'
  )

regex_pats <-
  structure(.Data =
              list(
                headers = structure(.Data = hdr_regex,
                                    names = names(hdr_regex)),
                values = structure(.Data = val_regex,
                                   names = names(val_regex))
              ))

## Won't need these beyond this point
rm(hdr_regex, val_regex)




#
# HELPER FUNCTIONS
#

# ===
## Readjusts the header.
## Collects first row and compares it with table header
## If we have a merged cells portion "Areas of Competence"
# ===
adjust_hdr <- function(df)
{
  stopifnot(inherits(df, 'data.frame'))
  mergedHdr <-
    grepl('area.+competence', colnames(df), ignore.case = TRUE)
  if (!any(mergedHdr))
    return(df)
  if (!any(grepl(regex_pats$headers$ea, row <-
                 unlist(df[1, ]), ignore.case = TRUE))) {
    warning('A data frame\'s headers did not match the predefined pattern')
    return(df)
  }
  ind <- which(mergedHdr)
  ind <- ind:(ind + 5)
  colnames(df) <- replace(colnames(df), ind, row[ind])
  df <- df[-1,]
}

# ===
## Removes specified unwanted columns from the original tables
# ===
remove_column <- function(data, x) {
  stopifnot(inherits(data, 'data.frame'))
  if (any(isThere <- grepl(x, colnames(data), ignore.case = TRUE)))
    data <- data[,!isThere]
  else
    data
}

# ===
## Repairs the awkwardness of the columns holding CAC Nos in some
## of the tables imported from the documents.
# ===
fix_cac_column <- function(x)
{
  stopifnot(inherits(x, 'data.frame'))
  if (any(grepl(regex_pats$headers$cac.no, colnames(x))))
    return(x)
  
  ## Divide table into right and left halves with left half
  ## ending with the column that has the rc numbers
  cutoff <- match('emails', colnames(x))
  lt <- select(x, 1:cutoff)
  rt <- select(x, (cutoff + 1):ncol(x))
  
  ## create a new column in the left half out of the last colums,
  ## extracting the rc numbers. Then join the table back together.
  lt <- lt %>%
    extract_col_str(old = 'email',
                    new = 'cac.no',
                    pattern = regex_pats$headers$cac.no)
  
  lt$emails <- lt$emails %>%
    str_replace(regex_pats$headers$cac.no, '')
  lt %>%
    bind_cols(rt)
}

# ===
## Specifies the column names. 
## @param x The data frame
## @param colList The desired names
## @param patList A list of regular expressions for checking column headers
# ===
process_header <- function(x, colList, patList)
{
  stopifnot(inherits(x, 'data.frame'))
  colnames(x) <-
    vapply(colnames(x), FUN.VALUE = character(1), function(n) {
      name <- n
      vapply(unlist(patList), FUN.VALUE = character(1), function(pat) {
        if (grepl(pat, n, ignore.case = TRUE))
          name <<-
            grep(pat, colList, ignore.case = TRUE, value = TRUE)
        else
          NA_character_
      })
      if (length(name) > 1)
        warning("Column",
                sQuote(n),
                "returned multiple matches. First match used.")
      name <- name[1]
    })
  x
}

# ===
## Arranges the columns for all the
## data frames in the desired order
# ===
set_column_order <- function(x, names)
{
  stopifnot(inherits(x, 'data.frame'))

  x <- map_dfc(names, function(nm) {
    if (nm %in% colnames(x))
      x[[nm]]
    else
      assign(as.character(nm), rep('', nrow(x)))
  }) %>%
    bind_cols()
  colnames(x) <- names
  x
}

# ===
## Creates new variables that contain strings
## from a given column in a data frame
# ===
extract_col_str <- function(data, old, new, pattern)
{
  stopifnot(inherits(data, 'data.frame'))
  data %>%
    # mutate(emailsList = strsplit(emails,  ' ')) %>%
    mutate(txtList = str_extract_all(emails, pattern)) %>%
    mutate(len = map_int(txtList, length)) %>%
    .spreadListColumn(new) %>%
    select(-c(txtList:len))
}

# ===
# Takes the strings in a list column and places them in
# individual colums of their own.
# ===
.spreadListColumn <- local({
  function(df, col) {
    stopifnot(inherits(df, 'data.frame'))
    colrange <- 1L:as.integer(max(df[['len']]))
    for (i in colrange) {
      coln <- if (length(colrange) > 1)
        paste0(col, i)
      else {
        col
      }
      df[[coln]] <- map_chr(df[['txtList']], function(v) {
        if (length(v) != 0 && length(v) >= i)
          v[i]
        else
          NA_character_
      })
    }
    df
  }
})
