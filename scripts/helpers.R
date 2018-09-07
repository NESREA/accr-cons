#
# helpers.R
#

#
# Helper data structures
#

# ===
## A decision was made to bring all the regular expression used in 
## this project into a single data structure to simplify maintenance.
# ===

regex_pats <- structure(
  list(
    structure(
      list(
        '(RC|KN)\\s.+\\d{3,}$',
        '[[:alnum:]]+@[[:alnum:]]+\\.(com|org|net|ng|uk|edu|biz)',
        '0[7-9][0-1][0-9]{8}'
      ), names = c('cac.no', 'email', 'phone')),
    structure(
      list(
        's(\\.|/)n',
        '(e|e\\.)mail',
        '^cert',
        '.?consult',
        '^represent',
        'ea|.+audit',
        'em|mgt|management.{1}sys',
        '.+studies',
        '.+tech',
        '^lab.',
        '^waste',
        '^(cac|rc)',
        '^remark'
      ), names = c('sno',
                   'eml',
                   'cert',
                   'cons',
                   'rep',
                   'aud',
                   'emgt',
                   'stud',
                   'tech',
                   'lab',
                   'wst',
                   'cac',
                   'rmk'
                  )
      )
    ),
  names = c('values', 'headers'))


#
# Helper functions
#

# ===
## Readjusts the header
## Collects first row and compares it with table header
## If we have a merged cells portion "Areas of Competence"
# ===
adjust_hdr <- function(df)
{
  stopifnot(inherits(df, 'data.frame'))
  mergedHdr <- grepl('area.+competence', colnames(df), ignore.case = TRUE)
  if (!any(mergedHdr))
    return(df)
  if (!any(grepl('ea|audit', row <- unlist(df[1,]), ignore.case = TRUE))) {
    warning('A data frame\'s headers did not match the predefined pattern')
    return(df)
  }
  ind <- which(mergedHdr)
  ind <- ind:(ind + 5)
  colnames(df) <- replace(colnames(df), ind, row[ind])
  df <- df[-1, ]
}

# ===
# ===
remove_column <- function(data, x) {
  stopifnot(inherits(data, 'data.frame'))
  if (any(isThere <- grepl(x, colnames(data), ignore.case = TRUE)))
    data <- data[ , !isThere]
  else
    data
}

# ===
# ===
fix_cac_column <- function(x)
{
  stopifnot(inherits(x, 'data.frame'))
  if (any(grepl(regex_pats$orig.hdrs[11], colnames(x))))
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
                    pattern = regex_pats$cac.no)

  lt$emails <- lt$emails %>% 
    str_replace(regex_pats$cac.no, '')
  lt %>% 
    bind_cols(rt)
}

# ===
## Change all column headers so same names
## and rearrange all columns in the same order
# ===
process_header <- local({
  function(x, colList, patList)
  {
    stopifnot(inherits(x, 'data.frame'))
    # TODO: Use vapply() or purrr::map()
    colnames(x) <- sapply(colnames(x), function(name) {
      ind <- NULL
      sapply(patList, function(pat) {
        if (grepl(pat, name, ignore.case = TRUE))  {
          ind <<- patList %in% pat
        }
      })
      name <- colList[ind]
    })
    x
  }
})

# ===
## Arrange the columns for all the 
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
# Takes a the strings in a list column and places them in
# individual colums of their own.
# ===
.spreadListColumn <- local({
  function(df, col) {
    stopifnot(inherits(df, 'data.frame'))
    colrange <- 1L:as.integer(max(df[['len']]))
    for (i in colrange) {
     coln <- if (length(colrange) > 1)
        paste0(col, i)
      else { col }
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
