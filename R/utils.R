#' sqlstring generic
#'
#' @param x object to sqlstring
#' @param tz timezone
#' @return sqlstring
#' @examples 
#'   require(tidyverse) 
#'   mytbl <- tibble::tibble(c1 = c(1, 2, 3), c2 = c(1.1, 2.1, 3.1), c3 = c(1, NA_real_, 3),
#'     c4 = c('A', 'B', 'C') , c5 = c(Sys.time(), Sys.time(), Sys.time()), 
#'     c5 = c(Sys.Date(), NA, Sys.Date), c6 = c(NA, NA, NA))
#'  mytbl %>% mutate_all(funs(sqlstring))
#'  mytbl %>% mutate_all(funs(sqlstring), tz="US/Eastern") 
sqlstring <- function(x, ...) {

  UseMethod("sqlstring")

}
sqlstring.default <- function(x, ...) {

  ifelse(is.na(x), 'NULL', paste0("'", x, "'"))

}
sqlstring.character <- function(x, ...) {

  gsub("'", "''", x)

}
sqlstring.POSIXct <- function(x, tz=Sys.timezone(), ...) {

  ifelse(is.na(x), 'NULL', format(x, format="'%Y/%m/%d %H:%M:%S'", tz=tz))

}
sqlstring.Date <- function(x, tz=Sys.timezone(), ...) {

  ifelse(is.na(x), 'NULL', format(x, format="'%Y/%m/%d'", tz=tz))

}
sqlstring.integer <- function(x, ...) {

  ifelse(is.na(x), 'NULL', x)

}
sqlstring.numeric <- function(x, ...) {

  ifelse(is.na(x), 'NULL', x)

}
sqlstring.na <- function(x, ...) {

  'NULL'

}

#' tibble sqlstring cell
#' 
#' @param dT, a tibble
#' @param tz, timezone
#' @return a list contains QueryStrings, Colnames, RowKB
#' @importFrom dplyr %>%
#' @importFrom dplyr funs
#' @importFrom dplyr mutate_all
#' @importFrom tibble as.tibble
tibbleiqs <- function(dT, tz=Sys.timezone()) {

  stopifnot(nrow(dT) > 0)

  dT <- as.tibble(dT)
  dO <- mutate_all(dT, funs(sqlstring), tz=tz)
  QueryStrings <- tibble(QueryString=apply(dO, 1, paste0, collapse=",")) %>%
    mutate(QueryString=paste0("(", QueryString, ")"))
  Colnames <- colnames(dT)
  RowKB <- as.numeric(sub("Kb", "", format(object.size(QueryStrings[1, 1]), units="Kb"))) 
  list(QueryStrings=QueryStrings, Colnames=Colnames, RowKB=RowKB)

}

