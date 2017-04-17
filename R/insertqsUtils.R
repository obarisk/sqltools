#' dataframe or tibble to MySQL insert query string
#'
#' @param dT, a tibble
#' @param tablename, table name
#' @param iLength, max size(row) counts in KB, default = 1024
#' @param tz, timezone
#' @param Colnames, boolean whether include colnames inside query string, default = TRUE 
#' @return a tibble of a segmented insert query string
#' @examples 
#' \dontrun{
#'   require(tidyverse) 
#'   mytbl <- tibble(a=c(1, 2, 3, 4), b=c('1', '2', '3', '4'), 
#'     c=c(Sys.Date(), Sys.Date(), Sys.Date(), Sys.Date()))
#'   my_qsg_fromdf(mytbl, 'tmp') 
#' } 
#' @importFrom tibble as.tibble 
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by 
#' @importFrom dplyr mutate
#' @importFrom dplyr select 
#' @importFrom dplyr summarize 
#' @importFrom dplyr row_number 
#' @export 
my_qsg_fromdf <- function(dT, tablename, iLength=1024, tz=Sys.timezone(), Colnames=TRUE) {
  
  dT <- as.tibble(dT)
  tQ <- tibbleiqs(dT, tz=tz)

  if( grepl('`', tablename) ) {
    tablename <- tablename
  }else{
    mtablename <- str_split(tablename, '\\.')[[1]]
    if( length(mtablename) == 2 ) {
      mtablename <- paste0("`", mtablename, "`")
      tablename <- paste0(mtablename[1], ".", mtablename[2])
    }else if( length(mtablename) == 1 ) {
      tablename <- paste0("`", tablename, "`")
    }else{
      tablename <- tablename
    }
  }

  myColnames <- paste0("(", paste0(my_columnnames(tQ$Colnames), collapse=","), ")")
  myLength <- floor(iLength / tQ$RowKB) # MySQL insert query string length limit

  if( Colnames ) {
    qsHeader <- sprintf("INSERT INTO %s %s VALUES ", tablename, myColnames)
  }else{
    qsHeader <- sprintf("INSERT INTO %s VALUES ", tablename)
  }

  QueryStrings <- tQ$QueryStrings %>% 
    mutate(
      Group = ceiling(row_number()/myLength)
    ) %>%
    group_by(Group) %>%
    summarize(QueryString = paste0(QueryString, collapse=",")) %>%
    mutate(QueryString = paste0(qsHeader, QueryString)) %>%
    select(QueryString)

  return(QueryStrings)

}

#' dataframe or tibble to PostgreSQL insert query string
#'
#' @param dT, a tibble
#' @param tablename, table name
#' @param iLength, max size(row) counts in KB, default = 1024
#' @param tz, timezone
#' @param Colnames, boolean whether include colnames inside query string, default = TRUE 
#' @return a tibble of a segmented insert query string
#' @examples 
#' \dontrun{
#'   require(tidyverse) 
#'   mytbl <- tibble(a=c(1, 2, 3, 4), b=c('1', '2', '3', '4'), 
#'     c=c(Sys.Date(), Sys.Date(), Sys.Date(), Sys.Date()))
#'   pg_qsg_fromdf(mytbl, 'tmp') 
#' } 
#' @importFrom tibble as.tibble 
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by 
#' @importFrom dplyr mutate
#' @importFrom dplyr select 
#' @importFrom dplyr summarize 
#' @importFrom dplyr row_number 
#' @export 
pg_qsg_fromdf <- function(dT, tablename, iLength=1024, tz=Sys.timezone(), Colnames=TRUE) {
  
  dT <- as.tibble(dT)
  tQ <- tibbleiqs(dT, tz=tz)

  if( grepl('"', tablename) ) {
    tablename <- tablename
  }else{
    tablename <- paste0('"', tablename, '"')
  }

  myColnames <- paste0("(", paste0(pg_columnnames(tQ$Colnames), collapse=","), ")")
  myLength <- floor(iLength / tQ$RowKB) # MySQL insert query string length limit

  if( Colnames ) {
    qsHeader <- sprintf("INSERT INTO %s %s VALUES ", tablename, myColnames)
  }else{
    qsHeader <- sprintf("INSERT INTO %s VALUES ", tablename)
  }

  QueryStrings <- tQ$QueryStrings %>% 
    mutate(
      Group = ceiling(row_number()/myLength)
    ) %>%
    group_by(Group) %>%
    summarize(QueryString = paste0(QueryString, collapse=",")) %>%
    mutate(QueryString = paste0(qsHeader, QueryString)) %>%
    select(QueryString)

  return(QueryStrings)

}

#' dataframe or tibble to MSSQL insert query string
#'
#' @param dT, a tibble
#' @param tablename, table name 
#' @param iLength, max insert rows in batch mode, default = 900
#' @param tz, timezone 
#' @param Colnames, boolean whether include colnames inside query string, default = FALSE
#' @return a tibble of a segmented insert query string 
#' @examples
#' \dontrun{
#'   ms_qsg_fromdf(iris[1:3, ], 'testdb..iris')
#'   ms_qsg_fromdf(iris[1:3, ], 'testdb.dbo.iris', iLength=1) 
#'   require(tibble) 
#'   ms_qsg_fromdf(tibble(c1=Sys.Date()), 'tmp', tz='US/Eastern') 
#'   ms_qsg_fromdf(iris[1:3, ], 'iris', Colnames=TRUE) 
#'   ms_qsg_fromdf(iris[1:3, ], '[testdp].dbo.[iris]') 
#' }
#' @importFrom dplyr %>% 
#' @importFrom dplyr group_by 
#' @importFrom dplyr mutate 
#' @importFrom dplyr select
#' @importFrom dplyr summarize 
#' @importFrom stringr str_extract 
#' @importFrom stringr str_split
#' @importFrom tibble as.tibble
ms_qsg_fromdf <- function(dT, tablename, iLength=900, tz=Sys.timezone(), Colnames=FALSE) {

  dT <- as.tibble(dT)
  tQ <- tibbleiqs(dT, tz=tz)

  if( grepl('\\[', tablename) ) {
    tablename <- tablename
  }else{
    mtablename <- str_split(tablename, '\\.dbo\\.|\\.guest\\.|\\.sys\\.|\\.\\.')[[1]]
    if( length(mtablename) == 2 ) {
      mtablename <- paste0("[", mtablename, "]")
      tablename <- paste0(mtablename[1], 
        str_extract(tablename, '\\.dbo\\.|\\.guest\\.|\\.sys\\.|\\.\\.'),
        mtablename[2])
    }else if( length(mtablename) == 1 ) {
      tablename <- paste0("[", tablename, "]")
    }else{
      tablename <- tablename
    }
  }

  myColnames <- paste0("(", paste0(ms_columnnames(tQ$Colnames), collapse=","), ")")
  myLength <- iLength

  if( Colnames ) {
    qsHeader <- sprintf("INSERT INTO %s %s VALUES ", tablename, myColnames)
  }else{
    qsHeader <- sprintf("INSERT INTO %s VALUES ", tablename)
  }

  QueryStrings <- tQ$QueryStrings %>% 
    mutate(
      Group = ceiling(row_number()/myLength)
    ) %>%
    group_by(Group) %>%
    summarize(QueryString = paste0(QueryString, collapse=",")) %>%
    mutate(QueryString = paste0(qsHeader, QueryString)) %>%
    select(QueryString)

  return(QueryStrings)

}

#' dataframe 2 mysql insert query string 
#' @param da, dataframe
#' @param tbn, table name
#' @return mysql insert query string
#' @export
data2insertmysqlqs <- function(da, tbn) {

  message("Deprecated, Please Use my_qsg_fromdf instead")

  unlist(my_qsg_fromdf(da, tbn), use.names=FALSE)
  
  #class(da) <- "data.frame"
  #
  ## set up colnames
  #cl <- colnames(da) %>% str_replace("^([0-9])", "_\\1") %>% 
  #  paste0("`", ., "`") %>% paste0(collapse=", ") %>%
  #  paste0("(", ., ")")
  #
  ## formate recoreds
  #for( .colnum in 1:ncol(da) ) {
  #  if( "POSIXt" %in% class(da[, .colnum]) ) {
  #    da[, .colnum] <- ifelse( is.na(da[, .colnum]), 'NULL',
  #      format(da[, .colnum], "'%Y-%m-%d %H:%M:%S'") )
  #  }else if( "Date" %in% class(da[, .colnum]) ) {
  #    da[, .colnum] <- ifelse( is.na(da[, .colnum]), 'NULL',
  #      format(da[, .colnum], "'%Y-%m-%d'") )
  #  }else if( sum(class(da[, .colnum]) %in% c("AsIs", "character") ) ) {
  #    da[, .colnum] <- ifelse( is.na(da[, .colnum]), 'NULL',
  #      paste0("'", da[, .colnum], "'") )
  #  }
  #}

  #da <- apply(da, 1, function(x) { paste0(x, collapse=", ") %>% paste0("(", ., ")") })
  #nb <- object.size(da[1]) %>% as.numeric() %>% `/`(8)
  #nl <- floor(1024*125 / nb)
  #gs <- 1:length(da) %>% `/`(nl) %>% ceiling
  #qh <- paste0("INSERT INTO `", tbn, "` ", cl, " VALUES ")
  #qs <- tapply(da, gs, function(x, qh) {
  #    paste0(qh, paste0(x, collapse=", ")) %>% gsub(" NA", " NULL", .)
  #  }, qh=qh)
  #qs 
  
}

#' dataframe 2 mssql insert query string 
#'
#' @param da, dataframe
#' @param tbn, table name
#' @param header, wheather include colnames or not
#' @return mssql insert query string
#' @export
data2insertmssqlqs <- function(da, tbn, header=FALSE) {
  
  message("Deprecated, Please Use ms_qsg_fromdf instead")

  unlist(ms_qsg_fromdf(da, tbn, Colnames=header), use.names=FALSE)

  #class(da) <- "data.frame"
  ## set up colnames
  #cl <- colnames(da) %>% str_replace("^([0-9])", "_\\1") %>% 
  #  paste0("[", ., "]") %>% paste0(collapse=", ") %>%
  #  paste0("(", ., ")")
  #
  ## formate recoreds
  #for( .colnum in 1:ncol(da) ) {
  #  if( "POSIXt" %in% class(da[, .colnum]) ) {
  #    da[, .colnum] <- ifelse( is.na(da[, .colnum]), 'NULL', 
  #      format(da[, .colnum], "'%Y-%m-%d %H:%M:%OS'") )
  #  }else if( "Date" %in% class(da[, .colnum]) ) {
  #    da[, .colnum] <- ifelse( is.na(da[, .colnum]), 'NULL',
  #      format(da[, .colnum], "'%Y-%m-%d'") )
  #  }else if( sum(class(da[, .colnum]) %in% c("AsIS", "character")) ) {
  #    da[, .colnum] <- ifelse( is.na(da[, .colnum]), 'NULL',
  #      paste0("'", da[, .colnum], "'") )
  #  }
  #}
  #da <- apply(da, 1, function(x) { paste0(x, collapse=", ") %>% paste0("(", ., ")") %>% 
  #  gsub("'NULL'", "NULL", .) })
  #nb <- object.size(da[1]) %>% as.numeric() %>% `/`(8)
  #nl <- floor(900)
  #gs <- 1:length(da) %>% `/`(nl) %>% ceiling
  #if( header ) {
  #  qh <- paste0("INSERT INTO ", tbn, " ", cl, " VALUES ")
  #}else{
  #  qh <- paste0("INSERT INTO ", tbn, " VALUES ")
  #}
  #qs <- tapply(da, gs, function(x, qh) {
  #    paste0(qh, paste0(x, collapse=", ")) %>% gsub(" NA", " NULL", .)
  #  }, qh=qh)
  #qs 
  
}

