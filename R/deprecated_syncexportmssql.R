#' syncqsbase 
#'
#' @param tablename, tablename
syncqsbase <- function(tablename, ...) {
  paste0("SELECT * FROM ", tablename)
}

#' syncqsdaily
#'
#' @param tablename, tablename 
#' @param columnname, columnname 
syncqsdaily <- function(tablename, columnname, ...) {
  paste0("DECLARE @t DATE=CONVERT(DATE, GETDATE())\n", syncqsbase(tablename), "\n", "WHERE ", columnname, "=@t")
}

#' syncmssql
#'
#' @param tablename, export tablename
#' @param filename, export data filename
#' @param qsfunc, querystring generate function
#' @param dbconnection, database connection
syncmssql <- function(tablename, filename, qsfunc, dbconnection, ...) {
  querystring <- qsfunc(tablename, ...)
  dat <- RODBC::sqlQuery(dbconnection, querystring)
  if( class(dat) == 'data.frame' ) {
    if( nrow(dat) > 0 ) {
      qs <- data2insertmssqlqs(dat, tablename)
      writeLines(unlist(qs), filename)
    }
    return(paste('export', nrow(dat), 'rows'))
  }else {
    return(paste0(dat, sep="\n"))
  }
}

