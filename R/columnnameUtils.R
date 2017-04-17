#' to MySQL column names
#'
#' let dataframe columnnames fit the constraint of MySQL column names
#' @param columnnames, vector of columnnames
#' @return a vector of MySQL compatible columnnames 
#' @export
my_columnnames <- function(columnnames) {

  invisible(paste0("`", columnnames, "`"))

}

#' to PostgreSQL column names
#'
#' let dataframe columnnames fit the constraint of PostgreSQL column names
#' @param columnnames, vector of columnnames
#' @return a vector of PostgreSQL compatible columnnames 
#' @export
pg_columnnames <- function(columnnames) {

  invisible(paste0("\"", columnnames, "\""))

}

#' to MSSQL column names
#'
#' let dataframe columnnames fit the constraint of MSSQL column names
#' @param columnnames, vector of columnnames
#' @return a vector of MSSQL compatible columnnames 
#' @export
ms_columnnames <- function(columnnames) {

  invisible(paste0("[", columnnames, "]"))

}

