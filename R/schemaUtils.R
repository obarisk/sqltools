#' use R dataframe or tibble to generate MySQL table schema
#'
#' @param dT, R data.frame or tibble
#' @param tablename, MySQL table name
#' @param lvarchar, length of varchar, default=50
#' @importFrom dplyr %>%
#' @import tibble 
#' @export
my_qsg_schema <- function(dT, tablename, lnvarchar=50) {

  dT <- as.tibble(dT)
  colnames(dT) <- my_columnnames(colnames(dT))
  schema <- tibble(
    ColumnName = colnames(dT), 
    DataType = vapply(dT, function(x) class(x)[1], character(1))
    )

  # varchar
  schema[schema$DataType=="character", "DataType"] <- "VARCHAR"
  schema <- cbind(schema, Schema=character(1))
  # varchar
  if( sum(schema$DataType) > 0 ) {
    schema[schema$DataType=="varchar", "Schema"] <-
      paste0(schema[schema$DataType, "DataType"], "(", lnvarchar, ")")
  }
  # int
  if( sum(schema$DataType=="integer") > 0 ) {
    schema[schema$DataType=="integer", "Schema"] <- "INTEGER"
  }
  # decimal
  if( sum(schema$DataType=="numeric") > 0 ) {
    schema[schema$DataType=="numeric", "Schema"] <- "DECIMAL(38, 18)"
  }
  # datetime
  if( sum(schema$DataType=="POSIXct") > 0 ) {
    schema[schema$DataType=="POSIXct", "Schema"] <- "DATETIME"
  }
  # date
  if( sum(schema$DataType=="Date") > 0 ) {
    schema[schema$DataType=="Date", "Schema"] <- "DATE"  
  }
  # default
  if( sum(schema$DataType=="") > 0 ) {
    schema[schema$Schema=="", "Schema"] <- schema[schema$Schema=="", "DataType"]
  }
  columnQS <- paste0(schema$ColumnName, " ", schema$Schema) %>% paste0(collapse=", ")
  queryString <- paste0("CREATE TABLE ", tablename, "( ", columnQS, ");")

  return(queryString)

}

#' use R dataframe or tibble to generate MSSQL table schema
#'
#' @param dT, R data.frame or tibble
#' @param tablename, MSSQL table name
#' @param lvarchar, length of varchar, default=50
#' @importFrom dplyr %>% 
#' @import tibble 
#' @export
ms_qsg_schema <- function(dT, tablename, lnvarchar=50) {

  dT <- as.tibble(dT)
  colnames(dT) <- ms_columnnames(colnames(dT))
  schema <- tibble(
    ColumnName = colnames(dT), 
    DataType = vapply(dT, function(x) class(x)[1], character(1))
    )

  # nvarchar
  schema[schema$DataType=="character", "DataType"] <- "NVARCHAR"
  schema <- cbind(schema, Schema=character(1))
  # varchar
  if( sum(schema$DataType) > 0 ) {
    schema[schema$DataType=="varchar", "Schema"] <-
      paste0(schema[schema$DataType, "DataType"], "(", lnvarchar, ")")
  }
  # int
  if( sum(schema$DataType=="integer") > 0 ) {
    schema[schema$DataType=="integer", "Schema"] <- "INTEGER"
  }
  # decimal
  if( sum(schema$DataType=="numeric") > 0 ) {
    schema[schema$DataType=="numeric", "Schema"] <- "DECIMAL(38, 18)"
  }
  # datetime
  if( sum(schema$DataType=="POSIXct") > 0 ) {
    schema[schema$DataType=="POSIXct", "Schema"] <- "DATETIME"
  }
  # date
  if( sum(schema$DataType=="Date") > 0 ) {
    schema[schema$DataType=="Date", "Schema"] <- "DATE"  
  }
  # default
  if( sum(schema$DataType=="") > 0 ) {
    schema[schema$Schema=="", "Schema"] <- schema[schema$Schema=="", "DataType"]
  }
  columnQS <- paste0(schema$ColumnName, " ", schema$Schema) %>% paste0(collapse=", ")
  queryString <- paste0("CREATE TABLE ", tablename, "( ", columnQS, ");")

  return(queryString)

}

#' use R dataframe or tibble to generate PostgreSQL table schema
#'
#' @param dT, R data.frame or tibble
#' @param tablename, PostgreSQL table name
#' @param lvarchar, length of varchar, default=50
#' @importFrom dplyr %>% 
#' @import tibble 
#' @export
pg_qsg_schema <- function(dT, tablename, lnvarchar=50) {

  dT <- as.tibble(dT)
  colnames(dT) <- pg_columnnames(colnames(dT))
  schema <- tibble(
    ColumnName = colnames(dT), 
    DataType = vapply(dT, function(x) class(x)[1], character(1))
    )

  # varchar
  schema[schema$DataType=="character", "DataType"] <- "VARCHAR"
  schema <- cbind(schema, Schema=character(1))
  # varchar
  if( sum(schema$DataType) > 0 ) {
    schema[schema$DataType=="varchar", "Schema"] <-
      paste0(schema[schema$DataType, "DataType"], "(", lnvarchar, ")")
  }
  # int
  if( sum(schema$DataType=="integer") > 0 ) {
    schema[schema$DataType=="integer", "Schema"] <- "INTEGER"
  }
  # decimal
  if( sum(schema$DataType=="numeric") > 0 ) {
    schema[schema$DataType=="numeric", "Schema"] <- "DECIMAL(38, 18)"
  }
  # datetime
  if( sum(schema$DataType=="POSIXct") > 0 ) {
    schema[schema$DataType=="POSIXct", "Schema"] <- "TIMESTAMP"
  }
  # date
  if( sum(schema$DataType=="Date") > 0 ) {
    schema[schema$DataType=="Date", "Schema"] <- "DATE"  
  }
  # default
  if( sum(schema$DataType=="") > 0 ) {
    schema[schema$Schema=="", "Schema"] <- schema[schema$Schema=="", "DataType"]
  }
  columnQS <- paste0(schema$ColumnName, " ", schema$Schema) %>% paste0(collapse=", ")
  queryString <- paste0("CREATE TABLE ", tablename, "( ", columnQS, ");")

  return(queryString)

}

