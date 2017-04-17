#' get queryString to set mysql table column collate as utf8_coration_ci
#'
#' @param tablename, mysql table name
#' @param columnname, table column name
#' @param charlength, varchar length default=50
#' @return mysql alter table sql string
#' @export
my_qsg_collateutf8 <- function(tablename, columnname, charlength=50) {

  queryString <- sprintf("ALTER TABLE `%s` CHANGE `%s` `%s`, VARCHAR(%d) CHARACTER
    SET utf8 COLLATE utf8_croatian_ci NULL DEFAULT NULL",
    tablename, columnname, columnname, charlength)
  return(queryString)

}

