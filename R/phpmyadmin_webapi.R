#' get phpmyadmin login token
#' 
#' @param user, phpmyadmin username
#' @param pass, phpmyadmin user password
#' @param Url, remote server URL
#' @return pma_token
#' @examples
#' \dontrun{
#'   mytoken <- pma_gettoken('test', '****', 'localhost:8080/phpmyadmin') 
#'   mytoken 
#' } 
#' @importFrom dplyr %>%
#' @importFrom stringr str_match
#' @importFrom stringr str_replace
#' @import httr 
#' @export
pma_gettoken <- function(user, pass, Url) {
  
  Url <- paste0(Url)
  Response <- POST(Url, body=list(pma_username=user, pma_password=pass), encode="form")
	stopifnot( status_code(Response)==200 )
  Token <- cache_info(Response)$url %>% 
    str_match("token.*$") %>% 
		str_replace("&.*$", "") %>% 
    str_replace("token=", "") 

  invisible(Token)

}

#' send a SQL query string to phpmyadmin
#'
#' @param queryString, SQL query string. Be care of the string length and string length each line.
#' @param dbname, database name
#' @param Url, remote server URL
#' @param Token, login token
#' @return http status code
#' @examples
#' \dontrun{
#'   mytoken <- pma_gettoken('test', '****', 'localhost:8080/phpmyadmin')
#'   pma_sendquery('truncate table iris', 'testdb', 'localhost:8080/phpmyadmin', mytoken)
#' }
#' @importFrom dplyr %>%
#' @import httr 
#' @export
pma_sendquery <- function(queryString, dbname, Url, Token) {
  
  Url <- paste0(Url, "/import.php")
  PostBody <- list(token=Token, db=dbname, sql_query=queryString)
  statuscode <- POST(Url, body=PostBody, encode="form") %>% status_code()
  return(statuscode)

}

#' show tables on phpmyadmin server
#'
#' @param dbname, database name
#' @param Url, remote server URL
#' @param Token, login token
#' @return table names
#' @importFrom dplyr %>% 
#' @importFrom stringr str_extract_all
#' @import httr 
#' @export
pma_gettable <- function(dbname, Url, Token) {
  
  Url <- paste0(Url, "/db_structure.php")
  PostBody <- list(db=dbname, token=Token)
  Response <- POST(Url, body=PostBody, encode="form")
  stopifnot( status_code(Response)==200 )
  tbls <- content(Response, as='text') %>% 
    str_extract_all("<th><a.*/th>") %>% 
    .[[1]] %>%
    .[!grepl("sort_order", .)] %>% 
    gsub("<.*?>", "", .)
	return(tbls)
  
}

#' get a query result of mysql
#' 
#' @param queryString, query string
#' @param dbname, database name
#' @param Url, remote server URL
#' @param Token, login token
#' @return tibble
#' @export
pma_getquery <- function(queryString, dbname, Url, Token) {
  
  stop("To be implemented")

  Url <- paste0(Url, "/import.php")
  PostBOdy <- list(db=dbname, token=Token, sql_query=queryString, ajax_request='true')
  Response <- POST(Url, body=PostBody, encode='form')

}

