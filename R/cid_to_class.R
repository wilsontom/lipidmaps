#'  PubChem CID to LIPIDMAPS Class
#'
#' @param CID
#' @return
#'
#' @export


cid_to_class <- function(cid)
{
  if (!is.numeric(cid)) {
    stop("Pub Chem CID must be numeric", call. = FALSE)
  }

  RESTURL <- "http://www.lipidmaps.org/rest/compound/pubchem_cid/"

  query <- paste0(RESTURL, cid, "/classification")

  query_res <-
    httr::GET(query) %>% httr::content(., 'parsed') %>% as_tibble()

  return(query_res)
}
