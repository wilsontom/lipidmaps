#' Search LIPID MAPS
#'
#'
#' @param type a character string indicating
#' @param value
#'
#' @return a `tibble` containing the following fields for each returned entry
#'        - __name__ Systematic name
#'        - __id__ LIPID MAPS database id
#'        - __mz__ Monoisotopic accurate mass
#'        - __inchi__ InChi Code
#'        - __inchi_key__ InChiKey
#'        - __smiles__ Smiles
#'        - __formula__ Molecular formula
#'        - __core_class__
#'        - __main_class__
#'        - __sub_class__
#'
#'
#' @export

search_lm <- function(type, value)
{
  RESTURL <- 'http://www.lipidmaps.org/rest/compound'

  url_request <-  paste0(RESTURL, '/', type, '/', value, '/all/')

  lm_res <- httr::GET(url_request) %>% httr::content(., 'parsed')

  lm_parse <-
    purrr::map(lm_res, ~ {
      t(unlist(.)) %>% as_tibble()
    }) %>% bind_rows()

  return(lm_parse)

}
