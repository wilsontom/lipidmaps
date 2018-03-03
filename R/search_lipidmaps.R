#' Search LIPID MAPS
#'
#' Search the LIPID MAPS database
#'
#' @param type a character string indicating
#' @param value the value for the matching `type`
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
#' @examples
#'
#' search_lipidmaps('inchi_key', 'ZQPPMHVWECSIRJ-KTKRTIGZSA-N')
#'
#' search_lipidmaps('formula', 'C20H34O')
#'
#' search_lipidmaps('pubchem_cid', 445639)

search_lipidmaps <- function(type, value)
{
  RESTURL <- 'http://www.lipidmaps.org/rest/compound'

  url_request <-  paste0(RESTURL, '/', type, '/', value, '/all/')

  lm_res <- httr::GET(url_request) %>% httr::content(., 'parsed')

  if (length(grep('Row', names(lm_res))) >= 1) {
    lm_parse <-
      purrr::map(lm_res, ~ {
        t(unlist(.)) %>% as_tibble()
      }) %>% bind_rows()
  } else{
    lm_parse <- as_tibble(lm_res)
  }

  return(lm_parse)
}
