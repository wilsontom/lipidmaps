#' Search LIPID MAPS
#'
#'
#' @param type a character string indicating
#' @param value
#'
#' @return a `tibble` containing the following fields for each returned entry
#'  - __lm_id__
#'  - __molecular_weight__
#'  - __exact_mass__
#'  - __heavy_atoms__
#'  - __rings__
#'  - __molecular_volume__
#'  - __rotatable_bonds__
#'  - __hbond_donors__
#'  - __hbond_acceptors__
#'  - __slogp__
#'  - __smr__
#'  - __tpsa__
#'  - __aromatic_rings__
#' @export

get_physchem <- function(type, value)
{
  RESTURL <- 'http://www.lipidmaps.org/rest/compound'

  url_request <-  paste0(RESTURL, '/', type, '/', value, '/physchem/')

  pc_res <- httr::GET(url_request) %>% httr::content(., 'parsed')

  pc_parse <-
    purrr::map(pc_res, ~ {
      t(unlist(.)) %>% as_tibble()
    }) %>% bind_rows()

  return(pc_parse)

}
