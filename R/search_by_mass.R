#' Search the LIPID MASS Virtual Database
#'
#' Perform a precursor (MS1) ion search using the LIPID MAPS virtual database
#'
#' @param mz
#' @param adduct
#' @param ppm
#' @return
#'
#' @export
#' @examples

search_by_mass <- function(mz, adduct, ppm = 1.0)
{
  RESTURL <- 'http://www.lipidmaps.org/rest/moverz/LIPIDS/'

  query <- paste0(RESTURL, mz, '/', adduct, '/', ppm, '/txt')

  mz_res <-
    hlm_res <- httr::GET(query) %>% httr::content(., 'parsed')

  mz_res_clean <-
    mz_res %>% str_split('\n') %>% .[[1]]  %>% str_split('\t')

  len <- purrr:::map(mz_res_clean, ~ {
    length(.) < 9
  }) %>% unlist()

  bool <- which(len == 'TRUE')

  mz_res_clean2 <- mz_res_clean[-bool]

  mz_tib <- purrr::map(mz_res_clean2,
                       ~ {
                         tibble::tibble(field = mz_res_clean2[[1]],
                                        value = .[1:9]) %>%
                           tidyr::spread(field, value)
                       }) %>% bind_rows() %>% select(
                         .,
                         'Input m/z',
                         'Matched m/z',
                         Delta,
                         Name,
                         Formula,
                         Ion,
                         Category,
                         'Main class',
                         'Sub class'
                       )

  mz_tib <- mz_tib[-1, ]

  mz_tib$`Input m/z` <- as.numeric(mz_tib$`Input m/z`)
  mz_tib$`Matched m/z` <- as.numeric(mz_tib$`Matched m/z`)
  mz_tib$Delta <- as.numeric(mz_tib$Delta)

  mz_tib <- purrr::map_at(mz_tib, 4:9, trimws) %>% as_tibble()

  return(mz_tib)
}
