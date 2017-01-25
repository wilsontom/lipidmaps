#' Seach LIPID MAPS
#'
#' Retrieve data from the LIPID MAPS database using either an \code{InChiKey} or \code{Molecular Formula} as an
#' input
#'
#' @param x a character of a valid \code{InChiKey} or \code{Molecular Formula}
#' @return a \code{data.frame} containing the following fields for each returned entry
#'     \describe{
#'         \item{name}{Systematic name}
#'         \item{id}{LIPID MAPS database id}
#'         \item{mz}{Monoisotopic accurate mass}
#'         \item{inchi}{InChi Code}
#'         \item{inchi_key}{InChiKey}
#'         \item{smiles}{Smiles}
#'         \item{formula}{Molecular formula}
#'         \item{core_class}
#'         \item{main_class}
#'         \item{sub_clss}
#'     }
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @examples \dontrun{
#'     LMsearch("C18H32O2")
#'     LMsearch("ZKRPGPZHULJLKJ-JHRQRACZSA-N")
#' }

LMsearch <- function(x)
  {
  RESTURL <- "http://www.lipidmaps.org/rest/compound"

  x <- as.list(match.call())$x
  xx <- gsub("-","", x)
  xs <- strsplit(xx, "")

  if(all(grepl("^[[:upper:]]",xs[[1]]) == TRUE)){
      print("InChi Key Detected")
      url_request <- paste0(RESTURL, "/inchi_key/", x, "/all/")
      optin <- "inchi_key"
  }else{
      if(any(grepl("[[:alnum:]]",x)) == TRUE){
        if(length(grep("C", xs[[1]])) == 1){
           print("Molecular Formula Detected")
           optin <- "formula"
           url_request <- paste0(RESTURL, "/formula/", x, "/all/")
      }else{
          stop("Input is not a valid InChiKey or Molecular Formula", call. = FALSE)
      }}
  }

  url_return <- RCurl::getURLContent(url_request)
  json_parse <- jsonlite::fromJSON(url_return)


  lm_df <- function(x)
    {

    df <- data.frame(name = x$sys_name, id = x$lm_id, mz = x$exactmass,
                      inchi = x$inchi, inchi_key = x$inchi_key, smiles = x$smiles,
                          formula = x$formula,core_class = x$core,
                              main_class = x$main_class,sub_class = x$sub_class)

    return(df)

    }

  if(optin == "inchi_key"){
      nullID <- as.numeric(which(sapply(json_parse, function(x)(which(is.null(x)))) == 1))
      if(length(nullID) != 0){
          json_parse[[nullID]] <- "NA"}
      final_df <- lm_df(json_parse)
  }

  if(optin == "formula"){
      parse_df <- NULL
      for(i in seq_along(json_parse)){
      nullID <- as.numeric(which(sapply(json_parse[[i]], function(x)(which(is.null(x)))) == 1))
      if(length(nullID) != 0){
          json_parse[[i]][nullID] <- "NA"
      }
      parse_df[[i]] <- lm_df(json_parse[[i]])
  }


  final_df <- do.call("rbind", parse_df)
  }

  return(final_df)

  }

