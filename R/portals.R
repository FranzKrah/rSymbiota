#' Portals available through Symbiota
#' @details Symbiota is an open source content management system for curating specimen- and observation-based biodiversity data
#' @author Franz-Sebastian Krah
#' @examples
#' \dontrun{
#' portals()
#' }
#' @export
portals <- function() {

  fpath <- system.file("extdata", "portals.csv", package="rSymbiota")
  tab <- read.csv(fpath, sep =";")
  tab[] <- apply(tab, 2, as.character)

  return(tab)
}

#' Find URL for portal db
#' @param db character string specifying Symbiota database
#' @author Franz-Sebastian Krah
#' @examples
#' \dontrun{
#' portal(db = "lichen")
#' }
#' @export
portal <- function(db = "lichen") {

  symbiota.tab <- portals()

  inc <- unlist(apply(symbiota.tab, 2, grep, pattern = db))
  if(length(inc)==0)
    inc <- apply(symbiota.tab, 2, match, x = db)

  inc <- unique(inc)
  if(any(is.na(inc)))
    inc <- stats::na.omit(inc)

  portal <- symbiota.tab[inc, ]
  portal <- portal[!duplicated(portal$collection_url),]
  portal

}

