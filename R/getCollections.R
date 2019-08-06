#' List of available collections
#'
#' @param db portal name, for an overview see \code{\link{portals}}
#' @details Get list of available collections from the MyCoPortal. For details also see \url{http://mycoportal.org/portal/collections/index.php}
#'
#' @author Franz-Sebastian Krah
#'
#' @importFrom XML xpathSApply htmlParse
#' @importFrom methods as
#' @importFrom xml2 read_html
#' @examples
#' \dontrun{
#' getCollections(db = "SCAN")
#' }
#' @export

getCollections <- function(db){

  ## Websites with collection picker
  ports <- portal(db)

  if(nrow(ports)>1){
    rownames(ports) <- NULL
    message(red("More than 1 portal found, please specify the number\n"))
    print(ports[,1, drop = FALSE])
    Sys.sleep(0.5)
    message("Please enter a row number:")
    ent <- scan(file = "", what = "", nmax = 1)
    ports <- ports[ent,]
  }

  portal.url <- ports$collection_url
  portal.name <- trimws(ports$Portal.Name)

  if(length(grep("unhcollection", portal.url))>0){
    stop("This portals doesn't have collections.")
  }

  y <- read_html(portal.url)
  coll <- htmlParse(y)

  ## xPath to collection names

  coll2 <- xpathSApply(coll, "//*[@id='specobsdiv']//form//div[2]//table/..//a")
  coll3 <- xpathSApply(coll, "//*[@id='specobsdiv']//form//div[3]//table/..//a")
  coll <- c(coll2, coll3)

  ## Extract names
  coll <- lapply(coll, as, "character")
  coll <- str_split(coll, "\t\t\t\t")
  coll <- unlist(coll)
  coll <- coll[-grep("href|</a>|\tmore", coll)]
  coll <- coll[lapply(coll, nchar) > 0]
  coll <- gsub("\t", "", coll)

  return(coll)
}
