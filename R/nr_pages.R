# Helper function nr_pages
#' @importFrom XML htmlTreeParse xpathApply xmlValue
#' @import stringr

nr_pages <- function(remdriver) {

  ## retrieve source from website
  nr <- remdriver$getPageSource()
  nr <- htmlTreeParse(nr[[1]], useInternalNodes = TRUE)
  nr <- xpathApply(nr, "//div//div", xmlValue)
  nr <- unlist(nr)

  ## extract number of pages
  nr <- nr[grep("records", nr)]
  nr <- str_extract_all(nr, "\\d*-\\d* of \\d*")
  nr <- Reduce(union, nr)
  count <- as.numeric(str_extract(nr, "\\d+$"))
  nr <- ceiling(count/1000)

  return(nr)
}
