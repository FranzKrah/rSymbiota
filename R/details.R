#' Retrieve images and meta-data for a specific record (specimen)
#' @param x object of S4 class \code{records} (output of \code{symbiota})
#' @param id, as found in output of \code{records}
#'
#' @import magick rvest
#' @import methods
#'
#' @author Franz-Sebastian Krah
#'
#' @examples
#' \dontrun{
#' # use function records to download records; then use one of the IDs:
#' spec.dist <- symbiota(taxon = "Amanita muscaria", db = "mycoportal", wait = 4)
#' pic <- details(spec.dist, id = 2136920)
#' # Look at one of the images in more detail
#' print(image_read(pic$urls[1])) # a rather orangeish specimen
#' library(magick)
#' par(mfrow = c(1,3))
#' plot(image_read(pic$urls[1]))
#' plot(image_read(pic$urls[2]))
#' plot(image_read(pic$urls[3]))
#' # Look at meta data
#' pic$meta
#' }
#' @export

details <- function(x, id){

  db <- slot(x, "db")

  ## Websites with collection picker
  ports <- portal(db)
  portal.url <- ports$collection_url
  portal.name <- trimws(ports$Portal.Name)

  portal.url <- str_replace(portal.url, "collections", "collections/individual")
  x <- read_html(paste0(portal.url, "?occid=", id, "&clid=0"))
  # https://scan-bugs.org/portal/collections/individual/index.php?occid=35798832&clid=0

  # x <- read_html(paste0("http://mycoportal.org/portal/collections/individual/index.php?occid=",
                        # Symbiota.ID,
                       # "&clid=0"))

  ## Extract meta-data
  lab <- html_nodes(x, "div div b") %>% html_text()
  res <- html_nodes(x, "div div") %>% html_text()
  res <- gsub("\t", "", res)

  ocs <- grep(paste(lab, collapse = "|"), res)
  n <- unlist(lapply(res[ocs], nchar))
  res <- res[ocs]
  res <- res[n<150]
  res <- gsub("\r\n", "", res)
  res <- gsub("#", "", res)
  res <- gsub(" :", ":", res)
  if(length(grep("ImagesOpen|CommentLogin", res))>0)
    res <- res[-grep("ImagesOpen|CommentLogin", res)]
  if(anyDuplicated(res))
    res <- res[!duplicated(res) ]

  res <- do.call(rbind, str_split(res, ":", n = 2))
  res <- trimws(res)
  res <- data.frame(res)
  names(res) <- c("name", "value")

  ## Extract URLs
  x <- html_attr(html_nodes(x, "a"), "href")
  urls <- x[grep("jpg|jpeg|png|tiff", x)]
  urls[grep("imglib", urls)] <- paste0("http://mycoportal.org", urls[grep("imglib", urls)])
  if(anyDuplicated(urls))
    urls <- urls[!duplicated(urls)]

  return(list(meta = res, urls = urls))
}
