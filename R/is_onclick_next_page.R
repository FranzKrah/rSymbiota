# Test if consecutive table page reached by onclick function or direct link
is_onclick_next_page <- function(remdriver){

  x <- remdriver$getPageSource()[[1]]
  x <- read_html(x)
  x <- html_attr(html_nodes(x, "a"), "href")
  x <- x[x!="#"]

  x <- grep("listtabledisplay", x)
  ifelse(length(x) > 0, FALSE, TRUE)

}
