
# Helper function; retry if internet connection temporatliy fluctuates
start_docker_try <- function(max_attempts, verbose, wait){

  for (j in seq_len(max_attempts)) {

    out <- tryCatch(start_docker(verbose = verbose, wait = wait),
                    warning = function(w) {"Unstable";},
                    error = function(e) {"Unstable";}
    )

    if(out == 0){
      return(out)
    }
    if(out == "Unstable"){
      if(verbose & j == max_attempts)
        message("Server does not respond. Please check of Docker is running correctly.")
      Sys.sleep(1)
    }
  }
}

# Helper function remote_table; scrapes the observation records
# Retry version of the remote_table function
#' @importFrom xml2 read_html
#' @importFrom rvest html_table
remote_table_retry <- function(remdriver, max_attempts, wait){

  remote_table <- function(remdriver, wait){

    x <- remdriver$findElement('class', 'styledtable')
    Sys.sleep(wait)
    x <- x$getPageSource()[[1]]
    x <- read_html(x)
    x <- html_table(x)[[1]]

    return(x)
  }

  message("...download")
  for (i in 1:max_attempts)
  {
    out <- tryCatch(remote_table(remdriver, wait),
                    warning = function(w) {"Unstable";},
                    error = function(e) {"Unstable";})
    if (identical(out, "Unstable"))
    {
      message("...retry\n")
      Sys.sleep(wait)
    }
    else
    {
      return (out)
    }
  }
}




# next_page_download <- function(z, remdriver, k, verbose, portal.name, wait) {
#
#   if(z == 0){
#     message("page (1)\n...download ")
#     tab <- remote_table(remdriver)
#     message("...done \n")
#   }else{
#     if(k == 0){
#       if(z == 1){
#
#
#         ## the portals have slightly different "next page" buttons,
#         ## therefore the if split here
#         if(portal.name %in% c("MyCoPortal",
#                               "Aquatic Invasives",
#                               "Monarch (California Academy of Sciences)",
#                               "University of Colorado Herbarium",
#                               "Symbiota Collections of Arthropods Network (SCAN)",
#                               "Monarch (California Academy of Sciences)",
#                               "Channel Islands Biodiversity Information System",
#                               "Documenting Ethnobiology in Mexico and Central America",
#                               "Consortium of Pacific Herbaria",
#                               "The University of New Hampshire Collection of Insects and Other Arthropods",
#                               "Macroalgal Consortium Herbarium Portal")){
#
#           webElem <-  remdriver$findElement("xpath", "//*[@id='tablediv']/div[1]/div[2]/a")
#
#         }else{
#           webElem <-  remdriver$findElement("xpath", "/html/body/div/div[2]/div[1]/div/a")
#         }
#         message("page (", z+1, ")")
#         webElem$clickElement()
#         Sys.sleep(wait)
#       }
#       if(z > 1){
#         if(portal.name %in% c("MyCoPortal",
#                               "Aquatic Invasives",
#                               "Monarch (California Academy of Sciences)",
#                               "University of Colorado Herbarium",
#                               "Symbiota Collections of Arthropods Network (SCAN)",
#                               "Monarch (California Academy of Sciences)",
#                               "Channel Islands Biodiversity Information System",
#                               "Documenting Ethnobiology in Mexico and Central America",
#                               "Consortium of Pacific Herbaria",
#                               "The University of New Hampshire Collection of Insects and Other Arthropods",
#                               "Macroalgal Consortium Herbarium Portal")){
#
#           webElem <- remdriver$findElement("xpath", "//*[@id='tablediv']/div[1]/div[2]/a[2]")
#         }else{
#           webElem <-  remdriver$findElement("xpath", "/html/body/div/div[2]/div[1]/div/a[2]")
#         }
#         message("page (", z+1, ")")
#         webElem$clickElement()
#         Sys.sleep(wait)
#       }
#     }
#     message("...download ")
#     tab <- remote_table(webElem)
#     message("...done\n")
#   }
#   return(tab)
# }


# retry_next_page_download <- function(z,
#                                      remdriver,
#                                      verbose,
#                                      max_attempts,
#                                      wait_seconds,
#                                      portal.name) {
#
#
#   k <- 0
#
#   for (j in seq_len(max_attempts)) {
#
#     out <- tryCatch(next_page_download(z, remdriver, k, portal.name = portal.name),
#                     # message = function(n) {"Unstable"},
#                     warning = function(w) {"Unstable";},
#                     error = function(e) {"Unstable";})
#
#     Sys.sleep(wait_seconds)
#
#     if (is.data.frame(out)) {
#       return(out)
#     }
#     if(out == "Unstable"){
#       if(verbose)
#         message("Lost connection\n")
#       if (wait_seconds > 0) {
#         if(verbose)
#           message("Retrying...")
#         if(j == 5)
#           remdriver$refresh()
#           Sys.sleep(wait_seconds)
#       }
#     }
#     k <- k + 1
#   }
# }

# retry_remote_table <- function(remdriver, max_attempts = 5,
#                                wait_seconds = 1) {
#
#
#   remote_table <- function(remdriver){
#
#     x <- remdriver$findElement('class', 'styledtable')
#     x <- x$getPageSource()[[1]]
#     x <- xml2::read_html(x)
#     x <- rvest::html_table(x)[[1]]
#
#     return(x)
#   }
#
#
#   for (j in seq_len(max_attempts)) {
#
#     out <- tryCatch(remote_table(remdriver),
#                     message = function(n) {"Unstable"},
#                     warning = function(w) {"Unstable";},
#                     error = function(e) {"Unstable";})
#
#     if (is.data.frame(out)) {
#       return(out)
#     }
#     if(out == "Unstable"){
#       message(red("\nLost connection\n"))
#       if (wait_seconds > 0) {
#         message(red("Retrying..."))
#         Sys.sleep(wait_seconds)
#         if(j == floor(max_attempts/2))
#           remdriver$refresh()
#       }
#     }
#   }
# }
