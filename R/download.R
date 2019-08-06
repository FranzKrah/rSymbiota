# This code is part of the rSymbiota R package ##
# Download records table from portal
download <- function(remdriver, portal.name, wait = 2, max_attempts = 5, nr_pages){


  ## in half of the portals the ">>" click is coded differently
  ## here a list of portals where it is coded via the "tablediv":
  nextclicktype <- portal.name %in% c("MyCoPortal",
                                      "Aquatic Invasives",
                                      "Monarch (California Academy of Sciences)",
                                      "University of Colorado Herbarium",
                                      "Symbiota Collections of Arthropods Network (SCAN)",
                                      "Monarch (California Academy of Sciences)",
                                      "Channel Islands Biodiversity Information System",
                                      "Documenting Ethnobiology in Mexico and Central America",
                                      "Consortium of Pacific Herbaria",
                                      "The University of New Hampshire Collection of Insects and Other Arthropods",
                                      "Macroalgal Consortium Herbarium Portal",
                                      "The Open Herbarium")

  tab <- list()
  for (i in 0:(nr_pages-1)) {

    message("page (", i+1, ")")
    if(i == 1){
      if(nextclicktype){
        webElem <-  remdriver$findElement("xpath", "//*[@id='tablediv']/div[1]/div[2]/a")
        Sys.sleep(1)
      }else{
        webElem <-  remdriver$findElement("xpath", "/html/body/div/div[2]/div[1]/div/a")
        Sys.sleep(1)
      }
      webElem$clickElement()
      Sys.sleep(wait)
    }
    if(i > 1){
      if(nextclicktype){
        webElem <- remdriver$findElement("xpath", "//*[@id='tablediv']/div[1]/div[2]/a[2]")
        Sys.sleep(1)
      }else{
        webElem <- remdriver$findElement("xpath", "/html/body/div/div[2]/div[1]/div/a[2]")
        Sys.sleep(1)
      }
      webElem$clickElement()
      Sys.sleep(wait)
    }

    tab[[i+1]] <- remote_table_retry(webElem, wait, max_attempts)

    message("...done \n")

  }
  tab <- do.call(rbind, tab)
  return(tab)
}
