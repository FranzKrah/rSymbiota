#' Retrieve records from the Symbiota portals
#' @param taxon character string specifying the taxon name (e.g., species name, family name or higher taxon). To query higher taxa, e.g. on order level, I recommend using \code{\link{mycoportal_hightax}}
#' @param db portal name, for an overview see \code{\link{portals}}
#' @param country character string specifying country, e.g., "USA"
#' @param state character string specifying state, e.g., "Massachusetts"
#' @param county character string specifying county, e.g., "Worcester"
#' @param locality character string specifying locality, e.g., "Harvard Forest"
#' @param elevation_from character string, meter, e.g., "1000"
#' @param elevation_to character string, meter
#' @param host character string specifying host species, e.g., "Betula alba"
#' @param collection either "all" or a vector or integers with number corresponding to collection. For a list of collections use function \code{getCollections()}
#' @param taxon_type integer, one of 1 to 5 representing "Family or Scientific Name", "Scientific Name only", "Family Only", "Higher Taxonomy", Common Name"
#' @param north_lat character string, coordinate e.g., "45"
#' @param south_lat character string, coordinate
#' @param west_lon character string, coordinate, e.g., "-72"
#' @param east_lon character string, coordinate
#' @param point_lat character string, coordinate
#' @param point_lon character string, coordinate
#' @param radius character string, km, e.g., "50"
#' @param collector character string specifying collector name
#' @param collector_num character string specifying collector number
#' @param coll_date1 character string specifying collection data from, e.g., "19 August 1926"
#' @param coll_date2 character string specifying collection data from, e.g., "19 August 2018"
#' @param syns logical, if TRUE synonyms from MycoBank and IndexFungorum are searched
#' @param port default is 4445L
#' @param remoteServerAddr default is "localhost
#' @param verbose logical
#' @param screenshot logical, whether screenshot of results should be displayed in Viewer
#' @param browserName character string specifying the browser to use, recommended: "chrome"
#' @param wait numberic specifying the seconds to wait for website to load, recommended 2 for good internet connections;
#' higher otherwise. It would be good to first look up the number of pages for a species and to compare it with the function output to see whether loading times are sufficient.
#'
#' @return x an object of class "\code{\link{records}}" with the following components:
#' \item{nr.records}{A numeric giving the number of records retrieved}
#' \item{citation}{A character string with the recommended citation from the website}
#' \item{query}{A list of the user arguments used}
#' \item{records}{A data.frame with the query records results}
#' \item{db}{A character string specifying the database (currently only MyCoPortal)}
#'
#' @details Interface to the web databases of the Symbiota portals.
#' Symbiota is an open source content management system for curating specimen- and observation-based biodiversity data.
#' Currently ca. 40 portals are avaiable:
#' Consortium of North American Lichen Herbaria, Arctic Lichen Flora, Consortium of North American Bryophyte Herbaria, Frullania Collaborative Research Network, Macroalgal Consortium Herbarium Portal, MyCoPortal, Smithsonian Tropical Research Institute Portal (STRI), Aquatic Invasives, Aquatic Invasives, Aquatic Invasives, Consortium of Midwest Herbaria, SEINet, Intermountain Region Herbaria Network (IRHN), SouthEast Regional Network of Expertise and Collections (SERNEC), North American Network of Small Herbaria, Northern Great Plains Herbaria, Consortium of Northeastern Herbaria (CNH), Madrean Archipelago Biodiversity Assessment (MABA), Madrean Archipelago Biodiversity Assessment (MABA) - Fauna, Herbario Virtual Austral Americano, CoTRAM – Cooperative Taxonomic Resource for Amer. Myrtaceae, InvertEBase Data Portal, Symbiota Collections of Arthropods Network (SCAN), Lepidoptera of North America Network (LepNet), Neotropical Entomology, Neotropical Flora, Monarch (California Academy of Sciences), The Lundell Plant Diversity Portal, Virtual Flora of Wisconsin, Red de Herbarios del Noroeste de México, University of Colorado Herbarium, The Open Herbarium, Consortium of Pacific Herbaria, Minnesota Biodiversity Atlas, Documenting Ethnobiology in Mexico and Central America, OpenZooMuseum, Mid-Atlantic Herbaria Consortium, Channel Islands Biodiversity Information System, Consortium of Small Vertebrate Collections (CSVColl), The University of New Hampshire Collection of Insects and Other Arthropods.
#' For an overview and URLs see \code{\link{portals()}}
#' @references \url{http://symbiota.org/docs/}
#' @references Gries, C., Gilbert, E. E., and Franz, N. M. (2014). Symbiota–a virtual platform for creating voucher-based biodiversity information communities. Biodiversity Data Journal, (2).
#'
#' @import RSelenium httr RCurl
#' @importFrom XML htmlParse xpathApply xmlValue
#' @importFrom crayon red
#'
#' @author Franz-Sebastian Krah
#'
#' @examples
#' \dontrun{
#' ## Download Amanita muscaria observations and plot visualize data
#' spec.dist <- symbiota(taxon = "Amanita muscaria", db = "mycoportal")
#' ## Symbiota Collections of Arthropods Network
#' spec.dist <- symbiota(taxon = "Aedes aegypti", db = "SCAN")
#' ## Consortium of North American Bryophyte Herbaria
#' spec.dist <- symbiota(taxon = "Funaria hygrometrica", db = "bryophyte")
#' ## Frullania Collaborative Research Network
#' spec.dist <- symbiota(taxon = "Frullania kunzei", db = "frullania")
#' ## InvertEBase Data Portal
#' spec.dist <- symbiota(taxon = "Lumbricus", db = "invertebase")
#' ## Consortium of North American Lichen Herbaria
#' spec.dist <- symbiota(taxon = "Parmelia cunninghamii", db = "lichen")
#' ## Smithsonian Tropical Research Institute Portal
#' spec.dist <- symbiota(taxon = "Atelopus zeteki", db = "STRI")
#' ## Aquatic Invasives
#' spec.dist <- symbiota(taxon = "Nuphar lutea", db = "symbaquatic") ## error
#' ## Consortium of Midwest Herbaria
#' spec.dist <- symbiota(taxon = "Hamamelis virginiana", db = "Midwest Herbaria")
#' ## Consortium of Midwest Herbaria
#' spec.dist <- symbiota(taxon = "Sambucus cerulea", db = "SEINet")
#' ## Intermountain Region Herbaria Network (IRHN)
#' spec.dist <- symbiota(taxon = "Carex microptera", db = "IRHN")
#' ## SouthEast Regional Network of Expertise and Collections (SERNEC)
#' spec.dist <- symbiota(taxon = "Diospyros virginiana", db = "SERNEC")
#' ## North American Network of Small Herbaria
#' spec.dist <- symbiota(taxon = "Ambrosia dumosa", db = "Small Herbaria")
#' ## Northern Great Plains Herbaria
#' spec.dist <- symbiota(taxon = "Parietaria pensylvanica", db = "Great Plains")
#' ## Consortium of Northeastern Herbaria (CNH)
#' spec.dist <- symbiota(taxon = "Lonicera morrowii", db = "symbcnh")
#' ## Madrean Archipelago Biodiversity Assessment (MABA) - Flora
#' spec.dist <- symbiota(taxon = "Anisacanthus thurberi", db = "symbflora")
#' ## Madrean Archipelago Biodiversity Assessment (MABA) - Fauna
#' spec.dist <- symbiota(taxon = "Ambystoma rosaceum", db = "symbfauna")
#' ## Herbario Virtual Austral Americano
#' spec.dist <- symbiota(taxon = "Calendula officinalis", db = "symbhvaa")
#' ## CoTRAM – Cooperative Taxonomic Resource for Amer. Myrtaceae
#' spec.dist <- symbiota(taxon = "Campomanesia eugenioides", db = "symbcotram")
#' ## InvertEBase Data Portal
#' spec.dist <- symbiota(taxon = "Birgus latro", db = "symbinvertebase")
#' ## Lepidoptera of North America Network (LepNet)
#' spec.dist <- symbiota(taxon = "Lepidopa californica", db = "LepNet")
#' ## Neotropical Entomology
#' spec.dist <- symbiota(taxon = "Physonota alutacea", db = "symbneotropentomology")
#' ## Neotropical Flora
#' spec.dist <- symbiota(taxon = "Macfadyena unguis-cati", db = "symbneotropplants")
#' ## Monarch (California Academy of Sciences)
#' spec.dist <- symbiota(taxon = "Canis lupus", db = "Monarch")
#' ## The Lundell Plant Diversity Portal
#' spec.dist <- symbiota(taxon = "Xanthisma gracile", db = "Lundell") ## not supported, no table
#' ## Virtual Flora of Wisconsin
#' spec.dist <- symbiota(taxon = "Fragaria virginiana", db = "Wisconsin")
#' ## Red de Herbarios del Noroeste de México
#' spec.dist <- symbiota(taxon = "Perityle californica", db = "Red de Herbarios")
#' ## University of Colorado Herbarium
#' spec.dist <- symbiota(taxon = "Cypripedium fasciculatum", db = "Colorado")
#' ## The Open Herbarium
#' spec.dist <- symbiota(taxon = "Biarum straussii", db = "symbhereb") ## error
#' ## Consortium of Pacific Herbaria
#' spec.dist <- symbiota(taxon = "Begonia hirtella", db = "Pacific") ## error
#' ## Documenting Ethnobiology in Mexico and Central America
#' spec.dist <- symbiota(taxon = "Salvia longispicata", db = "demca") ## error
#' ## OpenZooMuseu
#' spec.dist <- symbiota(taxon = "Micronisus gabar", db = "symbzoo")
#' ## Mid-Atlantic Herbaria Consortium
#' spec.dist <- symbiota(taxon = "Viburnum dentatum", db = "midatlanticherbaria")
#' ## Channel Islands Biodiversity Information System
#' spec.dist <- symbiota(taxon = "Heliotropium curassavicum", db = "CAL-IBIS")
#' ## Consortium of Small Vertebrate Collections (CSVColl)
#' spec.dist <- symbiota(taxon = "Taxidea taxus", db = "CSVColl")
#' ## The University of New Hampshire Collection of Insects and Other Arthropods
#' spec.dist <- symbiota(taxon = "Bombus borealis", db = "UNH server") ## error
#'
#'
#' plot_distmap(x = spec.dist, mapdatabase = "world", interactive = FALSE)
#' plot_distmap(x = spec.dist, mapdatabase = "state", interactive = FALSE)
#' plot_distmap(x = spec.dist, mapdatabase = "world", interactive = TRUE)
#' plot_datamap(x = spec.dist, mapdatabase = "world")
#' plot_recordstreemap(x = spec.dist, log = FALSE)
#' }
#' @export


symbiota <- function(taxon = "Amanita muscaria",
                     db = "SCAN",
                     country = "",
                     state = "",
                     county = "",
                     locality = "",
                     elevation_from = "",
                     elevation_to = "",
                     host = "",
                     taxon_type = 1,
                     collection = "all",
                     north_lat = "",
                     south_lat = "",
                     west_lon = "",
                     east_lon = "",
                     point_lat = "",
                     point_lon = "",
                     radius = "",
                     collector = "",
                     collector_num = "",
                     coll_date1 = "",
                     coll_date2 = "",
                     syns = TRUE,
                     verbose = TRUE,
                     screenshot = TRUE,
                     port = 4445L,
                     browserName = "chrome",
                     remoteServerAddr = "localhost",
                     wait = 2) {


  if(length(grep(db, "The Lundell Plant Diversity Portal"))>0)
    stop("This portal is currently not supported!")


  # test internet conectivity
  if(!url.exists("r-project.org") == TRUE)
    stop( "Not connected to the internet. Please create a stable connection and try again." )
  if(!is.character(getURL("http://mycoportal.org/portal/index.php")))
    stop(" Database is not available : http://mycoportal.org/portal/index.php")

  if(taxon_type == "4"){
    cat(red("*mycoportal* may be unstable for higher taxon queries.","\n",
            "If you encounter stabilitiy issues, try *mycoportal_hightax*"))

    ent <- readline(prompt="Continue? [y/n]")
    if(ent == "n")
      stop("Aborted by user")
  }

  if(missing(taxon))
    stop("At least a species name has to be specified")

  if(length(grep("_", taxon))>0)
    taxon <- gsub("_", " ", taxon)

  wait <- ifelse(wait<=2, 2, wait)

  # Initialize session -----------------------------------------------------
  start_docker(verbose = verbose)

  ## Set up remote
  dr <- remoteDriver(remoteServerAddr = "localhost", port = port, browserName = browserName)
  Sys.sleep(wait-1)

  ## Open connection; run server
  out <- capture.output(dr$open(silent = FALSE))
  Sys.sleep(2)
  if(verbose > 1)
    cat(out)

  if(dr$getStatus()$ready)
    cat(dr$getStatus()$message[1], "\n")
  if(!dr$getStatus()$ready)
    stop("Remote server is not running \n Please check if Docker is installed!")


  # Open Website -----------------------------------------------------------
  cat(ifelse(verbose, "Open website\n", ""))
  ## Navigate to website


  ports <- portal(db)

  if(nrow(ports)>1){
    rownames(ports) <- NULL
    cat(red("More than 1 portal found, please specify the number\n"))
    print(ports[,1, drop = FALSE])
    Sys.sleep(0.5)
    cat("Please enter a row number:")
    ent <- scan(file = "", what = "", nmax = 1)
    ports <- ports[ent,]
  }

  portal.url <- ports$collection_url
  portal.name <- trimws(ports$Portal.Name)

  if(collection == "all"){ #usually it is convenient to query all collections

    portal.url <- gsub("index.php", "harvestparams.php", portal.url)
    dr$navigate(portal.url)
    Sys.sleep(wait+1)

  }else{ ## however, user may want specific collections

    # [needs to be checked for all]

    dr$navigate("http://mycoportal.org/portal/collections/index.php")
    Sys.sleep(wait+1)

    ## uncheck all collections
    button <- dr$findElement('xpath', "//*[@id='dballcb']")
    button$clickElement()

    ctn <- getCollections()
    nonus <- grep("Addis", ctn)

    ## check the desired ones
    for(i in collection){
        button <- dr$findElement('xpath', paste0("//*[@id='specobsdiv']/form/div[",
                                                 ifelse(i < nonus, 2,3),
                                                 "]/table/tbody/tr[",
                                                 i,
                                                 "]/td[2]/input"))
        button$clickElement()
        Sys.sleep(1)
    }
  }

  ## Enter user query ------------------------------------------------------------------
  cat(ifelse(verbose, "Send user query to website:\n", ""))

  # input arguments (will be exportet)
  argg <- do.call(c, as.list(match.call()))
  query <- argg <- argg[-1]
  argg <- do.call(c, argg)
  print(argg)

  ## Fill elements: user defined query input

  ## Checkbox: Show results in table view
  # [test if wesite has a table button or a tick box for table view]

  if(!is_table_button(dr)){
    button <- dr$findElement('xpath', "//*[@id='showtable']")
    button$clickElement()
  }

  ## Checkbox: Include Synonyms from Taxonomic Thesaurus
  # [default is ticked]
  if(!syns){
    button <- dr$findElement('xpath', "//*[@id='harvestparams']/div[3]/span/input")
    button$clickElement()
  }

  ## Taxon type
  webElem <- dr$findElement(using = 'xpath', paste0("//*[@id='taxontype']/option[", taxon_type ,"]"))
  webElem$clickElement()

  ## Taxon
  webElem <- dr$findElement('id', "taxa")
  webElem$sendKeysToElement(list(taxon))

  ## Country
  if(country != ""){
    webElem <- dr$findElement('id', "country")
    webElem$sendKeysToElement(list(country))
  }

  ## State
  if(state != ""){
    webElem <- dr$findElement('id', "state")
    webElem$sendKeysToElement(list(state))
  }

  ## County
  if(county != ""){
    webElem <- dr$findElement('id', "county")
    webElem$sendKeysToElement(list(county))
  }
  ## Locality
  if(locality != ""){
    webElem <- dr$findElement('id', "locality")
    webElem$sendKeysToElement(list(locality))
  }

  ## Elevation lower border
  if(elevation_from != ""){
    webElem <- dr$findElement('id', "elevlow")
    webElem$sendKeysToElement(list(elevation_from))
  }

  ## Elevation upper border
  if(elevation_to != ""){
    webElem <- dr$findElement('id', "elevhigh")
    webElem$sendKeysToElement(list(elevation_to))
  }

  ## Host (Plant species name)
  if(host != ""){
    webElem <- dr$findElement('id', "assochost")
    webElem$sendKeysToElement(list(host))
  }

  ##### Latitude and Longitude
  ## North latitude border
  if(north_lat != ""){
    webElem <- dr$findElement('id', "upperlat")
    webElem$sendKeysToElement(list(north_lat))
  }
  ## South latitude border
  if(south_lat != ""){
    webElem <- dr$findElement('id', "bottomlat")
    webElem$sendKeysToElement(list(south_lat))
  }
  ## West longitude border
  if(west_lon != ""){
    webElem <- dr$findElement('id', "leftlong")
    webElem$sendKeysToElement(list(west_lon))
  }
  ## East longitude border
  if(state != ""){
    webElem <- dr$findElement('id', "rightlong")
    webElem$sendKeysToElement(list(east_lon))
  }

  ##### Point-Radius Search
  ## Latitude of point
  if(point_lat != ""){
    webElem <- dr$findElement('id', "pointlat")
    webElem$sendKeysToElement(list(point_lat))
  }

  ## Longitude of point
  if(point_lon != ""){
    webElem <- dr$findElement('id', "pointlong")
    webElem$sendKeysToElement(list(point_lon))
  }

  ## Radius in km
  if(radius != ""){
    webElem <- dr$findElement('id', "radiustemp")
    webElem$sendKeysToElement(list(radius))
  }

  ##### Collector Criteria
  ## Collector name
  if(collector != ""){
    webElem <- dr$findElement('id', "collector")
    webElem$sendKeysToElement(list(collector))
  }

  ## Collector numbner
  if(collector_num != ""){
    webElem <- dr$findElement('id', "collnum")
    webElem$sendKeysToElement(list(collector_num))
  }
  ## Date record was found (from)
  if(coll_date1 != ""){
    webElem <- dr$findElement('id', "eventdate1")
    webElem$sendKeysToElement(list(coll_date1))
  }

  ## Date record was found (to)
  if(coll_date2 != ""){
    webElem <- dr$findElement('id', "eventdate2")
    webElem$sendKeysToElement(list(coll_date2))
  }


  # Press Enter -----------------------------------------------------

  if(is_table_button(dr)){
    button <- dr$findElement('xpath', "//*[@id='harvestparams']/div[2]/div[2]/button")
    button$clickElement()
    Sys.sleep(wait+2)
  }else{
    webElem$sendKeysToElement(list(key = "enter"))
    Sys.sleep(wait+2)
  }

  if(screenshot)
    dr$screenshot(display = TRUE, useViewer = TRUE)


  # Test whether results were found --------------------------------
  res <- htmlParse(dr$getPageSource()[[1]])
  res <- xpathApply(res, "//div", xmlValue)
  res <- grep("No records found matching the query", res)

  if(length(res)>0){
    # close server
    dr$close()

    ## stop docker
    cat(ifelse(verbose, "Stop Docker\n", ""))
    system(
      "docker stop $(docker ps -a -q)",
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
    cat(red(paste0(paste(rep("#", 43), collapse = ""),
                    "\n### No records for this query ###\n",
                    paste(rep("#", 43), collapse = ""))))
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    return(records(nr.records = 0,
                    citation = "Not applicable",
                    query = query,
                    records = data.frame(NULL),
                    db = "MyCoPortal"))
  }


  # Download tables -------------------------------------------------
  nr.p <- nr_pages(dr)
  cat(ifelse(verbose, paste("Downloading", nr.p, "pages\n"), ""))
  cat(red("Make sure you have a stable internet connection!\n"))

  ## Download tables in page-wise batches
  tabs <- list()
  for (i in 0:(nr.p-1)) {
    tabs[[i + 1]] <- retry_next_page_download(
        z = i,
        remdriver = dr,
        max_attempts = 5,
        wait_seconds = wait
      )

  }

  ## Rbind all tables
  tabs <- do.call(rbind, tabs)
  cat(nrow(tabs), "records were downloaded \n")

  ## Add coordinates as lon lat column
  tabs$coord <- stringr::str_extract(tabs$Locality, "-?\\d*\\.\\d*\\s\\-?\\d*\\.\\d*")
  coords <- data.frame(do.call(rbind, strsplit(tabs$coord , " ")))
  names(coords) <- c("lat", "lon")
  coords <- suppressWarnings(apply(coords, 2, function(x) as.numeric(as.character(x))))
  tabs <- data.frame(tabs, coords)
  spec <- tabs$Scientific.Name
  spec <- gsub("\\([A-z]*\\)", "\\1", spec)
  spec <- gsub("\\s+", " ", spec)
  tabs$spec <- stringr::word(spec, 1,2)

  # Close Website and Server ------------------------------------------------
  cat(ifelse(verbose, "Close website and quit server\n", ""))

  ## Close Website
  dr$close()

  ## Stop docker
  stop_docker()

  ## Return downloaded query results as data.frame

  if(collection != "all"){
    ctn <- getCollections()
    collection <- ctn[collection]
    collection <- paste(collection, collapse = "; ")
  }else{
    collection <- "All available collections used"
  }

  cit <-
    paste0(
      "Biodiversity occurrence data published by: <",
      collection,
      "> (Accessed through MyCoPortal Data Portal, http//:mycoportal.org/portal/index.php, ",
      Sys.Date(),
      ")"
    )

  records(
    nr.records = nrow(tabs),
    citation = cit,
    query = query,
    records = tabs,
    db = portal.name
  )
}
