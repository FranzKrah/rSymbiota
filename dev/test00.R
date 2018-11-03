library(rSymbiota)

## Test of functions

## no results
## MyCoPortal
spec.dist <- symbiota(taxon = "Amanita muscaria", db = "mycoportal")
## Symbiota Collections of Arthropods Network
spec.dist <- symbiota(taxon = "Aedes aegypti", db = "SCAN")
## Consortium of North American Bryophyte Herbaria
spec.dist <- symbiota(taxon = "Funaria hygrometrica", db = "bryophyte")
## Frullania Collaborative Research Network
spec.dist <- symbiota(taxon = "Frullania kunzei", db = "frullania")
## InvertEBase Data Portal
spec.dist <- symbiota(taxon = "Lumbricus", db = "invertebase")
## Consortium of North American Lichen Herbaria
spec.dist <- symbiota(taxon = "Parmelia cunninghamii", db = "lichen")
## Smithsonian Tropical Research Institute Portal
spec.dist <- symbiota(taxon = "Atelopus zeteki", db = "STRI")
## Aquatic Invasives
spec.dist <- symbiota(taxon = "Nuphar lutea", db = "symbaquatic") ## error
## Consortium of Midwest Herbaria
spec.dist <- symbiota(taxon = "Hamamelis virginiana", db = "Midwest Herbaria")
## Consortium of Midwest Herbaria
spec.dist <- symbiota(taxon = "Sambucus cerulea", db = "SEINet")
## Intermountain Region Herbaria Network (IRHN)
spec.dist <- symbiota(taxon = "Carex microptera", db = "IRHN")
## SouthEast Regional Network of Expertise and Collections (SERNEC)
spec.dist <- symbiota(taxon = "Diospyros virginiana", db = "SERNEC")
## North American Network of Small Herbaria
spec.dist <- symbiota(taxon = "Ambrosia dumosa", db = "Small Herbaria")
## Northern Great Plains Herbaria
spec.dist <- symbiota(taxon = "Parietaria pensylvanica", db = "Great Plains")
## Consortium of Northeastern Herbaria (CNH)
spec.dist <- symbiota(taxon = "Lonicera morrowii", db = "symbcnh") ## error
## Madrean Archipelago Biodiversity Assessment (MABA) - Flora
spec.dist <- symbiota(taxon = "Anisacanthus thurberi", db = "symbflora")
## Madrean Archipelago Biodiversity Assessment (MABA) - Fauna
spec.dist <- symbiota(taxon = "Ambystoma rosaceum", db = "symbfauna")


x <- spec.dist
plot_distmap(x = x, mapdatabase = "world")
plot_recordstreemap(x = x, groupvar = "country", log = FALSE)
plot_datamap(x = x, mapdatabase = "state", index = "rec")
# plot_datamap(x = x, mapdatabase = "world", index = "rec",
             # area = list(min_long = -10, max_long = 30, min_lat = 30, max_lat = 70))

details(x, x@records$Symbiota.ID[1])



### for debugging:

library("RSelenium")
library("XML")
library("httr")
library("stringr")
library("rvest")
library("xml2")
library("ssh.utils")
library("crayon")
library("sys")

# taxon = "Amanita muscaria
taxon <- "Aedes aegypti"
taxon <- "Funaria hygrometrica"
# country = "";
# state = "";
# county = "";
# locality = "";
# elevation_from = "";
# elevation_to = "";
# host = "";
# taxon_type = 4;
# north_lat = ""; south_lat = ""; west_lon = ""; east_lon = "";
# point_lat = ""; point_lon = ""; radius = "";
# collector = ""; collector_num = ""; coll_date1 = ""; coll_date2 = "";
# syns = TRUE;
verbose = TRUE
screenshot <- TRUE
port = 4445L
browserName = "chrome"
remoteServerAddr = "localhost"
# radius <- "50"
# point_lat <- "42"
# point_lon <- "-72"
collection <- "all"
