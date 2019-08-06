library(rSymbiota)

## Test of functions

## no results
## MyCoPortal
spec.dist <- symbiota(taxon = "", db = "Open Herbarium", wait = 5,
                      verbose = TRUE, country = "USA")


spec.dist <- symbiota(taxon = "Amanita muscaria", db = "mycoportal", wait = 5, screenshot = TRUE)

## Symbiota Collections of Arthropods Network
spec.dist <- symbiota(taxon = "Aedes aegypti", db = "SCAN", wait = 5)

recordsTable(spec.dist)
details(spec.dist, 2799041)

## Consortium of North American Bryophyte Herbaria
spec.dist <- symbiota(taxon = "Funaria hygrometrica", db = "bryophyte", wait = 5)
## Frullania Collaborative Research Network
spec.dist <- symbiota(taxon = "Frullania kunzei", db = "frullania")
## InvertEBase Data Portal
spec.dist <- symbiota(taxon = "Lumbricus", db = "invertebase", wait = 4)
## Consortium of North American Lichen Herbaria
spec.dist <- symbiota(taxon = "Parmelia cunninghamii", db = "lichen")
## Smithsonian Tropical Research Institute Portal
spec.dist <- symbiota(taxon = "Atelopus zeteki", db = "STRI")
## Aquatic Invasives
spec.dist <- symbiota(taxon = "Morone americana", db = "symbaquatic")
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
spec.dist <- symbiota(taxon = "Lonicera morrowii", db = "symbcnh")
## Madrean Archipelago Biodiversity Assessment (MABA) - Flora
spec.dist <- symbiota(taxon = "Anisacanthus thurberi", db = "symbflora")
## Madrean Archipelago Biodiversity Assessment (MABA) - Fauna
spec.dist <- symbiota(taxon = "Ambystoma rosaceum", db = "symbfauna")
## Herbario Virtual Austral Americano
spec.dist <- symbiota(taxon = "Calendula officinalis", db = "symbhvaa")
## CoTRAM – Cooperative Taxonomic Resource for Amer. Myrtaceae
spec.dist <- symbiota(taxon = "Campomanesia eugenioides", db = "symbcotram")
## InvertEBase Data Portal
spec.dist <- symbiota(taxon = "Birgus latro", db = "symbinvertebase")
## Lepidoptera of North America Network (LepNet)
spec.dist <- symbiota(taxon = "Lepidopa californica", db = "LepNet")
## Neotropical Entomology
spec.dist <- symbiota(taxon = "Physonota alutacea", db = "symbneotropentomology")
## Neotropical Flora
spec.dist <- symbiota(taxon = "Macfadyena unguis-cati", db = "symbneotropplants")
## Monarch (California Academy of Sciences)
spec.dist <- symbiota(taxon = "Canis lupus", db = "Monarch", wait = 4)
## The Lundell Plant Diversity Portal
spec.dist <- symbiota(taxon = "Xanthisma gracile", db = "Lundell") ## not supported, no table
## Virtual Flora of Wisconsin
spec.dist <- symbiota(taxon = "Fragaria virginiana", db = "Wisconsin")
## Red de Herbarios del Noroeste de México
spec.dist <- symbiota(taxon = "Perityle californica", db = "Red de Herbarios")
## University of Colorado Herbarium
spec.dist <- symbiota(taxon = "Cypripedium fasciculatum", db = "Colorado")
## The Open Herbarium
spec.dist <- symbiota(taxon = "Biarum straussii", db = "symbhereb")
## Consortium of Pacific Herbaria
spec.dist <- symbiota(taxon = "Begonia hirtella", db = "Pacific") ## error
## Documenting Ethnobiology in Mexico and Central America
spec.dist <- symbiota(taxon = "Salvia longispicata", db = "demca") ## error
## OpenZooMuseu
spec.dist <- symbiota(taxon = "Accipitridae", db = "symbzoo", wait = 6, taxon_type = 4)
## Mid-Atlantic Herbaria Consortium
spec.dist <- symbiota(taxon = "Viburnum dentatum", db = "midatlanticherbaria")
## Channel Islands Biodiversity Information System
spec.dist <- symbiota(taxon = "Heliotropium curassavicum", db = "CAL-IBIS")
## Consortium of Small Vertebrate Collections (CSVColl)
spec.dist <- symbiota(taxon = "Taxidea taxus", db = "CSVColl")
## The University of New Hampshire Collection of Insects and Other Arthropods
spec.dist <- symbiota(taxon = "Bombus borealis", db = "UNH server", wait = 6) ## error


x <- spec.dist
plot_distmap(x = x, mapdatabase = "usa", interactive = FALSE, gazetter = F)
plot_recordstreemap(x = x, groupvar = "country", log = FALSE)
plot_datamap(x = x, mapdatabase = "state", index = "rich", gazetter = F)
plot_datamap(x = x, mapdatabase = "world", index = "rec",
             area = list(min_long = -10, max_long = 30, min_lat = 30, max_lat = 70))

details(x, x@records$Symbiota.ID[1])

plot_distmap(x = spec.dist, mapdatabase = "world", interactive = FALSE,
             gazetter = TRUE)
plot_distmap(x = spec.dist, mapdatabase = "usa", interactive = FALSE)
plot_distmap(x = spec.dist, mapdatabase = "world", interactive = TRUE)
plot_datamap(x = spec.dist, mapdatabase = "state", index = "rich")
plot_recordstreemap(x = spec.dist, log = FALSE)


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

taxon = "Bombus borealis"
db = "UNH server"

taxon = "Begonia hirtella"
db = "Pacific"
