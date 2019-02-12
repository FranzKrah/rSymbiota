#' Portals available through the Symbiota
#' @details Symbiota is an open source content management system for curating specimen- and observation-based biodiversity data
#' @author Franz-Sebastian Krah
#' @export
portals <- function() {
  tab <- (
    '"Portal Name";"Database";"url";"collection_url"
    "Consortium of North American Lichen Herbaria";"symblichens";"http://lichenportal.org/portal/index.php";"http://lichenportal.org/portal/collections/index.php"
    "Arctic Lichen Flora";"symblichens";"http://lichenportal.org/arctic/index.php";"http://lichenportal.org/portal/collections/index.php"
    "Consortium of North American Bryophyte Herbaria";"symbbryophytes";"http://bryophyteportal.org/portal/";"http://bryophyteportal.org/portal/collections/index.php"
    "Frullania Collaborative Research Network";"symbbryophytes";"http://bryophyteportal.org/frullania/";"http://bryophyteportal.org/frullania/collections/index.php"
    "Macroalgal Consortium Herbarium Portal";"symbalgae";"http://macroalgae.org/portal/index.php";"http://macroalgae.org/portal/collections/index.php"
    "MyCoPortal";"symbmycology";"http://mycoportal.org/portal/index.php";"http://mycoportal.org/portal/collections/index.php"
    "Smithsonian Tropical Research Institute Portal (STRI)";"symbstri";"http://stricollections.org/portal/";"http://stricollections.org/portal/collections/index.php"
    "Aquatic Invasives";"symbaquatic";"http://greatlakesinvasives.org/portal/index.php";"http://greatlakesinvasives.org/portal/collections/index.php"
    "Aquatic Invasives";"symbaquatic";"http://greatlakesinvasives.org/portal/index.php";"http://greatlakesinvasives.org/portal/collections/index.php"
    "Aquatic Invasives";"symbaquatic";"http://greatlakesinvasives.org/portal/index.php";"http://greatlakesinvasives.org/portal/collections/index.php"
    "Consortium of Midwest Herbaria";"symbseinet";"http://midwestherbaria.org/portal/index.php";"http://midwestherbaria.org/portal/collections/index.php"
    "SEINet";"symbseinet";"http://swbiodiversity.org/seinet/index.php";"http://swbiodiversity.org/seinet/collections/index.php"
    "Intermountain Region Herbaria Network (IRHN)";"symbseinet";"http://intermountainbiota.org/portal/index.php";"http://intermountainbiota.org/portal/collections/index.php"
    "SouthEast Regional Network of Expertise and Collections (SERNEC)";"symbseinet";"http://sernecportal.org/portal/";"http://sernecportal.org/portal/collections/index.php"
    "North American Network of Small Herbaria";"symbseinet";"http://nansh.org/portal/index.php";"http://nansh.org/portal/collections/index.php"
    "Northern Great Plains Herbaria";"symbseinet";"http://ngpherbaria.org/portal/index.php";"http://ngpherbaria.org/portal/collections/index.php"
    "Consortium of Northeastern Herbaria (CNH)";"symbcnh";"http://portal.neherbaria.org/portal/";"http://portal.neherbaria.org/portal/collections/index.php"
    "Madrean Archipelago Biodiversity Assessment (MABA) - Flora";"symbseinet";"http://madrean.org/symbflora/projects/index.php?proj=74";"http://madrean.org/symbflora/collections/index.php"
    "Madrean Archipelago Biodiversity Assessment (MABA) - Fauna";"symbfauna";"http://madrean.org/symbfauna/projects/index.php";"http://madrean.org/symbfauna/collections/index.php"
    "Herbario Virtual Austral Americano";"symbhvaa";"http://herbariovaa.org/";"http://herbariovaa.org/collections/index.php"
    "CoTRAM – Cooperative Taxonomic Resource for Amer. Myrtaceae";"symbcotram";"http://cotram.org/";"http://cotram.org/collections/index.php"
    "InvertEBase Data Portal";"symbinvertebase";"http://invertebase.org/portal/index.php";"http://invertebase.org/portal/collections/index.php"
    "Symbiota Collections of Arthropods Network (SCAN)";"symbscan";"http://symbiota4.acis.ufl.edu/scan/portal/index.php";"http://scan-bugs.org/portal/collections/index.php"
    "Lepidoptera of North America Network (LepNet)";"symbscan";"http://symbiota4.acis.ufl.edu/scan/lepnet/portal/index.php";"http://symbiota4.acis.ufl.edu/scan/lepnet/portal/collections/index.php"
    "Neotropical Entomology";"symbneotropentomology";"http://symbiota.org/neotrop/entomology/index.php";"http://hasbrouck.asu.edu/neotrop/entomology/collections/index.php"
    "Neotropical Flora";"symbneotropplants";"http://symbiota.org/neotrop/plantae/index.php";"http://hasbrouck.asu.edu/neotrop/plantae/collections/index.php"
    "Monarch (California Academy of Sciences)";"internal network";"http://monarch.calacademy.org/";"https://monarch.calacademy.org/collections/index.php"
    "The Lundell Plant Diversity Portal";"?";"http://prc-symbiota.tacc.utexas.edu/index.php";"https://prc-symbiota.tacc.utexas.edu/collections/index.php"
    "Virtual Flora of Wisconsin";"?";"http://symbiota.botany.wisc.edu";"http://symbiota.botany.wisc.edu/collections/index.php"
    "Red de Herbarios del Noroeste de México";"symbseinet";"http://herbanwmex.net/portal/index.php";"http://herbanwmex.net/portal/collections/index.php"
    "University of Colorado Herbarium";"?";"https://botanydb.colorado.edu/";"https://botanydb.colorado.edu/collections/index.php"
    "The Open Herbarium";"symbhereb";"http://openherbarium.org/index.php";"http://openherbarium.org/portal/collections/index.php"
    "Consortium of Pacific Herbaria";"[own private database]";"http://www.pacificherbaria.org/";"http://symbiota4.acis.ufl.edu/pacific/portal/collections/index.php"
    "Oregon Flora";"Oregon State";"http://symbiota.oregonflora.org/portal/index.php";"password required"
    "Minnesota Biodiversity Atlas";"";"http://bellatlas.umn.edu/";"http://bellatlas.umn.edu/collections/index.php"
    "Documenting Ethnobiology in Mexico and Central America";"";"http://demca.sites.gettysburg.edu/";"http://demca.sites.gettysburg.edu/collections/index.php"
    "OpenZooMuseum";"symbzoo";"http://openherbarium.org/zoo/portal/index.php";"http://openherbarium.org/zoo/portal/collections/index.php"
    "Mid-Atlantic Herbaria Consortium";"";"http://midatlanticherbaria.org/portal/";"http://midatlanticherbaria.org/portal/collections/index.php"
    "Channel Islands Biodiversity Information System";"CAL-IBIS";"http://www.cal-ibis.org/";"http://www.cal-ibis.org/collections/index.php"
    "Consortium of Small Vertebrate Collections (CSVColl)";"?";"http://csvcoll.org/portal/index.php";"http://csvcoll.org/portal/collections/index.php"
    "The University of New Hampshire Collection of Insects and Other Arthropods";"UNH server";"https://unhcollection.unh.edu/database/collections/harvestparams.php"; "https://unhcollection.unh.edu/database/index.php"
    "Madrean Archipelago Biodiversity Assessment (MABA) - Fauna"; "MABA"; "http://madrean.org/symbfauna/projects/index.php"; "http://madrean.org/symbfauna/collections/index.php"
    "Macroalgal Consortium Herbarium Portal"; "?"; "http://macroalgae.org/portal/index.php"; "http://macroalgae.org/portal/collections/index.php"
    "Consortium of Midwest Herbaria";"Consortium of Midwest Herbaria";"http://midwestherbaria.org/portal/index.php"; "http://midwestherbaria.org/portal/collections/index.php"
    "Oregon Flora"; "[not public]"; "http://symbiota.oregonflora.org/portal/index.php"; "http://symbiota.oregonflora.org/portal/index.php"
    "Minnesota Biodiversity Atlas"; "Minnesota Biodiversity Atlas"; "http://bellatlas.umn.edu"; "http://bellatlas.umn.edu/collections/index.php"
    "Capturing Californias Flowers"; "Capturing California`s Flowers"; "http://www.portal.capturingcaliforniasflowers.org/portal/index.php"; "http://www.portal.capturingcaliforniasflowers.org/portal/collections/index.php"
    "The Pteridological Collections Consortium"; "Pteridological"; "http://www.pteridoportal.org/portal/"; "http://www.pteridoportal.org/portal/collections/index.php"
    "Consortium of California Herbaria Portal (CCH2)"; "CCH2"; "http://www.portal.capturingcaliforniasflowers.org/portal/"; "http://www.portal.capturingcaliforniasflowers.org/portal/collections/index.php"
    "University of New Hampshire Collection of Insects and Other Arthropods (UNHC)"; "UNHC"; "https://unhcollection.unh.edu/database/index.php"; "https://unhcollection.unh.edu/database/collections/harvestparams.php"' )
  tab <- read.csv(text = tab, sep = ";")
  tab[] <- apply(tab, 2, as.character)
  tab[] <- apply(tab, 2, trimws)

  return(tab)
}


#' Find URL for portal db
#' @param db character string specifying Symbiota database
#' @author Franz-Sebastian Krah
#' @export
portal <- function(db = "lichen") {

  symbiota.tab <- portals()

  inc <- unlist(apply(symbiota.tab, 2, grep, pattern = db))
  if(length(inc)==0)
    inc <- apply(symbiota.tab, 2, match, x = db)

  inc <- unique(inc)
  if(any(is.na(inc)))
    inc <- na.omit(inc)
  portal <- symbiota.tab[inc, ]
  portal <- portal[!duplicated(portal$collection_url),]
  portal

}

