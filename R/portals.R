#' Portals available through the Symbiota
#' @details Symbiota is an open source content management system for curating specimen- and observation-based biodiversity data
#'
#' @export
portals <- function() {
  tab <- (
    '"Portal Name";"Database";"url";"collection_url"
    "Consortium of North American Lichen Herbaria";"symblichens";"http://lichenportal.org/portal/index.php";"http://lichenportal.org/portal/collections/index.php"
    "Arctic Lichen Flora";"symblichens";"http://lichenportal.org/arctic/index.php";"http://lichenportal.org/arctic/collections/index.php"
    "Consortium of North American Bryophyte Herbaria";"symbbryophytes";"http://bryophyteportal.org/portal/";"http://bryophyteportal.org/portal/collections/index.php"
    "Frullania Collaborative Research Network";"symbbryophytes";"http://bryophyteportal.org/frullania/";"http://bryophyteportal.org/frullania/collections/harvestparams.php?taxa=Frullania&reset=1"
    "Macroalgal Consortium Herbarium Portal";"symbalgae";"http://macroalgae.org/portal/index.php";"http://macroalgae.org/portal/collections/index.php"
    "MyCoPortal";"symbmycology";"http://mycoportal.org/portal/index.php";"http://mycoportal.org/portal/collections/index.php"
    "Smithsonian Tropical Research Institute Portal (STRI)";"symbstri";"http://stricollections.org/portal/";
    "Aquatic Invasives";"symbaquatic";"http://greatlakesinvasives.org/portal/index.php";
    "Consortium of Midwest Herbaria";"symbseinet";"http://midwestherbaria.org/portal/index.php";"http://midwestherbaria.org/portal/collections/index.php"
    "SEINet";"symbseinet";"http://swbiodiversity.org/seinet/index.php";"http://swbiodiversity.org/seinet/collections/index.php"
    "Intermountain Region Herbaria Network (IRHN)";"symbseinet";"http://intermountainbiota.org/portal/index.php";"http://intermountainbiota.org/portal/collections/index.php?catid=1"
    "SouthEast Regional Network of Expertise and Collections (SERNEC)";"symbseinet";"http://sernecportal.org/portal/";"http://sernecportal.org/portal/collections/index.php"
    "North American Network of Small Herbaria";"symbseinet";"http://nansh.org/portal/index.php";"http://nansh.org/portal/collections/index.php"
    "Northern Great Plains Herbaria";"symbseinet";"http://ngpherbaria.org/portal/index.php";"http://ngpherbaria.org/portal/collections/index.php"
    "Consortium of Northeastern Herbaria (CNH)";"symbcnh";"http://portal.neherbaria.org/portal/";"http://portal.neherbaria.org/portal/collections/index.php"
    "Madrean Archipelago Biodiversity Assessment (MABA) - Flora";"symbseinet";"http://madrean.org/symbflora/projects/index.php?proj=74";"http://madrean.org/symbflora/collections/index.php"
    "Madrean Archipelago Biodiversity Assessment (MABA) - Fauna";"symbfauna";"http://madrean.org/symbfauna/projects/index.php";"http://madrean.org/symbfauna/collections/index.php"
    "Herbario Virtual Austral Americano";"symbhvaa";"http://herbariovaa.org/";
    "CoTRAM – Cooperative Taxonomic Resource for Amer. Myrtaceae";"symbcotram";"http://cotram.org/";
    "InvertEBase Data Portal";"symbinvertebase";"http://invertebase.org/portal/index.php";
    "Symbiota Collections of Arthropods Network (SCAN)";"symbscan";"http://symbiota4.acis.ufl.edu/scan/portal/index.php";"http://scan-bugs.org/portal/collections/index.php"
    "Lepidoptera of North America Network (LepNet)";"symbscan";"http://symbiota4.acis.ufl.edu/scan/lepnet/portal/index.php";
    "Neotropical Entomology";"symbneotropentomology";"http://symbiota.org/neotrop/entomology/index.php";
    "Neotropical Flora";"symbneotropplants";"http://symbiota.org/neotrop/plantae/index.php";
    "Monarch (California Academy of Sciences)";"internal network";"http://monarch.calacademy.org/";
    "The Lundell Plant Diversity Portal";"?";"http://prc-symbiota.tacc.utexas.edu/index.php";
    "Virtual Flora of Wisconsin";"?";"http://symbiota.botany.wisc.edu";
    "Red de Herbarios del Noroeste de México";"symbseinet";"http://herbanwmex.net/portal/index.php";
    "University of Colorado Herbarium";"?";"https://botanydb.colorado.edu/";
    "The Open Herbarium";"symbhereb";"http://openherbarium.org/index.php";
    "Consortium of Pacific Herbaria";"[own private database]";"http://www.pacificherbaria.org/";
    "Oregon Flora";"Oregon State";"http://symbiota.oregonflora.org/portal/index.php";
    "Minnesota Biodiversity Atlas";"";"http://bellatlas.umn.edu/";
    "Documenting Ethnobiology in Mexico and Central America";"";"http://demca.sites.gettysburg.edu/";
    "OpenZooMuseum";"symbzoo";"http://openherbarium.org/zoo/portal/index.php";
    "Mid-Atlantic Herbaria Consortium";"";"http://midatlanticherbaria.org/portal/";
    "Channel Islands Biodiversity Information System";"CAL-IBIS";"http://www.cal-ibis.org/";
    "Consortium of Small Vertebrate Collections (CSVColl)";"?";"http://csvcoll.org/portal/index.php";
    "The University of New Hampshire Collection of Insects and Other Arthropods";"UNH server";"https://unhcollection.unh.edu/database/index.php"'
  )
  tab <- read.csv(text = tab, sep = ";")
  tab[] <- apply(tab, 2, as.character)

  return(tab)
}
