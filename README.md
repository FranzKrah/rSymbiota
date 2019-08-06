---
output:
  pdf_document: default
  html_document: default
---
# The Symbiota

The Symbiota is an open source content management system for curating specimen- and observation-based biodiversity data. 
Very detailled information can be found under: http://symbiota.org/docs/.

The rSymbiota R package is an interface to the content stored within the Symbiota portals. It allows to download records from the databases readily in R for further analysis. It further provides some basic plotting functions. Below I will show the basic usability and some further possibilites of using the data.


## Install rSymbiota
```{r setup, include=TRUE, eval=FALSE}

## Either via CRAN
install.packages("rSymbiota")

## Or via GitHub for the latest version
install.packages("devtools")
devtools::install_github("FranzKrah/rSymbiota")

```

## Install Docker

To use rMyCoPortal using Docker, you need to install Docker first.
Docker performs  virtualization, also known as "containerization". rSymbiota internally uses the R package RSelenium to create a Selenium Server from which the Symbiota portal websites is addressed. 
Docker needs to run before using the rSymbiota.

* If you work on Mac OS X, this is done with a DMG file that can be downloaded from [here](https://docs.docker.com/docker-for-mac/install/#install-and-run-docker-for-mac).
* If you work on Windows, you can download the executable file from [here](https://docs.docker.com/docker-for-windows/install/)
* If you work on Linux, check [this](https://docs.docker.com/engine/installation/).

Once Docker has been installed you need to start Docker and let it run in the background. You can now use rSymbiota without further notice of Docker.

## Load the package 
```{r example1, include=TRUE, eval=TRUE, echo=TRUE}
## Load library
library("rSymbiota")
```

## The portals
Symbiota currently hosts 43 portals on a diverse range of taxonomic groups and locations. To get an overview of available portals, see either function portals() or http://symbiota.org/docs/symbiota-introduction/active-symbiota-projects/.

See this vignette for a list of portals and code to interact with them. 
[Vignette: Portals](http://htmlpreview.github.io/?https://github.com/FranzKrah/rSymbiota/blob/master/vignettes/portals.html)



# Basics
## Download records for *Amanita muscaria*, the fly agaric
Amanita is a fungus and thus we will select the MyCoPortal:

```{r example1, include=TRUE, eval=TRUE, echo=TRUE}

## Download records
am.rec <- symbiota(taxon = "Amanita muscaria", db = "MyCoPortal")
am.rec
## The retrieved S4 class object stores a distribution table with 10224 records (Date: 2019-07-04).

## And this will give the actual records table downloaded from MyCoPortal:
recordsTable(am.rec)
head(recordsTable(am.rec))
```

## Visualization
We can now use several plotting methods to visualize the data.

```{r plots, include=TRUE, eval=TRUE, echo=TRUE}

x <- am.rec

## plot_distmap can be used to plot interactive and static distribution maps
plot_distmap(x = x, mapdatabase = "world", interactive = TRUE) # the default is interactive
plot_distmap(x = x, mapdatabase = "world", interactive = FALSE) # the default is interactive

## plot_recordstreemap can be used to visualize relative importance of aspects of the data
plot_recordstreemap(x = x, groupvar = "country", log = FALSE) # e.g., the country distribution

## plot_datamap can be used to get a quick overview of which countries are most records rich
plot_datamap(x = x, mapdatabase = "world", index = "rec")
# surprisingly a lot of records are there for Australia 

## the same but cropped to Europe
plot_datamap(x = x, mapdatabase = "world", index = "rec",
             area = list(min_long = -10, max_long = 30, min_lat = 30, max_lat = 70))


```

## Application
We could now use the data to look at the range of suitable climatic conditions for *A. muscaria*. Let's use mean annual temperature and mean annual precipitation for now. 

```{r clim, include=TRUE, eval=TRUE, echo=TRUE}
library(sf)
library(raster)
rec <- am.rec@records
rec <- rec[!(is.na(rec$lat) | is.na(rec$lon)), ]

my.sf.point <- st_as_sf(x = rec, 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84")

## crop to USA
area = list(min_long = -130, max_long = -60, min_lat = 25, max_lat = 52)
my.sf.point <- st_crop(my.sf.point, xmin = area$min_long, ymin = area$min_lat, xmax = area$max_long, ymax = area$max_lat)
my.sf.point <- SpatialPointsDataFrame(coords = st_coordinates(my.sf.point), data = as.data.frame(my.sf.point))

## Retrieve WorldClim data
clim <- raster::getData(name = "worldclim", res = "2.5", var = "bio")
clim <- crop(clim, extent(-130, -60, 25, 52))
clim <- stack(clim)

climdat <- extract(clim, my.sf.point)
climdat[,1] <- climdat[,1]/10
dat <- data.frame(as.data.frame(my.sf.point), climdat)

library(ggplot2)
p.mat <- ggplot(dat, aes(x = bio1)) +
  geom_histogram() +
  labs(x ="Mean annual temperature", y = "Count") +
  theme_bw() +
  geom_vline(aes(xintercept = mean(bio1, na.rm = TRUE)), col='red',size=2)

p.map <- ggplot(dat, aes(x = bio12)) +
  geom_histogram() +
  labs(x ="Mean annual precipitation sums", y = "Count") +
  theme_bw() +
  geom_vline(aes(xintercept = median(bio12, na.rm = TRUE)), col='red',size=2)


library(cowplot )
plot_grid(p.mat, p.map, ncol = 2)

```

## Caveats
We encountered that the table output of some portals truncates the coordinates text. We are in contact with the Symbiota portal maintainers to fix this issue. We, however, think this affects only small parts of the data avaiable via the portals. If only rough coordinates are needed we recommend filling the gaps with Gazeter data.


## Meta

Please note that this project is released with a Contributor [Code of Conduct](https://github.com/FranzKrah/rSymbiota/blob/master/CONDUCT.md). By participating in this project you agree to abide by its terms.

Please [report](https://github.com/FranzKrah/rSymbiota/issues) any issues or bugs.
 
