# Install these packages once if necessary
install.packages(c("tidyverse", "sf", "rnaturalearth",
                   "rnaturalearthdata", "leaflet", "viridis"))

library(tidyverse)
library(sf)
library(rnaturalearth)
library(leaflet)
library(viridis)

# Download the same indicator used by Our World in Data; I am confused on how to get it to ve the csv file name that I thought/think is in the working directory.
malaria <- read_csv(
  "https://ourworldindata.org/grapher/incidence-of-malaria.csv?v=1&csvType=full&useColumnShortNames=false"
) 

# Inspect the data
names(malaria)
head(malaria)

# Get world country boundaries
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Inspect the data
names(malaria)
names(world)
head(malaria)

glimpse(malaria)
glimpse(world)