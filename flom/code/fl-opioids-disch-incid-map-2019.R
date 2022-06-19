## Header ------------------------------------------------------------
##
## Using Administrative Data for Clinical and Health Services Research
##
## Map county-level incidence of opioid-related discharges in Florida
## in 2019.
##
## This work is based on a sample problem written for the Washinton
## University in St. Louis School of Medicine course, Using
## Administrative Data for Clinical and Health Services Research. The
## goal is to demonstrate data set merging, incidence rate
## calculations, and basic mapping using opioid-related discharges in
## the state of Florida in 2019. The incidence data are derived from
## the Healthcare Cost and Utilization Project (HCUP) State Inpatient
## Database (SID): <https://www.hcup-us.ahrq.gov/sidoverview.jsp>.
##
## John Sahrmann
## 20220605


## Setup -------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggspatial)
library(readr)
library(sf)
library(stringr)

# Create a temporary directory to store the GIS files extracted from
# the zip archive.
temp <- tempfile()


## Input data --------------------------------------------------------

# Read the county-level incidence counts for 2019.
fl_cty_opioids_incid_counts_2019 <- readr::read_csv(
  "../data/fl_cty_opioids_admit_incid_counts_2019.csv",
  col_types = "ci"
)

# Read the county-level population estimates for 2019. Data from the
# U.S. Census Bureau:
# <https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/>.
fl_cty_pop_est_2019 <- readr::read_csv(
  "../data/cc-est2019-alldata.csv.gz",
  col_types = "ccccccciii", col_select = !SUMLEV
) %>%
  dplyr::filter(
    STATE == "12",                 # Florida
    YEAR == "12",                  # 2019
    AGEGRP == "0"                  # total population
  ) %>%
  # Pad the county code with zeros so that, e.g., "1" becomes
  # "001". Then define the full county FIPS code by concatenating the
  # state and county FIPS codes.
  dplyr::mutate(
    COUNTY = stringr::str_pad(
      COUNTY, width = 3, side = "left", pad = "0"),
    ctyfips = stringr::str_c(STATE, COUNTY),
    .before = 1
  )

# Unzip the GIS files into the temporary directory, and read the
# shapefile. Data from the U.S. Census Bureau:
# <https://www2.census.gov/geo/tiger/TIGER2019/COUSUB/>.
unzip("../data/tl_2019_12_cousub.zip", exdir = temp)
fl_cty_gis_2019 <- sf::read_sf(
  list.files(temp, ".shp$", full.names = TRUE)
) %>%
  dplyr::mutate(
    ctyfips = stringr::str_c(STATEFP, COUNTYFP),
    .before = 1
  )


## Data management ---------------------------------------------------

# Select just the variables needed for the calculations and mapping.
fl_cty_pop_est_2019 <- fl_cty_pop_est_2019 %>%
  dplyr::select(ctyfips, TOT_POP)

# Ditto for the GIS data set.
fl_cty_gis_2019 <- fl_cty_gis_2019 %>%
  dplyr::select(ctyfips, geometry)

# Merge the data sets, and compute incidence rates per 100,000
# population.
fl_cty_opioids_incid_rate_map_2019 <-
  fl_cty_opioids_incid_counts_2019 %>%
  dplyr::inner_join(fl_cty_pop_est_2019, by = "ctyfips") %>%
  dplyr::inner_join(fl_cty_gis_2019, by = "ctyfips") %>%
  dplyr::mutate(
    incid_rate_per_1e5_pop = incid / TOT_POP * 1e5,
    before = geometry
  )


## Mapping -----------------------------------------------------------

# Plot the county-level incidence rates. Rates are mapped using a
# continuous fill scale. Counties with insufficient data are plotted
# separately in gray.
png(
  "../output/fl_opioid_incidence_map_2019.png",
  width = 1000, height = 1000, res = 150)
ggplot() +
  geom_sf(
    aes(fill = incid_rate_per_1e5_pop, geometry = geometry),
    data = dplyr::filter(
      fl_cty_opioids_incid_rate_map_2019,
      !is.na(incid_rate_per_1e5_pop)
    )
  ) +
  geom_sf(
    aes(geometry = geometry), fill = "gray40",
    data = dplyr::filter(
      fl_cty_opioids_incid_rate_map_2019,
      is.na(incid_rate_per_1e5_pop)
    )
  ) +
  scale_fill_continuous(
    "Opioid-Related Admits\nPer 100K Population",
    type = "viridis"
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = c(.25, .20)) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "grid", 
    pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering)
dev.off()
