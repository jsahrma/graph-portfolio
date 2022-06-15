## Header ------------------------------------------------------------
##
## CHEP Grant: Alternative Vaccine Schedules
##
## Produce and visualize clusters based on vaccination history in the
## first year of life.
##
## This work was done for the Center for Administrative Data Research
## (CADR) at the Washington University in St. Louis School of Medicine
## in March 2020. Results for this project have not been published as
## of the time of writing.
##
## John Sahrmann
## 20220601


## Setup -------------------------------------------------------------

library(dendextend)
library(dplyr)
library(gplots)
library(readr)


## Input data ---------------------------------------------------------

# Read the data set of daily presence/absence indicators for each
# vaccine type.
pa <- readr::read_csv(
  "../data/daily_presence_absence.csv.gz",
  col_types = rep("i", times = 3286))

# Take a random sample of 1,000 patients.
set.seed(719473)
pa1000 <- pa[sample(1:nrow(pa), 1000), ]


## Data management ---------------------------------------------------

# Select just the indicators associated with vaccines usually
# administered in the first year of life. This avoids the complication
# posed by different formulations (e.g., the two-dose RV1 vaccine
# versus the three-dose RV5 vaccine).
pa1000_first_year <- pa1000 %>%
  dplyr::select(
    dplyr::num_range('dtap_1_', 30:365),
    dplyr::num_range('dtap_2_', 30:365),
    dplyr::num_range('rv_1_', 30:365),
    dplyr::num_range('rv_2_', 30:365),
    dplyr::num_range('hib_1_', 30:365),
    dplyr::num_range('hib_2_', 30:365),
    dplyr::num_range('pcv_1_', 30:365),
    dplyr::num_range('pcv_2_', 30:365),
    dplyr::num_range('ipv_1_', 30:365)
  )


## Clustering --------------------------------------------------------

png(
  "../output/clusters.png", width = 2000, height = 1900, res = 150)
gplots::heatmap.2(
    x = as.matrix(pa1000_first_year), Rowv = TRUE, Colv = FALSE,
    distfun = function(x) dist(x, method = 'euclidean'),
    hclustfun = function(x) hclust(x, method = 'ward.D'),
    dendrogram = 'row', symm = TRUE, col = c('#4640ff', '#ffa340'),
    trace = 'none', hline = NA, vline = NA, key = FALSE
)
dev.off()
