## Hi Sena,

## I wanted to check in to see if you’ve had time to look into ways of visualizing the sequences of daily vaccination histories and to update you on some things I’ve done.

## One quick note is that I realized I had made a mistake in assigning times to the generic rotavirus and Hib date variables. So I replaced the analytic file in butler-chep/ with a corrected one dated 02mar2020.

## When I introduced the idea of visualizing the daily histories, I made a comparison to genetic sequence alignment, but as I looked into it further, I realized it may not be the most helpful analogy. One advantage we have is that all the sequences start at the same point, so as a consequence we don’t really need alignment per se but rather estimates of relatedness. The bioinformatics community has lots of ways of doing this, but they rely on models of the frequency of different types of mutations, including insertions and deletions, which don’t apply in our case.

## I ended up pursuing what is probably a pretty standard linear algebra approach. I applied a distance metric to build a matrix of pairwise differences and then used a hierarchical clustering procedure to build a dendrogram with a nice heat map. I’ve attached the resulting output. It includes the entire 1% random sample of children in the “daily_presence_absence” file in butler-chep/ and looks at all of the vaccinations they would be expected to receive by the second well-child visit, extending to 365 days of age to allow for capture of delayers. This is in some ways the cleanest comparison because we don’t have to worry about the different rotavirus and Hib formulations since every child should get at least two doses of both vaccines. The heat map is colored so that orange represents the presence of a given vaccine-dose, blue represents the absence, and white represents disenrollment.

## Some broad patterns are certainly evident, although the clusters aren’t as clear-cut as I might have hoped. Certainly the cluster at the bottom that diverges at the first branching point consists of the most problematic cases. Not many are delaying DTaP 1, but many are delaying DTaP 2. It looks like over half are skipping rotavirus entirely, and only a small minority are getting the second rotavirus dose (and much delayed at that). Hib and PCV are similar in their first doses being delayed less often, but many are delaying/skipping the second doses. Polio also shows a similar pattern to DTaP 1, Hib 1, and PCV 1. Pockets of delay show up at different places throughout the rest of the tree.

## While interesting, one difficulty is that there are numerous ways to define similarity among children and then numerous ways to construct a dendrogram based on these similarity metrics. Different choices can lead to very different looking trees, although the clustering in the heat map might be more consistent. I don’t have the expertise to say what the right choices are here.


# Header -------------------------------------------------------------
#
# CHEP Grant: Alternative Vaccine Schedules
#
# Produce and visualize clusters based on vaccination history in the
# first year of life.
#
# This work was done for the Center for Administrative Data Research
# (CADR) at the Washington University in St. Louis School of Medicine
# in March 2020. Results for this project have not been published as
# of the time of writing.
#
# John Sahrmann
# 20220601


# Setup --------------------------------------------------------------

library(ade4)
library(dendextend)
library(dplyr)
library(gplots)
library(poLCA)
library(readr)


# Input data ---------------------------------------------------------

pa <- readr::read_csv(
  "../data/daily_presence_absence.csv.gz",
  col_types = rep("i", times = 3286))

pa1000 <- pa[sample(1:nrow(pa), 1000), ]


# Clustering ---------------------------------------------------------

distance_matrix <- ade4::dist.binary(pa[, -1], method = 1)

dat2 <- pa1000 %>%
  dplyr::select(
    dplyr::num_range('dtap_1_', 30:365))


d <- dist(dat2, method = 'euclidean')
hc <- hclust(d, method = 'ward.D')

dend <- as.dendrogram(hc)
dend <- dendextend::color_branches(dend, k = 2)

plot(dend)

gplots::heatmap.2(
    x = as.matrix(dat2), Rowv = TRUE, Colv = FALSE,
    distfun = function(x) dist(x, method = 'euclidean'),
    hclustfun = function(x) hclust(x, method = 'complete'),
    dendrogram = 'row', symm = TRUE, col = hcl.colors(n = 2, palette = 'Burg'),
    trace = 'row', tracecol = 'grey', hline = NA, vline = NA, density.info = 'none')

gplots::heatmap.2(
    x = as.matrix(dat2), Rowv = TRUE, Colv = FALSE,
    distfun = function(x) dist(x, method = 'euclidean'),
    hclustfun = function(x) hclust(x, method = 'complete'),
    dendrogram = 'row', symm = TRUE, col = c('#4640ff', '#ffa340'),
    hline = NA, vline = NA, density.info = 'none')

dat3 <- tidyr::replace_na(dat2, as.list(rep(2, ncol(dat2))))

dat3 <- lapply(dat2, function(x) ifelse(is.na(x), 2, x))

dat3 <- dat2 %>%
  dplyr::mutate(
    dplyr::across(.fns = function(x) as.character(ifelse(is.na(x), 2, x)))
  )

dat3 <- dat2 %>%
  dplyr::mutate(
    dplyr::across(.fns = function(x) ifelse(is.na(x), 2, x))
  ) %>%
  dplyr::mutate(
    dplyr::across(.fns = function(x) x + 1)
  )

x <- cluster::daisy(dat3, metric = "gower")


mod2 <- poLCA::poLCA(as.matrix(dat3) ~ 1, data = dat3, nclass = 2, verbose = FALSE)
table(mod2$predclass)


pa <- readr::read_csv(
  "../data/daily_presence_absence.csv.gz",
  col_types = rep("i", times = 3286)
) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), 3, .x + 1)))


dat4 <- pa %>%
  dplyr::select(
    dplyr::num_range('dtap_1_', 30:365)
    # dplyr::starts_with("dtap_1")
  ) %>%
  dplyr::mutate(
    dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), 3, .x + 1))
  )

mod24 <- poLCA::poLCA(as.matrix(dat4) ~ 1, data = dat4, nclass = 2)


pa2 <- pa[1:4000, -1]


mod2 <- poLCA::poLCA(as.matrix(pa2) ~ 1, data = pa2, nclass = 2)
