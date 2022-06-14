#### CHEP Grant: Alternative Vaccine Schedules
#### PIs: Anne Butler, Jason Newland, Sena Sayood
#### John Sahrmann
#### 20200302


work <- 'p:/jsahrmann-work/'
setwd('p:/butler-chep/')
.libPaths('c:/Users/jsahrmann/Documents/R/win-library/3.6')


library('dendextend')
library('dplyr')
library('gplots')
library('haven')


dat0 <- read_sas('daily_presence_absence.sas7bdat')
# Clear attributes attached to the original SAS data set.
attr(dat0, 'label') <- NULL
for (var in names(dat0)) {
    attr(dat0[[var]], 'label') <- NULL
}

set.seed(65326)

dat1 <- dat0[sample(1:nrow(dat0), size = 1000, replace = FALSE), ]

## dat2 <- dat1 %>%
##     select(
##         starts_with(
##             c(
##                 'dtap')))
                ## 'dtap', 'rv_', 'hib_', 'pcv_1', 'pcv_2', 'ipv_1',
                ## 'ipv_2')),

dat2 <- dat1 %>%
    select(
        num_range('dtap_1_', 30:100))


d <- dist(dat2, method = 'euclidean')
hc <- hclust(d, method = 'ward.D')

dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 2)

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
    trace = 'row', tracecol = 'grey80', hline = NA, vline = NA, density.info = 'none')



dat2 <- dat1 %>%
    select(
        num_range('dtap_1_', 30:180),
        num_range('dtap_2_', 30:180))
gplots::heatmap.2(
    x = as.matrix(dat2), Rowv = TRUE, Colv = FALSE,
    distfun = function(x) dist(x, method = 'euclidean'),
    hclustfun = function(x) hclust(x, method = 'centroid'),
    dendrogram = 'row', symm = TRUE, col = c('#4640ff', '#ffa340'),
    trace = 'none', tracecol = 'grey80', hline = NA, vline = NA,
    density.info = 'none')

# ward.D, mcquitty


dat2 <- dat1 %>%
    select(
        num_range('dtap_1_', 30:365),
        num_range('dtap_2_', 30:365),
        num_range('rv_1_', 30:365),
        num_range('rv_2_', 30:365),
        num_range('hib_1_', 30:365),
        num_range('hib_2_', 30:365),
        num_range('pcv_1_', 30:365),
        num_range('pcv_2_', 30:365),
        num_range('ipv_1_', 30:365))

gplots::heatmap.2(
    x = as.matrix(dat2), Rowv = TRUE, Colv = FALSE,
    distfun = function(x) dist(x, method = 'euclidean'),
    hclustfun = function(x) hclust(x, method = 'ward.D'),
    dendrogram = 'row', symm = TRUE, col = c('#4640ff', '#ffa340'),
    trace = 'none', tracecol = 'grey80', hline = NA, vline = NA,
    density.info = 'none')

png(
    paste0(work, 'butler-chep/out/', 'test.png'),
    width = 4800, height = 3200, pointsize = 8, res = 600)
gplots::heatmap.2(
    x = as.matrix(dat2), Rowv = TRUE, Colv = FALSE,
    distfun = function(x) dist(x, method = 'euclidean'),
    hclustfun = function(x) hclust(x, method = 'ward.D'),
    dendrogram = 'row', symm = TRUE, col = c('#4640ff', '#ffa340'),
    trace = 'none', tracecol = 'grey80', hline = NA, vline = NA,
    density.info = 'none')
dev.off()

library('d3heatmap')

d3heatmap::d3heatmap(
    x = as.matrix(dat2), Rowv = TRUE, Colv = FALSE,
    distfun = function(x) dist(x, method = 'euclidean'),
    hclustfun = function(x) hclust(x, method = 'ward.D'),
    dendrogram = 'row', symm = TRUE, revC = FALSE,
    colors = c('#4640ff', '#ffa340'),
    na.rm = FALSE, show_grid = FALSE)
