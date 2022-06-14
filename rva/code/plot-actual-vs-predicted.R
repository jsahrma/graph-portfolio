# Header -------------------------------------------------------------
#
# Reproductive viability analysis (RVA) project
#
# Make a plot comparing actual pair success versus predicted success
# based on an early set of models.
#
# This work originally done at the Reproductive Management Center
# (RMC) at the Saint Louis Zoo in summer 2016. Extension of this work
# was later published as Bauman et al. (2019):
# <https://doi.org/10.1002/zoo.21477>.
#
# John Sahrmann
# 20220511


# Setup --------------------------------------------------------------

library(paletteer)
library(readr)


# Constants ----------------------------------------------------------

alpha <- 0.5
pal <- paletteer::paletteer_d("ggsci::default_jama")
actual_col <- "#000000"
glm_col <- adjustcolor(pal[[1]], alpha.f = alpha)
glmer_col <- adjustcolor(pal[[2]], alpha.f = alpha)
lasso_col <- adjustcolor(pal[[3]], alpha.f = alpha)
glmmlasso_col <- adjustcolor(pal[[4]], alpha.f = alpha)
rand_forest_col <- adjustcolor(pal[[5]], alpha.f = alpha)
actual_pch <- 1
glm_pch <- 15
glmer_pch <- 16
lasso_pch <- 17
glmmlasso_pch <- 18
rand_forest_pch <- 8


# Input data ---------------------------------------------------------

# Read the data set of pair-specific outcomes and predictions.
dat <- readr::read_csv(
  "../output/predictions.csv", col_types = "ciddddd")


# Data processing ----------------------------------------------------

# To keep the x-axis simple, we'll plot by index rather than by pair
# ID.
dat$x <- 1:nrow(dat)


# Plotting -----------------------------------------------------------

cairo_pdf(
  "../output/fig/actual-vs-predicted.pdf", width = 9, height = 7)
# Set up the plot. We'll use dotted light gray lines as a visual
# aid. To ensure these are in the background, we won't plot anything
# until we add these lines.
plot(
  success ~ x, data = dat, type = "n",
  main = "Predictions for 2015 Breeding Pairs",
  xlab = "Pair", ylab = "Predicted Probability of Success"
)
# Add said vertical lines.
abline(v = dat$x, lty = "dotted", col = "#eeeeee")
# Now add each set of points.
points(
  success ~ x, data = dat, pch = actual_pch, col = actual_col
)
points(
  glm_modavg_pred ~ x, data = dat, pch = glm_pch, col = glm_col
)
points(
  glmer_modavg_pred ~ x, data = dat, pch = glmer_pch, col = glmer_col
)
points(
  lasso_pred ~ x, data = dat, pch = lasso_pch, col = lasso_col
)
points(
  glmmlasso_pred ~ x, data = dat,
  pch = glmmlasso_pch, col = glmmlasso_col
)
points(
  rand_forest_pred ~ x, data = dat,
  pch = rand_forest_pch, col = rand_forest_col
)
legend(
  "topleft",
  legend = c(
    "Actual", "Model-averaged GLM", "Model-averaged GLMM",
    "Lasso GLM", "Lasso GLMM", "Random Forest"),
  pch = c(
    actual_pch, glm_pch, glmer_pch, lasso_pch, glmmlasso_pch,
    rand_forest_pch),
  col = c(
    actual_col, glm_col, glmer_col, lasso_col, glmmlasso_col,
    rand_forest_col),
  bty = "n", inset = 0.03
)
dev.off()
