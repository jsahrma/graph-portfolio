## Header ------------------------------------------------------------
##
## Plot hazard ratios for the sub-analysis looking at the effect of
## age at spay/neuter on risk of subsequent overweight/obese status.
##
## This work constitutes a piece of the data analysis consulting work
## I've done for the Alliance for Contraception in Cats and Dogs on a
## study looking at the association between spay/neuter status and
## subsequent weight gain in dogs. (Code for this project can be found
## at <https://github.com/jsahrma/accd-oo>.)
##
## John Sahrmann
## 20220710


## Setup -------------------------------------------------------------

library(data.table)
library(ggplot2)
library(paletteer)


## Constant definitions ----------------------------------------------

# Define color scheme for plot.
color5 <- rev(paletteer::paletteer_c("viridis::viridis", 5))


## Input data --------------------------------------------------------

load("../data/oo-results.Rdata")
ds <- data.table::copy(age_among_sn_results)


## Data processing ---------------------------------------------------

ds[,
  `:=`(
    comparator_age = data.table::fcase(
      size == "Toy and Small", comparator_age - 0.12,
      size == "Medium", comparator_age - 0.06,
      size == "Standard", comparator_age,
      size == "Large", comparator_age + 0.06,
      size == "Giant", comparator_age + 0.12
    ),
    reference_age = factor(
      reference_age,
      levels = unique(reference_age),
      labels = paste(
        rep(
          "Gonadectomized at",
          times = length(unique(reference_age))),
        unique(reference_age),
        c("Years", "Year",
          rep("Years", times = length(unique(reference_age)) - 2))
      )
    )
  )
]


## Plotting ----------------------------------------------------------

png(
  "../output/oo-age-effect-among-SN-1-year.png",
  width = 800, height = 700
)
ggplot(
    ds[reference_age == "Gonadectomized at 1 Year"],
    aes(
      x = comparator_age, y = hr, ymin = lo, ymax = hi, colour = size)
  ) +
  geom_pointrange(size = 0.85) +
  geom_line(size = 0.85) +
  scale_y_log10() +
  labs(title = "Compared to Dogs Spayed/Neutered at 1 Year") +
  xlab("\nAge at Spay/Neuter (Years)") +
  ylab("Hazard Ratio for Age\n") +
  scale_colour_manual("Breed Size", values = color5) +
  facet_wrap(vars(sex)) +
  theme_bw(base_size = 20) +
  theme(
    legend.position = "bottom"
  )
dev.off()
