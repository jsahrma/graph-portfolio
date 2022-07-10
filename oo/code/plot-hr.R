## Header ------------------------------------------------------------
##
## Plot hazard ratios for the sub-analysis looking at the effect of
## age at spay/neuter on risk of subsequent overweight/obese status.
##
## This work constitutes a piece of the data analysis consulting work
## I've done for the Alliance for Contraception in Cats and Dogs on a
## study looking at the association between spay/neuter status and
## subsequent weight gain in dogs. (Code for this project can be found
## at <https://github.com/jsahrma/accd-oo>.) The model upon which the
## analyses are based contains several interactions terms, which make
## straightforward interpretation of coefficients impossible. Rather,
## effects are contingent on a combination of age, sex, and breed
## size. To ease interpretation, I created effect plots that display
## hazard ratios for a wide array of characteristics.
##
## The graph below was constructed so that within sex and breed size
## class, a dog at one year of age is used as the reference. Points
## above 1.0 on the y-axis correspond to ages at spay/neuter
## associated with a higher risk of lifetime overweight/obese status
## compared to dogs with the same characteristics spayed/neutered at
## one year of age; the converse is true for points below 1.0 on the
## y-axis. For example, female toy/small dogs neutered at two years of
## age face an approximately 50% higher risk of subsequently being
## recorded as overweight/obese compared to female toy/small dogs
## neutered at one year of age. The overall trend is that spay/neuter
## at later ages is associated with higher risk than younger ages
## (with the noticeable exception of large breed dogs). This pattern
## is more pronounced in male dogs, but it's less clear that there's a
## trend with breed size.
##
## The decision whether and when to spay/neuter a dog requires
## balancing many different factors; these results provide additional
## information on just one aspect of that decision.
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
