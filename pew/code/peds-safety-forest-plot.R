## Header ------------------------------------------------------------
##
## Produce a forest plot for the Pew pediatrics safety analysis.
##
## Reference:
##
## Butler AM, Brown DS, Durkin MJ, et al. Association of Inappropriate
## Outpatient Pediatric Antibiotic Prescriptions With Adverse Drug
## Events and Health Care Expenditures. JAMA Netw Open.
## 2022;5(5):e2214153. doi:10.1001/jamanetworkopen.2022.14153
##
## John Sahrmann
## 20220613


## Setup -------------------------------------------------------------

library(dplyr)
library(forestplot)
library(readxl)


## Function definitions ----------------------------------------------


elog <- function(x) {
  return(exp(log(x)))
}

# Round in the way Excel does for consistency with other results.
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  return(z*posneg)
}

print_est <- function(est, ll, ul) {
  paste0(
    format(round2(est, n = 2), nsmall = 2),
    " (",
    format(round2(ll, n = 2), nsmall = 2),
    ", ",
    format(round2(ul, n = 2), nsmall = 2),
    ")")
}


## Constants ---------------------------------------------------------

toplines <- 2


## Input data --------------------------------------------------------

evt <- readxl::read_excel("../data/safety_event_counts_rates.xlsx")

hr <- bind_rows(
  readxl::read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "supp otit adj", na = "."),
  read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "pharyn adj", na = "."),
  read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "sinusit adj", na = "."),
  read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "flu adj", na = "."),
  read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "viral uri adj", na = "."),
  read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "bronchiol adj", na = "."),
  read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "bronchit adj", na = "."),
  read_excel(
    "../data/safety_coxph.xlsx",
    sheet = "nonsupp otit adj", na = ".")
)


c1 <- c(
  rep("", times = toplines),
  "",
  "",
  "Nausea/vomiting/abdominal pain",
  "  Suppurative OM",
  "  Pharyngitis",
  "  Sinusitis",
  "  Influenza",
  "  Viral URI",
  "  Bronchiolitis",
  "  Bronchitis",
  "  Non-suppurative OM",
  "",
  "Non-C. difficile diarrhea",
  "  Suppurative OM",
  "  Pharyngitis",
  "  Sinusitis",
  "  Influenza",
  "  Viral URI",
  "  Bronchiolitis",
  "  Bronchitis",
  "  Non-suppurative OM",
  "",
  "C. difficile infection",
  "  Suppurative OM",
  "  Pharyngitis",
  "  Sinusitis",
  "  Influenza",
  "  Viral URI",
  "  Bronchiolitis",
  "  Bronchitis",
  "  Non-suppurative OM",
  "",
  "Anaphylaxis/angioedema/laryngeal edema",
  "  Suppurative OM",
  "  Pharyngitis",
  "  Sinusitis",
  "  Influenza",
  "  Viral URI",
  "  Bronchiolitis",
  "  Bronchitis",
  "  Non-suppurative OM",
  "",
  "Skin rash/urticaria",
  "  Suppurative OM",
  "  Pharyngitis",
  "  Sinusitis",
  "  Influenza",
  "  Viral URI",
  "  Bronchiolitis",
  "  Bronchitis",
  "  Non-suppurative OM",
  "",
  "Unspecified allergy",
  "  Suppurative OM",
  "  Pharyngitis",
  "  Sinusitis",
  "  Influenza",
  "  Viral URI",
  "  Bronchiolitis",
  "  Bronchitis",
  "  Non-suppurative OM"
)
c2 <- c(
  rep("", times = toplines),
  "Appropriate", "Agent",
  "",
  evt$events_approp[1:8], "",
  "",
  evt$events_approp[9:16], "",
  "",
  evt$events_approp[17:24], "",
  "",
  evt$events_approp[25:32], "",
  "",
  evt$events_approp[33:40], "",
  "",
  evt$events_approp[41:48]
)
c3 <- c(
  rep("", times = toplines),
  "Inappropriate", "Agent",
  "",
  evt$events_inappr[1:8], "",
  "",
  evt$events_inappr[9:16], "",
  "",
  evt$events_inappr[17:24], "",
  "",
  evt$events_inappr[25:32], "",
  "",
  evt$events_inappr[33:40], "",
  "",
  evt$events_inappr[41:48]
)

derm <- filter(hr, ade == "derm")
gast <- filter(hr, ade == "gastro")
nonc <- filter(hr, ade == "non_cdiff")
cdff <- filter(hr, ade == "cdiff")
hgen <- filter(hr, ade == "hypersens_gen")
hypr <- filter(hr, ade == "hypersens")
c4 <- c(
  rep("", times = toplines),
  "Weighted HR", "(95% CI)",
  "",
  print_est(gast$HR[1], gast$LCL[1], gast$UCL[1]),
  print_est(gast$HR[2], gast$LCL[2], gast$UCL[2]),
  print_est(gast$HR[3], gast$LCL[3], gast$UCL[3]),
  print_est(gast$HR[4], gast$LCL[4], gast$UCL[4]),
  print_est(gast$HR[5], gast$LCL[5], gast$UCL[5]),
  print_est(gast$HR[6], gast$LCL[6], gast$UCL[6]),
  print_est(gast$HR[7], gast$LCL[7], gast$UCL[7]),
  print_est(gast$HR[8], gast$LCL[8], gast$UCL[8]),
  "", "",
  print_est(nonc$HR[1], nonc$LCL[1], nonc$UCL[1]),
  print_est(nonc$HR[2], nonc$LCL[2], nonc$UCL[2]),
  print_est(nonc$HR[3], nonc$LCL[3], nonc$UCL[3]),
  print_est(nonc$HR[4], nonc$LCL[4], nonc$UCL[4]),
  print_est(nonc$HR[5], nonc$LCL[5], nonc$UCL[5]),
  print_est(nonc$HR[6], nonc$LCL[6], nonc$UCL[6]),
  print_est(nonc$HR[7], nonc$LCL[7], nonc$UCL[7]),
  print_est(nonc$HR[8], nonc$LCL[8], nonc$UCL[8]),
  "", "",
  print_est(cdff$HR[1], cdff$LCL[1], cdff$UCL[1]),
  print_est(cdff$HR[2], cdff$LCL[2], cdff$UCL[2]),
  print_est(cdff$HR[3], cdff$LCL[3], cdff$UCL[3]),
  "NE",
  "NE",
  "NE",
  "NE",
  "NE",
  "", "",
  print_est(hgen$HR[1], hgen$LCL[1], hgen$UCL[1]),
  print_est(hgen$HR[2], hgen$LCL[2], hgen$UCL[2]),
  print_est(hgen$HR[3], hgen$LCL[3], hgen$UCL[3]),
  "NE",
  print_est(hgen$HR[5], hgen$LCL[5], hgen$UCL[5]),
  "NE",
  "NE",
  print_est(hgen$HR[8], hgen$LCL[8], hgen$UCL[8]),
  "", "",
  print_est(derm$HR[1], derm$LCL[1], derm$UCL[1]),
  print_est(derm$HR[2], derm$LCL[2], derm$UCL[2]),
  print_est(derm$HR[3], derm$LCL[3], derm$UCL[3]),
  print_est(derm$HR[4], derm$LCL[4], derm$UCL[4]),
  print_est(derm$HR[5], derm$LCL[5], derm$UCL[5]),
  print_est(derm$HR[6], derm$LCL[6], derm$UCL[6]),
  print_est(derm$HR[7], derm$LCL[7], derm$UCL[7]),
  print_est(derm$HR[8], derm$LCL[8], derm$UCL[8]),
  "", "",
  print_est(hypr$HR[1], hypr$LCL[1], hypr$UCL[1]),
  print_est(hypr$HR[2], hypr$LCL[2], hypr$UCL[2]),
  print_est(hypr$HR[3], hypr$LCL[3], hypr$UCL[3]),
  "NE",
  print_est(hypr$HR[5], hypr$LCL[5], hypr$UCL[5]),
  "NE",
  print_est(hypr$HR[7], hypr$LCL[7], hypr$UCL[7]),
  print_est(hypr$HR[8], hypr$LCL[8], hypr$UCL[8])
)

est <- c(
  rep(NA, times = toplines),
  NA, NA,
  NA, elog(gast$HR[1]), elog(gast$HR[2]), elog(gast$HR[3]), elog(gast$HR[4]),
  elog(gast$HR[5]), elog(gast$HR[6]), elog(gast$HR[7]), elog(gast$HR[8]), NA,
  NA, elog(nonc$HR[1]), elog(nonc$HR[2]), elog(nonc$HR[3]), elog(nonc$HR[4]),
  elog(nonc$HR[5]), elog(nonc$HR[6]), elog(nonc$HR[7]), elog(nonc$HR[8]), NA,
  NA, elog(cdff$HR[1]), elog(cdff$HR[2]), elog(cdff$HR[3]), elog(cdff$HR[4]),
  elog(cdff$HR[5]), elog(cdff$HR[6]), elog(cdff$HR[7]), elog(cdff$HR[8]), NA,
  NA, elog(hgen$HR[1]), elog(hgen$HR[2]), elog(hgen$HR[3]), elog(hgen$HR[4]),
  elog(hgen$HR[5]), elog(hgen$HR[6]), elog(hgen$HR[7]), elog(hgen$HR[8]), NA,
  NA, elog(derm$HR[1]), elog(derm$HR[2]), elog(derm$HR[3]), elog(derm$HR[4]),
  elog(derm$HR[5]), elog(derm$HR[6]), elog(derm$HR[7]), elog(derm$HR[8]), NA,
  NA, elog(hypr$HR[1]), elog(hypr$HR[2]), elog(hypr$HR[3]), elog(hypr$HR[4]),
  elog(hypr$HR[5]), elog(hypr$HR[6]), elog(hypr$HR[7]), elog(hypr$HR[8])
)
lcl <- c(
  rep(NA, times = toplines),
  NA, NA,
  NA, elog(gast$LCL[1]), elog(gast$LCL[2]), elog(gast$LCL[3]), elog(gast$LCL[4]),
  elog(gast$LCL[5]), elog(gast$LCL[6]), elog(gast$LCL[7]), elog(gast$LCL[8]), NA,
  NA, elog(nonc$LCL[1]), elog(nonc$LCL[2]), elog(nonc$LCL[3]), elog(nonc$LCL[4]),
  elog(nonc$LCL[5]), elog(nonc$LCL[6]), elog(nonc$LCL[7]), elog(nonc$LCL[8]), NA,
  NA, elog(cdff$LCL[1]), elog(cdff$LCL[2]), elog(cdff$LCL[3]), elog(cdff$LCL[4]),
  elog(cdff$LCL[5]), elog(cdff$LCL[6]), elog(cdff$LCL[7]), elog(cdff$LCL[8]), NA,
  NA, elog(hgen$LCL[1]), elog(hgen$LCL[2]), elog(hgen$LCL[3]), elog(hgen$LCL[4]),
  elog(hgen$LCL[5]), elog(hgen$LCL[6]), elog(hgen$LCL[7]), elog(hgen$LCL[8]), NA,
  NA, elog(derm$LCL[1]), elog(derm$LCL[2]), elog(derm$LCL[3]), elog(derm$LCL[4]),
  elog(derm$LCL[5]), elog(derm$LCL[6]), elog(derm$LCL[7]), elog(derm$LCL[8]), NA,
  NA, elog(hypr$LCL[1]), elog(hypr$LCL[2]), elog(hypr$LCL[3]), elog(hypr$LCL[4]),
  elog(hypr$LCL[5]), elog(hypr$LCL[6]), elog(hypr$LCL[7]), elog(hypr$LCL[8])
)
ucl <- c(
  rep(NA, times = toplines),
  NA, NA,
  NA, elog(gast$UCL[1]), elog(gast$UCL[2]), elog(gast$UCL[3]), elog(gast$UCL[4]),
  elog(gast$UCL[5]), elog(gast$UCL[6]), elog(gast$UCL[7]), elog(gast$UCL[8]), NA,
  NA, elog(nonc$UCL[1]), elog(nonc$UCL[2]), elog(nonc$UCL[3]), elog(nonc$UCL[4]),
  elog(nonc$UCL[5]), elog(nonc$UCL[6]), elog(nonc$UCL[7]), elog(nonc$UCL[8]), NA,
  NA, elog(cdff$UCL[1]), elog(cdff$UCL[2]), elog(cdff$UCL[3]), elog(cdff$UCL[4]),
  elog(cdff$UCL[5]), elog(cdff$UCL[6]), elog(cdff$UCL[7]), elog(cdff$UCL[8]), NA,
  NA, elog(hgen$UCL[1]), elog(hgen$UCL[2]), elog(hgen$UCL[3]), elog(hgen$UCL[4]),
  elog(hgen$UCL[5]), elog(hgen$UCL[6]), elog(hgen$UCL[7]), elog(hgen$UCL[8]), NA,
  NA, elog(derm$UCL[1]), elog(derm$UCL[2]), elog(derm$UCL[3]), elog(derm$UCL[4]),
  elog(derm$UCL[5]), elog(derm$UCL[6]), elog(derm$UCL[7]), elog(derm$UCL[8]), NA,
  NA, elog(hypr$UCL[1]), elog(hypr$UCL[2]), elog(hypr$UCL[3]), elog(hypr$UCL[4]),
  elog(hypr$UCL[5]), elog(hypr$UCL[6]), elog(hypr$UCL[7]), elog(hypr$UCL[8])
)


table_text <- cbind(c1, c2, c3, c4)

summaryp <- c(
  rep(TRUE, times = toplines),
  TRUE, TRUE,
  TRUE, rep(FALSE, times = 8), TRUE,
  TRUE, rep(FALSE, times = 8), TRUE,
  TRUE, rep(FALSE, times = 8), TRUE,
  TRUE, rep(FALSE, times = 8), TRUE,
  TRUE, rep(FALSE, times = 8), TRUE,
  TRUE, rep(FALSE, times = 8)
)

cairo_pdf(
  "../output/safety_forestplot.pdf", width = 14, height = 18.5)

grid::grid.rect(
  x = grid::unit(.5, "npc"), y = grid::unit(.893, "npc"),
  width = grid::unit(.98, "npc"), height = grid::unit(.045, "npc"),
  gp = grid::gpar(fill = "gray90", col = "gray90"))
grid::grid.rect(
  x = grid::unit(.5, "npc"), y = grid::unit(.744, "npc"),
  width = grid::unit(.98, "npc"), height = grid::unit(.045, "npc"),
  gp = grid::gpar(fill = "gray90", col = "gray90"))
grid::grid.rect(
  x = grid::unit(.5, "npc"), y = grid::unit(.595, "npc"),
  width = grid::unit(.98, "npc"), height = grid::unit(.045, "npc"),
  gp = grid::gpar(fill = "gray90", col = "gray90"))
grid::grid.rect(
  x = grid::unit(.5, "npc"), y = grid::unit(.446, "npc"),
  width = grid::unit(.98, "npc"), height = grid::unit(.045, "npc"),
  gp = grid::gpar(fill = "gray90", col = "gray90"))
grid::grid.rect(
  x = grid::unit(.5, "npc"), y = grid::unit(.297, "npc"),
  width = grid::unit(.98, "npc"), height = grid::unit(.045, "npc"),
  gp = grid::gpar(fill = "gray90", col = "gray90"))
grid::grid.rect(
  x = grid::unit(.5, "npc"), y = grid::unit(.148, "npc"),
  width = grid::unit(.98, "npc"), height = grid::unit(.045, "npc"),
  gp = grid::gpar(fill = "gray90", col = "gray90"))

forestplot::forestplot(
  cex = 1.2,
  labeltext = table_text, mean = est, lower = lcl, upper = ucl,
  align = c('l', 'c', 'c', 'c', 'c'),
  graph.pos = 5,
  hrzl_lines = list('5' = grid::gpar(col = 'black')),
  clip = c(0.45, 5.55),
  is.summary = summaryp,
  txt_gp = forestplot::fpTxtGp(
    label = grid::gpar(cex = 1.0),
    xlab = grid::gpar(cex = 1.1),
    ticks = grid::gpar(cex = 1.1)),
  xlog = TRUE,
  xlab = 'HR (95% CI)',
  lwd.zero = 2, boxsize = 0.3, lwd.ci = 2, lwd.xaxis = 2,
  ci.vertices.height = 0.15,
  col = forestplot::fpColors(line = 'black'),
  xticks = c(
    elog(0.5), elog(1.5), elog(2.5), elog(3.5), elog(4.5), elog(5.5)),
  new_page = FALSE
)

grid::grid.text(
  "Inappropriate agent\nnonharmful",
  x = grid::unit(.8045, "npc"), y = grid::unit(.9450, "npc"), just = "right",
  gp=grid::gpar(col="black", fontface = "bold", cex = 1.1))
grid::grid.text(
  "Inappropriate agent\nharmful",
  x = grid::unit(.8148, "npc"), y = grid::unit(.9450, "npc"), just = "left",
  gp=grid::gpar(col="black", fontface = "bold", cex = 1.1))
grid::grid.text(
  "No. of events\n(Rate per 10,000 person-days)",
  x = grid::unit(.432, "npc"), y = grid::unit(.9800, "npc"), just = "center",
  gp=grid::gpar(col="black", fontface = "bold", cex = 1.1))

dev.off()
