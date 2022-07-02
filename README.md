# Graph Portfolio

This repository contains a sample of the graphs I've produced throughout my career.


## Heat map / dendrogram of childhood vaccine histories

This plot was made as part of a (currently unpublished) project investigating patterns of the timing of childhood vaccinations and in particular patterns of delay (or outright skipping) of different vaccines in a sample of privately insured children. The data were deidentified and a small amount of noise added to the vaccination dates prior to plotting.

The motivation for this graph was to explore how vaccination histories could be used to cluster patients into groups. It uses daily indicator variables for presence/absence of each vaccine in the first year of life. If patients likely to delay or skip vaccines could be identified using early vaccine histories, timely reminders could be provided or outreach efforts used to address concerns of parents.

![Heat map / dendrogram of early childhood vaccination histories](/ecv/output/clusters.png)

Patient histories are shown on the horizontal axis; presence/absence of each vaccine from 30 days to one year of life are shown on the vertical axis. Blue regions represent absence of the vaccine, orange regions represent the presence of vaccine, and white regions represent missing data (i.e., insurance disenrollment). The size of the data set makes a direct visual inspection of the clusters difficult. This plot contains a random sample of 1,000 patient histories (with the data set in /ecv/data/ itself being a 1% sample of the complete analytic file). Nevertheless, we can see a cluster of patients with a significant proportion of their histories colored blue, indicating delay or skipping of recommended vaccinations.


## Mapping Opioid Admissions

This map was prepared as an example for the course "Using Administrative Data for Health Services Research" at the Washington University in St. Louis School of Medicine. The goal was to demonstrate data set merging and basic mapping using county-level opioid-related admissions for the state of Florida in 2019.

The course teaches students at the School of Medicine the basics of working with administrative healthcare data. A major part of that is learning the programming skills needed to open, manipulate, and analyze data sources. The Spring 2022 semester saw the transition from SAS to R programming, where this example was designed for a module on special topics. Updated course materials are currently being migrated to [this repository](https://github.com/jsahrma/admin-data-course).

![Incidence of opioid-related admissions in the state of Florida in 2019](/flom/output/fl_opioid_incidence_map_2019.png)

**_Note:_** This is not meant as a rigorous or authoritative analysis of this issue.


## Forest Plot of Survival Analysis Effect Estimates

The forest plot below summarizes the results of an extensive set of analyses comparing safety outcomes in a sample of privately insured children across the U.S. diagnosed with common bacterial and viral infections. (Results of the [full study](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2792723) were published in the journal *JAMA Network Open* in May 2022.) Patients were grouped into cohorts based on the type of infection diagnosed, and treatment was defined as appropriate (guideline-recommended therapy for bacterial infection cohorts and no antibiotic for viral infection cohorts) or inappropriate (non-guideline-recommended therapy for bacterial infection cohorts and any antibiotic for viral infection cohorts). Weights derived from propensity scores were used to achieve exchangeability between treatment groups within each cohort. Cox proportional hazards models were then used to compare risk of each safety outcome following initial treatment.

Much of the work in creating the plot is encapsulated by the `forestplot` function defined in a package of the same name, although there's a significant amount of work required to prepare the text and estimates for plotting. Other adornments were added by leveraging the lower-level functions provided by the *grid* package. Text annotations were added to the top of the plot to provide context to the event rate calculations shown in the middle two columns and to indicate what regions of the forest plot represent a protective effect of appropriate or inappropriate treatment. Gray backgrounds were added to help visually distinguish between the bacterial infection and viral infection cohorts.

![Forest plot of survival analysis effect estimates of pediatric antibiotics safety study](/pew/output/safety_forestplot.png)
