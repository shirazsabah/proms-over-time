# Early patient-reported outcomes following primary hip and knee replacement have improved over the past seven years: An analysis of the NHS PROMs dataset

This study has been published by the Bone & Joint Journal (doi: https://doi.org/10.1302/0301-620X.104B6.BJJ-2021-1577.R1)

# Study aim
The aim of the study was to address the research question: 

*Have early (6 months post-surgery) patient-reported outcomes from primary hip and knee arthroplasty improved over the past seven years?*

# Methodology
We analysed the NHS PROMs dataset from 2013-4 to 2019-20. We created multiple linear and logistic regression models to investigate the effect of year of surgery on post-operative joint function, quality of life, perceived success of surgery, patient satisfaction and complications.

# Study data

## Download link
Data for this study is open source and available to download from the NHS PROMs website (https://digital.nhs.uk/data-and-information/publications/statistical/patient-reported-outcome-measures-proms). Data for each financial year is available within 'CSV data packs', which are .zip archives that can be downloaded for each year of analysis. 

## PROM participation and linkage
The "Participation Linkage yyYY.csv" file provides summary (count) data indicating the level of participation in the PROMs programme. The total number of procedures is derived from the Hospital Episode Statistics Admitted Patient Care (HES APC) dataset. 

A list of relevant Office of Population Censuses and Surveys Classification of Surgical Operations and Procedures (OPCS-4) codes for eligible procedures is provided by NHS Digital within the publication: 

*NHS Digital. Patient Reported Outcome Measures (PROMs) in England - A guide to PROMS methodology. 2017.* https://digital.nhs.uk/binaries/content/assets/legacy/pdf/g/t/proms_guide_v12.pdf *(last accessed 20/04/2022)*.

## Patient-level PROM data
This is found within a number of different datasets (depending on the year). Relevant filenames include:

- "Record level... .csv"
- "... CCG ... .csv"
- "... Provider... .csv".

## Importing and assembling PROM files
R code for this can be found accompanying the following publication:

*Sabah SA, Alvand A, Beard DJ, Price AJ. Minimal important changes and differences were estimated for Oxford hip and knee scores following primary and revision arthroplasty. Journal of Clinical Epidemiology. 2022 Mar 1 [cited 2022 Jan 20];143:159–68. Available from: https://www.sciencedirect.com/science/article/pii/S0895435621004212*

# Statistical code in this repository
- `R PoT.R` = R code for the tables and figures in the BJJ study.
- `Do PoT regression models.dta` = Stata code for the multiple linear regression and multiple logistic regression models constructed for the BJJ study.
