############################################################################################################################
############################################             Tables             ###############################################
############################################################################################################################

directory <- "/Users/shirazsabah/Library/Mobile Documents/com~apple~CloudDocs/Revision TKR/NPROMS/PROMS Temporal Trends/TT"
setwd(directory)

pacman::p_load(pacman, dplyr, gtsummary, flextable, rio, ggplot2, ggthemes, tidyr)

df <- import("kneett.dta")

names(df)

# Recode age
df$age <- factor(df$age, levels=c(1,2,3,4), ordered=TRUE,
                              labels = c("<60","60-69","70-79","80+")) %>% forcats::fct_explicit_na()

# Gender
df$gender <- factor(df$gender, levels=c(2,1), ordered=TRUE,
                              labels = c("Female","Male")) %>% forcats::fct_explicit_na()

# Recode symptom period
df$preopqsymptomperiod <- factor(df$preopqsymptomperiod, levels=c(1,2,3,4), ordered=TRUE,
                 labels = c("Less than 1 year","1 - 5 years","6 - 10 years","10 years +")) %>% forcats::fct_explicit_na()


# Recode self-reported disability
df$preopqdisability <- factor(df$preopqdisability, levels=c(1,0), ordered=TRUE,
                              labels = c("Yes","No")) %>% forcats::fct_explicit_na()

# Recode comorbidities
df$comorb <- factor(df$comorb, levels=c(0,1,2,3), ordered=TRUE,
                    labels = c("0", "1", "2", "3+"))

# Complications
df <- df %>% mutate(ncomps = ifelse(complication > 3, 3, complication))
df$ncomps <- factor(df$ncomps, levels=c(0,1,2,3), ordered=TRUE, labels = c("0", "1", "2", "3+"))

# Identify records where all complications are missing
# i.e. postopqallergy, postopqbleeding, postopqwound, postopqurine, postopqfurthersurgery, postopqreadmitted
df <- df %>%
  rowwise() %>%
  mutate(N_comps_NA = sum(is.na(c(postopqallergy, postopqbleeding, postopqwound, postopqurine, postopqfurthersurgery, postopqreadmitted))))

# Check that missing doesn't add up to more than 6
# unique(df$N_comps_NA)
# Replace as missing if == 6
df <- df %>% mutate(ncompsNA =case_when(
  N_comps_NA == 6 ~ NA_character_,
  TRUE ~ as.character(ncomps)))

df$ncompsNA <- factor(df$ncompsNA, ordered=TRUE, labels = c("0", "1", "2", "3+")) %>% forcats::fct_explicit_na()

# unique(df$ncompsNA)
# levels(df$ncompsNA)

# Responders
df <- df %>% mutate(mic01 = case_when(coks>=7 ~ "Yes",
                                      coks<7 ~ "No"))

df$mic01 <- factor(df$mic01, levels=c("Yes", "No"), ordered=TRUE, labels = c("Yes", "No")) %>% forcats::fct_explicit_na()
levels(df$mic01)

# Satisfaction
df$satisf <- factor(df$satisf, levels=c(1,0), ordered=TRUE, labels = c("Yes", "No")) %>% forcats::fct_explicit_na()
levels(df$satisf)

# Success
df$success <- factor(df$success, levels=c(1,0), ordered=TRUE, labels = c("Yes", "No")) %>% forcats::fct_explicit_na()
levels(df$success)

#####

t1 = df %>% select("age", "gender", "preopqsymptomperiod", "preopqdisability", "comorb", "krpreopqscore", "krpostopqscore", "coks", "mic01", "preopqeq5dindex", "postopqeq5dindex", "ceq5d", "satisf", "success", "ncompsNA", "year") %>% droplevels()

#reset_gtsummary_theme()
theme_gtsummary_compact()

table1 <- 
  tbl_summary(
    t1,
    by = "year", # split table by group
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{N_miss} ({p_miss}%)"),
      all_categorical(dichotomous = FALSE) ~ "{n} ({p}%)",
      all_dichotomous() ~ "{n} ({p}%)"),
    label = list(
      age = "Age (years)",
      gender = "Gender",
      preopqsymptomperiod = "Duration of symptoms",
      preopqdisability = "Self-reported disability",
      comorb = "Number of comorbidities",
      krpreopqscore = "Pre-operative OKS",
      krpostopqscore = "Post-operative OKS",
      coks = "Change in OKS",
      mic01 = "Responder",
      preopqeq5dindex = "Pre-operative EQ-5D utility",
      postopqeq5dindex = "Post-operative EQ-5D utility",
      ceq5d = "Change in EQ-5D utility",
      satisf = "Patient satisfied",
      success = "Perceived success",
      ncompsNA = "Number of complications")) %>%
  add_overall() %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table()

table1



FitFlextableToPage <- function(ft, pgwidth = 5){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

tbl1 <- FitFlextableToPage(table1)







#################################


pacman::p_load(pacman, dplyr, gtsummary, flextable, rio)

dfh <- import("hiptt.dta")

names(dfh)

# Recode age
dfh$age <- factor(dfh$age, levels=c(1,2,3,4), ordered=TRUE,
                 labels = c("<60","60-69","70-79","80+")) %>% forcats::fct_explicit_na()

# Gender
dfh$gender <- factor(dfh$gender, levels=c(2,1), ordered=TRUE,
                    labels = c("Female","Male")) %>% forcats::fct_explicit_na()

# Recode symptom period
dfh$preopqsymptomperiod <- factor(dfh$preopqsymptomperiod, levels=c(1,2,3,4), ordered=TRUE,
                                 labels = c("Less than 1 year","1 - 5 years","6 - 10 years","10 years +")) %>% forcats::fct_explicit_na()


# Recode self-reported disability
dfh$preopqdisability <- factor(dfh$preopqdisability, levels=c(1,0), ordered=TRUE,
                              labels = c("Yes","No")) %>% forcats::fct_explicit_na()

# Recode comorbidities
dfh$comorb <- factor(dfh$comorb, levels=c(0,1,2,3), ordered=TRUE,
                    labels = c("0", "1", "2", "3+"))

# Complications
dfh <- dfh %>% mutate(ncomps = ifelse(complication > 3, 3, complication))
dfh$ncomps <- factor(dfh$ncomps, levels=c(0,1,2,3), ordered=TRUE, labels = c("0", "1", "2", "3+"))

# Identify records where all complications are missing
# i.e. postopqallergy, postopqbleeding, postopqwound, postopqurine, postopqfurthersurgery, postopqreadmitted
dfh <- dfh %>%
  rowwise() %>%
  mutate(N_comps_NA = sum(is.na(c(postopqallergy, postopqbleeding, postopqwound, postopqurine, postopqfurthersurgery, postopqreadmitted))))

# Check that missing doesn't add up to more than 6
# unique(dfh$N_comps_NA)
# Replace as missing if == 6
dfh <- dfh %>% mutate(ncompsNA =case_when(
  N_comps_NA == 6 ~ NA_character_,
  TRUE ~ as.character(ncomps)))

dfh$ncompsNA <- factor(dfh$ncompsNA, ordered=TRUE, labels = c("0", "1", "2", "3+")) %>% forcats::fct_explicit_na()

# unique(dfh$ncompsNA)
# levels(dfh$ncompsNA)

# Responders
dfh <- dfh %>% mutate(mic01 = case_when(cohs>=8 ~ "Yes",
                                      cohs<8 ~ "No"))

dfh$mic01 <- factor(dfh$mic01, levels=c("Yes", "No"), ordered=TRUE, labels = c("Yes", "No")) %>% forcats::fct_explicit_na()
levels(dfh$mic01)

# Satisfaction
dfh$satisf <- factor(dfh$satisf, levels=c(1,0), ordered=TRUE, labels = c("Yes", "No")) %>% forcats::fct_explicit_na()
levels(dfh$satisf)

# Success
dfh$success <- factor(dfh$success, levels=c(1,0), ordered=TRUE, labels = c("Yes", "No")) %>% forcats::fct_explicit_na()
levels(dfh$success)

#####

t1h = dfh %>% select("age", "gender", "preopqsymptomperiod", "preopqdisability", "comorb", "hrpreopqscore", "hrpostopqscore", "cohs", "mic01", "preopqeq5dindex", "postopqeq5dindex", "ceq5d", "satisf", "success", "ncompsNA", "year") %>% droplevels()

#reset_gtsummary_theme()
theme_gtsummary_compact()

table1h <- 
  tbl_summary(
    t1h,
    by = "year", # split table by group
    missing = "no",
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{N_miss} ({p_miss}%)"),
      all_categorical(dichotomous = FALSE) ~ "{n} ({p}%)",
      all_dichotomous() ~ "{n} ({p}%)"),
    label = list(
      age = "Age (years)",
      gender = "Gender",
      preopqsymptomperiod = "Duration of symptoms",
      preopqdisability = "Self-reported disability",
      comorb = "Number of comorbidities",
      hrpreopqscore = "Pre-operative OHS",
      hrpostopqscore = "Post-operative OHS",
      cohs = "Change in OHS",
      mic01 = "Responder",
      preopqeq5dindex = "Pre-operative EQ-5D utility",
      postopqeq5dindex = "Post-operative EQ-5D utility",
      ceq5d = "Change in EQ-5D utility",
      satisf = "Patient satisfied",
      success = "Perceived success",
      ncompsNA = "Number of complications")) %>%
  add_overall() %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table()

table1h

tbl1h <- FitFlextableToPage(table1h)




############################################################################################################################
############################################             Results             ###############################################
############################################################################################################################

### PROM participation rate

# Calculated manually

############################################################################################################################

### Trends over time in baseline characteristics of patients undergoing hip and knee replacement

# (a) Hips

# Description
##OHS
p_load(magrittr)
dfh %$% round(mean(hrpreopqscore, na.rm=TRUE),1)
dfh %$% round(sd(hrpreopqscore, na.rm=TRUE),1)
dfh %$% round(mean(hrpostopqscore, na.rm=TRUE),1)
dfh %$% round(sd(hrpostopqscore, na.rm=TRUE),1)


crudeohsyr <- dfh %>% group_by(year) %>% summarise(meanq1ohs = mean(hrpreopqscore, na.rm=TRUE),
                                                   sdq1ohs = sd(hrpreopqscore, na.rm=TRUE),
                                                   meanq2ohs = mean(hrpostopqscore, na.rm=TRUE),
                                                   sdq2ohs = sd(hrpostopqscore, na.rm=TRUE),
                                                   meancohs = mean(cohs, na.rm=TRUE),
                                                   sdcohs = sd(cohs, na.rm=TRUE),
                                                   iqrq1 = IQR(hrpreopqscore, na.rm=TRUE),
                                                   iqrq2 = IQR(hrpostopqscore, na.rm=TRUE),
                                                   iqrcohs = IQR(cohs, na.rm=TRUE))

crudeohsyr

##EQ-5D
crudeeq5dyrh <- dfh %>% group_by(year) %>% summarise(meanq1eq5d = mean(preopqeq5dindex, na.rm=TRUE),
                                                     meanq2eq5d = mean(postopqeq5dindex, na.rm=TRUE),
                                                     meanceq5d = mean(ceq5d, na.rm=TRUE),
                                                     iqrq1 = IQR(preopqeq5dindex, na.rm=TRUE),
                                                     iqrq2 = IQR(postopqeq5dindex, na.rm=TRUE),
                                                     iqrceq5d = IQR(ceq5d, na.rm=TRUE))

crudeeq5dyrh

# Fig - Comorbs

### Comorbidities over time

# Reverse the order of comorbidities
p_load(forcats)
dfh$ncomorbs <- fct_rev(dfh$comorb)

# Stacked bar plot of comorbs (0,1,2,3+)
comorbh <- dfh %>%
  group_by(ncomorbs, year) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)))

# Stacked bar plot of comorbs (0,1,2,3+)
pcomorbh <- ggplot(comorbh, aes(fill=ncomorbs, y=n, x=year)) +
  geom_bar(position="fill", stat="identity") +
  labs(x = "Year of surgery", y = "Proportion") +
  guides(fill=guide_legend(title="Comorbidities"))

pcomorbh

##############################################################

# (b) Knees

# Description
##OKS
df %$% round(mean(krpreopqscore, na.rm=TRUE),1)
df %$% round(sd(krpreopqscore, na.rm=TRUE),1)
df %$% round(mean(krpostopqscore, na.rm=TRUE),1)
df %$% round(sd(krpostopqscore, na.rm=TRUE),1)


crudeoksyr <- df %>% group_by(year) %>% summarise(meanq1oks = mean(krpreopqscore, na.rm=TRUE),
                                                  meanq2oks = mean(krpostopqscore, na.rm=TRUE),
                                                  meancoks = mean(coks, na.rm=TRUE),
                                                  iqrq1 = IQR(krpreopqscore, na.rm=TRUE),
                                                  iqrq2 = IQR(krpostopqscore, na.rm=TRUE),
                                                  iqrcoks = IQR(coks, na.rm=TRUE))

crudeoksyr

##EQ-5D
crudeeq5dyr <- df %>% group_by(year) %>% summarise(meanq1eq5d = mean(preopqeq5dindex, na.rm=TRUE),
                                                   meanq2eq5d = mean(postopqeq5dindex, na.rm=TRUE),
                                                   meanceq5d = mean(ceq5d, na.rm=TRUE),
                                                   iqrq1 = IQR(preopqeq5dindex, na.rm=TRUE),
                                                   iqrq2 = IQR(postopqeq5dindex, na.rm=TRUE),
                                                   iqrceq5d = IQR(ceq5d, na.rm=TRUE))

crudeeq5dyr

# Fig - Comorbs

### Comorbidities over time

# Reverse the order of comorbidities
p_load(forcats)
df$ncomorbs <- fct_rev(df$comorb)

# Stacked bar plot of comorbs (0,1,2,3+)
comorbk <- df %>%
  group_by(ncomorbs, year) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)))

# Stacked bar plot of comorbs (0,1,2,3+)
pcomorbk <- ggplot(comorbk, aes(fill=ncomorbs, y=n, x=year)) +
  geom_bar(position="fill", stat="identity") +
  labs(x = "Year of surgery", y = "Proportion") +
  guides(fill=guide_legend(title="Comorbidities"))

pcomorbk


############################################################################################################################

### Trends over time in patient-reported outcomes from surgery over time (crude)

###### (a) Hips

# OHS
ohs <- dfh %>%
  select(hrpreopqscore, hrpostopqscore, cohs, year) %>% 
  gather(variable, value, -year)

ohs$variable <- factor(ohs$variable, levels=c("hrpreopqscore", "hrpostopqscore", "cohs"), ordered=TRUE,
                       labels = c("Pre-operative OHS", "Post-operative OHS", "Change in OHS"))

cbpohs <-
  ohs %>% 
  ggplot(aes(value, year, fill = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  ylab("Year")+
  scale_x_continuous(breaks = seq(-48 , 48, 12)) +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_line(colour="white", size=0.5),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")
cbpohs  

# EQ-5D
eqh <- dfh %>%
  select(preopqeq5dindex, postopqeq5dindex, ceq5d, year) %>% 
  gather(variable, value, -year)

eqh$variable <- factor(eqh$variable, levels=c("preopqeq5dindex", "postopqeq5dindex", "ceq5d"), ordered=TRUE,
                       labels = c("Pre-operative EQ-5D", "Post-operative EQ-5D", "Change in EQ-5D"))

bpeqh <-
  eqh %>% 
  ggplot(aes(value, year, fill = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  ylab("Year")+
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_line(colour="white", size=0.5),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")
bpeqh

# Success
# Need to re-code success into yes/no first
p_load(collapse)
str(dfh$postopqsucess)
unique(dfh$postopqsucess)

fcolh <- dfh %>% select(postopqsucess, year) %>% filter(postopqsucess!=9) %>% mutate(suc = ifelse(postopqsucess < 3, 1, 0))

fcolh <- collap(fcolh, suc ~ year, FUN = list(fsum, fNobs))

fcolh$proportion <- fcolh$fsum.suc/fcolh$fNobs.suc

p_load(epitools)
fcol1h <- binom.wilson(fcolh$fsum.suc, fcolh$fNobs.suc, conf.level = 0.95)

# Merge dataframes
fcolh <- merge(fcolh,fcol1h,by=("proportion"))

such <- fcolh %>% 
  mutate_at(vars(proportion, upper, lower),
            .funs = funs(. * 100))

such

# Plot percentage of procedures with 95% CI by year
hipsuc <- ggplot(such) +
  geom_bar( aes(x=year, y=proportion), stat="identity", fill="skyblue", alpha=0.7) +
  scale_y_continuous(breaks = seq(0 , 100, 20), limits = c(0,100)) +
  geom_errorbar( aes(x=year, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.95, size=1.3)+
  labs(x = "Year of surgery", y = "Percentage")+
  theme_stata()
hipsuc

# Satisfaction
str(dfh$postopqsatisfaction)

fcolh <- dfh %>% select(postopqsatisfaction, year) %>% filter(postopqsatisfaction!=9) %>% mutate(sat = ifelse(postopqsatisfaction < 4, 1, 0))

fcolh <- collap(fcolh, sat ~ year, FUN = list(fsum, fNobs))

fcolh$proportion <- fcolh$fsum.sat/fcolh$fNobs.sat

fcol1h <- binom.wilson(fcolh$fsum.sat, fcolh$fNobs.sat, conf.level = 0.95)

# Merge dataframes
fcolh <- merge(fcolh,fcol1h,by=("proportion"))

sath <- fcolh %>% 
  mutate_at(vars(proportion, upper, lower),
            .funs = funs(. * 100))

sath

# Plot percentage of procedures with 95% CI by year
hipsat <- ggplot(sath) +
  geom_bar( aes(x=year, y=proportion), stat="identity", fill="skyblue", alpha=0.7) +
  scale_y_continuous(breaks = seq(0 , 100, 20), limits = c(0,100)) +
  geom_errorbar( aes(x=year, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.95, size=1.3)+
  labs(x = "Year of surgery", y = "Percentage")+
  theme_stata()

hipsat

# Complications

# Calculate percentage complications per year
fcolh <- dfh %>% select(ncompsNA, year) %>% filter(!is.na(ncompsNA)) %>% mutate(comp01 = case_when((ncompsNA >=1) ~ 1,
                                                                          TRUE ~0))

fcolh <- collap(fcolh, comp01 ~ year, FUN = list(fsum, fNobs))

fcolh$proportion <- fcolh$fsum.comp01/fcolh$fNobs.comp01

fcol1h <- binom.wilson(fcolh$fsum.comp01, fcolh$fNobs.comp01, conf.level = 0.95)

# Merge dataframes
fcolh <- merge(fcolh,fcol1h,by=("proportion"))

compsh <- fcolh %>% 
  mutate_at(vars(proportion, upper, lower),
            .funs = funs(. * 100))

compsh

# Plot percentage of procedures with a complication with 95% CI by year
hipcomp <- ggplot(compsh) +
  geom_bar( aes(x=year, y=proportion), stat="identity", fill="skyblue", alpha=0.7) +
  scale_y_continuous(breaks = seq(0 , 100, 20), limits = c(0,100)) +
  geom_errorbar( aes(x=year, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.95, size=1.3)+
  labs(x = "Year of surgery", y = "Percentage")+
  theme_stata()

hipcomp

# Stacked bar plot of number of complications per patient by year (coded as 0,1,2,3+)
p_load(forcats)
dfh$rncompsNA <- fct_rev(dfh$ncompsNA)

ncompsh <- dfh %>%
  group_by(rncompsNA, year) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)))

# Stacked bar plot of comps (0,1,2,3+)
pncompsh <- ggplot(ncompsh, aes(fill=rncompsNA, y=n, x=year)) +
  geom_bar(position="fill", stat="identity") +
  labs(x = "Year of surgery", y = "Proportion") +
  guides(fill=guide_legend(title="Complications \nper patient"))

pncompsh


############################################################################################################################

### Trends over time in patient-reported outcomes from surgery over time (crude)

###### (b) Knees

# OKS
oks <- df %>%
  select(krpreopqscore, krpostopqscore, coks, year) %>% 
  gather(variable, value, -year)

oks$variable <- factor(oks$variable, levels=c("krpreopqscore", "krpostopqscore", "coks"), ordered=TRUE,
                       labels = c("Pre-operative OKS", "Post-operative OKS", "Change in OKS"))

cbpoks <-
  oks %>% 
  ggplot(aes(value, year, fill = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  ylab("Year")+
  scale_x_continuous(breaks = seq(-48 , 48, 12)) +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_line(colour="white", size=0.5),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")
cbpoks  

# EQ-5D
eqk <- df %>%
  select(preopqeq5dindex, postopqeq5dindex, ceq5d, year) %>% 
  gather(variable, value, -year)

eqk$variable <- factor(eqk$variable, levels=c("preopqeq5dindex", "postopqeq5dindex", "ceq5d"), ordered=TRUE,
                       labels = c("Pre-operative EQ-5D", "Post-operative EQ-5D", "Change in EQ-5D"))

bpeqk <-
  eqk %>% 
  ggplot(aes(value, year, fill = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x", nrow = 1, strip.position = "bottom") +
  ylab("Year")+
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_line(colour="white", size=0.5),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.placement = "outside")
bpeqk

# Success
# Need to re-code success into yes/no first
str(df$postopqsucess)
unique(df$postopqsucess)

fcol <- df %>% select(postopqsucess, year) %>% filter(postopqsucess!=9) %>% mutate(suc = ifelse(postopqsucess < 3, 1, 0))

fcol <- collap(fcol, suc ~ year, FUN = list(fsum, fNobs))

fcol$proportion <- fcol$fsum.suc/fcol$fNobs.suc

p_load(epitools)
fcol1 <- binom.wilson(fcol$fsum.suc, fcol$fNobs.suc, conf.level = 0.95)

# Merge dataframes
fcol <- merge(fcol,fcol1,by=("proportion"))

suc <- fcol %>% 
  mutate_at(vars(proportion, upper, lower),
            .funs = funs(. * 100))

suc

# Plot percentage of procedures with 95% CI by year
kneesuc <- ggplot(suc) +
  geom_bar( aes(x=year, y=proportion), stat="identity", fill="skyblue", alpha=0.7) +
  scale_y_continuous(breaks = seq(0 , 100, 20), limits = c(0,100)) +
  geom_errorbar( aes(x=year, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.95, size=1.3)+
  labs(x = "Year of surgery", y = "Percentage")+
  theme_stata()
kneesuc

# Satisfaction
str(df$postopqsatisfaction)

fcol <- df %>% select(postopqsatisfaction, year) %>% filter(postopqsatisfaction!=9) %>% mutate(sat = ifelse(postopqsatisfaction < 4, 1, 0))

fcol <- collap(fcol, sat ~ year, FUN = list(fsum, fNobs))

fcol$proportion <- fcol$fsum.sat/fcol$fNobs.sat

fcol1 <- binom.wilson(fcol$fsum.sat, fcol$fNobs.sat, conf.level = 0.95)

# Merge dataframes
fcol <- merge(fcol,fcol1,by=("proportion"))

sat <- fcol %>% 
  mutate_at(vars(proportion, upper, lower),
            .funs = funs(. * 100))

sat

# Plot percentage of procedures with 95% CI by year
kneesat <- ggplot(sat) +
  geom_bar( aes(x=year, y=proportion), stat="identity", fill="skyblue", alpha=0.7) +
  scale_y_continuous(breaks = seq(0 , 100, 20), limits = c(0,100)) +
  geom_errorbar( aes(x=year, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.95, size=1.3)+
  labs(x = "Year of surgery", y = "Percentage")+
  theme_stata()

kneesat

# Complications

# Calculate percentage complications per year
fcol <- df %>% select(ncompsNA, year) %>% filter(!is.na(ncompsNA)) %>% mutate(comp01 = case_when((ncompsNA >=1) ~ 1,
                                                                                                   TRUE ~0))

fcol <- collap(fcol, comp01 ~ year, FUN = list(fsum, fNobs))

fcol$proportion <- fcol$fsum.comp01/fcol$fNobs.comp01

fcol1 <- binom.wilson(fcol$fsum.comp01, fcol$fNobs.comp01, conf.level = 0.95)

# Merge dataframes
fcol <- merge(fcol,fcol1,by=("proportion"))

comps <- fcol %>% 
  mutate_at(vars(proportion, upper, lower),
            .funs = funs(. * 100))

comps

# Plot percentage of procedures with a complication with 95% CI by year
kneecomp <- ggplot(comps) +
  geom_bar( aes(x=year, y=proportion), stat="identity", fill="skyblue", alpha=0.7) +
  scale_y_continuous(breaks = seq(0 , 100, 20), limits = c(0,100)) +
  geom_errorbar( aes(x=year, ymin=lower, ymax=upper), width=0.4, colour="orange", alpha=0.95, size=1.3)+
  labs(x = "Year of surgery", y = "Percentage")+
  theme_stata()

kneecomp

# Stacked bar plot of number of complications per patient by year (coded as 0,1,2,3+)
p_load(forcats)
df$rncompsNA <- fct_rev(df$ncompsNA)

ncompsk <- df %>%
  group_by(rncompsNA, year) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)))

# Stacked bar plot of comps (0,1,2,3+)
pncompsk <- ggplot(ncompsk, aes(fill=rncompsNA, y=n, x=year)) +
  geom_bar(position="fill", stat="identity") +
  labs(x = "Year of surgery", y = "Proportion") +
  guides(fill=guide_legend(title="Complications \nper patient"))

pncompsk