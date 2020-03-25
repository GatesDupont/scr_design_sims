wd = "~/Desktop/sims"

# # # # # # # # # # #
# Gates Dupont      #
# gdupont@umass.edu #
# # # # # # # # # # # 

library(Hmisc)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
select =  dplyr::select
ibm = c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000")

file = paste0(wd, "/", "0_functions.R")
source(file)


"_____________________Formatting Data_____________________"

#----Load data----
file <- paste0(wd, "/", "results_030520.csv")
df0 <- read.csv(file)


#----Ordering factors---
df <- df0 %>%
  mutate(X = 1:nrow(df0)) %>%
  select(i, X, everything()) %>%
  mutate(oSCR_model = replace(oSCR_model, oSCR_model == 1,  "d.")) %>%
  mutate(oSCR_model = replace(oSCR_model, oSCR_model == 2,  "ds")) %>%
  mutate(geom = factor(geom, levels = c("regular", "irregular"))) %>%
  mutate(density = factor(density, levels = c("uniform", "patchy", "directional"))) %>%
  mutate(density = fct_recode(density,  "uniform" = "uniform", "weak" = "patchy", "strong" = "directional" )) %>%
  arrange(X)


#----Failures----
fail_df <- df %>%
  filter(oSCR_model != "ds") %>%
  select(i, geom, design, density, coverage, failures) %>%
  group_by(i, geom, design, density, coverage) %>%
  summarise(mean_n_fails = mean(failures)) %>%
  filter(mean_n_fails > 0)


#----Go long!----
out <- df %>%
  gather(Parameter, Value, c(p0:d.beta, EN), factor_key=TRUE) %>%
  mutate(Truth = ifelse(Parameter == "d0" & geom == "regular", unique(d0_true)[1],
                        ifelse(Parameter == "d0" & geom == "irregular", unique(d0_true)[2],
                               ifelse(Parameter == "p0", p0_true,
                                      ifelse(Parameter == "sig" & geom == "regular", unique(sig_true)[1],
                                             ifelse(Parameter == "sig" & geom == "irregular", unique(sig_true)[2],
                                                    ifelse(Parameter == "EN", N_true,
                                                           ifelse(Parameter == "d.beta" & oSCR_model == "ds", d.beta_true, NA))))))))



"_____________________Summary_____________________"

#----Summary----
d.sum <- df %>%
  filter(oSCR_model != "ds") %>%
  select(i, X, geom, density, design, coverage, nind, avg.caps, avg.spatial) %>%
  group_by(geom, density, design, coverage) %>%
  mutate(
    avg.nind = mean(nind, na.rm = T),
    total.avg.caps = mean(avg.caps, na.rm = T),
    total.avg.spatial = mean(avg.spatial, na.rm = T),
    i = unique(i)
  ) %>%
  select(i, geom, density, design, coverage, avg.nind, total.avg.caps, total.avg.spatial)  %>%
  unique() %>%
  mutate_if(is.numeric, round, 2) %>%
  arrange(i)


"_____________________Evaluations_____________________"

#----Evaluations----
evals <- out %>%
  mutate(SME = 100 * (Value - Truth)/Truth) %>%
  group_by(geom, density, design, coverage, oSCR_model, Parameter) %>%
  mutate(CV = 100 * (sd(na.omit(Value)))/mean(Value, na.rm = T),
         SRMSE = calc_srmse(na.omit(Value), na.omit(unique(Truth)))) %>%
  summarise(SME = mean(SME, na.rm = T), 
            CV = unique(CV), 
            SRMSE = unique(SRMSE),
            i = unique(i)) %>%
  ungroup() %>%
  mutate(SRMSE = ifelse(oSCR_model == "d." & Parameter == "d.beta", NA, SRMSE)) %>%
  mutate(design = factor(design, levels = c("2sigma", "pbar", "p2bar"))) %>%
  filter(Parameter %in% c("p0", "sig", "EN"))  %>%
  drop_na(SME) %>%
  select(i, everything()) %>%
  arrange(i) %>%
  as.data.frame()
