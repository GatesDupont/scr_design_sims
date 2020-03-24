wd = "~/Desktop/sims"

# Gates Dupont      #
# gdupont@umass.edu #
# March 2020       #
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
# p2bar_fail <- df %>%
#   filter(EN > 3000) %>%
#   group_by(i, geom, design, density, coverage) %>%
#   summarise(mean_n_fails = n()/300) %>% # ASSUMING NSIM = 300 AND ds ISSUE ONLY
#   ungroup() %>%
#   select(i, geom, design, density, coverage, mean_n_fails)

fail_df <- df %>%
  filter(oSCR_model != "ds") %>%
  select(i, geom, design, density, coverage, failures) %>%
  group_by(i, geom, design, density, coverage) %>%
  summarise(mean_n_fails = mean(failures)) %>%
  filter(mean_n_fails > 0)

# rbind(as.data.frame(fail_df), as.data.frame(p2bar_fail)) %>%
#   mutate_if(is.numeric, round, 4)


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

# Merge failures here
# write.csv(d.sum, file = paste0(wd, "/", "summary_table.csv"))



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

add1 = expand.grid(i = 100,  geom ="regular", density = "uniform", 
                   design = c("2sigma", "pbar", "p2bar"), coverage = c("A", "B", "C"),  
                   oSCR_model = "ds", Parameter = c("p0", "sig", "d0", "d.beta", "EN"), 
                   SME = NA, CV = NA, SRMSE = NA)

add2 = expand.grid(i = 100,  geom ="irregular", density = "uniform", 
                   design = c("pbar", "p2bar"), coverage = c("A", "B", "C"),  
                   oSCR_model = "ds", Parameter = c("p0", "sig", "d0", "d.beta", "EN"), 
                   SME = NA, CV = NA, SRMSE = NA)

add = rbind(add1, add2)


"_____________________Figures 2 and 3_____________________"
prec_plot_df = evals %>%
  rbind(., add) %>%
  mutate(geom = recode(geom,
                       "regular" = "Regular",
                       "irregular" = "Irregular")) %>%
  mutate(design = recode(design,
                         "2sigma" = "2~sigma",
                         "pbar" = "Q[bar(p)]",
                         "p2bar" = "Q[bar(p)[m]]")) %>%
  mutate(design = factor(
    design, levels = c("2~sigma", "Q[bar(p)]", "Q[bar(p)[m]]"))) %>%
  filter(Parameter == "EN")  %>%
  mutate(coverage = recode(coverage, "A"="144", "B"="100", "C"="49"))

parse.xlab = c(as.character(unique(prec_plot_df$design)[1:3]),
               as.character(unique(prec_plot_df$design)[2:3]))


# BIAS FOR PUB  --  cant figure out math symbol parsing

bias_plot_df = evals %>%
  rbind(., add) %>%
  mutate(geom = recode(geom,
                       "regular" = "Regular",
                       "irregular" = "Irregular")) %>%
  mutate(density = recode(density,
                       "uniform" = "Uniform",
                       "weak" = "Weak",
                       "strong" = "Strong")) %>%
  mutate(design = recode(design,
                         "2sigma" = "2~sigma",
                         "pbar" = "Q[bar(p)]",
                         "p2bar" = "Q[bar(p)[m]]")) %>%
  mutate(design = factor(
    design, levels = c("2~sigma", "Q[bar(p)]", "Q[bar(p)[m]]"))) %>%
  filter(Parameter == "EN")  %>%
  mutate(coverage = recode(coverage, "A"="144", "B"="100", "C"="49"))

ggplot(data = bias_plot_df, 
       aes(x = design, y = SME, group = interaction(coverage, oSCR_model))) +
  geom_hline(yintercept = c(-5,5), color = "gray80", size = 0.5, linetype  = "dashed") +
  geom_hline(yintercept = 0, color = "gray80", size = 0.5) +
  geom_point(aes(color = design, fill = design,
                 shape = interaction(coverage, oSCR_model, sep = ",  ")),
             position = position_dodge(width=0.8), 
             size = 1.75, stroke = 0.85, na.rm = T) +
  scale_shape_manual(values=c(1,2,0,16,17,15)) +
  labs(shape="Effort, Model") +
  scale_color_manual(values = c("gray30", ibm[3], ibm[1]), 
                     labels=parse(text=as.character(unique(bias_plot_df$design)[1:3]))) +
  facet_grid(density~geom, labeller = label_parsed,
             scales = "free_x", space = "free_x") +
  labs(color = "Design") + xlab("Design") +
  theme_bw() + ylim(c(-15,15)) + ylab("%RB") +
  scale_x_discrete(labels=parse(text=parse.xlab)) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        title = element_text(size=14),
        strip.text = element_text(size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.spacing.y = unit(0, 'in'),
        legend.key.size = unit(0, "in"))

ggsave(plot = last_plot(), device = "jpg",
       dpi = 600, width = 6,  height = 4.5, units = "in",
       filename = "Figure3.jpg", path = "/Users/gatesdupont/Desktop")



# PRECISION FOR PUB -- unfinished, cant figure out math symbol parsing

prec_plot_df = evals %>%
  rbind(., add) %>%
  mutate(geom = recode(geom,
                       "regular" = "Regular",
                       "irregular" = "Irregular")) %>%
  mutate(density = recode(density,
                          "uniform" = "Uniform",
                          "weak" = "Weak",
                          "strong" = "Strong")) %>%
  mutate(design = recode(design,
                         "2sigma" = "2~sigma",
                         "pbar" = "Q[bar(p)]",
                         "p2bar" = "Q[bar(p)[m]]")) %>%
  mutate(design = factor(
    design, levels = c("2~sigma", "Q[bar(p)]", "Q[bar(p)[m]]"))) %>%
  filter(Parameter == "EN")  %>%
  mutate(coverage = recode(coverage, "A"="144", "B"="100", "C"="49"))

ggplot(data = prec_plot_df, 
       aes(x = design, y = CV, group = interaction(coverage, oSCR_model))) +
  geom_point(aes(color = design, fill = design,
                 shape = interaction(coverage, oSCR_model, sep = ",  ")),
             position = position_dodge(width=0.75), 
             size = 1.75, stroke = 0.7, na.rm = T) +
  scale_shape_manual(values=c(1,2,0,16,17,15)) +
  labs(shape="Effort, Model") +
  scale_color_manual(values = c("gray30", ibm[3], ibm[1]),
                     labels=parse(text=as.character(unique(prec_plot_df$design)[1:3]))) +
  facet_grid(density~geom, labeller = label_parsed,
             scales = "free_x", space = "free_x") +
  scale_x_discrete(labels=parse(text=parse.xlab)) +
  labs(color = "Design") + xlab("Design") +
  theme_bw() + guides(design=FALSE) + ylim(min(evals$CV),35) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        title = element_text(size=14),
        strip.text = element_text(size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.spacing.y = unit(0, 'in'),
        legend.key.size = unit(0, "in"))

ggsave(plot = last_plot(), device = "jpg", 
       dpi = 600, width = 6,  height = 4.5, units = "in",
       filename = "Appendix4.jpg", path = "/Users/gatesdupont/Desktop")

