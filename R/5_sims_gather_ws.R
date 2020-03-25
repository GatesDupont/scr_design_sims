# Gates Dupont      #
# gdupont@umass.edu #
# # # # # # # # # # # 

library(stringr)

source("R/0_functions.R")

wd = "~/scr_design_sims"

"LOADING DATA"

#----Load all data----

load_files(wd, folder = "statespaces")
load_files(wd, folder = "traps")
load_files(wd, folder = "designs")


#----Organizing----

statespaces = mget(c(rep("SS_regular", 27), rep("SS_irregular", 45-27)))
designs = mget(c(
  # REGULAR
  "designA_2sig__regular", 
  "designA_pbar__regular", "designA_p2bar__regular",
                 
  "designB_2sig__regular", 
  "designB_pbar__regular", "designB_p2bar__regular",
                 
  "designC_2sig__regular", 
  "designC_pbar__regular", "designC_p2bar__regular",
                 
  # "Irregular"
  "designA_pbar__irregular", "designA_p2bar__irregular",
                 
  "designB_pbar__irregular", "designB_p2bar__irregular",
                 
  "designC_pbar__irregular", "designC_p2bar__irregular"
  )) %>%
  rep(each = 3)


#----Formatting----

reg_ss.sig = possible_traps_regular %>% as.data.frame %>% 
  arrange(Y) %>% slice(1:2) %>% pull(X) %>% diff

irreg_ss.sig = possible_traps_irregular %>% as.data.frame %>% 
  arrange(Y) %>% slice(1:2) %>% pull(X) %>% diff

# Sim parameters
sig_true = c(rep(reg_ss.sig, 27), rep(irreg_ss.sig, 45-27))
N = rep(300, 45)
p0_true = rep(0.2, 45)
K = rep(5, 45)
d0_true = c(rep(300/nrow(SS_regular), 27), rep(300/nrow(SS_irregular), 45-27))


# Descriptives
names_design = c(
  rep(rep(c("2sigma", "pbar", "p2bar"), each = 3), 3),
  rep(rep(c("pbar", "p2bar"), each = 3), 3))

names_coverage = c(
  rep(c("A", "B", "C"), each = 9),
  rep(c("A", "B", "C"), each = 6))

names_geom = c(rep("regular", 27), rep("irregular", 45-27))

names_density = rep(c("uniform", "patchy", "directional"), 15)

# Final structure
sim_structure = data.frame(
  i = as.character(1:45),
  names_geom = names_geom,
  names_coverage = names_coverage,
  names_design = names_design,
  names_density,
  statespaces = names(statespaces),
  designs = names(designs),
  sig_true = sig_true,
  N = N,
  p0_true = p0_true,
  K = K,
  d0_true = d0_true
)
View(sim_structure)


#----Save the essentials----
keep = c("wd", "names_geom", "names_coverage", "names_design",
         "names_density", "statespaces", "designs", "sig_true", "N",
         "p0_true", "K", "d0_true")

rm(list = setdiff(ls(), keep))


#----Saving as RData workspace----

dir = paste0(wd, "/", "workspace")

if(!dir.exists(dir)){
  dir.create(dir)
}

file = paste0(dir, "/sims_ws.RData")
save.image(file)
