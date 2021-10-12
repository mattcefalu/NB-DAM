rm(list=ls())
library(data.table)
library(ggplot2)
library(rstan)
library(writexl)
library(digest)
options(mc.cores=parallel::detectCores())

# which outcomes -- Total.Firearm.Deaths, Firearm.Suicides, or Firearm.Homicides
outcomes = c("Total.Firearm.Deaths","Firearm.Suicides","Firearm.Homicides")

# the list of law names 
law.names = c("CAP","SYG","CCW.SI","PC","BCps","BCds","ma18poss","ma20ps","WP24p","WP7d")

# run mcmc
for (outcome in outcomes){
  set.seed(sum(strtoi(digest(outcome,algo = "sha256" , raw = T) , base=16)) + 251)
  source("code/run_mcmc.r")
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names")))
}

# run summaries
results_fit=NULL
for (outcome in outcomes){
  source("code/run_summary.r")
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names","results_fit")))
  
  # stack r2 and loo values so we only save one file
  out=readRDS(paste0("results/stan_fits/final_model_",outcome,"_stats.RDS"))
  results_fit=rbind(results_fit , c(outcome=outcome,out))
}

# save the loo and r2 values to excel
write_xlsx(data.frame(results_fit) , path="results/fit_statistics.xlsx")
