rm(list=ls())
library(data.table)
library(ggplot2)
library(rstan)
library(writexl)
library(digest)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write = TRUE)

# which outcomes -- Total.Firearm.Deaths, Firearm.Suicides, or Firearm.Homicides
outcomes = c("Total.Firearm.Deaths", "Firearm.Suicides","Firearm.Homicides")

# the list of law names 
law.names = c("CAP","SYG","CCW.SI","PC","BCps","BCds","ma18poss","ma20ps","WP24p","WP7d")

# loosen the priors by a factor of 10?
loosen_prior = FALSE

# include distractor/spoiler terms
distractor = FALSE

# run mcmc
for (outcome in outcomes){
  seed = sum(strtoi(digest(outcome,algo = "sha256" , raw = T) , base=16)) + 251*(2+loosen_prior-distractor)
  source("code/run_mcmc.r")
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names","loosen_prior","distractor")))
}

# run summaries
results_fit=NULL
for (outcome in outcomes){
  source("code/run_summary.r")
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names","results_fit","loosen_prior","distractor")))
  
  # stack r2 and loo values so we only save one file
  out=readRDS(paste0("results/stan_fits/",
                     ifelse(loosen_prior , "loosen_prior/" , ""),
                     ifelse(!distractor , "no_distractor/" , ""),"final_model_",outcome,"_stats.RDS"))
  results_fit=rbind(results_fit , c(outcome=outcome,out))
}

# save the loo and r2 values to excel
write_xlsx(data.frame(results_fit) , path=paste0("results/fit_statistics",
                                                 ifelse(loosen_prior , "_loosen_prior" , ""),
                                                 ifelse(!distractor , "_no_distractor" , ""),".xlsx"))
