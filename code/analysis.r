rm(list=ls())
library(data.table)
library(ggplot2)
library(rstan)
library(writexl)
options(mc.cores=parallel::detectCores())

# which outcomes -- Total.Firearm.Deaths, Firearm.Suicides, or Firearm.Homicides
outcomes = c("Total.Firearm.Deaths","Firearm.Suicides","Firearm.Homicides")

# the list of law names 
law.names = c("CAP","SYG","CCW.SI","PC","BCps","BCds","ma18poss","ma20ps","WP24p","WP7d")

# run mcmc
for (outcome in outcomes){
  source("code/run_mcmc.r")
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names")))
}

# run summaries
for (outcome in outcomes){
  source("code/run_summary.r")
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names")))
}
