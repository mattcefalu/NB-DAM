rm(list=ls())
library(data.table)
library(ggplot2)
library(rstan)
options(mc.cores=parallel::detectCores())

# which outcomes -- Total.Firearm.Deaths, Firearm.Suicides, or Firearm.Homicides
outcomes = c("Total.Firearm.Deaths","Firearm.Suicides","Firearm.Homicides")

# run mcmc
for (outcome in outcomes){
  source("code/run_mcmc.r")
  rm(list=setdiff(ls(),c("outcome","outcomes")))
}

# run summaries
for (outcome in outcomes){
  source("code/summary.r")
  rm(list=setdiff(ls(),c("outcome","outcomes")))
}