rm(list=ls())
library(data.table)
library(ggplot2)
library(rstan)
library(writexl)
library(digest)
library(ggpubr)
library(gridExtra)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write = TRUE)

# which outcomes -- Total.Firearm.Deaths, Firearm.Suicides, Firearm.Homicides, Total.Homicides, Total.Suicides
outcomes = c("Total.Firearm.Deaths","Firearm.Suicides","Firearm.Homicides")

# the list of law names 
law.names = c("CAP","SYG","CCW.SI","PC","BCps","BCds","ma18poss","ma20ps","WP24p","WP7d")

# loosen the priors by a factor of 10?
loosen_prior = FALSE

# include distractor/spoiler terms
distractor = TRUE

# run mcmc
for (outcome in outcomes){
  seed = sum(strtoi(digest(outcome,algo = "sha256" , raw = T) , base=16)) + 251*(2+loosen_prior-distractor)
  source("code/run_mcmc.r")
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names","loosen_prior","distractor")))
}
