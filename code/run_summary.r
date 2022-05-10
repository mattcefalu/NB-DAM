# load function
source("code/combine.r")

# locations to save files
results.folder = paste0("results/",outcome,"/")
figures.folder = paste0("figures/",outcome,"/", ifelse(loosen_prior , "loosen_prior/" , ""),ifelse(!distractor , "no_distractor/" , ""))

ylims = c(0.6,1.4)

# load model fit
res = readRDS(paste0("results/stan_fits/",
                     ifelse(loosen_prior , "loosen_prior/" , ""),
                     ifelse(!distractor , "no_distractor/" , ""),"final_model_",outcome,".RDS"))
m = res$m
P.names = res$law.names
X.names = res$X.names
U.names = res$U.names
rm(res)


# full summary of posterior
post.summary = summary(m , pars=grep("log_mu|log_lik", names(extract(m)) , value = T , invert = T) , use_cache=F)$summary
rownames(post.summary)[grepl("^gamma_U",rownames(post.summary))] = U.names
rownames(post.summary)[grepl("^gamma_X",rownames(post.summary))] = X.names
rownames(post.summary)[grepl("beta",rownames(post.summary))] = P.names

# some rounding
post.summary = round(post.summary,4)
post.summary[,"n_eff"] = round(post.summary[,"n_eff"],0)

# drop some rows/columns
d.out = as.data.frame(post.summary[,-2])
d.out <- cbind(" "=rownames(d.out), d.out)

write_xlsx(d.out,path=paste0(results.folder,"mcmc_coef_summary_",outcome,
                                                        ifelse(loosen_prior , "_loosen_prior" , ""),ifelse(!distractor , "_no_distractor" , ""),".xlsx"))

# extract mcmc
mcmc = extract(m , "beta")$beta


################################
# law combinations of interest #
combined.laws = list()

# most restrictive without CC or SYG
combined.laws[["most_restrictive"]] = list(P1.names=c("CAP","BCps","BCds","ma18po","ma20ps","WP24p","WP7d") ,
                                           P0.names=c("SYG","CCW","PC"))

# restricting use and storage
combined.laws[["use_and_storage"]] = list(P1.names=c("CAP") , 
                                           P0.names=c("SYG","CCW","PC"))

# restricting who can have a firearm
combined.laws[["restricting_who"]] = list(P1.names=c("BCps","BCds","ma18po","ma20ps","WP24p","WP7d") , 
                                          P0.names=c("NULL"))

# federal laws in 1994
combined.laws[["federal_1994"]] = list(P1.names=c("BCds","ma18po","WP24p","WP7d") , 
                                          P0.names=c("NULL"))

# background checks
combined.laws[["background_checks"]] = list(P1.names=c("BCps","BCds") , 
                                       P0.names=c("NULL"))

# age restrictions
combined.laws[["age_restrictions"]] = list(P1.names=c("ma18po","ma20ps") , 
                                            P0.names=c("NULL"))

# waiting periods
combined.laws[["waiting_periods"]] = list(P1.names=c("WP24p","WP7d") , 
                                           P0.names=c("NULL"))

# conceal carry
combined.laws[["conceal_carry"]] = list(P1.names=c("CCW","PC") , 
                                          P0.names=c("NULL"))

  
  

out = list()

# loop over the combinations of laws
for (combined.name in names(combined.laws)){
  out[[combined.name]]=combine(mcmc=mcmc , 
                               P1.names=combined.laws[[combined.name]]$P1.names ,
                               P0.names=combined.laws[[combined.name]]$P0.names ,
                               P.names=P.names,
                               file.table = paste0(results.folder,combined.name,".csv") , 
                               file.figure = paste0(figures.folder,combined.name,".pdf"),
                               ylim=ylims)
}
  
  

# loop over individual laws
for (law in law.names){
  # individual laws
  out[[law]]=combine( mcmc=mcmc , 
                                P1.names=law , 
                                P0.names=c("NULL"),
                                P.names=P.names,
                                file.table = paste0(results.folder,law,".csv") , 
                                file.figure = paste0(figures.folder,law,".pdf"),
                                ylim=ylims)
}


# create on big output table
results = NULL
for (law in names(out)){
  results = rbind( results , cbind(law=law , out[[law]]$res) )
}

write_xlsx(results , path=paste0(results.folder,outcome, 
                                 ifelse(loosen_prior , "_loosen_prior" , ""),ifelse(!distractor , "_no_distractor" , ""),".xlsx"))


