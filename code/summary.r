library(rstan)
library(data.table)
library(ggplot2)

res = readRDS("results/stan_fits/final_model.RDS")
m = res$m
law.names = res$law.names
X.names = res$X.names
U.names = res$U.names

rm(res)


# full summary of posterior
post.summary = summary(m)$summary

rownames(post.summary)[grepl("^gamma_U",rownames(post.summary))] = U.names
rownames(post.summary)[grepl("^gamma_X",rownames(post.summary))] = X.names
rownames(post.summary)[grepl("beta",rownames(post.summary))] = law.names

# convert time and X effects back
post.summary[X.names,1:8] = post.summary[X.names,1:8]/100
post.summary[grepl("zeta",rownames(post.summary)),1:8] = post.summary[grepl("zeta",rownames(post.summary)),1:8]/100

# some rounding
post.summary = round(post.summary,4)
post.summary[,"n_eff"] = round(post.summary[,"n_eff"],0)

# drop some rows/columns
write.csv(post.summary[,-2],file="results/mcmc_summary.csv")

# law combinations

# extract mcmc
mcmc = extract(m , "beta")$beta

combine = function(mcmc , P1.names , P0.names , file.table , file.figure , ylim=c(0.7,1.2)){
  P1 = matrix(0 , nrow=length(law.names) , ncol=6)
  rownames(P1) = law.names
  
  P0 = matrix(0 , nrow=length(law.names) , ncol=6)
  rownames(P0) = law.names
  
  P1.names = grep( paste(P1.names,collapse="|") , law.names , value=T)
  P0.names = grep( paste(P0.names,collapse="|") , law.names , value=T)
  
  # all instant effects on at all times
  P1[grep("instant",P1.names,value=T),] = 1 
  P0[grep("instant",P0.names,value=T),] = 1 
  
  # slow codings 
  for (p in grep("levels.coding.slow.(?!(lagged))",P1.names,value=T,perl=T)){
    P1[p,] = 0:5/5
  }
  
  for (p in grep("levels.coding.slow.(?!(lagged))",P0.names,value=T,perl=T)){
    P0[p,] = 0:5/5
  }
  
  # find the effect
  res = exp(mcmc%*%P1 - mcmc%*%P0)

  # summarize
  colMeans(res)
  d = t(apply(res,2,quantile,c(0.025,0.05,0.1,0.5,0.9,0.95,0.975)))
  colnames(d) = paste0("p",c("025","05","10","50","90","95","975"))
  d = data.frame(d)
  
  setDT(d)
  d[ , year := 0:5]
  d[ , mean := colMeans(res)]
  d[ , sd := apply(res,2,sd)]
  
  g = ggplot(d) + 
    geom_line(aes(x=year,y=p50)) + 
    geom_line(aes(x=year,y=p025),linetype="dashed") + 
    geom_line(aes(x=year,y=p975),linetype="dashed") + 
    geom_line(aes(x=year,y=p10),linetype="dotted") + 
    geom_line(aes(x=year,y=p90),linetype="dotted") + 
    scale_y_continuous("Risk Ratio",limits=ylim , expand = c(0,0)) + 
    scale_x_continuous("Years since implementation" , expand = c(0,0)) + 
    theme_bw()
  
  write.csv(d , file=file.table , row.names = F)
  pdf(file=file.figure , width=8,height=6)
    print(g)
  dev.off()
    
  return(list(res=d , g=g))
}

# most restrictive without CC or SYG
ylims = c(0.6,1.2)
combine(mcmc=mcmc , 
        P1.names=c("CAP","BCps","BCds","ma18po","ma20ps","WP24p","WP7d") ,
        P0.names=c("SYG","CCW","PC") ,
        file.table = "results/most_restrictive.csv" , 
        file.figure = "figures/most_restrictive.pdf",
        ylim=ylims)

# restricting use and storage
combine(mcmc=mcmc , 
        P1.names=c("CAP") , 
        P0.names=c("SYG","CCW","PC"),
        file.table = "results/use_and_storage.csv" , 
        file.figure = "figures/use_and_storage.pdf",
        ylim=ylims)

# restricting who can have a firearm
combine(mcmc=mcmc , 
        P1.names=c("BCps","BCds","ma18po","ma20ps","WP24p","WP7d") , 
        P0.names=c("NULL"),
        file.table = "results/restricting_who.csv" , 
        file.figure = "figures/restricting_who.pdf",
        ylim=ylims)

# federal laws in 1994
combine(mcmc=mcmc , 
        P1.names=c("BCds","ma18po","WP24p","WP7d") , 
        P0.names=c("NULL"),
        file.table = "results/federal_1994.csv" , 
        file.figure = "figures/federal_1994.pdf",
        ylim=ylims)

# background checks
combine(mcmc=mcmc , 
        P1.names=c("BCps","BCds") , 
        P0.names=c("NULL"),
        file.table = "results/background_checks.csv" , 
        file.figure = "figures/background_checks.pdf",
        ylim=ylims)

# age restrictions
combine(mcmc=mcmc , 
        P1.names=c("ma18po","ma20ps") , 
        P0.names=c("NULL"),
        file.table = "results/age_restrictions.csv" , 
        file.figure = "figures/age_restrictions.pdf",
        ylim=ylims)

# waiting periods
combine(mcmc=mcmc , 
        P1.names=c("WP24p","WP7d") , 
        P0.names=c("NULL"),
        file.table = "results/waiting_periods.csv" , 
        file.figure = "figures/waiting_periods.pdf",
        ylim=ylims)

# conceal carry
combine(mcmc=mcmc , 
        P1.names=c("CCW","PC") , 
        P0.names=c("NULL"),
        file.table = "results/conceal_carry.csv" , 
        file.figure = "figures/conceal_carry.pdf",
        ylim=ylims)

# CAP
combine(mcmc=mcmc , 
        P1.names=c("CAP") , 
        P0.names=c("NULL"),
        file.table = "results/CAP.csv" , 
        file.figure = "figures/CAP.pdf",
        ylim=ylims)

# SYG
combine(mcmc=mcmc , 
        P1.names=c("SYG") , 
        P0.names=c("NULL"),
        file.table = "results/SYG.csv" , 
        file.figure = "figures/SYG.pdf",
        ylim=ylims)
