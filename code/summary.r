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


# run summaries
results_fit=NULL
plots = list()
for (outcome in outcomes){
  source("code/run_summary.r")
  plots[[outcome]] = out
  rm(list=setdiff(ls(),c("outcome","outcomes","law.names","results_fit","loosen_prior","distractor","plots")))
  
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


# create single figure that holds the aggregate law effects
a = ggarrange( 
  ggarrange(
    plots$Total.Firearm.Deaths$restricting_who$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$restricting_who$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$restricting_who$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$use_and_storage$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$use_and_storage$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$use_and_storage$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),

  ggarrange(plots$Total.Firearm.Deaths$most_restrictive$g + 
              theme(axis.title=element_blank(),
                    plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
            ncol=1,nrow=1,widths=c(6),align='h'),
  ggarrange(
    plots$Firearm.Suicides$most_restrictive$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$most_restrictive$g + 
      theme(axis.text.y =element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  # ggarrange(
  #   plots$Total.Firearm.Deaths$federal_1994$g + 
  #     theme(axis.title=element_blank(),
  #           plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
  #   ncol=1,nrow=1,widths=6,align='h'),
  # ggarrange(
  #   plots$Firearm.Suicides$federal_1994$g + 
  #     theme(axis.text.y=element_text(colour = "white"),
  #           axis.title=element_blank(),
  #           plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
  #   ncol=1,nrow=1,widths=6,align='h'),
  # ggarrange(
  #   plots$Firearm.Homicides$federal_1994$g + 
  #     theme(axis.text.y=element_text(colour = "white"),
  #           axis.title=element_blank(),
  #           plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
  #   ncol=1,nrow=1,widths=6,align='h'),
  ncol=3 , nrow=3 , align="h"
)

a = a + theme(plot.margin=unit(c(0,0,0,0) , "lines"))

# lables
hom = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Firearm Homicides") + theme_void()
death = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Total Firearm Deaths") + theme_void()
sui = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Firearm Suicides") + theme_void()

# labels
most = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Most Restrictive", angle=90) + theme_void()
use = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Use & Storage", angle=90) + theme_void()
who = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Purchase & Possession", angle=90) + theme_void()
fed = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Brady and Crime Bills (1994)", angle=90) + theme_void()

ylab = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Risk Ratio" , angle=90) + theme_void()
xlab = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Years Since Implementation") + theme_void()


pdf(file=paste0("figures/grouped_laws",ifelse(loosen_prior , "_loosen_prior" , ""),
                ifelse(!distractor , "_no_distractor" , ""),".pdf") , width =8  , height= 8)
grid.arrange(
  grobs=list(who,use,most ,  ylab , ylab , ylab , 
             a , death , sui , hom , xlab , xlab , xlab),
  widths = c(1,0.5,6,6,6) , 
  heights = c(1,6,6,6,6,0.5) ,
  layout_matrix = rbind( c(NA , NA , 8,9,10) ,
                         c(1,4,7,7,7),
                         c(2,5,7,7,7),
                         c(3,6,7,7,7),
                         c(NA,NA,11,12,13))
)
dev.off()








# create single figure that holds the individual law effects
a = ggarrange( 
  ggarrange(plots$Total.Firearm.Deaths$background_checks$g + 
              theme(axis.title=element_blank(),
                    plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
            ncol=1,nrow=1,widths=c(6),align='h'),
  ggarrange(
    plots$Firearm.Suicides$background_checks$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$background_checks$g + 
      theme(axis.text.y =element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$age_restrictions$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$age_restrictions$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$age_restrictions$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$waiting_periods$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$waiting_periods$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$waiting_periods$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$CAP$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$CAP$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$CAP$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$conceal_carry$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$conceal_carry$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$conceal_carry$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$SYG$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$SYG$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$SYG$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ncol=3 , nrow=6 , align="h"
)

a = a + theme(plot.margin=unit(c(0,0,0,0) , "lines"))

# lables
hom = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Firearm Homicides") + theme_void()
death = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Total Firearm Deaths") + theme_void()
sui = ggplot() + annotate("text" , x=0 , y=0 , size=3.5 , label="Firearm Suicides") + theme_void()

# labels
cap = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Child Access", angle=90) + theme_void()
syg = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Stand-Your-Ground", angle=90) + theme_void()
ccw = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Concealed Carry", angle=90) + theme_void()
wp = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Waiting Periods", angle=90) + theme_void()
age= ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Minimum Age", angle=90) + theme_void()
bc = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Background Checks", angle=90) + theme_void()

ylab = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Risk Ratio" , angle=90) + theme_void()
xlab = ggplot() + annotate("text" , x=0 , y=0 , size=3 , label="Years Since Implementation") + theme_void()


pdf(file=paste0("figures/individual_laws",ifelse(loosen_prior , "_loosen_prior" , ""),
                ifelse(!distractor , "_no_distractor" , ""),".pdf") , width =8  , height= 11)
grid.arrange(
  grobs=list(bc, age, wp, cap, ccw, syg, 
             ylab , ylab , ylab , ylab , ylab, ylab , 
             a , death , sui , hom , xlab , xlab , xlab),
  widths = c(1,0.5,6,6,6) , 
  heights = c(1,6,6,6,6,6,6,0.5) ,
  layout_matrix = rbind( c(NA , NA , 14,15,16) ,
                         c(1,7,13,13,13),
                         c(2,8,13,13,13),
                         c(3,9,13,13,13),
                         c(4,10,13,13,13),
                         c(5,11,13,13,13),
                         c(6,12,13,13,13),
                         c(NA,NA,17,18,19))
)
dev.off()










### split individual laws into two figures


# create single figure that holds the individual law effects
a = ggarrange( 
  ggarrange(
    plots$Total.Firearm.Deaths$CAP$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$CAP$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$CAP$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$conceal_carry$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$conceal_carry$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$conceal_carry$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$SYG$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$SYG$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$SYG$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ncol=3 , nrow=3 , align="h"
)

a = a + theme(plot.margin=unit(c(0,0,0,0) , "lines"))



b = ggarrange( 
  ggarrange(plots$Total.Firearm.Deaths$background_checks$g + 
              theme(axis.title=element_blank(),
                    plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
            ncol=1,nrow=1,widths=c(6),align='h'),
  ggarrange(
    plots$Firearm.Suicides$background_checks$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$background_checks$g + 
      theme(axis.text.y =element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$age_restrictions$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$age_restrictions$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$age_restrictions$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  
  ggarrange(
    plots$Total.Firearm.Deaths$waiting_periods$g + 
      theme(axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0.25,0) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Suicides$waiting_periods$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.5,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ggarrange(
    plots$Firearm.Homicides$waiting_periods$g + 
      theme(axis.text.y=element_text(colour = "white"),
            axis.title=element_blank(),
            plot.margin=unit(c(0.25,0.75,0,-0.25) , "lines")),
    ncol=1,nrow=1,widths=6,align='h'),
  ncol=3 , nrow=3 , align="h"
)

b = b + theme(plot.margin=unit(c(0,0,0,0) , "lines"))


pdf(file=paste0("figures/individual_laws_A",ifelse(loosen_prior , "_loosen_prior" , ""),
                ifelse(!distractor , "_no_distractor" , ""),".pdf") , width =8  , height= 8)
grid.arrange(
  grobs=list(ccw, syg, cap,
             ylab , ylab , ylab , 
             a , death , sui , hom , xlab , xlab , xlab),
  widths = c(1,0.5,6,6,6) , 
  heights = c(1,6,6,6,0.5) ,
  layout_matrix = rbind( c(NA , NA , 8,9,10) ,
                         c(1,4,7,7,7),
                         c(2,5,7,7,7),
                         c(3,6,7,7,7),
                         c(NA,NA,11,12,13))
)
dev.off()


pdf(file=paste0("figures/individual_laws_B",ifelse(loosen_prior , "_loosen_prior" , ""),
                ifelse(!distractor , "_no_distractor" , ""),".pdf") , width =8  , height= 8)
grid.arrange(
  grobs=list(bc, age, wp,
             ylab , ylab , ylab , 
             b , death , sui , hom , xlab , xlab , xlab),
  widths = c(1,0.5,6,6,6) , 
  heights = c(1,6,6,6,0.5) ,
  layout_matrix = rbind( c(NA , NA , 8,9,10) ,
                         c(1,4,7,7,7),
                         c(2,5,7,7,7),
                         c(3,6,7,7,7),
                         c(NA,NA,11,12,13))
)
dev.off()



