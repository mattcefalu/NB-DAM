combine = function(mcmc , P1.names , P0.names , P.names , file.table , file.figure , ylim=c(0.7,1.2)){
  P1 = matrix(0 , nrow=length(P.names) , ncol=7)
  rownames(P1) = P.names
  
  P0 = matrix(0 , nrow=length(P.names) , ncol=7)
  rownames(P0) = P.names
  
  P1.names = grep( paste(P1.names,collapse="|") , P.names , value=T)
  P0.names = grep( paste(P0.names,collapse="|") , P.names , value=T)
  
  # all instant effects on at all times
  P1[grep("instant",P1.names,value=T),] = 1 
  P0[grep("instant",P0.names,value=T),] = 1 
  
  # slow codings 
  for (p in grep("levels.coding.slow.(?!(lagged))",P1.names,value=T,perl=T)){
    P1[p,] = pmin(0:6/5,1)
  }
  
  for (p in grep("levels.coding.slow.(?!(lagged))",P0.names,value=T,perl=T)){
    P0[p,] = pmin(0:6/5,1)
  }
  
  # find the effect
  res = exp(mcmc%*%P1 - mcmc%*%P0)
  
  # summarize
  d = t(apply(res,2,quantile,c(0.025,0.05,0.1,0.5,0.9,0.95,0.975)))
  colnames(d) = paste0("p",c("025","05","10","50","90","95","975"))
  d = data.frame(d)
  
  setDT(d)
  d[ , year := 0:6]
  d[ , mean := colMeans(res)]
  d[ , sd := apply(res,2,sd)]
  d[ , Pr:=colMeans(res<=1)]
  
  d.for.plot = rbind(data.table(p025=NA,p05=NA,p10=NA,p50=1,p90=NA,p95=NA,p975=NA,year=-1,mean=NA,sd=NA,Pr=NA) , 
                     data.table(p025=NA,p05=NA,p10=NA,p50=1,p90=NA,p95=NA,p975=NA,year=0,mean=NA,sd=NA,Pr=NA) , 
                     d )
  
  # 100 draws of the mcmc
  tmp = res[ seq(1,nrow(res),length.out=100), ]
  colnames(tmp) = paste0("Year",0:6)
  tmp = reshape(data.frame(tmp) , direction="long" , varying=paste0("Year",0:6),sep="",times = 0:6 , timevar="Year" , v.names="est")
  
  
  g = 
    
    ggplot(d.for.plot) + 
    geom_hline(yintercept = 1 , color=rgb(0,0,0,.4) , size=0.75) +
    geom_line(data=tmp , aes(x=Year , y=est , group=id) , color=rgb(0,0.7,0.3 , 0.3)) +
    geom_line(aes(x=year,y=p50), size=0.75) + 
    #geom_line(aes(x=year,y=p025),linetype="dashed", size=0.75) + 
    #geom_line(aes(x=year,y=p975),linetype="dashed", size=0.75) + 
    geom_line(aes(x=year,y=p10),linetype="dashed" , size=0.75) + 
    geom_line(aes(x=year,y=p90),linetype="dashed" , size=0.75) + 
    scale_y_continuous("Risk Ratio" , expand = c(0,0)) + 
    scale_x_continuous("Years since implementation" , expand = c(0,0)) + 
    coord_cartesian(ylim = ylim) +
    theme_bw() #+ 
    #theme(panel.ontop = TRUE,panel.background = element_rect(color = NA, fill = NA))
  
  #write.csv(d , file=file.table , row.names = F)
  pdf(file=file.figure , width=8,height=6)
  print(g)
  dev.off()
  
  return(list(res=d , g=g))
}
