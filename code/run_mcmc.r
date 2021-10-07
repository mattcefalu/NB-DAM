
# prior SD for policy effects
if (outcome%in%c("Total.Firearm.Deaths","Firearm.Homicides")){
  prior = 0.1/sqrt(2)
}else{
  prior = 0.09/sqrt(2)
}

# drop a few of the covariates
drop = c( "Percent.No.high.school.diploma" ,"Percent.Some.college","Police.Rate.L5") 

# which covariate set
j=4

# loads function that preps outcomes and laws
source("code/data_prep_outcomes_and_laws.r")

# the list of law names 
law.names = c("CAP","SYG","CCW.SI","PC","BCps","BCds","ma18poss","ma20ps","WP24p","WP7d")

# run the data prep
dta=data_prep(file=paste0("data/firearm.model.data",j,".2019.Rdata") , law.names = law.names)

# covariates with no shrinkage
X.names = colnames(dta)[11:15]

# covariates with shrinkage
U.full = U.names = colnames(dta)[16:41]
U.names = U.names[!U.names%in%drop]

# subset to years for model -- this drops NAs due to the lagged outcomes
dta=subset(dta , Year>=1981)
dta = as.data.frame(dta)


# extract outcomes needed for stan
Y = dta[,outcome]
L_1 = log(dta[,paste0(outcome,"_1")])
L_2 = log(dta[,paste0(outcome,"_2")])

# extract covariate sets
X = dta[,X.names]
U = dta[,U.names , drop=F]

# populations
offset = log(dta[,"Population"])
offset_1 = log(dta[,"Population_1"])
offset_2 = log(dta[,"Population_2"])

# time fixed effects -- drop first column as reference
Time = model.matrix(~0+as.factor(Year) , data=dta)[,-1] 
Time_1 = cbind(0,Time[,-ncol(Time)])
Time_2 = cbind(0,Time_1[,-ncol(Time_1)])

# state index for dispersion parameter -- not used
S = as.numeric(as.factor(dta$State))

# law matrix
list.of.laws = c(paste0("levels.coding.instant.",law.names),paste0("levels.coding.slow.",law.names),paste0("levels.coding.slow.lagged.",law.names))
P = dta[,list.of.laws]
P_1 = dta[,paste0(list.of.laws,"_1")]
P_2 = dta[,paste0(list.of.laws,"_2")]

# constants
N = length(Y)   # number of data points
KX = ncol(X)    # number of covariates in X
KU = ncol(U)    # number of covariates in U
KT = ncol(Time) # number of time points (minus one)
KS = max(S)     # number of states -- not used
KP = ncol(P)    # number of policy effects

# put in list for stan
stan.list = list(y=Y , l_1=L_1 , l_2=L_2 , 
                 offset=offset , offset_1=offset_1 , offset_2=offset_2 ,
                 X=X , U=U , S=S,
                 T=Time , T_1 = Time_1, T_2 = Time_2,
                 P=P , P_1=P_1 , P_2=P_2,
                 N=N , KX=KX , KU=KU , KT=KT , KS=KS , KP=KP)


#  load the stan model
file = "code/stan/NBDAM2.stan"
stan.model = stan_model(file = file)

# find posterior mode to use as initial values
bb= optimizing(stan.model , data=stan.list)
init = list(alpha=bb$par["alpha"],
            delta1=bb$par["delta1"] , delta2=bb$par["delta2"] , 
            gamma_X=bb$par[grepl("gamma_X",names(bb$par))] , 
            gamma_U=bb$par[grepl("gamma_U",names(bb$par))] , 
            zeta=bb$par[grepl("zeta",names(bb$par))] , 
            invphi=bb$par[grepl("^invphi",names(bb$par))] , 
            tau=bb$par["tau"],
            beta=bb$par[grepl("beta",names(bb$par))]
)
init = list(init,init,init,init)

# fit full mcmc
m_stan <- stan(file = file,
               data = stan.list ,
               init = init,
               iter=10000 , 
               control = list(max_treedepth = 14 , adapt_delta=0.9))
out = list(m=m_stan,law.names=list.of.laws,X.names=X.names,U.names=U.names)

# save results
saveRDS(out,file=paste0("results/stan_fits/final_model_",outcome,".RDS"))


