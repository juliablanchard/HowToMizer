### test only

Fmat<-read.csv("data/fmatWeighted.csv")
Catchmat<-read.csv("data/catchesMat.csv")


# read in baseline simulations

optim_sim <- readRDS("optim_sim.RDS")
# set the param object 
params_optim<-optim_sim@params
# run model from the equiibrium to check it's OK
params_optim <-setParams(params_optim)
sim_optim <- project(params_optim, effort = 1, t_max = 100, dt=0.1,initial_n = optim_sim@n[100,,],initial_n_pp = optim_sim@n_pp[100,])
plot(sim_optim)
plotYield(sim_optim)

#set up effort multipliers

effort_multiplier<-seq(0,10,0.1)

# store output

eqyield<-matrix(data=NA,nrow=length(effort_multiplier), ncol=12)

for (i in 1:length(effort_multiplier)){
sim_yield <- project(params_optim, effort = effort_multiplier[i], t_max = 100, dt=0.1,initial_n = sim_guessed@n[100,,],initial_n_pp = sim_guessed@n_pp[100,])
eqyield[i,]<-getYield(sim_yield)[100,]
}

cFs<-sim_optim@params@catchability

###### add model and data to plots

pdf("catchcurves.pdf")
for (sp in c(1:7,9:12)) {
 # plot(Fmat[,sp+1],Catchmat[,sp+1],xlab="Fishing mortality rate",ylab="Catch") 
  plot(effort_multiplier*cFs[sp],eqyield[,sp]/1e6,type="l")
  title(names(Fmat)[sp+1])
}

dev.off()

