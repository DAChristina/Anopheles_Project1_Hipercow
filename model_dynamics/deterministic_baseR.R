## Anopheles Population Dynamics Model

# Survival, Gompertz (Clements & Paterson, 1981) ###############################
# By using these parameters, age already in cycles,
# TRANSFIL time step (1 month) equals to 10 gonotrophic cycle of mosquitoes
g1 <- 0.356
g2 <- 0.097

survpercycle <- rep(0,10) # 8 cycles because the last 2 cycles have 0 value
for (age in 0:10){
  survpercycle[age] = 1*exp(-g1/g2*(exp(age*g2)-1)) # survival per cycle with daily mortality update
}
survpercycle

plot(seq(1,10,by=1),survpercycle, type = "p")

# Population Dynamics, Eggs -> Larvae -> Mature (White et al., 2011) ###########
beta <- 21.19*3 # egg deposition rate per-day * 3 days for 1 gonotrophic cycles
mu0 <- .034 # per-capita daily mortality rate of Eggs & early instar larvae * 3 days for 1 gonotrophic cycles
mu1 <- .035 # per-capita daily mortality rate of Late instar larvae * 3 days for 1 gonotrophic cycles
mu2 <- .25 # per-capita daily mortality rate of pupae * 3 days for 1 gonotrophic cycles
gE <- 1/(6.64/3) # time required for growth of Eggs to early instar larvae (in 1 cycle)
gL <- 1/((3.72+0.64)/3)  # time required for growth of early instar larvae to adult mosquitoes (in 1 cycle)

K <- 1000 # carrying capacity
sg <- 13.25 # effects of density-dependence on late instars (L) relative to early instars (E)

## S,E,I are arrays
lambda <- 10/10 # biting rate of mosquitoes per-cycle (source: TRANSFIL, 1 month of TRANSFIL has 10 cycles)
InfecMosq <- 0.37 # proportion of mosquitoes which pick up infection when biting an infective host (source: TRANSFIL)
epsilon <- 1/(14/3) # incubation rate of LF in mosquitoes (per-cycle, which means 3 days)
InfHuman <- 0.01 # 0.01 is trial # Proportion of infected human in the population with detectable microfilariae

# E is eggs, M is array of adults from M1 to M10 (in cycles) ###################
vectorPopDyn <- function(Eggs,Larvae,M){
  tmpEggs = Eggs
  tmpL = Larvae
  tmpM = M
  
  muE = mu0*(1+(tmpEggs+tmpL)/K)
  muL = mu1*(1+sg*(tmpEggs+tmpL)/K)
  
  Eggs = tmpEggs*exp(-muE)*(1-gE) + beta*sum(tmpM)
  Larvae = tmpL*exp(-muL)*(1-gL*exp(-mu2)) + tmpEggs*exp(-muE)*gE
  M = c(tmpL*exp(-muL)*gL*exp(-mu2)*.5,head(tmpM,-1)*survpercycle)
  
  if(Eggs<0) Eggs=0
  if(Larvae<0) Larvae = 0
  
  return(c(Eggs,Larvae,M))
}

# Have not changed the rs initial values yet
rs <- c(115, 60, 52,45,30,11,1.5,0.02,0,0,0) #inital values
timesteps <- 2000 #number of steps
res <- matrix(NA,nrow=timesteps,ncol=11) #saving outputs matrix

## run timesteps
for (i in 1:timesteps){
  rs <- vectorPopDyn(rs[1],rs[2],rs[3:11])
  res[i,] <- rs 
}

plot(res[,1],type="l")


### Disease dynamics ###########################################################
### DisDyn 2: Eggs-Larvae-Nulliparous-S-E-I, survival instead of deaths ########
E <- 21.19*3
L <- 0
N <- 0

S_v <- rep(0,10)
E_v <- rep(0,10)
I_v <- rep(0,10)

# 0. CYCLE ARRAYS either odin or I'm a dumb ass and don't understand how to use loops
N_cycle <- 10

cycle_width <- numeric(N_cycle)
for (i in 1:N_cycle) {
  cycle_width[i] <- 3 * i
}

cycle_rate <- numeric(N_cycle)
for (i in 1:(N_cycle - 1)) {
  cycle_rate[i] <- 1 / cycle_width[i]
}
cycle_rate[N_cycle] <- 0

vectorDisDyn <- function(E,L,N,S_v,E_v,I_v){
  tmpE = E
  tmpL = L
  tmpN = N
  tmpS_v = S_v
  tmpE_v = E_v
  tmpI_v = I_v
  
  muE = mu0^3*(1+(tmpE+tmpL)/K)
  muL = mu1^3*(1+sg*(tmpE+tmpL)/K)
  
  # Aquatic Phases
  # By using survived
  E = tmpE*exp(-muE)*(1-gE) + beta*sum(tmpS_v+tmpE_v+tmpI_v)
  L = tmpL*exp(-muL)*(1-gL*exp(-mu2)) + tmpE*exp(-muE)*gE
  N = tmpN*exp(-g1/g2*(exp(0*3*g2)-1)) + tmpL*exp(-muL)*(gL*exp(-mu2))*.5
  
  # By using mortality (hazard function), FAILED, Eggs will be increasing
  # E = beta*sum(tmpS_v+tmpE_v+tmpI_v) -tmpE*(gE+muE)
  # L = tmpE*gE -tmpL*(gL+muL)
  # N = tmpL*gL*(1-mu2)*.5 -tmpN*(g1*exp((0)*3*g2)) # given mortality as age = 1(*3 for 1 cycle), assume g1*exp((0)*3*g2) = baseline mortality rate at day 0
  
  # Adult Mosquitoes
  for (i in 1:N_cycle) {
    if (i == 1) {
      S_v[i] <- -S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*((1+lambda*InfecMosq*InfHuman)) +(N - cycle_rate[i]*S_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
    } else {
      S_v[i] <- -S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*((1+lambda*InfecMosq*InfHuman)) +(cycle_rate[i-1]*S_v[i-1]*(exp(-g1/g2*(exp((i-1)*g2)-1))) - cycle_rate[i]*S_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
    }
    
    if (i == 1) {
      E_v[i] <- S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(lambda*InfecMosq*InfHuman) -E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1+epsilon) - cycle_rate[i]*E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))
    } else {
      E_v[i] <- S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(lambda*InfecMosq*InfHuman) -E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1+epsilon) +(cycle_rate[i-1]*E_v[i-1]*(exp(-g1/g2*(exp((i-1)*g2)-1))) - cycle_rate[i]*E_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
    }
    
    if (i == 1) {
      I_v[i] <- E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1-epsilon) -I_v[i]*(exp(-g1/g2*(exp(i*g2)-1))) - cycle_rate[i]*I_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))
    } else {
      I_v[i] <- E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1-epsilon) -I_v[i]*(exp(-g1/g2*(exp(i*g2)-1))) +(cycle_rate[i-1]*I_v[i-1]*(exp(-g1/g2*(exp((i-1)*g2)-1))) - cycle_rate[i]*I_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
    }
  }
  
  if(E<0) E <- 0
  if(L<0) L <- 0
  if(N<0) N <- 0
  
  for (i in 1:N_cycle) {
    if(S_v[i] <0) S_v[i] <- 0
    if(E_v[i] <0) E_v[i] <- 0
    if(I_v[i] <0) I_v[i] <- 0
  }
  
  
  return(c(E, L, N, S_v, E_v, I_v))
}

rs <- c(115,60,52,rep(0,3*10)) # Initial values, ncol = 30
timesteps <- 500 #number of steps
res <- matrix(NA,nrow=timesteps,ncol=length(rs)) #saving outputs matrix

## run timesteps
for (i in 1:timesteps){
  # rs <- vectorDisDyn(rs[1],rs[2],rs[3],rs[4:12],rs[13:21],rs[22:30]) # for 9 cycles
  rs <- vectorDisDyn(rs[1],rs[2],rs[3],rs[4:13],rs[14:23],rs[24:33]) # for 10 cycles
  res[i,] <- rs 
}

plot(res[,1],type="l") # Eggs
plot(res[,2], col = "steelblue", type = "l") # Larvae
plot(res[,3], col = "blue", type = "l") # Nulliparous
plot(rowSums(res[,4:13]), col = "Orange", type = "l") # Susceptibles
plot(rowSums(res[,14:23]), col = "Orange", type = "l") # Exposed
plot(rowSums(res[,24:33]), col = "red", type="l") # Infected

plot(rowSums(res[,4:33]), col = "blue", type="l") # All adult parous mosquitoes
tail(rowSums(res[,4:33]))
plot(rowSums(res[,24:33])/rowSums(res[,4:33]), col = "red", type="l") # Proportion of Infectives
tail(rowSums(res[,24:33])/rowSums(res[,4:33]))

# Additional proportion of infected mosquitoes (whether they have mfE, L1, L2, OR L3)
plot(rowSums(res[,14:33])/rowSums(res[,4:33]), col = "red", type="l")
tail(rowSums(res[,14:33])/rowSums(res[,4:33]))
