# Odin is developed by Imperial College MRC-IDE: https://mrc-ide.github.io/odin/

transition <- odin::odin({
  N_cycle <- length(cycle_width)
  # Nulliparous means day 0 when mosquitoes emerged from aquatic stages
  cycle_width[] <- user() # First gonotrophic cycle, 3 days
  
  cycle_rate[1:(N_cycle - 1)] <- 1 / cycle_width[i]
  cycle_rate[N_cycle] <- 0
  
  # den = density of the population in each age group
  den[1] <- 1 / (1 + cycle_rate[1] / (1/beta))
  ## to work out the % of the population in each cycle group
  den[2:N_cycle] <- cycle_rate[i - 1] * den[i - 1] / (cycle_rate[i] + (1/beta))
  
  # 1. PARAMETERS ##############################################################
  # Gompertz mortality rate have already in cycle (Clements & Paterson, 1981)
  g1 <- user()
  g2 <- user()
  # With Survival(i) = (exp(-g1/g2*(exp(i*g2)-1))
  
  # Population Dynamics, Eggs -> Larvae -> Mature (White et al., 2011)
  beta <- 21.19*3 # FIXED, Egg deposition per-capita, per-day * 3 days for 1 gonotrophic cycles
  mu0 <- user() # .34; Per-capita daily mortality rate of Eggs & early instar larvae *3 days for 1 gonotrophic cycles
  mu1 <- .035 # FIXED, Per-capita daily mortality rate of Late instar larvae *3 days for 1 gonotrophic cycles
  mu2 <- .25 # FIXED, Per-capita daily mortality rate of pupae
  gE <- 1/(6.64/3) # FIXED, Time required for growth of Eggs to early instar larvae (in 1 cycle)
  gL <- 1/((3.72+0.64)/3)  # FIXED, Time required for growth of early instar larvae to adult mosquitoes (in 1 cycle)
  
  K <- user() # 267800; Saturation coefficient
  gamma_L <- user() # 13.25; Effects of density-dependence on late instars (L) relative to early instars (E)
  
  ## S,E,I are arrays
  lambda <- user() # 10/10; biting rate of mosquitoes per cycle (source: TRANSFIL, 1 month of TRANSFIL has 10 cycles)
  g_HV <- user() # 0.37; Vector competence, the proportion of mosquitoes which pick up the infection when biting an infective host (source: TRANSFIL)
  epsilon <- user() # 1/(12/3); incubation rate of LF in mosquitoes (per-cycle)
  I_H_per_H <- user() # 0.01; Proportion of infected humans in the population with detectable microfilariae
  
  # 2. INITIAL VALUES ############################################################
  initial(E) <- 115
  initial(L) <- 60
  initial(N) <- 52
  
  E_v0 <- 0
  I_v0 <- 0
  initial(S_v[1:N_cycle]) <- den[i]*(V_tot -(E_v_tot+I_v_tot))
  initial(E_v[1:N_cycle]) <- den[i]*E_v0
  initial(I_v[1:N_cycle]) <- den[i]*I_v0
  
  # Dimension & values of arrays
  dim(S_v) <- N_cycle
  dim(E_v) <- N_cycle
  dim(I_v) <- N_cycle
  dim(den) <- N_cycle
  dim(cycle_width) <- user()
  dim(cycle_rate) <- N_cycle
  
  # Define mortality rates in 1 cycle (3 days)
  muE <- mu0*3*(1+(E+L)/K)
  muL <- mu1*3*(1+gamma_L*(E+L)/K)
  
  # 3. DERIVATIVES #############################################################
  temp_deriv_E <- beta*V_tot -E*(gE+muE)
  temp_deriv_L <- E*gE -L*(gL+muL)
  
  deriv(E) <- if (temp_deriv_E > 0) temp_deriv_E else 0 # Checking of the result < 0, throw 0
  deriv(L) <- if (temp_deriv_L > 0) temp_deriv_L else 0 # Checking of the result < 0, throw 0
  deriv(N) <- L*gL*(1-mu2)*.5 -N*(g1*exp((0)*3*g2)) # given mortality as age = 1(*3 for 1 cycle), assume g1*exp((0)*3*g2) = baseline mortality rate at day 0
  
  # Insert deriv(N) as the first cycle of susceptible mosquitoes
  # Disease dynamics
  deriv(S_v[1])         <- -S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*((1+lambda*g_HV*I_H_per_H)) +(N                                                        - cycle_rate[i]*S_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
  deriv(S_v[2:N_cycle]) <- -S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*((1+lambda*g_HV*I_H_per_H)) +(cycle_rate[i-1]*S_v[i-1]*(exp(-g1/g2*(exp((i-1)*g2)-1))) - cycle_rate[i]*S_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
  # dS = [previous state] + [recent state]*(1-exposed-death)
  
  deriv(E_v[1])         <- S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(lambda*g_HV*I_H_per_H) -E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1+epsilon) +(                                                         - cycle_rate[i]*E_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
  deriv(E_v[2:N_cycle]) <- S_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(lambda*g_HV*I_H_per_H) -E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1+epsilon) +(cycle_rate[i-1]*E_v[i-1]*(exp(-g1/g2*(exp((i-1)*g2)-1))) - cycle_rate[i]*E_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
  # dE = [previous state] + [recent state]*(1-infected-death)
  
  deriv(I_v[1])         <- E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1-epsilon) -I_v[i]*(exp(-g1/g2*(exp(i*g2)-1))) +(                                                         - cycle_rate[i]*I_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
  deriv(I_v[2:N_cycle]) <- E_v[i]*(exp(-g1/g2*(exp(i*g2)-1)))*(1-epsilon) -I_v[i]*(exp(-g1/g2*(exp(i*g2)-1))) +(cycle_rate[i-1]*I_v[i-1]*(exp(-g1/g2*(exp((i-1)*g2)-1))) - cycle_rate[i]*I_v[i]*(exp(-g1/g2*(exp(i*g2)-1))))
  # dI = [previous state] + [recent state]*(1-death)
  
  # Cumulative S-E-I
  S_v_tot <- sum(S_v)
  E_v_tot <- sum(E_v)
  I_v_tot <- sum(I_v)
  V_tot <- S_v_tot + E_v_tot + I_v_tot
  
  output(S_v_tot) <- S_v_tot
  output(E_v_tot) <- E_v_tot
  output(I_v_tot) <- I_v_tot
  output(V_tot) <- V_tot
  
  output(prev) <- I_v_tot / V_tot
  output(pos) <- (E_v_tot+I_v_tot) / V_tot
  
  config(base) <- "transition"
})

# Function to retrieve species-specific parameters #############################
get_species <- function(species = "gambiae") {
  # Select parameters based on species
  if (species == "gambiae") {
    g1 <- .356
    g2 <- .097
  } else if (species == "arabiensis") {
    g1 <- .339
    g2 <- .225
  } else {
    stop("Only 'gambiae' or 'arabiensis' are available.")
  }
  
  return(list(g1 = g1, g2 = g2))
}
