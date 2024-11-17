# RUN odin!
source("model_dynamics/deterministic_odin.R") # Collected functions stored here!

# 1. Default parameters based on thesis ########################################
# Function to create and run the model
run_0default <- function(species) {
  species_params <- get_species(species) # get_species defined in deterministic_odin.R
  
  params <- list(
    cycle_width = seq(3, 30, by = 3),
    g1 = species_params$g1,
    g2 = species_params$g2,
    mu0 = .34, # Per-capita daily mortality rate of Eggs & early instar larvae *3 days for 1 gonotrophic cycles
    K = 267800, # Saturation coefficient
    gamma_L = 13.25, # Effects of density-dependence on late instars (L) relative to early instars (E)
    lambda = 10/10, # Biting rate of mosquitoes per cycle (1 cycle per 3 days)
    g_HV = .37, # Vector competence
    epsilon = 1/(12/3), # Incubation rate of LF in mosquitoes (per-cycle)
    I_H_per_H = .01 # Proportion of infected humans in the population
  )
  
  mod <- transition$new(user = params)
  timesteps <- seq(0, 2000, by = 1)
  result <- mod$run(timesteps)
  
  return(result)
}

# Run the model for different species
y_gambiae <- run_0default("gambiae")
y_arabiensis <- run_0default("arabiensis")

write.csv(y_gambiae, "outputs/odin_0defaut_An_gambiae.csv", row.names = F)
write.csv(y_arabiensis, "outputs/odin_0defaut_An_arabiensis.csv", row.names = F)


# 2. Sensitivity Analyses ######################################################
# 2.1. I_H_per_H ###############################################################
run_I_H_per_H <- function(species) {
  species_params <- get_species(species) # Assumes get_species function is defined in deterministic_odin.R
  
  params <- list(
    cycle_width = seq(3, 30, by = 3),
    g1 = species_params$g1,
    g2 = species_params$g2,
    mu0 = .34, # Per-capita daily mortality rate of Eggs & early instar larvae *3 days for 1 gonotrophic cycle
    K = 267800, # Saturation coefficient
    gamma_L = 13.25, # Density-dependence effect on late instars relative to early instars
    lambda = 10/10, # Biting rate of mosquitoes per cycle
    g_HV = .37, # Vector competence
    epsilon = 1/(12/3), # Incubation rate of LF in mosquitoes (per-cycle)
    I_H_per_H = c(seq(0, 0.02, by = 0.001), seq(0.02, 1, by = 0.01)) # Prevalence ranges for infective humans
  )
  
  # Initialize vectors for storage
  S_v_loop <- numeric(length(params$I_H_per_H))
  E_v_loop <- numeric(length(params$I_H_per_H))
  I_v_loop <- numeric(length(params$I_H_per_H))
  V_loop <- numeric(length(params$I_H_per_H))
  Prev_loop <- numeric(length(params$I_H_per_H))
  Pos_loop <- numeric(length(params$I_H_per_H))
  
  # Loop through each value of I_H_per_H
  for (i in seq_along(params$I_H_per_H)) {
    # Update InfHuman in parameters
    current_params <- params
    current_params$I_H_per_H <- params$I_H_per_H[i]
    
    # Create the model and run it
    mod <- transition$new(user = current_params)
    timesteps <- seq(0, 2000, by = 1)
    y <- mod$run(timesteps)
    
    # Store results for the last timestep
    S_v_loop[i] <- tail(y[,"S_v_tot"], 1)
    E_v_loop[i] <- tail(y[,"E_v_tot"], 1)
    I_v_loop[i] <- tail(y[,"I_v_tot"], 1)
    V_loop[i] <- tail(y[,"V_tot"], 1)
    Prev_loop[i] <- tail(y[,"prev"], 1)
    Pos_loop[i] <- tail(y[,"pos"], 1)
  }
  
  # Create a df to store the output
  Output_InfHuman <- data.frame(
    I_H_per_H = params$I_H_per_H,
    S_v_loop = S_v_loop,
    E_v_loop = E_v_loop,
    I_v_loop = I_v_loop,
    V_loop = V_loop,
    Prev_loop = Prev_loop,
    Pos_loop = Pos_loop
  )
  
  return(Output_InfHuman)
}

# Run the model for different species
y_gambiae <- run_I_H_per_H("gambiae")
y_arabiensis <- run_I_H_per_H("arabiensis")

write.csv(y_gambiae, "outputs/odin_IHperH_An_gambiae.csv", row.names = F)
write.csv(y_arabiensis, "outputs/odin_IHperH_An_arabiensis.csv", row.names = F)


# 2.3. epsilon #################################################################
run_epsilon <- function(species) {
  species_params <- get_species(species) # Assumes get_species function is defined in deterministic_odin.R
  
  params <- list(
    cycle_width = seq(3, 30, by = 3),
    g1 = species_params$g1,
    g2 = species_params$g2,
    mu0 = .34, # Per-capita daily mortality rate of Eggs & early instar larvae *3 days for 1 gonotrophic cycle
    K = 267800, # Saturation coefficient
    gamma_L = 13.25, # Density-dependence effect on late instars relative to early instars
    lambda = 10/10, # Biting rate of mosquitoes per cycle
    g_HV = .37, # Vector competence
    epsilon = seq(0.1875, 0.5, by = 0.01), # 1/(14/3) = 0.2142857; we wanna test 1/(7/3) to 1/(16/3)
    I_H_per_H = .01 # Proportion of infected humans in the population
  )
  
  # Initialize vectors for storage
  S_v_loop <- numeric(length(params$epsilon))
  E_v_loop <- numeric(length(params$epsilon))
  I_v_loop <- numeric(length(params$epsilon))
  V_loop <- numeric(length(params$epsilon))
  Prev_loop <- numeric(length(params$epsilon))
  Pos_loop <- numeric(length(params$epsilon))
  
  # Loop through each value of epsilon
  for (i in seq_along(params$epsilon)) {
    # Update InfHuman in parameters
    current_params <- params
    current_params$epsilon <- params$epsilon[i]
    
    # Create the model and run it
    mod <- transition$new(user = current_params)
    timesteps <- seq(0, 2000, by = 1)
    y <- mod$run(timesteps)
    
    # Store results for the last timestep
    S_v_loop[i] <- tail(y[,"S_v_tot"], 1)
    E_v_loop[i] <- tail(y[,"E_v_tot"], 1)
    I_v_loop[i] <- tail(y[,"I_v_tot"], 1)
    V_loop[i] <- tail(y[,"V_tot"], 1)
    Prev_loop[i] <- tail(y[,"prev"], 1)
    Pos_loop[i] <- tail(y[,"pos"], 1)
  }
  
  # Create a df to store the output
  Output_epsilon <- data.frame(
    epsilon = params$epsilon,
    S_v_loop = S_v_loop,
    E_v_loop = E_v_loop,
    I_v_loop = I_v_loop,
    V_loop = V_loop,
    Prev_loop = Prev_loop,
    Pos_loop = Pos_loop
  )
  
  return(Output_epsilon)
}

# Run the model for different species
y_gambiae <- run_epsilon("gambiae")
y_arabiensis <- run_epsilon("arabiensis")

write.csv(y_gambiae, "outputs/odin_epsilon_An_gambiae.csv", row.names = F)
write.csv(y_arabiensis, "outputs/odin_epsilon_An_arabiensis.csv", row.names = F)


# 2.4. gamma_L #################################################################
run_gamma_L <- function(species) {
  species_params <- get_species(species) # Assumes get_species function is defined in deterministic_odin.R
  
  params <- list(
    cycle_width = seq(3, 30, by = 3),
    g1 = species_params$g1,
    g2 = species_params$g2,
    mu0 = .34, # Per-capita daily mortality rate of Eggs & early instar larvae *3 days for 1 gonotrophic cycle
    K = 267800, # Saturation coefficient
    gamma_L = seq(9.82, 18, by = 0.01), # 13.25; Density-dependence effect on late instars relative to early instars
    lambda = 10/10, # Biting rate of mosquitoes per cycle
    g_HV = .37, # Vector competence
    epsilon = 1/(12/3), # Incubation rate of LF in mosquitoes (per-cycle)
    I_H_per_H = .01 # Proportion of infected humans in the population
  )
  
  # Initialize vectors for storage
  Eggs <- numeric(length(params$gamma_L))
  Larvae <- numeric(length(params$gamma_L))
  Nullipars <- numeric(length(params$gamma_L))
  
  S_v_loop <- numeric(length(params$gamma_L))
  E_v_loop <- numeric(length(params$gamma_L))
  I_v_loop <- numeric(length(params$gamma_L))
  V_loop <- numeric(length(params$gamma_L))
  Prev_loop <- numeric(length(params$gamma_L))
  Pos_loop <- numeric(length(params$gamma_L))
  
  # Loop through each value of gamma_L
  for (i in seq_along(params$gamma_L)) {
    # Update InfHuman in parameters
    current_params <- params
    current_params$gamma_L <- params$gamma_L[i]
    
    # Create the model and run it
    mod <- transition$new(user = current_params)
    timesteps <- seq(0, 2000, by = 1)
    y <- mod$run(timesteps)
    
    # Store results for the last timestep
    Eggs <- tail(y[,"E"], 1)
    Larvae <- tail(y[,"L"], 1)
    Nullipars <- tail(y[,"N"], 1)
    
    S_v_loop[i] <- tail(y[,"S_v_tot"], 1)
    E_v_loop[i] <- tail(y[,"E_v_tot"], 1)
    I_v_loop[i] <- tail(y[,"I_v_tot"], 1)
    V_loop[i] <- tail(y[,"V_tot"], 1)
    Prev_loop[i] <- tail(y[,"prev"], 1)
    Pos_loop[i] <- tail(y[,"pos"], 1)
  }
  
  # Create a df to store the output
  Output_gamma_L <- data.frame(
    gamma_L = params$gamma_L,
    Eggs = Eggs,
    Larvae = Larvae,
    Nullipars = Nullipars,
    S_v_loop = S_v_loop,
    E_v_loop = E_v_loop,
    I_v_loop = I_v_loop,
    V_loop = V_loop,
    Prev_loop = Prev_loop,
    Pos_loop = Pos_loop
  )
  
  return(Output_gamma_L)
}

# Run the model for different species
y_gambiae <- run_gamma_L("gambiae")
y_arabiensis <- run_gamma_L("arabiensis")

write.csv(y_gambiae, "outputs/odin_gamma_L_An_gambiae.csv", row.names = F)
write.csv(y_arabiensis, "outputs/odin_gamma_L_An_arabiensis.csv", row.names = F)


# 2.5. lambda ##################################################################
run_lambda <- function(species) {
  species_params <- get_species(species) # Assumes get_species function is defined in deterministic_odin.R
  
  params <- list(
    cycle_width = seq(3, 30, by = 3),
    g1 = species_params$g1,
    g2 = species_params$g2,
    mu0 = .34, # Per-capita daily mortality rate of Eggs & early instar larvae *3 days for 1 gonotrophic cycle
    K = 267800, # Saturation coefficient
    gamma_L = 13.25, # Density-dependence effect on late instars relative to early instars
    lambda = seq(0, 100, by = 0.1), # Assume biting rates can increase tenfolds outdoor
    g_HV = .37, # Vector competence
    epsilon = 1/(12/3), # Incubation rate of LF in mosquitoes (per-cycle)
    I_H_per_H = .01 # Proportion of infected humans in the population
  )
  
  # Initialize vectors for storage
  S_v_loop <- numeric(length(params$lambda))
  E_v_loop <- numeric(length(params$lambda))
  I_v_loop <- numeric(length(params$lambda))
  V_loop <- numeric(length(params$lambda))
  Prev_loop <- numeric(length(params$lambda))
  Pos_loop <- numeric(length(params$lambda))
  
  # Loop through each value of lambda
  for (i in seq_along(params$lambda)) {
    # Update InfHuman in parameters
    current_params <- params
    current_params$lambda <- params$lambda[i]
    
    # Create the model and run it
    mod <- transition$new(user = current_params)
    timesteps <- seq(0, 2000, by = 1)
    y <- mod$run(timesteps)
    
    # Store results for the last timestep
    S_v_loop[i] <- tail(y[,"S_v_tot"], 1)
    E_v_loop[i] <- tail(y[,"E_v_tot"], 1)
    I_v_loop[i] <- tail(y[,"I_v_tot"], 1)
    V_loop[i] <- tail(y[,"V_tot"], 1)
    Prev_loop[i] <- tail(y[,"prev"], 1)
    Pos_loop[i] <- tail(y[,"pos"], 1)
  }
  
  # Create a df to store the output
  Output_lambda <- data.frame(
    lambda = params$lambda,
    S_v_loop = S_v_loop,
    E_v_loop = E_v_loop,
    I_v_loop = I_v_loop,
    V_loop = V_loop,
    Prev_loop = Prev_loop,
    Pos_loop = Pos_loop
  )
  
  return(Output_lambda)
}

# Run the model for different species
y_gambiae <- run_lambda("gambiae")
y_arabiensis <- run_lambda("arabiensis")

write.csv(y_gambiae, "outputs/odin_lambda_An_gambiae.csv", row.names = F)
write.csv(y_arabiensis, "outputs/odin_lambda_An_arabiensis.csv", row.names = F)


# 2.6. mu0 #####################################################################
run_mu0 <- function(species) {
  species_params <- get_species(species) # Assumes get_species function is defined in deterministic_odin.R
  
  params <- list(
    cycle_width = seq(3, 30, by = 3),
    g1 = species_params$g1,
    g2 = species_params$g2,
    mu0 = seq(0.025, 0.045, by = 0.001), # .34; Per-capita daily mortality rate of Eggs & early instar larvae *3 days for 1 gonotrophic cycle
    K = 267800, # Saturation coefficient
    gamma_L = 13.25, # Density-dependence effect on late instars relative to early instars
    lambda = 10/10, # Biting rate of mosquitoes per cycle
    g_HV = .37, # Vector competence
    epsilon = 1/(12/3), # Incubation rate of LF in mosquitoes (per-cycle)
    I_H_per_H = .01 # Proportion of infected humans in the population
  )
  
  # Initialize vectors for storage
  Eggs <- numeric(length(params$mu0))
  Larvae <- numeric(length(params$mu0))
  Nullipars <- numeric(length(params$mu0))
  
  S_v_loop <- numeric(length(params$mu0))
  E_v_loop <- numeric(length(params$mu0))
  I_v_loop <- numeric(length(params$mu0))
  V_loop <- numeric(length(params$mu0))
  Prev_loop <- numeric(length(params$mu0))
  Pos_loop <- numeric(length(params$mu0))
  
  # Loop through each value of mu0
  for (i in seq_along(params$mu0)) {
    # Update InfHuman in parameters
    current_params <- params
    current_params$mu0 <- params$mu0[i]
    
    # Create the model and run it
    mod <- transition$new(user = current_params)
    timesteps <- seq(0, 2000, by = 1)
    y <- mod$run(timesteps)
    
    # Store results for the last timestep
    Eggs <- tail(y[,"E"], 1)
    Larvae <- tail(y[,"L"], 1)
    Nullipars <- tail(y[,"N"], 1)
    
    S_v_loop[i] <- tail(y[,"S_v_tot"], 1)
    E_v_loop[i] <- tail(y[,"E_v_tot"], 1)
    I_v_loop[i] <- tail(y[,"I_v_tot"], 1)
    V_loop[i] <- tail(y[,"V_tot"], 1)
    Prev_loop[i] <- tail(y[,"prev"], 1)
    Pos_loop[i] <- tail(y[,"pos"], 1)
  }
  
  # Create a df to store the output
  Output_mu0 <- data.frame(
    mu0 = params$mu0,
    Eggs = Eggs,
    Larvae = Larvae,
    Nullipars = Nullipars,
    S_v_loop = S_v_loop,
    E_v_loop = E_v_loop,
    I_v_loop = I_v_loop,
    V_loop = V_loop,
    Prev_loop = Prev_loop,
    Pos_loop = Pos_loop
  )
  
  return(Output_mu0)
}

# Run the model for different species
y_gambiae <- run_mu0("gambiae")
y_arabiensis <- run_mu0("arabiensis")

write.csv(y_gambiae, "outputs/odin_mu0_An_gambiae.csv", row.names = F)
write.csv(y_arabiensis, "outputs/odin_mu0_An_arabiensis.csv", row.names = F)

