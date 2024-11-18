# RUN odin!
source("model_dynamics/deterministic_odin.R") # Collected functions stored here!
# Additional script for sensitivity analyses when I stored the output into *.rds
# For chosen sensitivity analysis of some parameters only

# 2. Sensitivity Analyses ######################################################
# K ############################################################################
run_K_matrix <- function(species) {
  species_params <- get_species(species)  # Assumes get_species is defined in deterministic_odin.R
  
  # Set parameters with species-specific values
  params <- list(
    cycle_width = seq(3, 30, by = 3),
    g1 = species_params$g1,
    g2 = species_params$g2,
    mu0 = .34,  # Per-capita daily mortality rate
    K = seq(0, 268000, by = 100),  # Range for saturation coefficient
    gamma_L = 13.25,  # Density-dependence effect on late instars
    lambda = 10/10,  # Biting rate of mosquitoes per cycle
    g_HV = .37,  # Vector competence
    epsilon = 1/(12/3),  # Incubation rate of LF in mosquitoes (per-cycle)
    I_H_per_H = .01  # Proportion of infected humans
  )
  
  # Define timesteps and preallocate matrices for results
  timesteps <- seq(0, 2000, by = 1)
  K_values <- params$K
  
  E_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  L_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  N_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  
  S_v_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  E_v_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  I_v_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  
  V_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  
  Prev_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  Pos_mtx <- matrix(NA, nrow = length(timesteps), ncol = length(K_values))
  
  # Loop through each value of K and run the model
  for (i in seq_along(K_values)) {
    current_params <- params
    current_params$K <- K_values[i]
    
    mod <- transition$new(user = current_params)
    y <- mod$run(timesteps)
    
    # Store the results for each K value in the corresponding matrix column
    E_mtx[, i] <- y[, "E"]
    L_mtx[, i] <- y[, "L"]
    N_mtx[, i] <- y[, "N"]
    
    S_v_mtx[, i] <- y[, "S_v_tot"]
    E_v_mtx[, i] <- y[, "E_v_tot"]
    I_v_mtx[, i] <- y[, "I_v_tot"]
    
    V_mtx[, i] <- y[, "V_tot"]
    
    Prev_mtx[, i] <- y[, "prev"]
    Pos_mtx[, i] <- y[, "pos"]
    
    # Display progress
    print(paste("Processing K =", K_values[i]))
  }
  
  # Combine all matrices into a single list
  result_list <- list(
    E_mtx = E_mtx,
    L_mtx = L_mtx,
    N_mtx = N_mtx,
    S_v_mtx = S_v_mtx,
    E_v_mtx = E_v_mtx,
    I_v_mtx = I_v_mtx,
    V_mtx = V_mtx,
    Prev_mtx = Prev_mtx,
    Pos_mtx = Pos_mtx
  )
  
  # Return the list of matrices
  return(result_list)
}

# Run the model for different species
y_gambiae <- run_K_matrix("gambiae")
y_arabiensis <- run_K_matrix("arabiensis")

saveRDS(y_gambiae, "outputs/odin_K_matrices_An_gambiae.rds")
saveRDS(y_arabiensis, "outputs/odin_K_matrices_An_arabiensis.rds")

