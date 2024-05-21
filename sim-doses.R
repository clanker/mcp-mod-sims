# Define libraries
library(DoseFinding)
library(gridExtra)
library(tidyverse)

sim_doses_list <- rep(list(c(0, 250, 500, 1000),
                       c(0, 333, 667, 1000),
                       c(0, 500, 1000)), 2)
sim_n_list <- rep(c(120, 360), each = 3)
sim_symm_list <- rep(c("asymmetric", "symmetric", "reduced"), 3)
B <- 5000

model_name_list <- c("linear", "quadratic", "exponential", "emax1", "emax2", "sigEmax1", "sigEmax2")
mod_list <- c("linear", "quadratic", "exponential", "emax", "sigEmax")

# storage for results
res_sim <- as_tibble(matrix(0, nrow = 0, ncol = 7, dimnames = list(NULL, model_name_list)))
res_aic <- as_tibble(matrix(0, nrow = 0, ncol = 5, dimnames = list(NULL, mod_list)))
res_par <- as_tibble(matrix(NA, nrow = 0, ncol = 7, dimnames = list(NULL, c("id", "n", "K", "symmetric", "mu", "S", "df"))))

for (sim_num in 1:length(sim_doses_list)) {
  n <- sim_n_list[[sim_num]]
  sim_doses <- sim_doses_list[[sim_num]]
  K <- length(sim_doses)

  cat("Starting n =", n, " doses =", sim_doses, " K =", K, "...\n")

  # define true dose-response curve
  mod_true <- Mods(emax = 200, doses = sim_doses, placEff = 2, maxEff = 2.5)
  mod_curve <- plotMods(mod_true, nPoints = 1001)$data
  group_n <- rep(n/K, K)
  group_var <- rep(9, K)
  group_means <- mod_curve$response[sim_doses + 1]
  sample_means <- rep(group_means, group_n)

  # define candidate models
  mods <- Mods(linear = NULL,
               quadratic = -0.0008,
               exponential = 300,
               emax = c(100, 400),
               sigEmax = matrix(c(250, 5, 400, 8), byrow = TRUE, 2, 2),
               placEff = 2, maxEff = 2, doses = sim_doses)

  # loop over the B simulated trials
  for (b in 1:B) {

    row_num <- B*(sim_num-1) + b

    # random seed for repeatability
    # Note: I want the same residuals across scenarios, so I reuse the seed
    set.seed(row_num)

    # covariance matrix is diagonal
    random_values <- rnorm(sum(group_n), 0, rep(group_var, group_n) |> sqrt())

    # compose Phase 2B study data frame
    study2b_data <- data.frame(dose = rep(sim_doses, group_n),
                               resp = sample_means + random_values)

    # get linear model estimates
    fitlm <- lm(resp ~ factor(dose) - 1, data = study2b_data)
    mu_hat <- coef(fitlm)
    S_hat <- vcov(fitlm)
    anova_df <- fitlm$df.residual

    # record the simulation parameters (with some lm fits)
    res_par[row_num, "id"] <- b
    res_par[row_num, "n"] <- n
    res_par[row_num, "K"] <- K
    res_par[row_num, "symmetric"] <- sim_symm_list[sim_num]
    res_par[row_num, "mu"] <- nest(tibble(dose = sim_doses, mu = as.vector(mu_hat)))
    res_par[row_num, "S"] <- S_hat[1, 1]
    res_par[row_num, "df"] <- anova_df

    # Run MCP-MOD
    out <- MCPMod(dose, resp, study2b_data, mods, Delta=2, selModel = "aveAIC")

    # Record MCP step results
    res_sim[row_num, 1:7] <- attr(out$MCTtest$tStat, "pVal") |> t()

    # combine AIC results
    if (length(out$selMod) > 0) {

      # Record MOD step results
      res_aic[row_num, names(out$selMod)] <- t(out$selMod)
      res_sim[row_num, "dose_aic"] <- sum(out$doseEst * out$selMod)
      selOne <- (1.001 * out$selMod / max(out$selMod)) |> floor()   # use only the max
      selOne <- selOne / sum(selOne)   # perhaps other methods were within 0.1% of best...
      res_sim[row_num, "dose_best"] <- sum(out$doseEst * selOne)
    } else {

      # If out$selMod is NULL, then none of the models passed the MCP step
      res_aic[row_num, ] <- NA
    }

    if (b %% 200 == 0) cat("*")
    if (b %% 1000 == 0) cat("=")
  }
  cat("Completed ", sim_num, "\n")
}

# MCP-MOD weights according to AIC, aligns with rows of res_par
mat_wts <- res_aic |>
  dplyr::mutate(id = 1:nrow(res_aic)) |>
  tidyr::pivot_longer(linear:sigEmax, names_to = "model", values_to = "weight") |>
  tidyr::replace_na(list(weight = 0)) |>
  tidyr::pivot_wider(names_from = "model", values_from = "weight") |>
  dplyr::select(-id)

# assigns best model (after MCP-MOD modeling step) to res_par$best
col_max <- mat_wts |>
  apply(1, which.max)
flag_no_models <- (rowSums(mat_wts) == 0)
col_max[flag_no_models] <- 6
res_par$best <- factor(c(names(res_aic), "none"),
                       levels = c(names(res_aic), "none"))[col_max]

# assigns best candidate model fit to res_sim$lowest (for lowest p-value)
col_max2 <- res_sim |>
  dplyr::select(linear:sigEmax2) |>
  apply(1, which.min)
res_sim$lowest <- factor(names(res_sim)[1:7],
                         levels = names(res_sim)[1:7])[col_max2]


# Perform a bootstrap analysis for calculated TD
dat <- res_par |>
  dplyr::select(id:symmetric) |>
  dplyr::bind_cols(res_sim |>
                     dplyr::select(dose_aic, dose_best)) |>
  dplyr::mutate(dose_aic = pmin(dose_aic, 1000),
                dose_best = pmin(dose_best, 1000))


dat_summary <- dat |>
  dplyr::group_by(n, K, symmetric) |>
  dplyr::count(n)

for (i in 1:nrow(dat_summary)) {

  dat_sub <- dat |>
    dplyr::filter(n == dat_summary$n[i],
                  K == dat_summary$K[i],
                  symmetric == dat_summary$symmetric[i]) |>
    dplyr::select(dose_aic, dose_best)
  rmse_aic <- rmse_best <- rep(0, 4000)

  for (j in 1:4000) {
    set.seed(j)
    id_list <- sample(1:nrow(dat_sub), replace = TRUE)

    rmse_aic[j] <- sqrt(mean((dat_sub$dose_aic[id_list] - 400)^2, na.rm = TRUE))
    rmse_best[j] <- sqrt(mean((dat_sub$dose_aic[id_list] - 400)^2, na.rm = TRUE))
  }
    dat_summary$dose_aic[i] = tibble(rmse = rmse_aic)
    dat_summary$dose_best[i] = tibble(rmse = rmse_best)

}

dat_summary$dose_aic[1]

res_rmse <- dat_summary |>
  dplyr::select(-nn) |>
  unnest(c(dose_aic, dose_best))

save(res_rmse, res_par, res_sim, res_aic, file = "sim-doses.Rdata")


## Analyze the results

# I envision a 3 panel plot (1 row) with 6 horizontal boxplots
#   for the scenarios, the panels being:
#   (1) method failure
#   (2) estimated TD (aic-weighting)
#   (3) estimated TD (best model)

# 1. Compare MCP-MOD failure across the scenarios
# Panel (1)



# 2. Compare boxplots for TD:aic and TD:best across the scenarios
# Panels (2) and (3)
