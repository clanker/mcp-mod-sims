# Define libraries
library(DoseFinding)
library(gridExtra)
library(tidyverse)

# # Define true dose-response model
# sim_doses <- c(0, 125, 250, 500, 1000)
# mod_true <- Mods(emax = 200, doses = sim_doses, placEff = 2, maxEff = 2.5)
# mod_curve <- plotMods(mod_true, nPoints = 1001)$data
#
# # Simulate a Study 2B data set
# set.seed(2024)
# group_n <- rep(20, 5)
# group_var <- rep(16, 5)
# group_means <- mod_curve$response[sim_doses + 1]
# sample_means <- rep(group_means, group_n)
# random_values <- rnorm(sum(group_n), 0, rep(group_var, group_n) |> sqrt())
# study2b_data <- data.frame(dose = rep(sim_doses, group_n),
#                   resp = sample_means + random_values)
# ggplot(study2b_data) + geom_jitter(aes(dose, resp), height = 0, width = 4) +
#   labs(title = "Simulated Study 2B by dose (jittered horizontally)") +
#   xlab("dose [mg]") + ylab("resp [points]")
#
# # Define MCP-MOD candidate models
# mods <- Mods(linear = NULL,
#              quadratic = -0.0008,
#              exponential = 300,
#              emax = c(100, 400),
#              sigEmax = matrix(c(250, 5, 400, 8), byrow = TRUE, 2, 2),
#              placEff = 2, maxEff = 2, doses = sim_doses)
# plot(mods)
# #TD(mods, Delta = 1.6, doses = sim_doses)
#
# # Determine MCP-MOD contrasts
# optC <- optContr(mods, w=1)
# print(optC)
# plot(optC)
#
# # Run MCP-MOD on the simulated data
# test_normal <- MCTtest(dose = dose, resp = resp, models = mods, data = study2b_data)
# print(test_normal)
#
# fit_mods <- list()
# fit_mods[[1]] <- fitMod(dose, resp, data = study2b_data, model = "linear")
# fit_mods[[2]] <- fitMod(dose, resp, data = study2b_data, model = "quadratic")
# fit_mods[[3]] <- fitMod(dose, resp, data = study2b_data, model = "exponential", bnds = c(10, 10000))
# fit_mods[[4]] <- fitMod(dose, resp, data = study2b_data, model = "emax", bnds = c(1, 1000))
# fit_mods[[5]] <- fitMod(dose, resp, data = study2b_data, model = "logistic",
#                         bnds = matrix(c(1, 1000, 1, 1000), byrow = TRUE, 2, 2))
# fit_mods[[6]] <- fitMod(dose, resp, data = study2b_data, model = "sigEmax",
#                       bnds = matrix(c(1, 1000, 0.5, 10), byrow = TRUE, 2, 2))
#
# plot_fits <- list()
# for (i in 1:6)  plot_fits[[i]] <- plot(fit_mods[[i]], CI = TRUE, plotData = "meansCI")
#
# do.call("grid.arrange", c(plot_fits, ncol = 3))
#
# out <- MCPMod(dose, resp, study2b_data, mods, Delta=2, selModel = "aveAIC")
#
# res_t <- as_tibble(out$MCTtest$tStat |> t())
#
# # combine AIC results
# res_aic <- as_tibble(matrix(0, nrow = 0, ncol = 6, dimnames = list(NULL, mod_list)))
# res_aic[1, names(out$selMod)] <- t(out$selMod)
# res_t[["dose_aic"]] <- sum(out$doseEst * out$selMod)
# # use only the max
# selOne <- (1.001 * out$selMod / max(out$selMod)) |> floor()
# selOne <- selOne / sum(selOne)
# res_t[["dose_best"]] <- sum(out$doseEst * selOne)
#
# res_aic %>% replace(is.na(.), 0) |>
#   apply(1, rank, decreasing = TRUE)


# Set up a simulation study of 1000 runs over 6 doses and 2 sample sizes

sim_doses_list <- rep(list(c(0, 250, 500, 1000),
                       c(0, 125, 250, 500, 1000),
                       c(0, 62, 125, 250, 500, 1000),
                       c(0, 333, 667, 1000),
                       c(0, 250, 500, 750, 1000),
                       c(0, 200, 400, 600, 800, 1000)), 2)
sim_n_list <- rep(c(120, 240), each = 6)
sim_symm_list <- rep(c("asymmetric", "symmetric", "asymmetric", "symmetric"), each = 3)
B <- 2500

model_name_list <- c("linear", "quadratic", "exponential", "emax1", "emax2", "sigEmax1", "sigEmax2")
mod_list <- c("linear", "quadratic", "exponential", "emax", "sigEmax")
res_sim <- as_tibble(matrix(0, nrow = 0, ncol = 7, dimnames = list(NULL, names(out$MCTtest$tStat))))
res_aic <- as_tibble(matrix(0, nrow = 0, ncol = 5, dimnames = list(NULL, mod_list)))
res_par <- as_tibble(matrix(NA, nrow = 0, ncol = 7, dimnames = list(NULL, c("id", "n", "K", "symmetric", "mu", "S", "df"))))

for (sim_num in 1:12) {
  n <- sim_n_list[[sim_num]]
  sim_doses <- sim_doses_list[[sim_num]]
  K <- length(sim_doses)

  cat("Starting", n, sim_doses, K, "...\n")
  # define candidate models
  mods <- Mods(linear = NULL,
               quadratic = -0.0008,
               exponential = 300,
               emax = c(100, 400),
               sigEmax = matrix(c(250, 5, 400, 8), byrow = TRUE, 2, 2),
               placEff = 2, maxEff = 2, doses = sim_doses)

  # define true dose-response curve
  mod_true <- Mods(emax = 200, doses = sim_doses, placEff = 2, maxEff = 2.5)
  mod_curve <- plotMods(mod_true, nPoints = 1001)$data
  group_n <- rep(n/K, K)
  group_var <- rep(9, K)
  group_means <- mod_curve$response[sim_doses + 1]
  sample_means <- rep(group_means, group_n)

  for (b in 1:B) {

    row_num <- B*(sim_num-1) + b
    set.seed(row_num)
    random_values <- rnorm(sum(group_n), 0, rep(group_var, group_n) |> sqrt())
    study2b_data <- data.frame(dose = rep(sim_doses, group_n),
                               resp = sample_means + random_values)

    # get linear model estimates
    fitlm <- lm(resp ~ factor(dose) - 1, data = study2b_data)
    mu_hat <- coef(fitlm)
    S_hat <- vcov(fitlm)
    anova_df <- fitlm$df.residual

    res_par[row_num, "id"] <- b
    res_par[row_num, "n"] <- n
    res_par[row_num, "K"] <- K
    res_par[row_num, "symmetric"] <- sim_symm_list[sim_num]
    res_par[row_num, "mu"] <- nest(tibble(dose = sim_doses, mu = as.vector(mu_hat)))
    res_par[row_num, "S"] <- S_hat[1, 1]
    res_par[row_num, "df"] <- anova_df

    out <- MCPMod(dose, resp, study2b_data, mods, Delta=2, selModel = "aveAIC")

    res_sim[row_num, 1:7] <- attr(out$MCTtest$tStat, "pVal") |> t()

    # combine AIC results
    if (length(out$selMod) > 0) {
      res_aic[row_num, names(out$selMod)] <- t(out$selMod)
      res_sim[row_num, "dose_aic"] <- sum(out$doseEst * out$selMod)
      # use only the max
      selOne <- (1.001 * out$selMod / max(out$selMod)) |> floor()
      selOne <- selOne / sum(selOne)
      res_sim[row_num, "dose_best"] <- sum(out$doseEst * selOne)
    } else {
      res_aic[row_num, ] <- NA
    }

    if (b %% 200 == 0) cat("*")
    if (b %% 1000 == 0) cat("=")
  }
  cat("Completed ", sim_num, "\n")
}

mat_wts <- res_aic |>
  dplyr::mutate(id = 1:nrow(res_aic)) |>
  tidyr::pivot_longer(linear:sigEmax, names_to = "model", values_to = "weight") |>
  tidyr::replace_na(list(weight = 0)) |>
  tidyr::pivot_wider(names_from = "model", values_from = "weight") |>
  dplyr::select(-id)

col_max <- mat_wts |>
  apply(1, which.max)
flag_no_models <- (rowSums(mat_wts) == 0)
col_max[flag_no_models] <- 6
res_par$best <- factor(c(names(res_aic), "none"),
                       levels = c(names(res_aic), "none"))[col_max]

col_max2 <- res_sim |>
  dplyr::select(linear:sigEmax2) |>
  apply(1, which.min)
res_sim$lowest <- factor(names(res_sim)[1:7],
                         levels = names(res_sim)[1:7])[col_max2]

#res_par$symmetric <- rep(rep(c("asymmetric", "symmetric"), each = 600), 2)
save(res_par, res_sim, res_aic, file = "save-2500.Rdata")

load("save-2500.Rdata")

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
  rmse_aic <- rmse_best <- rep(0, 1000)

  for (j in 1:1000) {
    set.seed(j)
    id_list <- sample(1:nrow(dat_sub), replace = TRUE)

    rmse_aic[j] <- sqrt(mean((dat_sub$dose_aic[id_list] - 400)^2, na.rm = TRUE))
    rmse_best[j] <- sqrt(mean((dat_sub$dose_best[id_list] - 400)^2, na.rm = TRUE))
  }
    dat_summary$dose_aic[i] = tibble(rmse = rmse_aic)
    dat_summary$dose_best[i] = tibble(rmse = rmse_best)

}

dat_summary$dose_aic[1]

res_rmse <- dat_summary |>
  dplyr::select(-nn) |>
  unnest(c(dose_aic, dose_best))

save(res_rmse, file = "rmse-data-2500.Rdata")
