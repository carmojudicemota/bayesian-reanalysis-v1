library(RoBTT); library(dplyr); library(purrr)

run_family_C <- function(claim, priors){
  gp <- load_wave2_data(claim$study_id) # the list(x1, x2)
  grid <- priors |> filter(prior_family == "welch_averaged", param == "delta_scale")
  pmap_dfr(grid, function(prior_label, value, ...){
    fit <- RoBTT::RoBTT(x1 = gp$x1, x2 = gp$x2, 
                        prior_delta = RoBTT::prior("cauchy", list(location = 0, scale = value)),
                        prior_rho = RoBTT::prior("beta", list(alpha = 1, beta = 1)),
                        chains = 4, iter = 8000, warmup = 2000, seed = 2026)
    dg <- summary(fit)$diagnostics
    record_mcmc_diagnostics(claim$claim_id, dg$rhat_max, dg$ess_min, dg$divergences, dg$ppc_ok)
    wave2_row(claim, prior_label, value, summary(fit)$estimates["delta","BF"], "welch_averaged",
              "delta = 0 (variance-averaged)", "delta ~ Cauchy(0,r), variance free")
  })
}

