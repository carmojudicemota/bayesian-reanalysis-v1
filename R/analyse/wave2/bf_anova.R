library(BayesFactor); library(dplyr); library(purrr)

run_family_A <-function(claim, priors){
  d <- load_wave2_data(claim$study_id)
  grid <- priors |> filter(prior_family == "anova_cauchy", parm == "rscale_fixed")
  pmap_dfr(grid, function(prior_label, value, ...) {
    bfs <- BayesFactor::generalTestBF(outcome ~ A * B, data = d,
                                      whichModels = "all", rscaleFixed = value,
                                      progress = FALSE)
    bf <- bfs[claim$model_alt] / bfs[claim$model_null]
    tab <- BayesFactor::extractBF(bf)
    wave2_row(claim, prior_label, value, as.numeric(tab$bf)[1], "anova_cauchy",
              claim$model_null, claim$model_alt, as.numeric(tab$error)[1])
  })
}

run_family_B <- function(claim, prior){
  long <- load_wave2_data(claim$study_id)
  rr <- priors$value[priors$prior_family == "anova_cauchy" & priors$param == "rscale_random"][1]
  grid <- priors |> filter(prior_family == "anova_cauchy", param == "rscale_fixed")
  pmap_dfr(grid, function(prior_label, value,...){
    bfs <- BayesFactor::anovaBF(score ~condition + subject, data = long, 
                                whichRandom = "subject", rscaleFixed = value,
                                rscaleRandom = rr, progress = FALSE)
    bf <- bfs["condition + subject"] / bfs["subject"]
    tab <- BayesFactor::extractBF(bf)
    wave2_row(claim, prior_label, value, as.numeric(tab$bf)[1], "anova_cauchy",
              "subject", "condition + subject", as.numeric(tab$error)[1])
  })
}

