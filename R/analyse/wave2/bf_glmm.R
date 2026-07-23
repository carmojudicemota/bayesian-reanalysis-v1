library(dplyr); library(purrr)

run_family_D <- function(claim, priors) {
  grid <- priors |> filter(prior_family == "glmm_normal", param == "prior_sd")
  pmap_dfr(grid, function(prior_label, value, ...) {
    b  <- as.numeric(claim$estimate)
    se <- as.numeric(claim$se_estimate)
    bf <- dnorm(b, 0, sqrt(se^2 + value^2)) / dnorm(b, 0, se)
    wave2_row(claim, prior_label, value, bf, "glmm_normal",
              "beta = 0", paste0("beta ~ N(0, ", value, "^2)"))
  })
}
