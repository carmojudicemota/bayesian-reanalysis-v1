
source("R/reproduce/00_reproduction_helpers.R")

study_scripts <- sort(list.files(
  path = "R/reproduce", pattern = "^study_[0-9]+\\.R$", full.names = TRUE
))
message("Found ", length(study_scripts), " study reproduction scripts.")

for (script in study_scripts) source(script)

reproduce_fns <- sort(ls(pattern = "^reproduce_study_[0-9]+$", envir = .GlobalEnv))
if (length(reproduce_fns) != length(study_scripts)) {
  warning(
    length(study_scripts), " scripts sourced but only ", length(reproduce_fns),
    " reproduce_study_*() functions found -- check for naming mismatches."
  )
}

for (fn_name in reproduce_fns) {
  message("Running ", fn_name, "()...")
  get(fn_name)()
}

source("R/reproduce/98_compile_recomputed_results.R")

# Validate at source: schema completeness + the directionality invariant
# (stored p must match its p_sidedness label). Stops if anything is off.
source("R/reproduce/99_validate_recomputed_results.R")
