# Provenance script for R/sysdata.rda (internal package data).
#
# R/sysdata.rda holds the fitted Kublin (TapeR) mixed-effects taper models
# used by kublin_nor(), one `par.lme` object per species:
#   - kublin_par_lme_spruce
#   - kublin_par_lme_pine
#
# Each object is the `$par.lme` element returned by TapeR::TapeR_FIT_LME.f().
# This script rebuilds R/sysdata.rda from the per-species fit output. Point the
# paths below at the relevant fit objects (kept outside the package).

# --- pine -------------------------------------------------------------------
# Full TapeR_FIT_LME.f() output for Scots pine (list with $fit.lme and $par.lme).
pine_fit_rds <- "../../pine_model.rds"
kublin_par_lme_pine <- readRDS(pine_fit_rds)$par.lme

# --- spruce -----------------------------------------------------------------
# The spruce par.lme predates this script; reuse the object already shipped in
# R/sysdata.rda. Replace with the original fit output if it is re-fitted.
.e <- new.env()
load("R/sysdata.rda", envir = .e)
kublin_par_lme_spruce <- .e$kublin_par_lme_spruce

# --- write ------------------------------------------------------------------
# version = 2 keeps the file readable by R (>= 2.10) (see DESCRIPTION Depends).
save(
  kublin_par_lme_spruce,
  kublin_par_lme_pine,
  file = "R/sysdata.rda",
  version = 2,
  compress = "bzip2"
)
