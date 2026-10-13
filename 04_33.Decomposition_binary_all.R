###########################################################################
# Fairlie Nonlinear Decomposition                                         #
# LGBQ vs Straight BC_UTD -- Sample_ElgBC sample, HRSN with MIS dummies   #
# Adapted from Fairlie (2017) official R code (decomp_random_v7_3)        #
#                                                                         #
# SEs: delta method as in the official code (SE = sqrt(D V D'), averaged  #
#      across iterations), with V = design-based vcov from svyglm.        #
#      Extended to HRSN subtotal and total explained by summing the       #
#      group gradient vectors.                                            #
# Crude gap CI: design-based, via svyby + svycontrast.                    #
###########################################################################

library(survey)
library(dplyr)
library(future)
library(future.apply)
library(progressr)

# -------------------------------------------------------------------------
# STEP 0: Setup
# -------------------------------------------------------------------------

numiterations <- 1000

var_groups <- list(
  EmoSupport = c("EmoSupport_Yes", "EmoSupport_MIS"),
  EmployIns  = c("EmployIns_Yes", "EmployIns_MIS"),
  FoodIns    = c("FoodIns_Yes", "FoodIns_MIS"),
  FoodStamp  = c("FoodStamp_Yes", "FoodStamp_MIS"),
  HouseIns   = c("HouseIns_Yes", "HouseIns_MIS"),
  TransIns   = c("TransIns_Yes", "TransIns_MIS"),
  UtilityIns = c("UtilityIns_Yes", "UtilityIns_MIS"),
  Age        = c("Age"),
  Race       = c("Race_BL", "Race_OT", "Race_ML", "Race_HI", "Race_MI"),
  Region     = c("Region_SO", "Region_MW", "Region_WT"),
  Urban      = c("Urban_No", "Urban_MI"),
  Edu        = c("Edu_LHS", "Edu_HSG", "Edu_ASS", "Edu_MIS"),
  Employ     = c("Employ_No", "Employ_MIS"),
  Marital    = c("Marital_Div", "Marital_Nev", "Marital_MIS"),
  Income     = c("Inc_1", "Inc_2", "Inc_MIS"),
  Health     = c("HLTHSTAT_POOR", "HLTHSTAT_MIS"),
  Chronic    = c("Chronic_Yes", "Chronic_MIS"),
  Insurance  = c("Ins_No", "Ins_MIS"),
  RegSource  = c("Reg_No", "Reg_MIS"),
  Wellness   = c("Wellness_No", "Wellness_MIS"),
  MedDiff    = c("Med_Yes", "Med_MIS")
)

hrsn_groups <- c("EmoSupport", "EmployIns", "FoodIns", "FoodStamp",
                 "HouseIns", "TransIns", "UtilityIns")

all_vars <- unlist(var_groups, use.names = FALSE)
n_groups <- length(var_groups)


# -------------------------------------------------------------------------
# STEP 1: Survey design + pooled regression
# -------------------------------------------------------------------------

set.BRFSS_dum <- f.svy_set(BRFSS_dum)
set.BRFSS_dum <- update(set.BRFSS_dum,
                        BC_UTD_num = as.numeric(as.character(BC_UTD)))

des_an <- subset(set.BRFSS_dum,
                 Sample_ElgBC == 1 & LGBQ_bin %in% c(0, 1))

pooled_formula <- as.formula(
  paste("BC_UTD ~ LGBQ_bin +", paste(all_vars, collapse = " + "))
)

pooled_reg <- svyglm(pooled_formula, design = des_an,
                     family = quasibinomial(link = logit))

coef_names   <- c("(Intercept)", all_vars)
decomp_coefs <- coef(pooled_reg)[coef_names]
b <- as.numeric(decomp_coefs)
V <- vcov(pooled_reg)[coef_names, coef_names]   # design-based covariance

if (anyNA(b)) stop("NA coefficients (collinear dummy?): ",
                   paste(coef_names[is.na(b)], collapse = ", "))
cat("Pooled regression fitted.\n")


# -------------------------------------------------------------------------
# STEP 2: Analysis subsets + crude gap (with design-based CI)
# -------------------------------------------------------------------------

lgbq_data <- BRFSS_dum %>%
  filter(Sample_ElgBC == 1 & LGBQ_bin == 1) %>%
  select(BC_UTD, wgt = Weight_2, all_of(all_vars))

strt_data <- BRFSS_dum %>%
  filter(Sample_ElgBC == 1 & LGBQ_bin == 0) %>%
  select(BC_UTD, wgt = Weight_2, all_of(all_vars))

mobs <- nrow(lgbq_data)
wobs <- nrow(strt_data)

gap_by    <- svyby(~BC_UTD_num, ~LGBQ_bin, des_an, svymean, covmat = TRUE)
gap_ct    <- svycontrast(gap_by, c(1, -1))
crude_gap <- as.numeric(coef(gap_ct))
gap_ci    <- as.numeric(confint(gap_ct))

cat("Prevalence (straight, LGBQ):", round(coef(gap_by), 4), "\n")
cat("Crude gap:", round(crude_gap, 4),
    " 95% CI:", round(gap_ci, 4), "\n\n")


# -------------------------------------------------------------------------
# STEP 3: One iteration (decomposition + delta-method SEs)
# -------------------------------------------------------------------------

f.decomp_one <- function(iter_seed) {
  
  set.seed(iter_seed)
  
  # Weighted random matching (as in official code)
  strt_idx    <- sample(seq_len(wobs), size = mobs, replace = TRUE,
                        prob = strt_data$wgt)
  strt_sample <- strt_data[strt_idx, ]
  
  lgbq_idx    <- sample(seq_len(mobs), size = mobs, replace = FALSE,
                        prob = lgbq_data$wgt)
  lgbq_sample <- lgbq_data[lgbq_idx, ]
  
  strt_sample <- strt_sample[sample(seq_len(mobs)), ]
  
  # Random ordering of variable groups
  group_order <- sample(names(var_groups))
  
  wL <- lgbq_sample$wgt / sum(lgbq_sample$wgt)
  XS <- as.matrix(strt_sample[, all_vars])
  X1 <- cbind("(Intercept)" = 1, as.matrix(lgbq_sample[, all_vars]))
  
  # Weighted mean prediction and its gradient w.r.t. beta
  eval_X <- function(X) {
    p <- as.vector(plogis(X %*% b))
    list(pred = sum(wL * p),
         grad = colSums(wL * p * (1 - p) * X))
  }
  
  prev    <- eval_X(X1)
  contrib <- setNames(numeric(n_groups), names(var_groups))
  D       <- matrix(0, n_groups, ncol(X1),
                    dimnames = list(names(var_groups), colnames(X1)))
  
  # Sequential cumulative replacement LGBQ -> straight
  for (g in group_order) {
    v <- var_groups[[g]]
    X1[, v] <- XS[, v]
    new <- eval_X(X1)
    contrib[g] <- new$pred - prev$pred
    D[g, ]     <- new$grad - prev$grad
    prev <- new
  }
  
  se_fun <- function(d) sqrt(as.numeric(t(d) %*% V %*% d))
  
  c(contrib,
    HRSN_total         = sum(contrib[hrsn_groups]),
    total_explained    = sum(contrib),
    setNames(apply(D, 1, se_fun), paste0("se_", names(var_groups))),
    se_HRSN_total      = se_fun(colSums(D[hrsn_groups, , drop = FALSE])),
    se_total_explained = se_fun(colSums(D)))
}


# -------------------------------------------------------------------------
# STEP 4: Parallel execution
# -------------------------------------------------------------------------

n_cores <- max(1, parallel::detectCores() - 2)
cat("Using", n_cores, "cores in parallel...\n")
plan(multisession, workers = n_cores)

handlers("txtprogressbar")   # no handlers(global = TRUE): fails inside Rmd/Quarto

t0 <- Sys.time()
with_progress({
  p <- progressor(steps = numiterations)
  results_matrix <- future_sapply(
    X = seq_len(numiterations),
    FUN = function(i) {
      res <- f.decomp_one(i)
      p()
      res
    },
    future.seed = TRUE
  )
})
results_matrix <- t(results_matrix)      # iterations x outputs
plan(sequential)

elapsed <- as.numeric(Sys.time() - t0, units = "secs")
cat("\nElapsed:", round(elapsed, 1), "seconds (",
    round(elapsed / numiterations, 3), "sec/iter)\n\n")


# -------------------------------------------------------------------------
# STEP 5: Summarize (official approach: average contributions and SEs
#         across iterations; CI = est +/- 1.96 SE)
# -------------------------------------------------------------------------

m   <- colMeans(results_matrix)
est <- m[c(names(var_groups), "HRSN_total", "total_explained")]
se  <- m[paste0("se_", names(est))]
names(se) <- names(est)

# % of gap treats crude gap as fixed (consistent with official code)
final_results <- data.frame(
  Group    = names(est),
  Contr_pp = round(est * 100, 2),
  SE_pp    = round(se * 100, 2),
  CI_lo_pp = round((est - 1.96 * se) * 100, 2),
  CI_hi_pp = round((est + 1.96 * se) * 100, 2),
  Pct_gap  = round(est / crude_gap * 100, 1),
  Pct_lo   = round((est - 1.96 * se) / crude_gap * 100, 1),
  Pct_hi   = round((est + 1.96 * se) / crude_gap * 100, 1)
)

cat("=== Fairlie Decomposition Results (Sample_ElgBC) ===\n")
cat("Iterations:", numiterations, "\n")
print(final_results, row.names = FALSE)


# -------------------------------------------------------------------------
# STEP 6: Manuscript-ready tables
# -------------------------------------------------------------------------

fmt <- function(x, lo, hi, d = 1) sprintf("%.*f (%.*f, %.*f)", d, x, d, lo, d, hi)

row_pp  <- function(k) fmt(est[k] * 100, (est[k] - 1.96 * se[k]) * 100,
                           (est[k] + 1.96 * se[k]) * 100)
row_pct <- function(k) fmt(est[k] / crude_gap * 100,
                           (est[k] - 1.96 * se[k]) / crude_gap * 100,
                           (est[k] + 1.96 * se[k]) / crude_gap * 100)

table_out <- data.frame(
  Row = c("Total explained (percentage point)",
          "Explained by unmet HRSNs (percentage point)",
          "Crude gap (percentage point)",
          "% of gap explained",
          "% of gap explained by unmet HRSNs"),
  Value = c(row_pp("total_explained"),
            row_pp("HRSN_total"),
            fmt(crude_gap * 100, gap_ci[1] * 100, gap_ci[2] * 100),
            row_pct("total_explained"),
            row_pct("HRSN_total"))
)
print(table_out, row.names = FALSE)
