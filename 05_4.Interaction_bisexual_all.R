###########################################################################
# Interaction Analysis: Differential Effects of Unmet Social Needs       #
# by Sexual Orientation on Breast Cancer Screening                       #
#                                                                         #
# Design: Survey-weighted logistic regression with LGBQ * unmet need      #
#         interaction terms (7 interactions, pre-specified)              #
#                                                                         #
# Companion to Fairlie decomposition:                                     #
#   - Decomposition: "Are LGBQ disproportionately exposed to unmet needs?"#
#   - Interaction:   "Do unmet needs affect LGBQ more strongly?"         #
###########################################################################

# -------------------------------------------------------------------------
# STEP 1: Define variables
# -------------------------------------------------------------------------

unmet_needs <- c("EmoSupport", "EmployIns", "FoodIns", "FoodStamp",
                 "HouseIns", "TransIns", "UtilityIns")

other_covariates <- c(
  "Age",                                          # continuous
  "Race", "Region", "Urban",                      # demographics
  "Edu", "Marital_Cat", "Income_Cat", "Employ",   # SES
  "HLTHSTAT", "Chronic_Cat",                      # health status
  "Hlthplan", "RegSource", "MED_Dff", "Wellness"  # healthcare access
)

all_main_effects <- c(unmet_needs, other_covariates)


# -------------------------------------------------------------------------
# STEP 2: Factor coding
# -------------------------------------------------------------------------

factor_vars <- c("Race", "Region", "Urban", "Edu", "Marital_Cat",
                 "Income_Cat", "Employ", "HLTHSTAT", "Chronic_Cat",
                 "Hlthplan", "RegSource", "MED_Dff", "Wellness")

BRFSS_final <- BRFSS_final %>%
  mutate(across(all_of(factor_vars), as.factor))

for (v in factor_vars) {
  cat(sprintf("%-15s : %s\n", v, paste(levels(BRFSS_final[[v]]), collapse = " | ")))
}
cat("\n")


# -------------------------------------------------------------------------
# STEP 3: Sanity check — cell sizes for each unmet need * LGBQ
# -------------------------------------------------------------------------

analytic_sub <- BRFSS_final %>%
  filter(Sample_ElgBC == 1 & LGBQ_bis %in% c(0, 1))

for (un in unmet_needs) {
  tab <- table(analytic_sub[[un]], analytic_sub$LGBQ_bis,
               dnn = c(un, "LGBQ_bis"), useNA = "ifany")
  cat("\n", un, ":\n")
  print(tab)
  
  # Flag small cells
  if (any(tab[!is.na(tab)] < 30)) {
    cat("WARNING: Cell(s) with n < 30 — interaction estimate may be unstable\n")
  }
}
cat("\n")


# -------------------------------------------------------------------------
# STEP 4: Set up survey design
# -------------------------------------------------------------------------

analytic_design_full <- svydesign(
  ids     = ~PSU,
  strata  = ~Strata,
  weights = ~Weight_2,
  data    = BRFSS_final,
  nest    = TRUE
)

analytic_design <- subset(
  analytic_design_full,
  Sample_ElgBC == 1 & LGBQ_bis %in% c(0, 1)  ### DIFFERENT !!!
)


# -------------------------------------------------------------------------
# STEP 5: Fit two models — main-effects-only and with interactions
# -------------------------------------------------------------------------

# Model A: main effects only (for joint test comparison)
formula_main <- as.formula(paste(
  "BC_UTD ~ LGBQ_bis +",
  paste(all_main_effects, collapse = " + ")
))

model_main <- svyglm(
  formula_main,
  design = analytic_design,
  family = quasibinomial(link = logit)
)

# Model B: main effects + 7 LGBQ * unmet need interactions
interaction_terms <- paste0("LGBQ_bis:", unmet_needs)

formula_interaction <- as.formula(paste(
  "BC_UTD ~ LGBQ_bis +",
  paste(all_main_effects, collapse = " + "), "+",
  paste(interaction_terms, collapse = " + ")
))

model_interaction <- svyglm(
  formula_interaction,
  design = analytic_design,
  family = quasibinomial(link = logit)
)

cat("\n")


# -------------------------------------------------------------------------
# STEP 6: Joint test — are the 7 interactions jointly significant?
# -------------------------------------------------------------------------

joint_test <- regTermTest(
  model_interaction,
  test.terms = as.formula(paste("~", paste(interaction_terms, collapse = " + ")))
)
print(joint_test)
cat("\n")


# -------------------------------------------------------------------------
# STEP 7: Individual interaction term estimates
# -------------------------------------------------------------------------

coef_summary <- summary(model_interaction)$coefficients
int_rows <- grep("^LGBQ_bis:", rownames(coef_summary))
int_table_raw <- coef_summary[int_rows, , drop = FALSE]

n_tests <- length(unmet_needs)
alpha_bonf <- 0.05 / n_tests

int_table <- data.frame(
  UnmetNeed     = gsub("LGBQ_bis:", "", rownames(int_table_raw)),
  Beta_int      = round(int_table_raw[, "Estimate"], 4),
  SE            = round(int_table_raw[, "Std. Error"], 4),
  OR_int        = round(exp(int_table_raw[, "Estimate"]), 3),
  CI_lo         = round(exp(int_table_raw[, "Estimate"] -
                              1.96 * int_table_raw[, "Std. Error"]), 3),
  CI_hi         = round(exp(int_table_raw[, "Estimate"] +
                              1.96 * int_table_raw[, "Std. Error"]), 3),
  p_raw         = round(int_table_raw[, "Pr(>|t|)"], 4),
  p_Bonferroni  = round(pmin(int_table_raw[, "Pr(>|t|)"] * n_tests, 1), 4),
  Sig_Bonf      = ifelse(int_table_raw[, "Pr(>|t|)"] < alpha_bonf, "*", ""),
  row.names     = NULL
)

cat("=== Interaction term estimates (Bonferroni α =",
    round(alpha_bonf, 4), ") ===\n")
print(int_table)
cat("\nInterpretation of OR_int:\n")
cat("  > 1: Effect of unmet need is WEAKER (less negative) in LGBQ vs Straight\n")
cat("  < 1: Effect of unmet need is STRONGER (more negative) in LGBQ vs Straight\n\n")


# -------------------------------------------------------------------------
# STEP 8: Average Marginal Effects (AME) by group — pp scale
# -------------------------------------------------------------------------

ame_results <- avg_comparisons(
  model_interaction,
  variables = unmet_needs,
  by        = "LGBQ_bis",
  type      = "response"
)

ame_tidy <- ame_results %>%
  as.data.frame() %>%
  mutate(
    Group     = ifelse(LGBQ_bis == 0, "Straight", "LGBQ"),
    AME_pp    = round(estimate * 100, 2),
    SE_pp    = round(std.error * 100, 2),
    CI_lo_pp  = round(conf.low * 100, 2),
    CI_hi_pp  = round(conf.high * 100, 2),
    p_value   = round(p.value, 4)
  ) %>%
  select(UnmetNeed = term, Group, AME_pp, SE_pp, CI_lo_pp, CI_hi_pp, p_value) %>%
  arrange(UnmetNeed, Group)

print(ame_tidy)
cat("\n  AME_pp = percentage point change in screening probability when unmet need goes from 0 to 1 (within each group)\n\n")


# -------------------------------------------------------------------------
# STEP 9: Differential effect — LGBQ AME minus Straight AME
# -------------------------------------------------------------------------

ame_diff <- avg_comparisons(
  model_interaction,
  variables  = unmet_needs,
  by         = "LGBQ_bis",
  hypothesis = ~ pairwise | term,
  type       = "response"
)

ame_diff_tidy <- ame_diff %>%
  as.data.frame() %>%
  mutate(
    UnmetNeed     = gsub(" b2-b1=0$", "", term),
    Diff_pp       = round(estimate * 100, 2),
    SE_pp         = round(std.error * 100, 2),
    CI_lo_pp      = round(conf.low * 100, 2),
    CI_hi_pp      = round(conf.high * 100, 2),
    p_value       = round(p.value, 4),
    p_Bonferroni  = round(pmin(p.value * length(unmet_needs), 1), 4),
    Sig_Bonf      = ifelse(p.value < (0.05 / length(unmet_needs)), "*", "")
  ) %>%
  select(UnmetNeed, Diff_pp, SE_pp, CI_lo_pp, CI_hi_pp,
         p_value, p_Bonferroni, Sig_Bonf)

print(ame_diff_tidy)
cat("\n  Diff_pp < 0: Unmet need has STRONGER negative effect in LGBQ\n")
cat("  Diff_pp > 0: Unmet need has WEAKER negative effect in LGBQ\n\n")


# -------------------------------------------------------------------------
# STEP 10: Visualization
# -------------------------------------------------------------------------
# library(ggplot2)
# ggplot(ame_diff_tidy, aes(x = reorder(UnmetNeed, Diff_pp), y = Diff_pp)) +
#   geom_pointrange(aes(ymin = CI_lo_pp, ymax = CI_hi_pp)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   coord_flip() +
#   labs(x = "Unmet Need",
#        y = "Differential effect (LGBQ − Straight), pp",
#        title = "Differential associations of unmet needs with BC screening",
#        subtitle = "Negative values: stronger negative effect in LGBQ") +
#   theme_minimal()


# -------------------------------------------------------------------------
# STEP 11: P-value only include 7 Yes
# -------------------------------------------------------------------------

b   <- coef(model_interaction)
V   <- vcov(model_interaction)

idx <- grep("^LGBQ_bin:.*1$", names(b))

stopifnot(length(idx) == 7)
names(b)[idx]

L <- matrix(0, nrow = length(idx), ncol = length(b))
L[cbind(seq_along(idx), idx)] <- 1

chi   <- as.numeric(t(L %*% b) %*% solve(L %*% V %*% t(L)) %*% (L %*% b))
q     <- length(idx)
ddf   <- degf(model_interaction$survey.design)

Fstat <- chi / q
pval  <- pf(Fstat, q, ddf, lower.tail = FALSE)

sprintf("F(%d, %d) = %.2f, P = %.3f", q, ddf, Fstat, pval)