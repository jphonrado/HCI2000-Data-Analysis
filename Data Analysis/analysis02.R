# =====================================================================
# HCI2000 analysis02 (R script version of analysis02.ipynb)
# HOW TO RUN: open this file in VS Code / RStudio and run it all, or in Terminal:
#     Rscript analysis02.R
# or in the R console:  source("<path to this file>")
# Run preprocessing.ipynb first: it creates the cleaned files read below.
# The explanations from the notebook are kept as comments, in presenter voice.
# =====================================================================
 
# Install any missing package once (a CRAN mirror is given so R never asks for one)
for (p in c("afex", "lme4", "ARTool")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}


# # HCI2000 Analysis 02: Inferential statistics (in R)
#
# ## What this notebook does, and why it is in R
# The earlier notebooks produced a clean dataset and described it. Here we answer the research questions with statistical tests:
#
# | Hypothesis | Question | Role |
# |---|---|---|
# | **H3** | Does the effect of tab orientation (horizontal vs vertical) **change with tab load**? (the interaction) | primary |
# | **H1** | Does tab orientation matter on its own? (main effect) | supporting |
# | **H2** | Does tab load matter on its own? (main effect) | supporting |
#
# Each hypothesis is tested on three outcomes: **target-tab retrieval time**, **first-selection accuracy** and **weighted NASA-TLX workload**.
#
# The analysis plan in Section 4.7 of the paper uses methods that are standard in R: a repeated-measures ANOVA (package `afex`), the aligned rank transform (package `ARTool`)
# and a logistic mixed-effects model with likelihood-ratio tests (package `lme4`). We use these established packages, so every statistic in this notebook comes from software that the field already trusts.
#
# **Run `preprocessing.ipynb` first**, because we read the cleaned files that it creates.
#
# ## The plan, in the order we run it
# 1. **Load** the cleaned tables.
# 2. **Check normality** of each outcome's within-participant differences (paper 4.7.3). This decides, *before* we look at any hypothesis test, whether an outcome gets
#    the parametric route (repeated-measures ANOVA) or the non-parametric route (aligned rank transform ANOVA).
# 3. **Run the tests** for the three effects on each outcome. Accuracy is modelled trial by trial with a logistic mixed model.
# 4. **Adjust for multiple testing** (Holm): each hypothesis is tested on three outcomes.
# 5. **Follow-up comparisons**: horizontal vs vertical at low load and at high load.
# 6. **Save** the tables.


# ## 0. Setup


# We load the three R packages we need. If R says a package is missing, install it once with `install.packages("name")` and run this cell again.
# As in the other notebooks, `BASE` is the only line to change if the project folder moves.

library(afex)      # repeated-measures ANOVA
library(lme4)      # logistic mixed-effects model
library(ARTool)   # aligned rank transform ANOVA

# ---- the ONE line to change if the project folder moves ----
BASE <- "/Users/jphonrado/Documents/School/Term 1 A.Y. 26-27/HCI2000/HCI2000-Data-Analysis"

CLEAN_FOLDER <- paste0(BASE, "/Data Preprocessing/cleaned")     # made by preprocessing.ipynb
TABLE_FOLDER <- paste0(BASE, "/Data Analysis/tables")
FIGURE_FOLDER <- paste0(BASE, "/Data Analysis/figures")
dir.create(TABLE_FOLDER, showWarnings = FALSE)
dir.create(FIGURE_FOLDER, showWarnings = FALSE)

ALPHA <- 0.05     # significance level used throughout the paper
options(width = 150)

# ## 1. Load the cleaned data
# Two tables from the preprocessing step:
#
# - `condition_level.csv`: one row per participant per condition (64 rows). Retrieval time and weighted TLX are analysed from this table.
# - `trial_level.csv`: one row per trial (384 rows). The accuracy model uses this one, because accuracy is a yes/no result on each trial.
#
# Orientation and tab load are **categories**, so we tell R to treat them as factors. `participant` is also a factor: it is the unit that is measured repeatedly.

condition <- read.csv(paste0(CLEAN_FOLDER, "/condition_level.csv"))
trial <- read.csv(paste0(CLEAN_FOLDER, "/trial_level.csv"))

# treat these columns as categories
condition$participant <- factor(condition$participant)
trial$participant <- factor(trial$participant)
trial$orientation <- factor(trial$orientation, levels = c("H", "V"))
trial$tab_load <- factor(trial$tab_load, levels = c("low", "high"))

cat("condition_level:", nrow(condition), "rows,", nlevels(condition$participant), "participants\n")
cat("trial_level:", nrow(trial), "rows\n")

# ## 2. Normality checks (paper 4.7.3)
# A repeated-measures ANOVA on our 2 x 2 design is, in effect, a test on three **difference scores for each participant**.
# Take one participant's four values: *H-low, H-high, V-low, V-high*. Then:
#
# | Effect | Difference score |
# |---|---|
# | Orientation | average of the two horizontal conditions minus average of the two vertical conditions |
# | Tab load | average of the two high-load conditions minus average of the two low-load conditions |
# | Interaction | (horizontal minus vertical at high load) minus (horizontal minus vertical at low load) |
#
# The ANOVA assumes these **differences** are roughly normally distributed, so they are what we test, not the raw times.
# We use the **Shapiro-Wilk test** (its null hypothesis is "the differences are normal", at alpha = .05) and **Q-Q plots** (if the points follow the line, the differences are close to normal), as the paper specifies.
# We do this for retrieval time and weighted TLX. We also do it for accuracy, because we need it for a fallback analysis and for the follow-up tests.
#
# One caution we state openly: with only 16 participants, the Shapiro-Wilk test has little power, so "not significant" does **not** prove normality. That is why the Q-Q plot carries as much weight as the p-value.

# One outcome as a table: one row per participant, one column per condition
# (only participants who have all four conditions are kept, because a repeated-measures test needs all four)
get_table <- function(column) {
  m <- tapply(condition[[column]], list(condition$participant, condition$condition), mean)
  m <- m[, c("H-L", "H-H", "V-L", "V-H")]
  m[complete.cases(m), ]
}

# The three difference scores per participant (see the table above)
difference_scores <- function(m) {
  data.frame(
    Orientation = (m[, "H-L"] + m[, "H-H"]) / 2 - (m[, "V-L"] + m[, "V-H"]) / 2,
    Load        = (m[, "H-H"] + m[, "V-H"]) / 2 - (m[, "H-L"] + m[, "V-L"]) / 2,
    Interaction = (m[, "H-H"] - m[, "V-H"]) - (m[, "H-L"] - m[, "V-L"]))
}

# Shapiro-Wilk test on each of the three difference scores
check_normality <- function(m, name) {
  scores <- difference_scores(m)
  result <- data.frame(outcome = name, effect = names(scores), n = nrow(scores))
  result$W <- unname(sapply(scores, function(x) shapiro.test(x)$statistic))
  result$p <- unname(sapply(scores, function(x) shapiro.test(x)$p.value))
  result$normality_rejected <- result$p < ALPHA
  result
}

# Q-Q plots of the three difference scores (shown here and saved as a picture)
draw_qq <- function(m, title) {
  scores <- difference_scores(m)
  par(mfrow = c(1, 3))
  for (effect in names(scores)) {
    qqnorm(scores[[effect]], main = paste(title, "-", effect), pch = 19, col = "#2E6F95")
    qqline(scores[[effect]])
  }
}
save_qq <- function(m, title, file) {
  png(paste0(FIGURE_FOLDER, "/", file), width = 2200, height = 700, res = 200)
  draw_qq(m, title)
  invisible(dev.off())
}

# ### 2.1 Retrieval time: raw seconds and log seconds
# Retrieval times are always positive and usually right-skewed (a few very slow trials). The paper allows a **log transformation** if the raw differences look skewed, with the checks repeated.
# We check both, so the choice can be justified rather than assumed.

rt <- get_table("mean_rt_s")                      # participants x 4 conditions

normality_rt_raw <- check_normality(rt, "Retrieval time (raw)")
normality_rt_log <- check_normality(log(rt), "Retrieval time (log)")

draw_qq(rt, "RT")
save_qq(rt, "RT", "qq_retrieval_time_raw.png")
draw_qq(log(rt), "log RT")
save_qq(log(rt), "log RT", "qq_retrieval_time_log.png")

print(rbind(normality_rt_raw, normality_rt_log))

# ### 2.2 Weighted NASA-TLX and first-selection accuracy

tlx <- get_table("weighted_tlx")
accuracy <- get_table("first_try_accuracy")

cat("Weighted TLX:", nrow(tlx), "of", nlevels(condition$participant),
    "participants have all four conditions (someone without a weighting form cannot enter a repeated-measures test)\n")

normality_tlx <- check_normality(tlx, "Weighted NASA-TLX")
normality_accuracy <- check_normality(accuracy, "First-selection accuracy")

draw_qq(tlx, "TLX")
save_qq(tlx, "TLX", "qq_weighted_tlx.png")

print(rbind(normality_tlx, normality_accuracy))

# ## 3. Decide the analysis route for each outcome, before testing
# The paper fixes the rule:
# - if the differences look normal, the outcome goes to the **repeated-measures ANOVA** (parametric route);
# - otherwise it goes to the **aligned rank transform (ART) ANOVA** (non-parametric route).
#
# One ANOVA covers all three effects of an outcome, so an outcome takes the non-parametric route if normality is rejected for **any** of its three difference scores.
# For retrieval time there is one extra step: if the raw scale fails but the log scale passes, we analyse **log retrieval time**.
#
# The decision is made here, in plain code, and everything below simply follows it. If the Q-Q plots lead us to a different judgement for a borderline case,
# we can change the route in this cell and explain why in the paper, so the decision stays visible.

# Retrieval time
rt_route <- "non-parametric"
rt_use_log <- FALSE
if (!any(normality_rt_raw$normality_rejected)) {
  rt_route <- "parametric"                         # raw seconds look normal
} else if (!any(normality_rt_log$normality_rejected)) {
  rt_route <- "parametric"                         # raw fails but log seconds look normal
  rt_use_log <- TRUE
}

# Weighted TLX
tlx_route <- "parametric"
if (any(normality_tlx$normality_rejected)) tlx_route <- "non-parametric"

# Accuracy (only used for the fallback analysis and the follow-up tests)
accuracy_route <- "parametric"
if (any(normality_accuracy$normality_rejected)) accuracy_route <- "non-parametric"

routes <- data.frame(
  outcome = c("Retrieval time", "First-selection accuracy", "Weighted NASA-TLX"),
  route = c(rt_route, accuracy_route, tlx_route),
  scale = c(ifelse(rt_use_log, "log seconds", "raw"), "raw", "raw"))
print(routes)

# ## 4. The tests: orientation, tab load and their interaction
# ### 4.1 Retrieval time and weighted TLX
# **Parametric route: repeated-measures ANOVA** (`afex::aov_ez`). The same participants are measured under all four conditions, so their scores are related; a repeated-measures ANOVA takes that into account
# and compares each person with themselves. Orientation and tab load are the two within-participant factors. Because each factor has only two levels, every effect has one degree of freedom,
# so no sphericity correction is needed (as the paper notes). The effect size is **partial eta squared**: the share of the variation that the effect explains.
#
# **Non-parametric route: aligned rank transform ANOVA** (`ARTool::art`). Simply ranking the raw data would blur the three effects together. The aligned rank transform first *aligns* the data for each effect
# (it removes everything except the effect of interest), then ranks, then runs the same repeated-measures ANOVA on the ranks. `ARTool` prints two self-checks: the aligned columns should sum to zero,
# and the ANOVAs on effects we are *not* interested in should give F near zero. Because this route tests ranks, its results are interpreted as shifts in the distribution of values, not as differences in means (the paper says the same).
# Its partial eta squared is calculated from F and the degrees of freedom.

# The ANOVA functions need the data in "long" format: one row per participant per condition
make_long <- function(m, use_log) {
  if (use_log) m <- log(m)
  n <- nrow(m)
  data.frame(
    participant = factor(rep(rownames(m), 4)),
    orientation = factor(rep(c("H", "H", "V", "V"), each = n)),
    tab_load    = factor(rep(c("low", "high", "low", "high"), each = n), levels = c("low", "high")),
    y           = c(m[, "H-L"], m[, "H-H"], m[, "V-L"], m[, "V-H"]))
}

effect_names <- c("Orientation", "Tab load", "Interaction")

# Run the right test for an outcome and return a tidy table
run_anova <- function(m, route, use_log = FALSE) {
  long <- make_long(m, use_log)

  if (route == "parametric") {
    fit <- suppressMessages(aov_ez(id = "participant", dv = "y", data = long,
                                   within = c("orientation", "tab_load"),
                                   anova_table = list(es = "pes", correction = "none")))
    a <- as.data.frame(fit$anova_table)
    result <- data.frame(model = "RM-ANOVA", effect = effect_names, n = nlevels(long$participant),
                         stat_name = "F", stat = a$F, df1 = a[["num Df"]], df2 = a[["den Df"]],
                         p = a[["Pr(>F)"]], effect_size_name = "partial eta^2", effect_size = a$pes)
  } else {
    fit <- art(y ~ orientation * tab_load + Error(participant / (orientation * tab_load)), data = long)
    print(summary(fit))                              # the two ART self-checks
    a <- as.data.frame(anova(fit))
    F <- a[["F value"]]
    result <- data.frame(model = "ART-ANOVA", effect = effect_names, n = nlevels(long$participant),
                         stat_name = "F", stat = F, df1 = a$Df, df2 = a$Df.res,
                         p = a[["Pr(>F)"]], effect_size_name = "partial eta^2",
                         effect_size = F * a$Df / (F * a$Df + a$Df.res))
  }
  result
}

results_rt <- cbind(outcome = "Retrieval time", run_anova(rt, rt_route, rt_use_log))
results_tlx <- cbind(outcome = "Weighted NASA-TLX", run_anova(tlx, tlx_route))

# ### 4.2 First-selection accuracy: a logistic mixed model
# Accuracy is a yes/no result on each trial (was the first selection correct?). Averaging it into a percentage per person would throw information away,
# and an ANOVA expects a continuous outcome. So the paper models the **probability of a correct first selection** at the **trial level** with a **logistic model**
# that contains orientation, tab load, their interaction, and a **random intercept for each participant**.
#
# *Why the random intercept?* One participant contributes many trials, and some people are simply more accurate than others.
# If we ignored that, we would treat 384 trials as if they came from 384 different people, and we would overstate how much evidence we have.
#
# *How each effect is tested:* with a **likelihood-ratio test** that compares two nested models (a model with the effect and the same model without it):
# - **Interaction (H3):** the full model against the model without the interaction.
# - **Orientation (H1):** the model with both factors (no interaction) against the model without orientation.
# - **Tab load (H2):** the model with both factors (no interaction) against the model without tab load.
#
# Trials with no observable first selection are left out, as defined in the paper. If this model cannot be fitted, the paper's **fallback** applies:
# each participant's accuracy per condition is analysed like the other outcomes. The code below does that automatically and says which one was used.

# only trials where a first selection could be observed (first_selection_correct is 1 = correct, 0 = wrong)
accuracy_data <- trial[!is.na(trial$first_selection_correct), ]
accuracy_data$correct <- accuracy_data$first_selection_correct
cat(nrow(accuracy_data), "trials used;", nrow(trial) - nrow(accuracy_data), "left out (no observable first selection)\n")

# fit a model; if it fails, return NULL instead of stopping
fit_model <- function(formula) {
  tryCatch(suppressMessages(glmer(formula, family = binomial, data = accuracy_data)),
           error = function(e) NULL)
}

model_full <- fit_model(correct ~ orientation * tab_load + (1 | participant))   # both factors + interaction
model_both <- fit_model(correct ~ orientation + tab_load + (1 | participant))   # both factors, no interaction
model_no_orientation <- fit_model(correct ~ tab_load + (1 | participant))
model_no_load <- fit_model(correct ~ orientation + (1 | participant))

model_works <- !is.null(model_full) && !is.null(model_both) && !is.null(model_no_orientation) && !is.null(model_no_load)
cat("Mixed model fitted:", model_works, "\n")
if (model_works) {
  cat("Singular fit (participants barely differ beyond the fixed effects):", isSingular(model_full), "\n")
}

# If R printed a **warning about convergence** above, the model did not fit reliably, and the fallback (participant accuracy per condition) is the honest choice: the paper says so.
# A **singular fit** is different: the model did fit, but it estimated that participants do not differ much from each other beyond the effects we modelled. We report this and keep the model.
#
# Odds ratios below describe the effects: an odds ratio **above 1** means higher odds of a correct first selection; **below 1** means lower odds.

if (model_works) {
  # likelihood-ratio test: compare two nested models
  lrt <- function(small_model, big_model) {
    a <- anova(small_model, big_model)
    c(stat = a$Chisq[2], df = a$Df[2], p = a[["Pr(>Chisq)"]][2])
  }
  test_orientation <- lrt(model_no_orientation, model_both)
  test_load <- lrt(model_no_load, model_both)
  test_interaction <- lrt(model_both, model_full)

  # odds ratios (exp of the model coefficients)
  odds_orientation <- exp(fixef(model_both)["orientationV"])                 # vertical vs horizontal
  odds_load <- exp(fixef(model_both)["tab_loadhigh"])                         # high vs low load
  odds_interaction <- exp(fixef(model_full)["orientationV:tab_loadhigh"])     # ratio of the two odds ratios

  results_accuracy <- data.frame(
    outcome = "First-selection accuracy", model = "Logistic mixed model (LRT)", effect = effect_names,
    n = nlevels(accuracy_data$participant), stat_name = "Chi^2",
    stat = c(test_orientation["stat"], test_load["stat"], test_interaction["stat"]),
    df1 = c(test_orientation["df"], test_load["df"], test_interaction["df"]), df2 = NA,
    p = c(test_orientation["p"], test_load["p"], test_interaction["p"]),
    effect_size_name = c("odds ratio (V vs H)", "odds ratio (high vs low)", "ratio of odds ratios"),
    effect_size = c(odds_orientation, odds_load, odds_interaction), row.names = NULL)

  cat("Odds ratios with 95% confidence intervals:\n")
  print(round(exp(confint(model_both, parm = "beta_", method = "Wald")), 3))
} else {
  cat("FALLBACK: the trial-level model could not be fitted, so we analyse each participant's accuracy per condition instead.\n")
  results_accuracy <- cbind(outcome = "First-selection accuracy", run_anova(accuracy, accuracy_route))
}

# ## 5. Putting the three outcomes together: the Holm adjustment
# Each hypothesis (orientation, tab load, interaction) is tested on **three outcomes**. The null hypothesis for an effect is rejected if **any** of its three outcomes shows an effect.
# But every extra outcome is another chance to get a "significant" result by luck, so the three p-values for an effect are **adjusted together** with the **Holm procedure**
# (it keeps the overall false-positive rate at 5%, and is less strict than the simple Bonferroni correction).
#
# A hypothesis is rejected if at least one of its adjusted p-values is at or below .05. The per-outcome rows then show **which outcome drives** the decision.

omnibus <- rbind(results_rt, results_accuracy, results_tlx)
rownames(omnibus) <- NULL

# Holm adjustment: for each effect, adjust its three p-values (one per outcome) together
omnibus$p_holm <- NA
for (effect in effect_names) {
  rows <- omnibus$effect == effect
  omnibus$p_holm[rows] <- p.adjust(omnibus$p[rows], method = "holm")
}
omnibus$significant_after_holm <- omnibus$p_holm <= ALPHA

# one line per hypothesis
decisions <- data.frame(
  hypothesis = c("H3 interaction (primary)", "H1 orientation", "H2 tab load"),
  effect = c("Interaction", "Orientation", "Tab load"))
decisions$smallest_adjusted_p <- sapply(decisions$effect, function(e) min(omnibus$p_holm[omnibus$effect == e]))
decisions$H0_rejected <- decisions$smallest_adjusted_p <= ALPHA
decisions$driven_by <- sapply(decisions$effect, function(e) {
  rows <- omnibus$effect == e & omnibus$significant_after_holm
  if (any(rows)) paste(omnibus$outcome[rows], collapse = ", ") else "-"
})
print(decisions)

# The full table behind those decisions (statistic, degrees of freedom, p-values and effect sizes for every outcome and effect):

shown <- omnibus
shown$stat <- round(shown$stat, 2)
shown$p <- signif(shown$p, 3)
shown$p_holm <- signif(shown$p_holm, 3)
shown$effect_size <- round(shown$effect_size, 3)
print(shown)

# ## 6. Follow-up comparisons: where does the gap open up?
# Our expected pattern is a **vertical advantage that grows with tab load**. In a 2 x 2 design, the way to examine that is to compare horizontal with vertical **separately at low load and at high load**
# (these are the "differences of differences" the paper mentions).
#
# - **Parametric outcome:** a paired t-test, with **Cohen's d** (paired) as the effect size.
# - **Non-parametric outcome:** a Wilcoxon signed-rank test, with the **rank-biserial correlation r** as the effect size.
#
# They are paired tests because every participant is in both orientations. We read them mainly when the interaction is significant, which the last column flags.
# For accuracy, the participant-level accuracy per condition is used. The paper does not Holm-adjust these follow-ups.

# horizontal vs vertical, at low load and at high load, for one outcome
follow_up <- function(m, route, use_log = FALSE) {
  if (use_log) m <- log(m)
  rows <- list()
  for (load in c("low", "high")) {
    horizontal <- m[, paste0("H-", ifelse(load == "low", "L", "H"))]
    vertical <- m[, paste0("V-", ifelse(load == "low", "L", "H"))]
    d <- horizontal - vertical

    if (route == "parametric") {
      test <- t.test(horizontal, vertical, paired = TRUE)
      row <- data.frame(test = "paired t", statistic = unname(test$statistic), p = test$p.value,
                        effect_size_name = "Cohen's d", effect_size = mean(d) / sd(d))
    } else {
      test <- suppressWarnings(wilcox.test(horizontal, vertical, paired = TRUE, exact = FALSE))
      d <- d[d != 0]
      ranks <- rank(abs(d))
      r <- (sum(ranks[d > 0]) - sum(ranks[d < 0])) / sum(ranks)       # rank-biserial correlation
      row <- data.frame(test = "Wilcoxon signed-rank", statistic = unname(test$statistic), p = test$p.value,
                        effect_size_name = "rank-biserial r", effect_size = r)
    }
    rows[[load]] <- cbind(comparison = paste("Horizontal - Vertical at", load, "load"),
                          mean_difference = mean(horizontal - vertical), row)
  }
  do.call(rbind, rows)
}

follow_ups <- rbind(
  cbind(outcome = "Retrieval time", follow_up(rt, rt_route, rt_use_log)),
  cbind(outcome = "First-selection accuracy", follow_up(accuracy, accuracy_route)),
  cbind(outcome = "Weighted NASA-TLX", follow_up(tlx, tlx_route)))
rownames(follow_ups) <- NULL

# is the interaction significant for this outcome (before Holm)?
interaction_p <- omnibus[omnibus$effect == "Interaction", c("outcome", "p")]
follow_ups$interaction_significant <- interaction_p$p[match(follow_ups$outcome, interaction_p$outcome)] < ALPHA

follow_ups$mean_difference <- round(follow_ups$mean_difference, 3)
follow_ups$statistic <- round(follow_ups$statistic, 3)
follow_ups$p <- signif(follow_ups$p, 3)
follow_ups$effect_size <- round(follow_ups$effect_size, 3)
print(follow_ups)

# ## 7. Condition means: reading the direction of the effects
# A significant effect tells us *that* conditions differ; the means tell us *in which direction*. For example, whether vertical was faster than horizontal, and by how much.
# Retrieval time is shown on the scale that was analysed (log seconds if the log route was chosen).

means <- rbind(
  "Retrieval time" = colMeans(if (rt_use_log) log(rt) else rt),
  "First-selection accuracy" = colMeans(accuracy),
  "Weighted NASA-TLX" = colMeans(tlx))
print(round(means, 3))

# ## 8. Save the results
# Everything above is saved to `Data Analysis/tables`, so the numbers in the paper come straight from this notebook.

normality_all <- rbind(normality_rt_raw, normality_rt_log, normality_tlx, normality_accuracy)

write.csv(normality_all, paste0(TABLE_FOLDER, "/normality_checks.csv"), row.names = FALSE)
write.csv(routes, paste0(TABLE_FOLDER, "/routes_used.csv"), row.names = FALSE)
write.csv(omnibus, paste0(TABLE_FOLDER, "/tests_all_effects.csv"), row.names = FALSE)
write.csv(decisions, paste0(TABLE_FOLDER, "/tests_hypothesis_decisions.csv"), row.names = FALSE)
write.csv(follow_ups, paste0(TABLE_FOLDER, "/tests_follow_ups.csv"), row.names = FALSE)
write.csv(means, paste0(TABLE_FOLDER, "/condition_means.csv"))
cat("Saved 6 tables to", TABLE_FOLDER, "\n")

# ## 9. Reading these results responsibly
# - The **interaction (H3)** is the primary question; the main effects are supporting. If the interaction is significant, the main effects should be read in its light, because a main effect can be driven by one condition only.
# - A non-significant result is **not** evidence that the two orientations perform the same. The paper commits to not interpreting it that way.
# - Effect sizes accompany the p-values: partial eta squared for the ANOVAs, odds ratios for the accuracy model, Cohen's d or the rank-biserial correlation for the follow-ups.
# - With 16 participants, borderline p-values deserve caution, and the size of the effects matters as much as the significance stars.
# - Participants who did not submit the weighting form are not part of the weighted-TLX test. The paper should say so.

