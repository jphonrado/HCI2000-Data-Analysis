# =============================================================================
# HCI2000 - 2.3 Comparing conditions (paper section 4.7.4)
#
# Run this AFTER main.ipynb. It reads the cleaned tables saved in notebook 1.3
# and the analysis routes chosen in notebook 2.2.4.
# =============================================================================


# ---- 2.3.0 Setup -------------------------------------------------------------

# The project folder (the one containing main.ipynb). Change if it moves.
PROJECT_DIR <- "~/Documents/School/Term 1 A.Y. 26-27/HCI2000/HCI2000-Data-Analysis"

# Same as DATA_DIR in main.ipynb: "dummy_data" now, the real-data folder later.
DATA_DIR <- "dummy_data"

ALPHA <- 0.05
options(width = 200)   # print wide tables on one line

# Install any missing packages once, then load them.
#   afex    - repeated-measures ANOVA (parametric route)
#   ARTool  - aligned rank transform ANOVA (non-parametric route)
#   lme4    - logistic mixed model for first-selection accuracy
#   emmeans - simple effects for accuracy follow-ups (installed with ARTool)
packages <- c("afex", "ARTool", "lme4", "emmeans")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(afex)
  library(ARTool)
  library(lme4)
  library(emmeans)
})

# Read the files saved by main.ipynb
setwd(path.expand(PROJECT_DIR))
cleaned_dir <- file.path(DATA_DIR, "cleaned")
results_dir <- file.path(DATA_DIR, "results")
dir.create(results_dir, showWarnings = FALSE)

condition_data <- read.csv(file.path(cleaned_dir, "condition_level.csv"))
trial_data     <- read.csv(file.path(cleaned_dir, "trial_level.csv"))
routes         <- read.csv(file.path(cleaned_dir, "analysis_routes.csv"))

# Factors with a fixed order: H before V, low before high.
as_design_factors <- function(df) {
  df$participant <- factor(df$participant)
  df$orientation <- factor(df$orientation, levels = c("H", "V"))
  df$tab_load    <- factor(df$tab_load, levels = c("low", "high"))
  df
}
condition_data <- as_design_factors(condition_data)
trial_data     <- as_design_factors(trial_data)

# pandas saves true/false as "True"/"False", which R reads as text. Convert to
# R's TRUE/FALSE; blank cells (not applicable) become NA.
as_logical <- function(x) {
  x <- trimws(tolower(as.character(x)))
  ifelse(x == "true", TRUE, ifelse(x == "false", FALSE, NA))
}
for (col in c("include_rt", "include_accuracy", "first_selection_correct",
              "no_selection", "technical_loss", "is_repeat")) {
  trial_data[[col]] <- as_logical(trial_data[[col]])
}

# The route chosen for each outcome in notebook 2.2.4
ROUTE_LIST <- setNames(routes$route, routes$outcome)
route_for <- function(outcome) ROUTE_LIST[[outcome]]

cat("Routes from notebook 2.2.4:\n")
print(routes, row.names = FALSE)

# ---- 2.3.1 Retrieval time and weighted NASA-TLX ------------------------------
# One value per participant x condition (condition_level.csv).
#   parametric     -> two-way repeated-measures ANOVA (afex::aov_ez)
#   non-parametric -> aligned rank transform ANOVA     (ARTool::art)
# Both test orientation, load and their interaction; effect size = partial eta^2.
# A repeated-measures ANOVA needs all four conditions per participant, so a
# participant missing any condition (e.g. no weighting form) is left out of
# that outcome's test, and n shows how many were used.

complete_participants <- function(df, outcome) {
  df <- df[!is.na(df[[outcome]]), c("participant", "orientation", "tab_load", outcome)]
  counts <- table(df$participant)
  droplevels(df[df$participant %in% names(counts)[counts == 4], ])
}

EFFECT_NAMES <- c(orientation = "orientation", tab_load = "load",
                  "orientation:tab_load" = "interaction")

analyse_continuous <- function(outcome, route) {
  df <- complete_participants(condition_data, outcome)
  n <- nlevels(df$participant)

  if (route == "parametric") {
    fit <- suppressMessages(aov_ez(id = "participant", dv = outcome, data = df,
                                   within = c("orientation", "tab_load"),
                                   anova_table = list(es = "pes")))
    tab <- fit$anova_table
    result <- data.frame(term = rownames(tab), df1 = tab[["num Df"]], df2 = tab[["den Df"]],
                         F = tab[["F"]], partial_eta_sq = tab[["pes"]], p = tab[["Pr(>F)"]])
    test <- "RM-ANOVA"
  } else {
    fit <- art(as.formula(paste(outcome, "~ orientation * tab_load + Error(participant)")), data = df)
    tab <- as.data.frame(anova(fit))
    # partial eta^2 from F and its degrees of freedom
    result <- data.frame(term = tab$Term, df1 = tab$Df, df2 = tab$Df.res, F = tab$`F value`,
                         partial_eta_sq = tab$`F value` * tab$Df / (tab$`F value` * tab$Df + tab$Df.res),
                         p = tab$`Pr(>F)`)
    test <- "ART ANOVA"
  }

  data.frame(outcome = outcome, route = route, test = test,
             effect = unname(EFFECT_NAMES[result$term]),
             n = n, df1 = result$df1, df2 = result$df2, statistic = result$F,
             effect_size = result$partial_eta_sq, effect_size_type = "partial eta^2",
             p = result$p)
}

continuous_results <- rbind(
  analyse_continuous("mean_rt_s", route_for("mean_rt_s")),
  analyse_continuous("weighted_tlx", route_for("weighted_tlx"))
)

print(continuous_results, row.names = FALSE, digits = 3)

# ---- 2.3.2 First-selection accuracy -------------------------------------------
# Trial level (trial_level.csv), only trials marked include_accuracy.
# Logistic mixed model: correct ~ orientation * tab_load + (1 | participant).
# Each effect is tested with a likelihood ratio test (LRT) between nested models:
#   interaction: full model           vs  model without the interaction
#   orientation: orientation + load   vs  load only
#   load:        orientation + load   vs  orientation only
# Effect size = odds ratio (OR) with a 95% CI.
# If the model cannot be fitted, the paper's fallback is used: each participant's
# accuracy % per condition goes through the same route as the other outcomes.

acc_trials <- trial_data[trial_data$include_accuracy, ]
acc_trials$correct <- as.integer(acc_trials$first_selection_correct)

fit_accuracy_models <- function(df) {
  control <- glmerControl(optimizer = "bobyqa")
  list(
    full        = glmer(correct ~ orientation * tab_load + (1 | participant), df, binomial, control = control),
    additive    = glmer(correct ~ orientation + tab_load + (1 | participant), df, binomial, control = control),
    load_only   = glmer(correct ~ tab_load + (1 | participant), df, binomial, control = control),
    orient_only = glmer(correct ~ orientation + (1 | participant), df, binomial, control = control)
  )
}

lrt_row <- function(smaller, larger, effect, odds_model, coef_name) {
  test <- anova(smaller, larger)
  ci <- exp(confint(odds_model, parm = coef_name, method = "Wald"))
  data.frame(outcome = "first_selection_correct", route = "logistic mixed model", test = "LRT",
             effect = effect, n = nlevels(droplevels(acc_trials$participant)),
             df1 = test$Df[2], df2 = NA, statistic = test$Chisq[2],
             effect_size = exp(fixef(odds_model)[[coef_name]]),
             effect_size_type = sprintf("odds ratio (95%% CI %.2f-%.2f)", ci[1], ci[2]),
             p = test$`Pr(>Chisq)`[2])
}

accuracy_models <- tryCatch(fit_accuracy_models(acc_trials), error = function(e) {
  message("Logistic mixed model could not be fitted: ", conditionMessage(e))
  NULL
})

if (!is.null(accuracy_models)) {
  m <- accuracy_models
  if (isSingular(m$full)) {
    message("Note: the accuracy model has a singular fit (participant variance near 0). ",
            "This is reported, not treated as a failure.")
  }
  # Odds ratios: main effects from the additive model, interaction from the full model.
  #   orientationV              - odds of a correct first selection, vertical vs horizontal
  #   tab_loadhigh              - high vs low load
  #   orientationV:tab_loadhigh - how much the vertical/horizontal OR changes at high load
  accuracy_results <- rbind(
    lrt_row(m$load_only,   m$additive, "orientation", m$additive, "orientationV"),
    lrt_row(m$orient_only, m$additive, "load",        m$additive, "tab_loadhigh"),
    lrt_row(m$additive,    m$full,     "interaction", m$full,     "orientationV:tab_loadhigh")
  )
} else {
  # Fallback (paper 4.7.4): accuracy % per participant x condition goes through the
  # same route as the other outcomes. Its route is decided here with the same rule
  # as notebook 2.2: Shapiro-Wilk on the three within-participant differences.
  df <- complete_participants(condition_data, "accuracy_pct")
  w <- tapply(df$accuracy_pct, list(df$participant, paste(df$orientation, df$tab_load)), mean)
  diffs <- list(
    orientation = (w[, "H low"] + w[, "H high"]) / 2 - (w[, "V low"] + w[, "V high"]) / 2,
    load        = (w[, "H high"] + w[, "V high"]) / 2 - (w[, "H low"] + w[, "V low"]) / 2,
    interaction = (w[, "H high"] - w[, "V high"]) - (w[, "H low"] - w[, "V low"])
  )
  # shapiro.test fails if every value is identical (e.g. all 100 %): treat as not normal
  shapiro_p <- vapply(diffs, function(d) tryCatch(shapiro.test(d)$p.value, error = function(e) 0), numeric(1))
  ROUTE_LIST["accuracy_pct"] <- if (all(shapiro_p >= ALPHA)) "parametric" else "non-parametric"
  message("Accuracy fallback route: ", ROUTE_LIST[["accuracy_pct"]],
          " (Shapiro-Wilk p = ", paste(round(shapiro_p, 3), collapse = ", "), ")")
  accuracy_results <- analyse_continuous("accuracy_pct", ROUTE_LIST[["accuracy_pct"]])
}

print(accuracy_results, row.names = FALSE, digits = 3)

# ---- 2.3.3 Holm adjustment and hypothesis decisions --------------------------
# Each null hypothesis (H0,1 orientation; H0,2 load; H0,3 interaction) is tested
# through three outcomes. Their three p-values are Holm-adjusted together, and
# H0 is rejected if at least one adjusted p <= 0.05.

all_results <- rbind(continuous_results, accuracy_results)
all_results$p_holm <- ave(all_results$p, all_results$effect,
                          FUN = function(p) p.adjust(p, method = "holm"))
all_results$significant <- all_results$p_holm <= ALPHA

hypotheses <- data.frame(
  hypothesis = c("H0,1 (RQ1)", "H0,2 (RQ2)", "H0,3 (RQ3, primary)"),
  effect = c("orientation", "load", "interaction")
)
hypotheses$rejected <- vapply(hypotheses$effect, function(e) {
  any(all_results$significant[all_results$effect == e])
}, logical(1))
hypotheses$outcomes_responsible <- vapply(hypotheses$effect, function(e) {
  hits <- all_results$outcome[all_results$effect == e & all_results$significant]
  if (length(hits) == 0) "-" else paste(hits, collapse = ", ")
}, character(1))

print(all_results[, c("outcome", "effect", "p", "p_holm", "significant")], row.names = FALSE, digits = 3)
print(hypotheses, row.names = FALSE)

# ---- 2.3.4 Follow-ups for significant interactions ---------------------------
# For each outcome whose interaction is significant (Holm-adjusted), compare the
# orientations at low load and at high load separately (difference = H - V):
#   parametric      -> paired t-test,  effect size Cohen's d (d_z)
#   non-parametric  -> Wilcoxon signed-rank test, effect size rank-biserial r
#   accuracy        -> odds ratio H vs V at each load, from the full model
# The expected pattern (paper 4.7.4) is a vertical advantage that grows with load.

rank_biserial <- function(d) {
  d <- d[d != 0]
  r <- rank(abs(d))
  (sum(r[d > 0]) - sum(r[d < 0])) / sum(r)
}

follow_up_continuous <- function(outcome, route) {
  df <- complete_participants(condition_data, outcome)
  rows <- lapply(c("low", "high"), function(load) {
    h <- df[df$orientation == "H" & df$tab_load == load, ]
    v <- df[df$orientation == "V" & df$tab_load == load, ]
    h <- h[order(h$participant), outcome]
    v <- v[order(v$participant), outcome]
    d <- h - v
    if (route == "parametric") {
      t <- t.test(h, v, paired = TRUE)
      data.frame(tab_load = load, mean_difference_H_minus_V = mean(d), test = "paired t-test",
                 statistic = unname(t$statistic), effect_size = mean(d) / sd(d),
                 effect_size_type = "Cohen's d_z", p = t$p.value)
    } else {
      w <- suppressWarnings(wilcox.test(h, v, paired = TRUE, exact = FALSE))
      data.frame(tab_load = load, mean_difference_H_minus_V = mean(d), test = "Wilcoxon signed-rank",
                 statistic = unname(w$statistic), effect_size = rank_biserial(d),
                 effect_size_type = "rank-biserial r", p = w$p.value)
    }
  })
  cbind(outcome = outcome, do.call(rbind, rows))
}

follow_up_accuracy <- function() {
  simple <- summary(pairs(emmeans(accuracy_models$full, ~ orientation | tab_load), type = "response"))
  data.frame(outcome = "first_selection_correct", tab_load = as.character(simple$tab_load),
             mean_difference_H_minus_V = NA, test = "Wald z (model-based)",
             statistic = simple$z.ratio, effect_size = simple$odds.ratio,
             effect_size_type = "odds ratio H / V", p = simple$p.value)
}

significant_interactions <- all_results$outcome[all_results$effect == "interaction" & all_results$significant]
follow_ups <- do.call(rbind, lapply(significant_interactions, function(outcome) {
  if (outcome == "first_selection_correct") follow_up_accuracy()
  else follow_up_continuous(outcome, route_for(outcome))
}))

if (is.null(follow_ups)) {
  cat("No significant interaction, so no follow-ups.\n")
} else {
  print(follow_ups, row.names = FALSE, digits = 3)
  for (outcome in unique(follow_ups$outcome[!is.na(follow_ups$mean_difference_H_minus_V)])) {
    d <- follow_ups$mean_difference_H_minus_V[follow_ups$outcome == outcome]
    cat(sprintf("%s: H - V is %.3f at low load and %.3f at high load -> %s\n", outcome, d[1], d[2],
                if (abs(d[2]) > abs(d[1])) "the orientation difference grows with load"
                else "the orientation difference does not grow with load"))
  }
}

# ---- 2.3.5 Saving the results --------------------------------------------------
write.csv(all_results, file.path(results_dir, "test_results.csv"), row.names = FALSE)
write.csv(hypotheses, file.path(results_dir, "hypothesis_decisions.csv"), row.names = FALSE)
if (!is.null(follow_ups)) {
  write.csv(follow_ups, file.path(results_dir, "follow_ups.csv"), row.names = FALSE)
}
cat("\nResults saved to", normalizePath(results_dir), "\n")
print(list.files(results_dir))

# ---- 2.3.6 Visualising the orientation difference -----------------------------
# One panel per outcome. Each grey line is one participant's orientation
# difference (Horizontal - Vertical) at low and high load; the blue line is the
# mean with a 95% confidence interval. Lines that are not flat show the
# interaction (RQ3); where the blue points sit relative to 0 shows the
# follow-ups from 2.3.4. Accuracy uses each participant's accuracy %.
# The figure appears in the RStudio Plots pane and is saved as a PNG.

PLOT_OUTCOMES <- list(
  mean_rt_s    = list(title = "Retrieval time (s)",          note = "above 0: horizontal slower"),
  accuracy_pct = list(title = "First-selection accuracy (%)", note = "below 0: horizontal less accurate"),
  weighted_tlx = list(title = "Weighted NASA-TLX",            note = "above 0: horizontal more demanding")
)

orientation_gap <- function(outcome) {
  df <- complete_participants(condition_data, outcome)
  w <- tapply(df[[outcome]], list(df$participant, paste(df$orientation, df$tab_load)), mean)
  cbind(low = w[, "H low"] - w[, "V low"], high = w[, "H high"] - w[, "V high"])
}

draw_orientation_gap <- function() {
  old <- par(mfrow = c(1, 3), mar = c(4.5, 4.8, 4.2, 1), las = 1, family = "sans",
             col.axis = "#52514e", col.lab = "#52514e", fg = "#52514e")
  on.exit(par(old))
  for (outcome in names(PLOT_OUTCOMES)) {
    gap <- orientation_gap(outcome)
    n <- nrow(gap)
    mean_gap <- colMeans(gap)
    ci <- qt(0.975, n - 1) * apply(gap, 2, sd) / sqrt(n)
    ylim <- range(c(gap, mean_gap - ci, mean_gap + ci, 0))
    plot(NA, xlim = c(0.75, 2.25), ylim = ylim, xaxt = "n", bty = "l",
         xlab = "Tab load", ylab = "Horizontal - Vertical")
    abline(h = pretty(ylim), col = "#e5e4e0", lwd = 0.8)          # light grid
    abline(h = 0, col = "#8a8984", lty = 2, lwd = 1.2)             # no difference
    axis(1, at = 1:2, labels = c("Low", "High"))
    segments(1, gap[, "low"], 2, gap[, "high"], col = "#cfcdc6", lwd = 1)              # participants
    points(rep(1:2, each = n), c(gap[, "low"], gap[, "high"]), pch = 16, col = "#bdbbb3", cex = 0.9)
    arrows(1:2, mean_gap - ci, 1:2, mean_gap + ci, angle = 90, code = 3, length = 0.05,  # 95% CI
           col = "#2a78d6", lwd = 2)
    lines(1:2, mean_gap, col = "#2a78d6", lwd = 2.5)                                     # mean
    points(1:2, mean_gap, pch = 16, col = "#2a78d6", cex = 1.8)
    title(main = PLOT_OUTCOMES[[outcome]]$title, adj = 0, line = 2, cex.main = 1.1, font.main = 1, col.main = "#0b0b0b")
    mtext(PLOT_OUTCOMES[[outcome]]$note, side = 3, adj = 0, line = 0.7, cex = 0.75, col = "#52514e")
  }
}

draw_orientation_gap()                                   # show in RStudio
png(file.path(results_dir, "orientation_gap_by_load.png"), width = 1800, height = 650, res = 150)
draw_orientation_gap()                                   # save as PNG
invisible(dev.off())
