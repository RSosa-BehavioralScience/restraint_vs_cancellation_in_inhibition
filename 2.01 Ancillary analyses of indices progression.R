# =============================================================================
# Supplementay analyses of indices progression
# Analyses of the per-rat t-half values and of the session-by-session RRI-RCI
# correlation. Run after Script number 2. Includes:
#
# 1. Paired Wilcoxon signed-rank test of t-half(RCI) - t-half(RRI),
#    bootstrap CI of the median paired difference, and a spaghetti 
#    (connected dots) figure.
# 2. Pearson correlation between t1/2(RRI) and t1/2(RCI) with a 
#    one-sided inferiority tests (H0: rho >= bound) against 1.0 and, as an
#    exploratory extension, against the mirrored bounds.
# 3. Cross-sectional RRI-RCI correlation at every session, with CIs and a
#    weighted linear trend, to look for a monotonic change across training.
#
# =============================================================================

# ---- Packages and settings ------------------------------------------------
library(ggplot2)                       

SEED        <- 1401                    # seed for the bootstrap 
N_BOOT      <- 10000                   # bootstrap resamples 
CONF        <- 0.95                    # confidence level for all intervals
TOST_BOUNDS <- c(1.0, 0.9, 0.7)        # bounds b for H0: rho >= b. 1.0 is the confirmatory test;
# 0.9 and 0.7 are the exploratory mirrored bounds (edit freely)

# p-value formatter for captions and console output ("p < 0.001" or "p = 0.034")
fmt_p <- function(p) if (is.na(p)) "p = NA" else if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)

has_toster <- requireNamespace("TOSTER", quietly = TRUE)   # TRUE when TOSTER is installed
if (!has_toster) message("TOSTER not installed: the TOSTER cross-check column will be NA.")

# ---- Time to half-criterion: builds thalf_table, dat and N_SESS from RvsC -----
# Input is the long data frame RvsC (columns rat, session, RRI, RCI) already in the session.
# RvsC itself is not modified; the block works on a copy called rv.
# All t-half values are expressed in re-indexed sessions (gaps left by removed sessions are closed).
LAST_K         <- 9                    # size of the terminal window (last k usable sessions)
CRITERION      <- 0.5                  # proportion of the terminal value defining t1/2
SMOOTH_SPAN    <- 5                    # sliding-window width, in sessions
MIN_OBS_WINDOW <- 4                    # a window is used only if at least this many of its
# SMOOTH_SPAN sessions have an observed value
# (SMOOTH_SPAN - 1 = at most one missing value per window)
TERMINAL_RULE  <- "both"               # "both"  : usable = RRI and RCI both observed in that session
# "index" : usable = the index being analysed is observed
# (set this to whatever the main analysis used)
PLOT_SCALE     <- "terminal"           # display scale only; t1/2 is unaffected
# "terminal" : each index as a proportion of its own terminal
#              value (0 = baseline, 1 = terminal, 0.5 = criterion)
# "z"        : z-score within each rat and index

# ---- Copy the input and force numeric columns ----------------------------
stopifnot(all(c("rat", "session", "RRI", "RCI") %in% names(RvsC)))  # fail early on wrong columns
rv <- RvsC                                             # work on a copy
# If NaN/NA were imported as text, RRI or RCI arrive as character or factor and
# mean() returns NA with a warning. Convert explicitly and report what was lost.
to_num <- function(x) {
  x_chr <- as.character(x)                             # factor -> its labels, not its integer codes
  x_chr <- gsub(",", ".", x_chr, fixed = TRUE)         # tolerate decimal commas from spreadsheets
  out   <- suppressWarnings(as.numeric(x_chr))         # "NaN"/"NA" and any other text become NA
  bad   <- is.na(out) & !is.na(x) & !(x_chr %in% c("NaN", "NA", ""))  # text that was neither blank nor NaN/NA
  if (any(bad)) warning(sum(bad), " value(s) could not be read as numbers and were set to NA: ",
                        paste(unique(x_chr[bad]), collapse = ", "))
  out
}
rv$rat     <- as.character(rv$rat)                 # animal IDs as plain text
rv$session <- to_num(rv$session)                   # session numbers must be numeric
rv$RRI     <- to_num(rv$RRI)                       # restraint index
rv$RCI     <- to_num(rv$RCI)                       # cancellation index

# ---- Helper: sliding-window mean placed at the window midpoint ------------
# Slides a window of `span` consecutive sessions over one rat's trajectory.
# Each window mean is assigned to the midpoint of its sessions (a 5-session
# window over sessions 1-5 is placed at 3, a 4-session window at 2.5), so the
# smoothed curve is not shifted in time. Windows are never truncated at the edges, and a window with
# fewer than `min_obs` observed values is dropped. `session` must be the
# consecutive, sorted session grid of that rat (guaranteed by the re-indexing step below).
smooth_win <- function(session, y, span, min_obs) {
  K <- length(y)                                       # number of sessions on the grid
  if (K < span) return(data.frame(x = numeric(0), v = numeric(0)))  # trajectory shorter than the window
  starts <- seq_len(K - span + 1)                      # first session of every full window
  x <- numeric(length(starts))                         # window midpoints
  v <- rep(NA_real_, length(starts))                   # window means
  for (j in seq_along(starts)) {                       # loop over windows
    idx  <- starts[j]:(starts[j] + span - 1)           # sessions inside this window
    x[j] <- mean(session[idx])                         # midpoint of the window
    if (sum(!is.na(y[idx])) >= min_obs) {              # enough observed values?
      v[j] <- mean(y[idx], na.rm = TRUE)               # mean of the observed values
    }
  }
  out <- data.frame(x = x, v = v)                      # assemble
  out[!is.na(out$v), ]                                 # keep only windows that could be computed
}

# ---- Helper: t1/2 for one rat and one index -------------------------------
# d     : data frame for ONE rat with columns session, RRI, RCI (full session grid)
# index : "RRI" or "RCI"
extract_thalf <- function(d, index) {
  y    <- d[[index]]                                   # raw trajectory of this index
  sess <- d$session                                    # re-indexed session numbers (1..K)
  
  # -- 4a. choose the usable sessions that define the terminal window
  if (TERMINAL_RULE == "both") {
    usable <- !is.na(d$RRI) & !is.na(d$RCI)            # both indices observed
  } else {
    usable <- !is.na(y)                                # this index observed
  }
  usable_sess <- sess[usable]                          # session numbers that are usable
  last_sess   <- tail(sort(usable_sess), LAST_K)       # the last LAST_K of them
  n_term      <- length(last_sess)                     # how many we actually obtained
  terminal    <- mean(y[sess %in% last_sess], na.rm = TRUE)  # terminal value = window mean
  crit        <- CRITERION * terminal                  # half-criterion; baseline is zero
  
  # -- 4b. a half-criterion is only meaningful for a positive terminal value
  base <- data.frame(rat = d$rat[1], index = index,    # skeleton of the result row
                     terminal = terminal, criterion = crit,
                     n_terminal = n_term, t_half = NA_real_, status = NA_character_)
  if (n_term == 0 || is.na(terminal) || terminal <= 0) {  # is.na() is TRUE for both NA and NaN
    base$status <- "nonpositive terminal"              # no rise above zero to halve
    return(base)
  }
  
  # -- 4c. smooth the trajectory with full sliding windows
  sm <- smooth_win(sess, y, SMOOTH_SPAN, MIN_OBS_WINDOW)  # window midpoints (x) and means (v)
  s  <- sm$x                                           # midpoints, in session units
  v  <- sm$v                                           # smoothed values
  
  # -- 4d. first crossing of the criterion, linearly interpolated
  k <- which(v >= crit)[1]                             # first smoothed point at/above criterion
  if (is.na(k)) {                                      # never reached the criterion
    base$status <- "never crossed"
    return(base)
  }
  if (k == 1) {                                        # first full window is already above criterion
    base$t_half <- s[1]                                # t1/2 is at most the first window midpoint
    base$status <- "left-censored"
    return(base)
  }
  # v[k-1] < crit <= v[k] by construction; interpolate between the two midpoints
  base$t_half <- s[k - 1] + (crit - v[k - 1]) * (s[k] - s[k - 1]) / (v[k] - v[k - 1])
  base$status <- "ok"                                  # regular crossing
  base                                                 # return the completed row
}

# ---- Close the gaps left by removed sessions ------------------------------
# A session counts as removed when no rat has an observed RRI or RCI in it.
# The surviving sessions are renumbered 1..K so that curves have no gap.
has_data  <- !is.na(rv$RRI) | !is.na(rv$RCI)       # rows carrying any observed index value
kept_orig <- sort(unique(rv$session[has_data]))      # original numbers of the surviving sessions
removed   <- setdiff(seq(min(kept_orig), max(kept_orig)), kept_orig)  # original numbers with no data
cat("Sessions with no data for any rat, closed up:",    # tell the user exactly what was removed
    if (length(removed)) paste(removed, collapse = ", ") else "none", "\n")
session_map <- data.frame(session_orig = kept_orig,    # original -> re-indexed mapping
                          session = seq_along(kept_orig))
rv <- rv[rv$session %in% kept_orig, ]            # drop rows belonging to removed sessions
rv$session <- session_map$session[match(rv$session, session_map$session_orig)]  # renumber
stopifnot(!anyDuplicated(rv[, c("rat", "session")])) # one row per rat and session
N_SESS <- nrow(session_map)                            # number of sessions after re-indexing

# ---- Put every rat on the full re-indexed grid ----------------------------
rats <- sort(unique(rv$rat))                         # the 16 animals
grid <- expand.grid(rat = rats, session = seq_len(N_SESS),       # every rat x session cell
                    stringsAsFactors = FALSE)
dat  <- merge(grid, rv[, c("rat", "session", "RRI", "RCI")],   # rat-specific absences become NA rows
              by = c("rat", "session"), all.x = TRUE)
dat  <- dat[order(dat$rat, dat$session), ]             # sort for tidy per-rat slices

# ---- Extract t1/2 for every rat and both indices --------------------------
rows <- list()                                         # collector for result rows
for (r in rats) {                                      # loop over animals
  d_r <- dat[dat$rat == r, ]                           # this rat's full-grid data
  for (ix in c("RRI", "RCI")) {                        # loop over the two indices
    rows[[length(rows) + 1]] <- extract_thalf(d_r, ix) # compute and store
  }
}
thalf_table <- do.call(rbind, rows)                    # stack into one table
rownames(thalf_table) <- NULL

# ---- Per-rat quality-control figure ----------------------------------------
# One panel per rat, both indices on a common scale (display only, t-half is unaffected).
# a. smoothed trajectories for every rat and index (window midpoints and means)
smooth_df <- do.call(rbind, lapply(rats, function(r) {
  do.call(rbind, lapply(c("RRI", "RCI"), function(ix) {
    d_r <- dat[dat$rat == r, ]                         # this rat's full-grid data
    sm  <- smooth_win(d_r$session, d_r[[ix]], SMOOTH_SPAN, MIN_OBS_WINDOW)
    if (nrow(sm) == 0) return(NULL)                    # nothing to draw for this panel
    data.frame(rat = r, index = ix, x = sm$x, v = sm$v)
  }))
}))

# b. per rat and index, the affine transform (value - center) / scale used for display
tr <- do.call(rbind, lapply(rats, function(r) {
  do.call(rbind, lapply(c("RRI", "RCI"), function(ix) {
    y    <- dat[[ix]][dat$rat == r]                    # raw trajectory
    trow <- thalf_table[thalf_table$rat == r & thalf_table$index == ix, ]
    if (PLOT_SCALE == "terminal") {                    # proportion of the terminal value
      center <- 0                                      # zero baseline is kept
      scale  <- trow$terminal                          # terminal value maps to 1
    } else {                                           # z-score within rat and index
      center <- mean(y, na.rm = TRUE)
      scale  <- sd(y, na.rm = TRUE)
    }
    if (is.na(center)) center <- 0                     # fallbacks so a degenerate panel is drawn raw
    if (is.na(scale) || scale <= 0) scale <- 1         # (this also covers nonpositive terminal values)
    data.frame(rat = r, index = ix, center = center, scale = scale)
  }))
}))
tr_key  <- paste(tr$rat, tr$index)                     # lookup key for the transform
to_plot <- function(df, col) {                         # apply the transform to column `col` of df
  m <- match(paste(df$rat, df$index), tr_key)          # matching transform row for each record
  (df[[col]] - tr$center[m]) / tr$scale[m]
}

# c. long format of the raw data, and display-scale versions of everything drawn
long <- rbind(
  data.frame(dat[, c("rat", "session")], index = "RRI", value = dat$RRI),
  data.frame(dat[, c("rat", "session")], index = "RCI", value = dat$RCI))
long$value_p      <- to_plot(long, "value")            # raw points on the display scale
smooth_df$v_p     <- to_plot(smooth_df, "v")           # smoothed curve on the display scale
crit_df           <- thalf_table                       # half-criterion and t1/2 per panel
crit_df$crit_p    <- to_plot(crit_df, "criterion")     # criterion on the display scale
zero_df           <- tr                                # zero baseline on the display scale
zero_df$zero_p    <- (0 - tr$center) / tr$scale

# d. draw: one panel per rat, one shared y-axis so panels are comparable
y_lab <- if (PLOT_SCALE == "terminal") "Proportion of terminal value" else "z-score (within rat and index)"
pqc <- ggplot() +
  geom_line(data = smooth_df, aes(x, v_p, colour = index, group = index),    # smoothed trajectory
            linewidth = 0.9) +
  geom_hline(data = crit_df, aes(yintercept = crit_p),                       # half-criterion, neutral colour
             colour = "grey40", linetype = "dashed", linewidth = 0.3) +
  geom_vline(data = crit_df[!is.na(crit_df$t_half), ],                       # extracted t-half
             aes(xintercept = t_half, colour = index), linetype = "dotted", linewidth = 0.7) +
  scale_colour_manual(values = c(RCI = "#FF4500", RRI = "#5349FF")) +        # fixed colour per index
  scale_x_continuous(breaks = seq(0, N_SESS, 5)) +                           # session axis
  facet_wrap(~ rat) +                                                        # one panel per rat, shared axes
  labs(x = "Session", y = y_lab, colour = NULL) +
  theme_bw(base_size = 9)
#ggsave("thalf_QC.png", pqc, width = 11, height = 8, dpi = 150)
pqc
#save at 550x450

# ---- One row per rat with both t-half values --------------------------------
rri <- thalf_table[thalf_table$index == "RRI", c("rat", "t_half", "status")]  # RRI side
rci <- thalf_table[thalf_table$index == "RCI", c("rat", "t_half", "status")]  # RCI side
names(rri) <- c("rat", "t_RRI", "status_RRI")                                 # rename for the merge
names(rci) <- c("rat", "t_RCI", "status_RCI")
pairs_all <- merge(rri, rci, by = "rat")                                      # one row per rat
pairs_all <- pairs_all[!is.na(pairs_all$t_RRI) & !is.na(pairs_all$t_RCI), ]   # complete pairs only
pairs_all$censored <- pairs_all$status_RRI == "left-censored" |               # either value is an upper bound
  pairs_all$status_RCI == "left-censored"
pairs_all$diff <- pairs_all$t_RCI - pairs_all$t_RRI                           # positive = RCI later than RRI
cat("Rats with a complete t-half pair:", nrow(pairs_all), "of", length(unique(thalf_table$rat)),
    "| pairs containing a left-censored value:", sum(pairs_all$censored), "\n")

# two analysis sets. The first is the main one. Left-censored values are upper bounds
# on t1/2, so the second set checks that they do not drive the result.
sets <- list("All complete pairs"       = pairs_all,
             "Excluding left-censored"  = pairs_all[!pairs_all$censored, ])

# ---- Helper function performing Wilcoxon + correlation + inferiority tests for one set -----
analyse_pairs <- function(pr, label) {
  n <- nrow(pr)                                        # number of rats in this set
  d <- pr$diff                                         # paired differences, RCI - RRI
  
  # -- Wilcoxon signed-rank test 
  wt <- wilcox.test(pr$t_RCI, pr$t_RRI, paired = TRUE,  # H0: median paired difference is 0
                    conf.int = TRUE, conf.level = CONF)
  nz   <- d[d != 0]                                    # zero differences carry no rank
  rk   <- rank(abs(nz))                                # ranks of the absolute differences
  Wpos <- sum(rk[nz > 0])                              # rank sum where RCI is later
  Wneg <- sum(rk[nz < 0])                              # rank sum where RCI is earlier
  r_rb <- (Wpos - Wneg) / (Wpos + Wneg)                # matched-pairs rank-biserial correlation
  
  # -- percentile bootstrap CI of the median paired difference (resampling rats)
  set.seed(SEED)                                       # same seed for every set
  boot_med <- replicate(N_BOOT, median(sample(d, replace = TRUE)))
  boot_ci  <- quantile(boot_med, c((1 - CONF) / 2, 1 - (1 - CONF) / 2), names = FALSE)
  
  wil <- data.frame(set = label, n = n,
                    n_RCI_later = sum(d > 0), n_RCI_earlier = sum(d < 0), n_tie = sum(d == 0),
                    median_diff = median(d), boot_lo = boot_ci[1], boot_hi = boot_ci[2],
                    HL_estimate = unname(wt$estimate), HL_lo = wt$conf.int[1], HL_hi = wt$conf.int[2],
                    V = unname(wt$statistic), p_wilcoxon = wt$p.value, rank_biserial = r_rb)
  
  # -- Pearson correlation of the two t-half values
  ct <- cor.test(pr$t_RRI, pr$t_RCI, method = "pearson", conf.level = CONF)
  r  <- unname(ct$estimate)                            # observed correlation
  z  <- atanh(r)                                       # Fisher z of r
  se <- 1 / sqrt(n - 3)                                # standard error of z
  upper1 <- tanh(z + qnorm(CONF) * se)                 # one-sided upper bound 
  
  # -- inferiority tests, H0: rho >= b versus H1: rho < b, one bound per row.
  # p = P(Z <= (z_r - z_b) / se). For b = 1, atanh(1) is infinite, so p is exactly 0 whenever n > 3
  tost <- do.call(rbind, lapply(TOST_BOUNDS, function(b) {
    p_manual <- pnorm((z - atanh(b)) / se)             # Fisher-z inferiority test 
    p_toster <- NA_real_                               # cross-check with TOSTER, when available
    if (has_toster) {
      p_toster <- tryCatch(
        TOSTER::z_cor_test(pr$t_RRI, pr$t_RCI, alternative = "less", method = "pearson",
                           alpha = 1 - CONF, null = b)$p.value,
        error = function(e) NA_real_)                  # NA if TOSTER rejects this call (e.g. null = 1)
    }
    data.frame(set = label, n = n, bound = b, r = r,
               ci_lo = ct$conf.int[1], ci_hi = ct$conf.int[2], upper_onesided = upper1,
               p_inferiority = p_manual, p_TOSTER = p_toster,
               rejects_H0 = p_manual < (1 - CONF))
  }))
  list(wil = wil, tost = tost)
}

res      <- Map(analyse_pairs, sets, names(sets))      # run both sets
wil_res  <- do.call(rbind, lapply(res, `[[`, "wil"))   # Wilcoxon table
tost_res <- do.call(rbind, lapply(res, `[[`, "tost"))  # correlation / inferiority table
rownames(wil_res) <- rownames(tost_res) <- NULL
#write.csv(wil_res,  "thalf_results_wilcoxon.csv", row.names = FALSE)
#write.csv(tost_res, "thalf_results_corr.csv",     row.names = FALSE)
cat("\n---- Paired Wilcoxon (difference = t1/2 RCI - t1/2 RRI; positive = RCI later) ----\n")
print(wil_res, digits = 3)
cat("\n---- Pearson r of t1/2 and inferiority tests (H0: rho >= bound) ----\n")
print(tost_res, digits = 3)

# ---- Spaghetti figure for the paired comparison (main set) ----------------
# The legend assumes that no pair is tied and no value is left-censored
stopifnot("Ties or left-censored pairs present; the legend of this figure assumes none." =
            !any(pairs_all$diff == 0) && !any(pairs_all$censored))
dir_of <- function(d) factor(ifelse(d > 0, "RCI later", "RCI earlier"),   # line colour = direction of the difference
                             levels = c("RCI later", "RCI earlier"))
sp <- rbind(                                           # long format: one row per rat and index
  data.frame(rat = pairs_all$rat, index = "RRI", t_half = pairs_all$t_RRI,
             direction = dir_of(pairs_all$diff)),
  data.frame(rat = pairs_all$rat, index = "RCI", t_half = pairs_all$t_RCI,
             direction = dir_of(pairs_all$diff)))
sp$index <- factor(sp$index, levels = c("RRI", "RCI"))          # RRI on the left
med_df <- data.frame(index = factor(c("RRI", "RCI"), levels = c("RRI", "RCI")),
                     t_half = c(median(pairs_all$t_RRI), median(pairs_all$t_RCI)),  # group medians
                     what = "Group median")                     # label that feeds the legend

psp <- ggplot(sp, aes(index, t_half)) +
  geom_line(aes(group = rat, colour = direction), linewidth = 0.5, alpha = 0.8) +   # one line per rat
  geom_point(aes(colour = direction), size = 2.2) +                                 # one dot per rat and index
  geom_line(data = med_df, aes(index, t_half, group = 1, linetype = what),          # median line
            inherit.aes = FALSE, colour = "black", linewidth = 1.1) +
  geom_point(data = med_df, aes(index, t_half, shape = what),                       # median markers
             inherit.aes = FALSE, size = 4.5, colour = "black") +
  scale_colour_manual(values = c("RCI later" = "#5349FF", "RCI earlier" = "#FF4500"),
                      labels = c("RCI later" = "RRI earlier", "RCI earlier" = "RCI earlier"),  # legend text only
                      name = NULL) +                                                # lists only directions that occur
  scale_shape_manual(values = c("Group median" = 18), name = NULL) +                # black diamond
  scale_linetype_manual(values = c("Group median" = "solid"), name = NULL) +        # same label, so the two merge
  scale_x_discrete(expand = expansion(add = 0.4)) +
  labs(x = "Inhibition index", y = "Sessions to half of terminal performance (t-half)") +
  theme_bw(base_size = 10) +
  theme(legend.position = "right")
#ggsave("thalf_paired.png", psp, width = 6.5, height = 5, dpi = 150)
psp
#500x400

# ---- Cross-sectional correlation at every session -------------------------
# Raw RRI and RCI across rats, one correlation per re-indexed session.
q_two <- qnorm(1 - (1 - CONF) / 2)                     # two-sided normal quantile
sc <- do.call(rbind, lapply(seq_len(N_SESS), function(s) {
  d  <- dat[dat$session == s & !is.na(dat$RRI) & !is.na(dat$RCI), ]   # rats observed on both indices
  n  <- nrow(d)
  na_row <- data.frame(session = s, n = n, r = NA_real_, lo = NA_real_, hi = NA_real_,
                       z = NA_real_, se = NA_real_)
  if (n < 4 || sd(d$RRI) == 0 || sd(d$RCI) == 0) return(na_row)       # CI needs n >= 4 and variance
  r  <- cor(d$RRI, d$RCI)                              # Pearson r at this session
  if (abs(r) >= 1) return(na_row)                      # Fisher z undefined at |r| = 1
  z  <- atanh(r); se <- 1 / sqrt(n - 3)                # Fisher z and its standard error
  data.frame(session = s, n = n, r = r, lo = tanh(z - q_two * se), hi = tanh(z + q_two * se),
             z = z, se = se)
}))
write.csv(sc, "session_cor.csv", row.names = FALSE)

# weighted linear trend of Fisher z on session (weights = inverse sampling variance, n - 3),
# plus Kendall's tau between session and r as a rank-based monotonicity check
sc_ok <- sc[!is.na(sc$r), ]                            # sessions with an estimable correlation
fit_df <- NULL; cap_sc <- "Too few sessions with an estimable correlation for a trend."
if (nrow(sc_ok) >= 3) {
  fit  <- lm(z ~ session, data = sc_ok, weights = n - 3)             # trend on the z scale
  cf   <- summary(fit)$coefficients["session", ]                     # slope, SE, t, p
  cf_ci <- confint(fit, "session", level = CONF)                     # slope CI
  kt   <- cor.test(sc_ok$session, sc_ok$r, method = "kendall")       # monotonic trend, rank-based
  grid_s <- data.frame(session = seq(min(sc_ok$session), max(sc_ok$session), length.out = 100))
  pr_fit <- predict(fit, newdata = grid_s, se.fit = TRUE)            # fitted z and its SE
  tq     <- qt(1 - (1 - CONF) / 2, df = fit$df.residual)             # t quantile for the band
  fit_df <- data.frame(session = grid_s$session, fit = tanh(pr_fit$fit),               # back to r scale
                       lo = tanh(pr_fit$fit - tq * pr_fit$se.fit), hi = tanh(pr_fit$fit + tq * pr_fit$se.fit))
  cat(sprintf("\n---- Cross-sectional correlation trend ----\nWeighted slope (Fisher z per session) = %.4f [%.4f, %.4f], %s\nKendall tau (session vs r) = %.3f, %s\n",
              cf["Estimate"], cf_ci[1], cf_ci[2], fmt_p(cf["Pr(>|t|)"]),
              unname(kt$estimate), fmt_p(kt$p.value)))
  cap_sc <- sprintf(paste0("Points = Pearson r across rats with %d%% CIs (n = %d to %d per session). ",
                           "Line = weighted linear trend of Fisher z (slope %.3f per session, %d%% CI %.3f to %.3f, %s), ",
                           "band = %d%% CI of the fit; Kendall tau = %.2f (%s). Shaded = last %d sessions (nominal terminal window). ",
                           "Descriptive only, since the same rats contribute to every session."),
                    round(100 * CONF), min(sc_ok$n), max(sc_ok$n), cf["Estimate"], round(100 * CONF),
                    cf_ci[1], cf_ci[2], fmt_p(cf["Pr(>|t|)"]),
                    round(100 * CONF), unname(kt$estimate), fmt_p(kt$p.value), LAST_K)
  cap_sc <- paste(strwrap(cap_sc, width = 120), collapse = "\n")
}
psc <- ggplot(sc_ok, aes(session, r)) +
  annotate("rect", xmin = N_SESS - LAST_K + 0.5, xmax = N_SESS + 0.5, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +                                  # nominal terminal window
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +          # no-association reference
  { if (!is.null(fit_df)) geom_ribbon(data = fit_df, aes(session, ymin = lo, ymax = hi),
                                      inherit.aes = FALSE, fill = "gold", alpha = 0.15) } +
  { if (!is.null(fit_df)) geom_line(data = fit_df, aes(session, fit), inherit.aes = FALSE,
                                    colour = "gold", linewidth = 0.9) } +  # linear trend (r scale)
  geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.35) +                 # r with its CI
  scale_x_continuous(breaks = seq(0, N_SESS, 5)) +
  coord_cartesian(ylim = c(-1, 1)) +                                        # full correlation range
  labs(x = "Session", y = "Pearson r between RRI and RCI") +
  theme_bw(base_size = 10)
#ggsave("session_cor.png", psc, width = 7.5, height = 5, dpi = 150)
psc

range(sc$r)                         # lowest and highest coefficient
range(c(sc$lo, sc$hi))              # lowest lower bound, highest upper bound

