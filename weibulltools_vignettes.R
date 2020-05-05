# weibulltools ID plots

failures <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Failure1.csv")
suspensions <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Suspensions_Iststand.csv")

library(weibulltools)
library(dplyr)

library(SPREDA) # for dataset shock
data(total)
# generate random ids for units:
total$id <- sample(c(letters, LETTERS), size = nrow(total), replace = FALSE)

# using tibble for better print:
as_tibble(total)

# Comparison of failure modes:
ggplot(data = total, aes(x = Mode, y = Distance)) +
  geom_boxplot() +
  theme_bw()

df_mr <- mr_method(id = total$id[total$Censor == 1],
                   x = total$Distance[total$Censor == 1],
                   event = total$Censor[total$Censor == 1])
knitr::kable(df_mr, format = "html", row.names = FALSE, align = "c",
             caption = "Table 1: Failure probabilities using failed items.")

# second case where both survived and filed units are considered
df_john <- johnson_method(id = total$id, x = total$Distance, event = total$Censor)
knitr::kable(df_john, format = "html", row.names = FALSE, align = "c",
             caption = "Table 2: Failure probabilities using all items.")

# Weibull grid for probabilities calculated with Johnson:
weibull_grid <- plot_prob(x = df_john$characteristic, y = df_john$prob,
                          event = df_john$status, id = df_john$id,
                          distribution = "weibull",
                          title_main = "Weibull Probability Plot",
                          title_x = "Mileage in km",
                          title_y = "Probability of Failure in %",
                          title_trace = "Failures (Johnson)")

# Adding a trace so that estimated probabilities of mr_method can be plotted in
# the same graph:
# Arguments inside add_trace:
#   y: Must be transformed such that quantiles of smallest extreme value distribution are plotted.
#   x: Since distribution in plot_prob is "weibull" the x axis is already on log scale.
#      Thus x can be plugged in on natural scale.
weibull_grid_both <- weibull_grid %>%
  add_trace(data = df_mr, type = "scatter", mode = "markers", x = ~characteristic,
            y = ~SPREDA::qsev(prob), name = "Failures (MR)", color = I("#006400"),
            hoverinfo = "text", text = ~paste("ID:", id,
                                              paste("<br>", paste0("Mileage", ":")), characteristic,
                                              paste("<br>", paste0("Probability", ":")), round(prob, digits = 5)))
weibull_grid_both

# log normal grid for probabilities calculated with johnson
lognorm_grid <- plot_prob(x = df_john$characteristic, y = df_john$prob,
                          event = df_john$status, id = df_john$id,
                          distribution = "lognormal",
                          title_main = "Log-Normal Probability Plot",
                          title_x = "Mileage in km",
                          title_y = "Probability of Failure in %",
                          title_trace = "Defect Shock Absorbers")
lognorm_grid

# MRR-Code for two parametric Weibull
data(total)
total$id <- sample(c(letters, LETTERS), size = nrow(total), replace = FALSE)

# Rank Regression:
# for rank_regression(), estimated failure probabilities are required:
df_shock <- johnson_method(id = total$id, x = total$Distance, event = total$Censor)

# Using all models which are provided in rank_regression:
dists <- c("weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
           "weibull3", "lognormal3", "loglogistic3")

mrr_list <- lapply(dists, rank_regression, x = df_shock$characteristic,
                   y = df_shock$prob, event = df_shock$status)

r_sq_vec <- sapply(mrr_list, "[[", "r_squared")
names(r_sq_vec) <- dists
r_sq_vec

# Construct a plot and add regression line
mrr_weibull <- rank_regression(x = df_shock$characteristic, y = df_shock$prob,
                               event = df_shock$status, distribution = "weibull")
mrr_weibull

# Probability plot:
weibull_grid <- plot_prob(x = df_shock$characteristic, y = df_shock$prob,
                          event = df_shock$status, id = df_shock$id,
                          distribution = "weibull",
                          title_main = "Weibull Probability Plot",
                          title_x = "Mileage in km",
                          title_y = "Probability of Failure in %",
                          title_trace = "Defect Shock Absorbers")

library(plotly) # pipe operator
# Add regression line:
weibull_plot <- weibull_grid %>%
  plot_mod(x = df_shock$characteristic, loc_sc_params = mrr_weibull$loc_sc_coefficients,
           distribution = "weibull",
           title_trace = "Median Rank Regression")
weibull_plot

# Using all models which are provided in ml_estimation:
ml_list <- lapply(dists, ml_estimation, x = df_shock$characteristic,
                  event = df_shock$status)

loglik_vec <- sapply(ml_list, "[[", "logL")
names(loglik_vec) <- dists
loglik_vec

# Again estimating weibull:
ml_weibull <- ml_estimation(x = df_shock$characteristic, event = df_shock$status,
                            distribution = "weibull")
ml_weibull

# Add ML estimation to weibull_plot:
## predict_prob to calculate CDF with ML-parameters:
ml_prob <- predict_prob(q = seq(6200, 30600, length.out = 100),
                        loc_sc_params = ml_weibull$loc_sc_coefficients,
                        distribution = "weibull")

weibull_both <- weibull_plot %>%
  add_lines(x = seq(6200, 30600, length.out = 100), y = SPREDA::qsev(ml_prob),
            name = "Maximum Likelihood", color = I("#006400"), hoverinfo = "text",
            text = ~paste(paste("\u03B7<sub>ML</sub>", ":",
                                round(ml_weibull$coefficients[[1]], digits = 2)),
                          "<br>", paste("\u03B2<sub>ML</sub>", ":",
                                        round(ml_weibull$coefficients[[2]], digits = 2))))
weibull_both


# ML-Code for two and three parametric lognormal
# Data:
cycles <- c(300, 300, 300, 300, 300, 291, 274, 271, 269, 257, 256, 227, 226,
            224, 213, 211, 205, 203, 197, 196, 190, 189, 188, 187, 184, 180,
            180, 177, 176, 173, 172, 171, 170, 170, 169, 168, 168, 162, 159,
            159, 159, 159, 152, 152, 149, 149, 144, 143, 141, 141, 140, 139,
            139, 136, 135, 133, 131, 129, 123, 121, 121, 118, 117, 117, 114,
            112, 108, 104, 99, 99, 96, 94)
state <- c(rep(0, 5), rep(1, 67))
id <- 1:length(cycles)

# Two-parameter Log-normal:
ml_lognormal <- ml_estimation(x = cycles, event = state,
                              distribution = "lognormal")
ml_lognormal

# Three-parameter Log-normal:
ml_lognormal3 <- ml_estimation(x = cycles, event = state,
                               distribution = "lognormal3")
ml_lognormal3

# Constructing probability plot:
df_alloy <- johnson_method(x = cycles, event = state, id = id)
lognormal_grid <- plot_prob(x = df_alloy$characteristic, y = df_alloy$prob,
                            event = df_alloy$status, id = df_alloy$id,
                            distribution = "lognormal",
                            title_main = "Log-normal Probability Plot",
                            title_x = "Cycles",
                            title_y = "Probability of Failure in %",
                            title_trace = "Failed Units")

# Add three-parametric model to grid:
lognormal_plot <- lognormal_grid %>%
  plot_mod(x = df_alloy$characteristic, loc_sc_params = ml_lognormal3$loc_sc_coefficients,
           distribution = "lognormal3",
           title_trace = "Three-parametric Log-normal")
lognormal_plot

# Add two-parametric model to lognormal_plot:
## predict_prob to calculate CDF with ML-parameters:
ml_prob_lognormal <- predict_prob(q = seq(85, 325, length.out = 100),
                                  loc_sc_params = ml_lognormal$loc_sc_coefficients,
                                  distribution = "lognormal")

lognormal_both <- lognormal_plot %>%
  add_lines(x = seq(85, 325, length.out = 100), y = qnorm(ml_prob_lognormal),
            name = "Two-parametric Log-normal", color = I("#006400"), hoverinfo = "text",
            text = ~paste(paste("\u03BC<sub>ML</sub>", ":",
                                round(ml_lognormal$loc_sc_coefficients[[1]], digits = 2)),
                          "<br>", paste("\u03C3<sub>ML</sub>", ":",
                                        round(ml_lognormal$loc_sc_coefficients[[2]], digits = 2))))
lognormal_both


# Mixture models: Segmented regression and EM algorithm
hours <- c(2, 28, 67, 119, 179, 236, 282, 317, 348, 387, 3, 31, 69, 135,
           191, 241, 284, 318, 348, 392, 5, 31, 76, 144, 203, 257, 286,
           320, 350, 412, 8, 52, 78, 157, 211, 261, 298, 327, 360, 446,
           13, 53, 104, 160, 221, 264, 303, 328, 369, 21, 64, 113, 168,
           226, 278, 314, 328, 377)

state <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,
           1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
           1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           0, 1, 1, 1, 1, 1, 1)

id <- 1:length(hours)

# Estimating failure probabilities:
df_john <- johnson_method(id = id, x = hours, event = state)

# Probability plot:
weibull_plot <- plot_prob(x = df_john$characteristic, y = df_john$prob,
                          event = df_john$status, id = df_john$id,
                          distribution = "weibull",
                          title_main = "Weibull Probability Plot",
                          title_x = "Time in Hours",
                          title_y = "Probability of Failure in %",
                          title_trace = "Defect Items")
weibull_plot

# Applying mixmod_regression():
mixreg_weib <- mixmod_regression(x = df_john$characteristic, y = df_john$prob,
                                 event = df_john$status, distribution = "weibull")

# Using plot_prob_mix().
mix_reg_plot <- plot_prob_mix(x = hours, event = state, id = id,
                              distribution = "weibull", mix_output = mixreg_weib,
                              title_main = "Weibull Mixture Regression", title_x = "Time in Hours",
                              title_y = "Probability of Failure", title_trace = "Subgroup")
mix_reg_plot

# Using plot_mod_mix() to visualize regression lines of subgroups:
mix_reg_lines <- plot_mod_mix(mix_reg_plot, x = hours, event = state,
                              mix_output = mixreg_weib, distribution = "weibull", title_trace = "Fitted Line")
mix_reg_lines

# Applying mixmod_regression():
mixem_weib <- mixmod_em(x = hours, event = state, distribution = "weibull",
                        conf_level = 0.95, k = 2, method = "EM", n_iter = 150)

# Using plot_prob_mix():
mix_em_plot <- plot_prob_mix(x = hours, event = state, id = id,
                             distribution = "weibull", mix_output = mixem_weib,
                             title_main = "Weibull Mixture EM", title_x = "Time in Hours",
                             title_y = "Probability of Failure", title_trace = "Subgroup")
mix_em_plot

# Using plot_mod_mix() to visualize regression lines of subgroups:
mix_em_lines <- plot_mod_mix(mix_em_plot, x = hours, event = state,
                             mix_output = mixem_weib, distribution = "weibull", title_trace = "Fitted Line")
mix_em_lines

# okay lets apply what we learned to the suspension & failure data we have
#
# suspensions and failures will be data frames with columns "Hours" and "ID"
# The ID column will not necessarily exist.

# the reference for these data frames is datasuspensions()$inputdata

failures <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Failure1.csv")
suspensions <- scan("I:\\airworthiness.p\\01_Mitarbeiter\\Benedikt Arnthof\\WeibullApp\\Suspensions_Iststand.csv")
# these are converted to data frames usually
suspensions <- data.frame(hours = suspensions)
failures <- data.frame(hours = failures)

# function to clean data and return the necessary structure for weibulltools syntax
prep_4_weibulltools <- function(suspensions, failures) {
  susps <- id_colcheck(suspensions)
  fails <- id_colcheck(failures)
  total <- dplyr::bind_rows(fails, susps)
  total$ID <- as.character(levels(total$ID))[total$ID]
  total$ID[is.na(total$ID)] <- (nrow(total) - (length(total$ID[is.na(total$ID)])) + 1):nrow(total)
  total$event <- c(rep(1, nrow(fails)), rep(0, nrow(susps)))
  total
}

# function to create column ID if it is missing
id_colcheck <- function(df) {
  if (!("ID" %in% colnames(df))) {
    df$ID <- NA
  }
  df
}

total <- prep_4_weibulltools(suspensions, failures)

# Next steps: ====
# use output of prep_4_weibulltools to get the johnson method ranks
# use userinput to decide rank regression or maximum likelihood
df_johnson <- johnson_method(id = total$ID, x = total$hours, event = total$event)

# Using all models which are provided in rank_regression:
dists <- c("weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
           "weibull3", "lognormal3", "loglogistic3")

mrr_list <- lapply(dists, rank_regression, x = df_johnson$characteristic,
                   y = df_johnson$prob, event = df_johnson$status)

r_sq_vec <- sapply(mrr_list, "[[", "r_squared")
names(r_sq_vec) <- dists
r_sq_vec

# Construct a plot and add regression line
mrr_weibull <- rank_regression(x = df_johnson$characteristic, y = df_johnson$prob,
                               event = df_johnson$status, distribution = "weibull")
mrr_weibull

# Probability plot:
weibull_grid <- plt_probweibulltools(x = df_johnson$characteristic, y = df_johnson$prob,
                                     event = df_johnson$status, id = df_johnson$id,
                                     distribution = "weibull",
                                     title_main = "Weibull Probability Plot",
                                     title_x = "Age in hours",
                                     title_y = "Probability of Failure in %",
                                     title_trace = "Uncensored Events")

library(plotly) # pipe operator
# Add regression line:
weibull_plot <- weibull_grid %>%
  plt_weibulltoolsmod(x = df_johnson$characteristic, loc_sc_params = mrr_weibull$loc_sc_coefficients,
                      distribution = "weibull",
                      title_trace = "Median Rank Regression")
weibull_plot

# Using all models which are provided in ml_estimation:
ml_list <- lapply(dists, ml_estimation, x = df_johnson$characteristic,
                  event = df_johnson$status)

loglik_vec <- sapply(ml_list, "[[", "logL")
names(loglik_vec) <- dists
loglik_vec

# Again estimating weibull:
ml_weibull <- ml_estimation(x = df_johnson$characteristic, event = df_johnson$status,
                            distribution = "weibull")
ml_weibull

# Add ML estimation to weibull_plot:
## predict_prob to calculate CDF with ML-parameters:
ml_prob <- predict_prob(q = seq(6200, 30600, length.out = 100),
                        loc_sc_params = ml_weibull$loc_sc_coefficients,
                        distribution = "weibull")

weibull_both <- weibull_plot %>%
  add_lines(x = seq(6200, 30600, length.out = 100), y = SPREDA::qsev(ml_prob),
            name = "Maximum Likelihood", color = I("#006400"), hoverinfo = "text",
            text = ~paste(paste("\u03B7<sub>ML</sub>", ":",
                                round(ml_weibull$coefficients[[1]], digits = 2)),
                          "<br>", paste("\u03B2<sub>ML</sub>", ":",
                                        round(ml_weibull$coefficients[[2]], digits = 2))))
weibull_both


plt_weibulltoolsmod <- function(p_obj, x, y = NULL, loc_sc_params, distribution = c("weibull",
                                                                                    "lognormal", "loglogistic", "normal", "logistic", "sev",
                                                                                    "weibull3", "lognormal3", "loglogistic3"), title_trace = "Fit") {
  distribution <- match.arg(distribution)
  if (!(distribution %in% c("weibull", "lognormal", "loglogistic",
                            "normal", "logistic", "sev", "weibull3", "lognormal3",
                            "loglogistic3"))) {
    stop("No valid distribution!")
  }
  # if (is.null(y)) {
  # limits are calculated here. need to assimilate to the workings of weibullr
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  x_low <- x_min - 10^floor(log10(x_min)) * 0.25
  x_high <- x_max + 10^floor(log10(x_max)) * 0.25
  x_p <- seq(x_low, x_high, length.out = 200)
  y_p <- predict_prob(q = x_p, loc_sc_params = loc_sc_params,
                      distribution = distribution)
  # }
  # else {
  #   x_p <- x
  #   y_p <- y
  # }
  df_p <- data.frame(x_p = x_p, y_p = y_p)
  x_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$xaxis$title,
                            " "))[1]
  y_mark <- unlist(strsplit(p_obj$x$layoutAttrs[[2]]$yaxis$title,
                            " "))[1]
  if (distribution %in% c("weibull", "weibull3", "sev")) {
    q <- SPREDA::qsev(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
                   round(loc_sc_params[[2]], digits = 2))
    param_label <- c("µ:", "s:")
    if (distribution == "weibull") {
      param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
                     round(1/loc_sc_params[[2]], digits = 2))
      param_label <- c("<U+03B7>:", "ß:")
    }
    if (distribution == "weibull3") {
      param_val <- c(round(exp(loc_sc_params[[1]]), digits = 2),
                     round(1/loc_sc_params[[2]], digits = 2), round(loc_sc_params[[3]],
                                                                    digits = 2))
      param_label <- c("<U+03B7>:", "ß:", "<U+03B3>:")
    }
  }
  if (distribution %in% c("lognormal", "lognormal3", "normal")) {
    q <- stats::qnorm(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
                   round(loc_sc_params[[2]], digits = 2))
    param_label <- c("µ:", "s:")
    if (distribution == "lognormal3") {
      param_val <- c(param_val, round(loc_sc_params[[3]],
                                      digits = 2))
      param_label <- c(param_label, "<U+03B3>:")
    }
  }
  if (distribution %in% c("loglogistic", "loglogistic3", "logistic")) {
    q <- stats::qlogis(y_p)
    param_val <- c(round(loc_sc_params[[1]], digits = 2),
                   round(loc_sc_params[[2]], digits = 2))
    param_label <- c("µ:", "s:")
    if (distribution == "loglogistic3") {
      param_val <- c(param_val, round(loc_sc_params[[3]],
                                      digits = 2))
      param_label <- c(param_label, "<U+03B3>:")
    }
  }
  hovertext <- paste(paste0(x_mark, ":"), round(x_p, digits = 2),
                     paste("<br>", paste0(y_mark, ":")), round(y_p, digits = 5),
                     "<br>", paste(param_label[1], param_val[1]), "<br>",
                     paste(param_label[2], param_val[2]))
  if (length(loc_sc_params) == 3) {
    hovertext <- paste(hovertext, "<br>", paste(param_label[3],
                                                param_val[3]))
  }
  p_mod <- plotly::add_lines(p = p_obj, data = df_p, x = ~x_p,
                             y = ~q, type = "scatter", mode = "lines", hoverinfo = "text",
                             color = I("#CC2222"), name = title_trace, text = ~hovertext)
  return(p_mod)
}

########################################
plt_layoutweibulltools <- function(x, distribution = c("weibull", "lognormal", "loglogistic",
                                                       "normal", "logistic", "sev"), title_main = "Probability Plot",
                                   title_x = "Characteristic", title_y = "Unreliability")
{
  distribution <- match.arg(distribution)
  if (!(distribution %in% c("weibull", "lognormal", "loglogistic",
                            "normal", "logistic", "sev"))) {
    stop("No valid distribution!")
  }
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    x_base <- function(xb) floor(log10(xb))
    xlog10_range <- (x_base(min(x)) - 1):x_base(max(x))
    x_ticks <- sapply(xlog10_range, function(z) seq(10^z,
                                                    10^(z + 1), 10^z), simplify = TRUE)
    x_ticks <- round(as.numeric(x_ticks), digits = 10)
    x_ticks <- x_ticks[!duplicated(x_ticks)]
    x_labels <- x_ticks
    x_labels[c(rep(F, 3), rep(T, 6))] <- ""
  }
  y_s <- c(1e-07, 1e-06, 1e-05, 1e-04, 0.001, 0.01, 0.05, 0.1,
           0.2, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999,
           0.9999, 0.99999)
  if (distribution %in% c("weibull", "sev")) {
    y_ticks <- SPREDA::qsev(y_s)
  }
  if (distribution %in% c("lognormal", "normal")) {
    y_ticks <- stats::qnorm(y_s)
  }
  if (distribution %in% c("loglogistic", "logistic")) {
    y_ticks <- stats::qlogis(y_s)
  }
  y_labels <- y_s * 100
  xaxs_type <- ifelse(test = distribution %in% c("sev", "normal",
                                                 "logistic"), yes = "-", no = "log")
  x_config <- list(color = "#000000", title = title_x, titlefont = list(family = "Arial",
                                                                        size = 12, color = "#A3A3A3"), type = xaxs_type, autorange = TRUE,
                   rangemode = "normal", ticks = "inside", tickcolor = "#a0a0a0",
                   tickwidth = 1, tickfont = list(family = "Arial", size = 10,
                                                  color = "#a0a0a0"), showticklabels = TRUE, zeroline = FALSE,
                   showgrid = TRUE, gridwidth = 1, exponentformat = "none",
                   showline = TRUE, linecolor = "#a0a0a0")
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    x_config <- c(x_config, list(tickvals = x_ticks, ticktext = x_labels))
  }
  y_config <- list(color = "#000000", title = title_y, titlefont = list(family = "Arial",
                                                                        size = 12, color = "#A3A3A3"), autorange = TRUE, tickvals = y_ticks,
                   ticktext = y_labels, ticks = "inside", tickcolor = "#a0a0a0",
                   tickwidth = 1, tickfont = list(family = "Arial", size = 10,
                                                  color = "#a0a0a0"), showticklabels = TRUE, zeroline = FALSE,
                   showgrid = TRUE, gridwidth = 1, exponentformat = "none",
                   showline = TRUE, linecolor = "#a0a0a0")
  l <- list(titlefont = list(family = "Arial", size = 10, color = "#000000"))
  m <- list(l = 55, r = 10, b = 55, t = 25, pad = 4)
  p <- plotly::plotly_empty() %>% plotly::layout(title = title_main,
                                                 titlefont = list(family = "Arial", size = 16, color = "#000000"),
                                                 separators = ".", legend = l, xaxis = x_config, yaxis = y_config,
                                                 margin = m)
  return(p)
}
######################

plt_probweibulltools <- function(x, y, event, id = rep("XXXXXX", length(x)), distribution = c("weibull",
                                                                                              "lognormal", "loglogistic", "normal", "logistic", "sev"),
                                 title_main = "Probability Plot", title_x = "Characteristic",
                                 title_y = "Unreliability", title_trace = "Sample")
{
  distribution <- match.arg(distribution)
  if (!(distribution %in% c("weibull", "lognormal", "loglogistic",
                            "normal", "logistic", "sev"))) {
    stop("No valid distribution!")
  }
  x_s <- x[event == 1]
  y_s <- y[event == 1]
  id_s <- id[event == 1]
  x_s <- x_s[order(x_s)]
  y_s <- y_s[order(x_s)]
  id_s <- id_s[order(x_s)]
  p <- plt_layoutweibulltools(x = x, distribution = distribution, title_main = title_main,
                              title_x = title_x, title_y = title_y)
  mark_x <- unlist(strsplit(title_x, " "))[1]
  mark_y <- unlist(strsplit(title_y, " "))[1]
  if (distribution %in% c("weibull", "sev")) {
    q <- SPREDA::qsev(y_s)
  }
  if (distribution %in% c("lognormal", "normal")) {
    q <- stats::qnorm(y_s)
  }
  if (distribution %in% c("loglogistic", "logistic")) {
    q <- stats::qlogis(y_s)
  }
  plot <- p %>% plotly::add_trace(x = ~x_s, y = ~q, type = "scatter",
                                  mode = "markers", hoverinfo = "text", color = I("#3C8DBC"),
                                  name = title_trace, text = ~paste("ID:", id_s, paste("<br>",
                                                                                       paste0(mark_x, ":")), x_s, paste("<br>", paste0(mark_y,
                                                                                                                                       ":")), round(y_s, digits = 5))) %>% plotly::layout(showlegend = TRUE)
  return(plot)
}
