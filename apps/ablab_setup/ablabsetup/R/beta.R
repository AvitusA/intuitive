days_vs_volume <- function(days, daily_volume, volume_split, theta_a, theta_b) {
  tibble::tibble(day = days,
                 total_volume = days * daily_volume,
                 a_alpha = total_volume * volume_split * theta_a,
                 a_beta = total_volume * volume_split * (1 - theta_a),
                 b_alpha = total_volume * (1 - volume_split) * theta_b,
                 b_beta = total_volume * (1 - volume_split) * (1 - theta_b))
}

quotient_samples <- function(numerator,
                             denominator,
                             samples) {
  rbeta(samples, numerator$alpha, numerator$beta) /
    rbeta(samples, denominator$alpha, denominator$beta)
}

posterior_summary <- function(samples, levels) {
  tail_size <- (1 - levels) / 2
  low_quantiles <- tail_size
  high_quantiles <- 1 - tail_size
  q <- quantile(samples, c(low_quantiles, high_quantiles))
  # This can be replaced with HDI instead of quantiles
  list(mean = mean(samples),
       levels = purrr::imap(levels, ~ list(
         "level" = .x,
         "low" = q[[.y]],
         "high" = q[[length(levels) + .y]]
       )))
}

effect_distributions <- function(days_vs_volume_tbl,
                                 a_prior = list(alpha = 1, beta = 1),
                                 b_prior = list(alpha = 1, beta = 1),
                                 samples = 10e4,
                                 levels = c(0.8, 0.95)) {
  days_vs_volume_tbl %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(~ .x %>% dplyr::mutate(sample_summary =
                                        list(posterior_summary(
                                          quotient_samples(numerator =
                                                             list(alpha = .x$b_alpha + b_prior$alpha,
                                                                  beta = .x$b_beta + b_prior$beta),
                                                           denominator =
                                                             list(alpha = .x$a_alpha + a_prior$alpha,
                                                                  beta = .x$a_beta + a_prior$beta),
                                                           samples),
                                          levels)))) %>%
    dplyr::bind_rows() %>%
    tidyr::unnest_wider(sample_summary) %>%
    tidyr::unnest(levels) %>%
    tidyr::unnest_wider(levels) %>%
    dplyr::mutate(across(c(mean, low, high), .fns = function(x) { x - 1 })) # Effect size rather than quotient
}

plot_uplift_credible_vs_time <- function(days_vs_distr_tbl, critical_effect = 0) {
  the_levels <- sort(unique(days_vs_distr_tbl$level), decreasing = T)
  level_labels <- paste(round(100 * the_levels), "%", sep = "")
  
  days_vs_distr_tbl %>%
    dplyr::mutate(level = factor(level, levels = the_levels, labels = level_labels)) %>%
    ggplot2::ggplot(ggplot2::aes(x = day)) +
    ggplot2::geom_ribbon(ggplot2::aes(fill = level,
                                      ymin = low,
                                      ymax = high),
                         color = "gray50") +
    ggplot2::geom_hline(yintercept = critical_effect) +
    ggplot2::scale_x_sqrt(breaks = c(7, 14, 21, seq(28, max(days_vs_distr_tbl$day), 28)),
                          minor_breaks = seq(0, max(days_vs_distr_tbl$day), 7),
                          name = "Days") +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                name = "Effect Size") +
    ggplot2::scale_fill_brewer(name = "Confidence", direction =  -1)
}

timeline_insights <- function(day_vs_distr_tbl, critical_effect) {
  first_day <- function(x, predicate, name) {
    predicate <- rlang::enexpr(predicate)
    day_vs_distr_tbl %>%
      dplyr::group_by(level) %>%
      dplyr::filter(!!predicate) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::mutate(type = name) %>%
      dplyr::ungroup()
  }
  
  dplyr::bind_rows(
    first_day(day_vs_distr_tbl, low > 0, "gt_0"),
    first_day(day_vs_distr_tbl, high < 0, "lt_0"),
    first_day(day_vs_distr_tbl, low > critical_effect, "gt_crit"),
    first_day(day_vs_distr_tbl, high < critical_effect, "lt_crit")
  ) %>%
    dplyr::arrange(day) %>%
    dplyr::select(day, level, type)
}
