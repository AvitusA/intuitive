##' @export
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

posterior_summary <- function(samples, crit) {
  list(p_gt_0 = mean(samples > 1),
       p_gt_crit = mean(samples > (1 + crit)),
       crit = crit)
}

##' @export
effect_distributions <- function(days_vs_volume_tbl,
                                 a_prior = list(alpha = 1, beta = 1),
                                 b_prior = list(alpha = 1, beta = 1),
                                 samples = 10e4,
                                 crit = 0) {
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
                                          crit)))) %>%
    dplyr::bind_rows() %>%
    tidyr::unnest_wider(sample_summary)
}

##' @export
credible_vs_time <- function(days_vs_distr_tbl, volume_per_day, levels) {
  day_breaks <- c(7, 14, seq(28, max(days_vs_distr_tbl$day), 28))
  volume_breaks <- day_breaks * volume_per_day
  crit <- unique(days_vs_distr_tbl$crit)
  
  all_timeline_insights <- timeline_insights(days_vs_distr_tbl, levels)
  
  filtered_days_vs_distr_tbl <- days_vs_distr_tbl %>%
    dplyr::select(day, p_gt_0, p_gt_crit) %>%
    tidyr::pivot_longer(c(p_gt_0, p_gt_crit)) %>%
    dplyr::filter(crit != 0 | name != "p_gt_crit") %>%
    dplyr::left_join(all_timeline_insights, by = c("day", "name"))
  filtered_days_vs_distr_tbl %>% 
    # TODO: Factor labels instead
    dplyr::mutate(name = factor(name, 
                                levels = c("p_gt_0", "p_gt_crit"),
                                labels = c("B is better than A",
                                           if(crit > 0) { 
                                             paste("B is at least ", round(crit * 100), "% better than A", sep = "")
                                           } else {
                                             paste("B is no more than ", round(-crit * 100), "% worse than A", sep = "")
                                           }))) %>%
    ggplot2::ggplot(ggplot2::aes(x = day)) +
    ggplot2::geom_line(ggplot2::aes(y = value, color = name)) +
    ggrepel::geom_label_repel(ggplot2::aes(y = value, label = id), nudge_y = -0.15) +
    ggplot2::scale_x_sqrt(breaks = day_breaks,
                          minor_breaks = seq(0, max(days_vs_distr_tbl$day), 7),
                          guide = ggplot2::guide_axis(angle = 45),
                          name = "Experiment Duration (Days)",
                          sec.axis = ggplot2::sec_axis(~ . * volume_per_day,
                                                       name = "Total Exposures",
                                                       breaks = volume_breaks,
                                                       labels = human_format,
                                                       guide = ggplot2::guide_axis(angle = 45))) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                name = "Probability",
                                breaks = c(0, 0.05, 0.10, 0.20, 
                                           0.5,
                                           0.8, 0.9, 0.95),
                                guide = ggplot2::guide_axis(check.overlap = T)) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::scale_color_discrete("") +
    ggplot2::theme(legend.position = "bottom",
                   axis.text = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 16),
                   legend.text = ggplot2::element_text(size = 16)) -> the_plot
  
  list(plot = the_plot,
       insights = all_timeline_insights)
}

##' @export
human_format <- Vectorize(function(x, digits = 2) {
  suffix <- NA
  mantissa <- NA
  if (x < 1e3) {
    suffix <- ""
    mantissa <- x
  } else if(x < 1e6) {
    suffix <- "k"
    mantissa <- x / 1e3
  } else if(x < 1e9) {
    suffix <- "M"
    mantissa <- x / 1e6
  } else if(x < 1e12) {
    suffix <- "G"
    mantissa <- x / 1e9
  } else {
    suffix <- "T"
    mantissa <- x / 1e12
  }
  paste(signif(mantissa, digits), suffix, sep = "")
})

##' @export
timeline_insights <- function(day_vs_distr_tbl, levels) {
  first_day <- function(predicate, name, desc) {
    predicate <- rlang::enexpr(predicate)
    day_vs_distr_tbl %>%
      tidyr::crossing(level = levels) %>%
      dplyr::filter(!!predicate) %>%
      dplyr::group_by(level) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::mutate(name = name,
                    desc = desc) %>%
      dplyr::ungroup()
  }
  critical_effect <- unique(day_vs_distr_tbl$crit)
  p_gt_0 <- dplyr::bind_rows(first_day(p_gt_0 > level, "p_gt_0", "B is better than A"),
                             first_day(p_gt_0 < 1 - level, "p_gt_0", "B is worse than A"))
  
  p_gt_crit <- if(critical_effect == 0) {
    NULL
  } else {
    dplyr::bind_rows(first_day(p_gt_crit > level, "p_gt_crit",
                               dplyr::if_else(critical_effect > 0,  
                                              paste("B is at least ", round(critical_effect*100), "% better than A.", sep = ""),
                                              paste("B is not more than ", round(-critical_effect*100), "% worse than A.", sep = ""))),
                     first_day(p_gt_crit < 1 - level, "p_gt_crit",
                               dplyr::if_else(critical_effect > 0,
                                              paste("B is NOT at least ", round(critical_effect*100), "% better than A.", sep = ""),
                                              paste("B is at least ", round(-critical_effect*100), "% worse than A.", sep = ""))))
      
  }
  
  dplyr::bind_rows(p_gt_0, p_gt_crit) %>%
    dplyr::arrange(day) %>%
    dplyr::transmute(id = dplyr::row_number(), day, total_volume, level, name, desc)
}