library(tidyverse)
library(cowplot)
make_funnel_phe <- function(data, numerator, denominator, highlighted = FALSE, y_limits = NULL) {
  numerator <- enquo(numerator)
  denominator <- enquo(denominator)
  highlighted <- enquo(highlighted)
  make_funnel_lims <- function(x_max, mean_prop, conf_int = 0.95) {
    tibble(
      x = accumulate(1:100, ~max(round((x_max / .x) ^ (1 / (101 - .y)) * .x, 0), .x + 1), .init = 1),
      mean = mean_prop,
      lower = pmax(0,((mean_prop*(x/qnorm((1-conf_int)/2)^2+1)-sqrt((-8*mean_prop*(x/qnorm((1-conf_int)/2)^2+1))^2-64*(1/qnorm((1-conf_int)/2)^2 +1/x)*mean_prop*(x*(mean_prop*(x/qnorm((1-conf_int)/2)^2+2)-1)+qnorm((1-conf_int)/2)^2*(mean_prop-1)))/8)/(1/qnorm((1-conf_int)/2)^2 +1/x)/x)),
      upper = pmin(1,((mean_prop*(x/qnorm((1-conf_int)/2)^2+1)+sqrt((-8*mean_prop*(x/qnorm((1-conf_int)/2)^2+1))^2-64*(1/qnorm((1-conf_int)/2)^2 +1/x)*mean_prop*(x*(mean_prop*(x/qnorm((1-conf_int)/2)^2+2)-1)+qnorm((1-conf_int)/2)^2*(mean_prop-1)))/8)/(1/qnorm((1-conf_int)/2)^2 +1/x)/x))
    )
  }
  
  num_vals <- rlang::eval_tidy(numerator, data)
  den_vals <- rlang::eval_tidy(denominator, data)
  mean_prop <- sum(num_vals) / sum(den_vals)
  funnel_lims <- list(
    `2SD` = 0.95,
    `3SD` = 0.998
  ) %>% 
    map_dfr(~make_funnel_lims(1.1 * max(den_vals), mean_prop, .x), .id = "limits")
  
  if (is.null(y_limits)) {
    y_limits <- c(
      floor(10 * min(num_vals / den_vals)) / 10,
      ceiling(10 * max(num_vals / den_vals)) / 10
    )
  }

  ggplot(arrange(data, !!highlighted), aes(x = !!denominator, y = !!numerator / !!denominator)) +
    geom_line(aes(x = x, y = lower, linetype = limits), data = funnel_lims, colour = "blue") +
    geom_line(aes(x = x, y = upper, linetype = limits), data = funnel_lims, colour = "blue") +
    geom_hline(aes(yintercept = mean_prop, linetype = "mean")) +
    geom_point(aes(fill = !!highlighted, colour = !!highlighted, size = !!highlighted), shape = 21) +
    scale_linetype_manual("Limits", values = c("dashed", "longdash", "solid")) +
    theme_cowplot() +
    scale_x_continuous("Population") +
    scale_y_continuous("Percentage of population", labels = scales::percent, limits = y_limits) +
    scale_fill_manual(
      values = c(
        `FALSE` = "orange",
        `TRUE` = "green"
      ),
      guide = "none"
    ) +
    scale_colour_manual(
      values = c(
        `FALSE` = "dark orange",
        `TRUE` = "dark green"
      ),
      guide = "none"
    ) +
    scale_size_manual(
      values = c(
        `FALSE` = 2,
        `TRUE` = 4
      ),
      guide = "none"
    )
}
  
# sample_data <- read_tsv("sample_data.txt")
# make_funnel_phe(sample_data, numerator, denominator, pct == "North Dorset PCT")