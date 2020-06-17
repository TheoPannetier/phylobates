#' Get lineage-through-time summaries for a set of phylogenetic trees
#'
#' Computes the mean, SD, median, and some quantiles for the number of lineages
#' through time across a set of trees.
#'
#' @param phylos a list of phylogenetic trees.
#' @param time_seq a numeric vector containing the sequence of time points at
#' which the summaries are to be computed.
#'
#' @author Théo Pannetier
#' @export

get_summary_ltt <- function(phylos, time_seq) {

  summary_func <- mean

  ltt_funcs <- purrr::map(
    phylos,
    function(x) get_ltt_func(x)
  )

  combined_ltt_tbl <- expand_grid(
    "t" = time_seq,
    "i" = seq_along(phylos)
  ) %>%
    dplyr::mutate(
      # Get n at time t for each tree i
      "n" = purrr::map2_dbl(i, t, function(i, t) ltt_funcs[[i]](t)
      )
    )

  summary_ltt <- combined_ltt_tbl %>%
    dplyr::group_by(t) %>%
    dplyr::summarise(
      "mean_n" = mean(n),
      "sd_n" = sd(n),
      "median" = median(n),
      "q1" = quantile(n, 0.25),
      "q3" = quantile(n, 0.75),
      "qt_10" = quantile(n, 0.10),
      "qt_90" = quantile(n, 0.90)
    )
  return(summary_ltt)
}
