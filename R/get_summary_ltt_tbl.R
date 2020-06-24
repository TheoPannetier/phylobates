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

get_summary_ltt_tbl <- function(phylos, time_seq) {

  ltts <- TreeSim::LTT.plot.gen(list(phylos))
  # drop avg LTT
  ltts[[1]] <- NULL

  ltt_funcs <- purrr::map(
    ltts,
    function(ltt) {
      ltt_func <- stats::stepfun(
        x = ltt[-1, 1],
        y = ltt[, 2]
      )
      return(ltt_func)
    })

  summary_tbl <- tidyr::expand_grid(
    "i" = seq_along(ltt_funcs),
    "t" = time_seq
  ) %>%
    dplyr::mutate(
      "n" = purrr::map2_dbl(i, t, function(i, t) ltt_funcs[[i]](t))
    ) %>%
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
  return(summary_tbl)
}
