#' Lineage-through-time plot for a tree
#'
#' Uses `ggplot` to plot the LTT of a tree
#'
#' @inheritParams pars_doc
#' @author Théo Pannetier
#' @export
#'
plot_ltt <- function(phylo) {
  ltt_tbl <- get_ltt_tbl(phylo)

  gg <- ltt_tbl %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = N)) +
    ggplot2::geom_step()

  return(gg)
}
