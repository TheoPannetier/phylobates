#' Get the lineage-through-time function of a tree
#'
#' Builds the lineage-through-time table from the tree, and converts it
#' to a stepwise function of time with [stats::stepfun()]
#'
#' @inheritParams pars_doc
#' @return a stepwise function of time that returns the number of lineages
#' in the tree at any point in time
#' @author Théo Pannetier
#' @export
#'
get_ltt_func <- function(phylo) {
  ltt_tbl <- get_ltt_tbl(phylo)
  ltt_func <- stats::stepfun(
    x = ltt_tbl$time[-1],
    y = ltt_tbl$N
  )
  return(ltt_func)
}
