#' Get a table with node and edge info
#'
#' Returns a table containing the tree topology (parent-to-child node pairs;
#' whether a node is a tip or internal), edge (branch) lengths and timing of
#' nodes (branching times).
#'
#' @inheritParams pars_doc
#' @return a `tibble` with one observation per branch and five variables:
#'  * `parent_node` ID of the parent node
#'  * `child_node` ID of the child node
#'  * `is_tip` logical, is the (child) node a tip (`TRUE`) of the tree or an
#'  internal node (`FALSE`)
#'  * `edge_length` numeric length of the edge between parent and child,
#'  i.e. the branch length
#'  * `time` for timetrees, the time of the node relative to present (taken as
#'  the latest tip(s) in the tree)
#'
#' @author Théo Pannetier
#' @export
#'
get_edge_tbl <- function(phylo) {

  branching_times <- numeric(phylo$Nnode)
  ntips <- length(phylo$tip.label)
  crown_node <- ntips + 1

  edge_tbl <- tibble::tibble(
    "parent_node" = phylo$edge[, 1],
    "child_node" = phylo$edge[, 2],
    "is_tip" = "child_node" <= ntips,
    "edge_length" = phylo$edge.length
  ) %>%
    # entry for crown node
    dplyr::bind_rows(
      tibble::tibble(
        "parent_node" = NA,
        "child_node" = crown_node,
        "is_tip" = FALSE,
        "edge_length" = 0
      )
    ) %>%
    # initialise times
    dplyr::mutate(
      "time" = ifelse("child_node" == crown_node, 0, NA)
    )

  # fill times
  for (i in 1:(nrow(edge_tbl) - 1)) { # skip last row (crown)
    parent_i <- which(edge_tbl$child_node == edge_tbl$parent_node[i])
    edge_tbl$time[i] <- edge_tbl$edge_length[i] + edge_tbl$time[parent_i]
  }
  # transform times relative to present
  crown_age <- max(edge_tbl$time)
  edge_tbl$time <- round(edge_tbl$time - crown_age, 3)

  return(edge_tbl)
}
