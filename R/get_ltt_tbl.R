#' Get a lineage-through-time table
#'
#' Pairs the time of every node in the tree with the number of species
#' in the tree at that point in time, i.e. lineages-through-time if the tree is
#' reconstructed, or species-through-time if the tree includes lineages extinct
#' before present.
#'
#' @inheritParams pars_doc
#' @return a `tibble` with one observation per node and two variables:
#'  * `time` the time relative to present (taken as the latest tip(s) in
#'  the tree)
#'  * `N` number of lineages or species in the phylogeny at this point in time
#'
#'  @author Théo Pannetier
#'  @export

get_ltt_tbl <- function(phylo) {

  time <- NULL
  N <- NULL
  event <- NULL
  increment <- NULL
  time <- NULL

  ltt_tbl <- get_edge_tbl(phylo) %>%
    dplyr::transmute(
      "time" = time,
      "event" = dplyr::case_when(
        is.na(parent_node) ~ "crown",
        !is_tip ~ "speciation",
        is_tip & time < 0 ~ "extinction",
        is_tip & time == 0 ~ "present"
      )
    ) %>%
    dplyr::filter(event != "present") %>%
    dplyr::arrange(time) %>%
    # compute the running number of lineages from event sequence
    dplyr::mutate(
      "increment" = dplyr::case_when(
        event == "crown" ~ 2,
        event == "speciation" ~ 1,
        event == "extinction" ~ -1
      ),
      "N" = cumsum(increment)
    ) %>%
    # drop unused variables
    dplyr::select(time, N)

  return(ltt_tbl)
}
