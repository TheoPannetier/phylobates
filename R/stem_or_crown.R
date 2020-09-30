#' Find wheter origin is stem or crown lineages
#'
#' `phylo` is rooted on its stem if nb nodes = nb tips, and on its crown
#' otherwise
#'
#' @inheritParams pars_doc
#'
#' @author Théo Pannetier
#' @export
stem_or_crown <- function(phylo) {
  ifelse(
    ape::Nnode(phylo) == ape::Ntip(phylo),
    "stem",
    "crown"
  )
}

