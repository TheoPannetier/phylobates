get_ltt_func <- function(phylo) {
  ltt_tbl <- get_ltt_tbl(phylo)
  ltt_func <- stats::stepfun(
    x = ltt_tbl$time[-1],
    y = ltt_tbl$N
  )
  return(ltt_func)
}
