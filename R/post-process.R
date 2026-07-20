## If we have an edge that goes to another block (instead of
## going inside the same block), and the source block of the edge
## has a "last" subblock, then we rewire this edge, such that it
## leaves from the "last" subblock.
##
## The "last" subblock might be a list of ids, in this case we
## need to add extra edges. (This only really happens for
## if-else.
##
## Because we might extend edges as we go along, the loop is
## trciky, so we implement it manually instead of a 'for'.
## The potential new edges are added at the loop cursor, and
## are processed again.

post_process <- function(nodes, edges) {
  ## Build an id -> "last" lookup once (hashed environment), instead of
  ## scanning the whole `nodes` data frame for every edge.
  last_env <- new.env(parent = emptyenv(), hash = TRUE, size = nrow(nodes) * 2L)
  ids <- nodes$id
  lasts <- nodes$last
  for (i in seq_along(ids)) {
    assign(ids[i], lasts[[i]], envir = last_env)
  }

  from_all <- edges$from
  to_all <- edges$to
  res_from <- vector("list", length(from_all))
  res_to <- vector("list", length(from_all))

  ## Resolve each edge independently. When the source block has a "last"
  ## sub-block (and the edge does not go into that block), the edge is
  ## rewired to leave from the "last" sub-block(s). Each rewired source
  ## may itself need rewiring, so we iterate with an explicit stack.
  for (k in seq_along(from_all)) {
    stack_f <- from_all[k]
    stack_t <- to_all[k]
    out_f <- character()
    out_t <- character()
    while (length(stack_f)) {
      f <- stack_f[[1L]]
      t <- stack_t[[1L]]
      stack_f <- stack_f[-1L]
      stack_t <- stack_t[-1L]
      lst <- last_env[[f]]
      if (length(lst) && !is_child(t, f)) {
        stack_f <- c(lst, stack_f)
        stack_t <- c(rep(t, length(lst)), stack_t)
      } else {
        out_f <- c(out_f, f)
        out_t <- c(out_t, t)
      }
    }
    res_from[[k]] <- out_f
    res_to[[k]] <- out_t
  }

  from <- unlist(res_from, use.names = FALSE)
  to <- unlist(res_to, use.names = FALSE)
  keep <- !duplicated.default(paste(from, to, sep = "\r"))
  data.frame(
    stringsAsFactors = FALSE,
    from = from[keep],
    to = to[keep]
  )
}

is_child <- function(child, parent) {
  substring(child, 1, nchar(parent)) == parent && child != parent
}
