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
  ## Look up each node's "last" sub-block by id, instead of rescanning
  ## the whole `nodes` data frame on every edge.
  last_by_id <- nodes$last
  names(last_by_id) <- nodes$id

  e <- 1
  while (e <= nrow(edges)) {
    from <- edges$from[e]
    to <- edges$to[e]
    last <- last_by_id[[from]]

    ## If there is no last sub-block, or the edge edge is going to
    ## a sub-block, then we are all good. Otherwise rewire.
    if (length(last) && !is_child(to, from)) {
      edges$from[e] <- last[1]
      for (l in last[-1]) {
        new_edge <- data.frame(
          stringsAsFactors = FALSE,
          from = l,
          to = to
        )
        edges <- rbind(edges[1:e, ], new_edge, edges[-(1:e), ])
      }
    } else {
      e <- e + 1
    }
  }
  unique(edges)
}

is_child <- function(child, parent) {
  substring(child, 1, nchar(parent)) == parent && child != parent
}
