#' Trait correlation block
#'
#' Computes the Pearson correlation matrix of the numeric breed traits (dropping
#' the `lat`/`lng` coordinates) and returns it in long form (`x`, `y`, `corr`),
#' ready to feed a heatmap block. A tiny custom block, in the spirit of the
#' "no limits" part of the talk.
#'
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A `correlation_block` object.
#' @export
new_correlation_block <- function(...) {
  new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            e <- quote(local({
              d <- .(data)
              # numeric, more than two distinct values (drops 0/1 flags), and
              # not the map coordinates
              keep <- vapply(d, function(col) is.numeric(col) &&
                               length(unique(col[!is.na(col)])) > 2, logical(1L))
              num <- d[, keep & !names(d) %in% c("lat", "lng"), drop = FALSE]
              m <- round(stats::cor(num, use = "pairwise.complete.obs"), 2)
              long <- as.data.frame(as.table(as.matrix(m)),
                                    stringsAsFactors = FALSE)
              stats::setNames(long, c("x", "y", "corr"))
            }))
            bbquote(.(e), list(e = e))
          }),
          state = list()
        )
      })
    },
    function(id) tagList(),
    class = "correlation_block",
    expr_type = "bquoted",
    ...
  )
}
