#' Cat breeds data block
#'
#' A blockr data block that pulls live cat breed data from The Cat API. It has
#' no inputs: it just returns the expression that fetches every breed.
#'
#' @param ... Forwarded to [blockr.core::new_data_block()].
#' @return A `catbreeds_block` object.
#' @export
new_catbreeds_block <- function(...) {
  new_data_block(
    function(id) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive(quote(blockr.catbreeds::catbreeds_fetch())),
          state = list()
        )
      })
    },
    function(id) tagList(),
    class = "catbreeds_block",
    ...
  )
}
