#' Cat breeds data block
#'
#' A blockr data block that returns the bundled cat breeds dataset. It exposes a
#' single input: a "Rare breeds only" toggle that prefilters to breeds flagged
#' `rare`.
#'
#' @param rare Logical. Initial state of the "Rare breeds only" toggle.
#' @param ... Forwarded to [blockr.core::new_data_block()].
#' @return A `catbreeds_block` object.
#' @export
new_catbreeds_block <- function(rare = FALSE, ...) {
  new_data_block(
    function(id) {
      moduleServer(id, function(input, output, session) {
        r_rare <- reactiveVal(rare)
        observeEvent(input$rare, r_rare(input$rare))

        list(
          expr = reactive(
            bbquote(
              blockr.catbreeds::catbreeds_fetch(rare = .(rare)),
              list(rare = r_rare())
            )
          ),
          state = list(rare = r_rare)
        )
      })
    },
    function(id) {
      tagList(
        checkboxInput(
          NS(id, "rare"),
          "Rare breeds only",
          value = rare
        )
      )
    },
    class = "catbreeds_block",
    ...
  )
}
