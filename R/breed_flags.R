#' Breed special-traits badges
#'
#' Shows the picked breed's boolean traits as little badges: a green check when
#' the breed has the trait (lap cat, hypoallergenic, hairless, rare, indoor) and
#' a grey dash when it does not. Expects a one-row data frame (e.g. the picked
#' breed) with the 0/1 flag columns.
#'
#' @param ... Forwarded to [blockr.core::new_block()].
#' @return A `breed_flags_block` object.
#' @export
new_breed_flags_block <- function(...) {
  new_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            e <- quote(local({
              r <- .(data)[1, ]
              flags <- list(
                "Lap cat"        = r$lap,
                "Hypoallergenic" = r$hypoallergenic,
                "Hairless"       = r$hairless,
                "Rare breed"     = r$rare,
                "Indoor"         = r$indoor
              )
              badge <- function(label, value) {
                on <- isTRUE(suppressWarnings(as.integer(value)) == 1L)
                htmltools::tags$span(
                  style = paste0(
                    "display:inline-flex; align-items:center; gap:.4rem;",
                    "margin:4px; padding:.35rem .8rem; border-radius:9999px;",
                    "font-size:.85rem; font-weight:500;",
                    if (on) "background:#e6f4ea; color:#1e7e34;"
                    else "background:#f1f1f3; color:#9aa0a6;"
                  ),
                  htmltools::tags$b(if (on) "✓" else "–"),
                  label
                )
              }
              htmltools::tags$div(
                style = "padding:12px; font-family:'Open Sans',sans-serif;",
                lapply(names(flags), function(nm) badge(nm, flags[[nm]]))
              )
            }))
            bbquote(.(e), list(e = e))
          }),
          state = list()
        )
      })
    },
    function(id) tagList(),
    class = "breed_flags_block",
    expr_type = "bquoted",
    ...
  )
}

#' @importFrom blockr.core block_output
#' @importFrom shiny renderUI
#' @exportS3Method
block_output.breed_flags_block <- function(x, result, session) {
  renderUI(result)
}

#' @importFrom blockr.core block_ui
#' @importFrom shiny uiOutput NS
#' @exportS3Method
block_ui.breed_flags_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
