#' Breed card block
#'
#' Renders a small "trading card" for a single breed: its photo, temperament
#' tags and description. Expects a one-row data frame (e.g. fed from a filter
#' that keeps the picked breed) with `name`, `image_url`, `temperament` and
#' `description` columns.
#'
#' @param ... Forwarded to [blockr.core::new_block()].
#' @return A `breed_card_block` object.
#' @export
new_breed_card_block <- function(...) {
  new_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            e <- quote(local({
              r <- .(data)[1, ]
              htmltools::tags$div(
                style = paste(
                  "padding:12px; font-family:'Open Sans',sans-serif;",
                  "max-width:520px; margin:0 auto;"
                ),
                if (!is.na(r$image_url)) {
                  htmltools::tags$img(
                    src = r$image_url,
                    style = paste(
                      "width:100%; max-height:300px; object-fit:cover;",
                      "border-radius:10px; box-shadow:0 2px 10px rgba(0,0,0,.12);"
                    )
                  )
                },
                htmltools::tags$h3(
                  style = "margin:.5em 0 .1em;", r$name
                ),
                htmltools::tags$div(
                  style = "color:#3451b2; font-weight:600; font-size:.9em;",
                  r$temperament
                ),
                htmltools::tags$p(
                  style = "color:#555; font-size:.85em; line-height:1.45;",
                  r$description
                )
              )
            }))
            bbquote(.(e), list(e = e))
          }),
          state = list()
        )
      })
    },
    function(id) tagList(),
    class = "breed_card_block",
    expr_type = "bquoted",
    ...
  )
}

#' @importFrom blockr.core block_output
#' @importFrom shiny renderUI
#' @exportS3Method
block_output.breed_card_block <- function(x, result, session) {
  renderUI(result)
}

#' @importFrom blockr.core block_ui
#' @importFrom shiny uiOutput NS
#' @exportS3Method
block_ui.breed_card_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
