#' Temperament word-cloud block
#'
#' Splits the comma-separated `temperament` text across every breed and renders
#' an echarts word cloud sized by how often each personality word appears.
#'
#' @param ... Forwarded to [blockr.core::new_block()].
#' @return A `temperament_cloud_block` object.
#' @export
new_temperament_cloud_block <- function(...) {
  new_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            e <- quote(local({
              w <- trimws(unlist(strsplit(.(data)$temperament, ",\\s*")))
              w <- w[!is.na(w) & nzchar(w)]
              tab <- as.data.frame(table(word = w), stringsAsFactors = FALSE)
              names(tab) <- c("word", "freq")
              tab |>
                echarts4r::e_charts() |>
                echarts4r::e_cloud(word, freq, sizeRange = c(14, 64)) |>
                echarts4r::e_tooltip() |>
                echarts4r::e_text_style(fontFamily = "Open Sans")
            }))
            bbquote(.(e), list(e = e))
          }),
          state = list()
        )
      })
    },
    function(id) tagList(),
    class = "temperament_cloud_block",
    expr_type = "bquoted",
    ...
  )
}

#' @importFrom blockr.core block_output
#' @exportS3Method
block_output.temperament_cloud_block <- function(x, result, session) {
  echarts4r::renderEcharts4r(result)
}

#' @importFrom blockr.core block_ui
#' @exportS3Method
block_ui.temperament_cloud_block <- function(id, x, ...) {
  tagList(echarts4r::echarts4rOutput(NS(id, "result")))
}
