#' Breed ranking cards
#'
#' KPI-style cards for the picked breed: weight and life span, each shown with
#' the value and a dynamic subtitle comparing it to the other breeds (e.g.
#' "22% higher than other breeds"). Unlike a static KPI block, the comparison
#' recomputes whenever the pick changes. Expects the full breed set with a
#' `highlight` column (the picked breed's value is its own name, the rest
#' `"Other breeds"`).
#'
#' @param ... Forwarded to [blockr.core::new_block()].
#' @return A `breed_stats_block` object.
#' @export
new_breed_stats_block <- function(...) {
  new_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            e <- quote(local({
              d <- .(data)
              pick <- d[d$highlight != "Other breeds", ][1, ]
              other <- d[d$highlight == "Other breeds", ]

              compare <- function(value, ref) {
                pct <- round(100 * (value / mean(ref, na.rm = TRUE) - 1), 1)
                paste0(abs(pct), "% ", if (pct >= 0) "higher" else "lower",
                       " than others")
              }
              # health_issues is 1 (fewest problems) to 4 (most), so word it and
              # frame the comparison as fewer/more issues (lower is better).
              health_words <- c("Robust", "Healthy", "Sensitive", "Fragile")
              health_word <- if (!is.na(pick$health_issues) &&
                                 pick$health_issues %in% 1:4) {
                health_words[pick$health_issues]
              } else {
                "Unknown"
              }
              compare_health <- function(value, ref) {
                pct <- round(100 * (value / mean(ref, na.rm = TRUE) - 1), 1)
                paste0(abs(pct), "% ", if (pct <= 0) "fewer" else "more",
                       " issues than others")
              }
              card <- function(title, value, sub, color, extra = NULL) {
                htmltools::tags$div(
                  style = paste(
                    "flex:1; min-width:150px; margin:6px; padding:1rem;",
                    "background:#fff; border:1px solid #e5e7eb; border-radius:1rem;"
                  ),
                  htmltools::tags$div(
                    style = paste0(
                      "display:inline-block; background:", color, "; color:#fff;",
                      "font-size:.72rem; padding:.25rem .75rem; border-radius:9999px;",
                      "margin-bottom:.5rem;"
                    ),
                    title
                  ),
                  htmltools::tags$div(
                    style = "font-size:1.7rem; font-weight:600; color:#111827;", value
                  ),
                  extra,
                  htmltools::tags$div(
                    style = "font-size:.8rem; color:#6b7280; margin-top:.4rem;", sub
                  )
                )
              }
              # health_issues 1 (robust) .. 4 (fragile); the healthier the breed
              # the more full stars, so a robust cat (1) gets all 4 coloured.
              n_full <- if (!is.na(pick$health_issues) &&
                            pick$health_issues %in% 1:4) {
                5L - pick$health_issues
              } else {
                0L
              }
              stars <- htmltools::tags$div(
                style = "font-size:1.15rem; letter-spacing:3px; margin-top:.35rem;",
                lapply(1:4, function(i) {
                  htmltools::tags$span(
                    style = paste0(
                      "color:", if (i <= n_full) "#E69F00" else "#e5e7eb", ";"
                    ),
                    "\u2605"
                  )
                })
              )

              htmltools::tags$div(
                style = "display:flex; flex-wrap:wrap; font-family:'Open Sans',sans-serif;",
                card("Weight", paste0(pick$weight_kg, " kg"),
                     compare(pick$weight_kg, other$weight_kg), "#3451b2"),
                card("Life span", paste0(pick$life_span_yrs, " yrs"),
                     compare(pick$life_span_yrs, other$life_span_yrs), "#0072B2"),
                card("Health", health_word,
                     compare_health(pick$health_issues, other$health_issues),
                     "#E69F00", extra = stars)
              )
            }))
            bbquote(.(e), list(e = e))
          }),
          state = list()
        )
      })
    },
    function(id) tagList(),
    class = "breed_stats_block",
    expr_type = "bquoted",
    ...
  )
}

#' @importFrom blockr.core block_output
#' @importFrom shiny renderUI
#' @exportS3Method
block_output.breed_stats_block <- function(x, result, session) {
  renderUI(result)
}

#' @importFrom blockr.core block_ui
#' @importFrom shiny uiOutput NS
#' @exportS3Method
block_ui.breed_stats_block <- function(id, x, ...) {
  tagList(uiOutput(NS(id, "result")))
}
