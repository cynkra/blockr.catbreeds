#' Similar-breeds block
#'
#' Finds the breeds closest to the picked one by Euclidean distance over the five
#' core 1-5 traits, and returns the top matches as a tidy table (`name`,
#' `similarity`) ready for a bar chart. Expects the full breed set with a
#' `highlight` column (the picked breed's value is its own name, the rest
#' `"Other breeds"`).
#'
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#' @return A `similar_breeds_block` object.
#' @export
new_similar_breeds_block <- function(...) {
  new_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        list(
          expr = reactive({
            e <- quote(local({
              d <- .(data)
              traits <- intersect(
                c("affection_level", "energy_level", "intelligence",
                  "dog_friendly", "child_friendly", "stranger_friendly",
                  "adaptability", "grooming", "shedding_level", "social_needs",
                  "vocalisation", "health_issues"),
                names(d)
              )
              pick <- d[d$highlight != "Other breeds", ][1, ]
              others <- d[d$highlight == "Other breeds", ]
              dist <- sqrt(rowSums(
                (as.matrix(others[, traits]) -
                   matrix(unlist(pick[, traits]), nrow(others), length(traits),
                          byrow = TRUE))^2
              ))
              o <- order(dist)[seq_len(min(6L, length(dist)))]
              # % match: 100 = identical traits, 0 = maximally different
              # (each trait can differ by at most 4, over `length(traits)` dims)
              max_dist <- 4 * sqrt(length(traits))
              data.frame(
                name = others$name[o],
                similarity = round(100 * (1 - dist[o] / max_dist)),
                stringsAsFactors = FALSE
              )
            }))
            bbquote(.(e), list(e = e))
          }),
          state = list()
        )
      })
    },
    function(id) tagList(),
    class = "similar_breeds_block",
    expr_type = "bquoted",
    ...
  )
}
