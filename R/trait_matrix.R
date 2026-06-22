#' Trait matrix heatmap
#'
#' A breeds-by-traits matrix: one tile per (breed, trait), filled by the 1-5
#' score, in the spirit of a ratings heatmap (rows ordered by mean score so a
#' gradient emerges). Two selectors drive it: which `traits` to show (columns)
#' and which `breeds` to keep (rows; none selected = all). Pivots the wide trait
#' columns to long form internally, so it takes the raw breed table straight in.
#' The plot fills the panel height (`block_ui` below), so the rows stay legible
#' even with every breed shown.
#'
#' @param traits Character vector of trait columns to show. Default: all eleven
#'   1-5 traits.
#' @param breeds Character vector of breed names to keep. Default: empty, which
#'   shows every breed.
#' @param ... Forwarded to [blockr.ggplot::new_ggplot_transform_block()].
#' @return A `trait_matrix_block` object.
#' @export
new_trait_matrix_block <- function(traits = NULL, breeds = character(), ...) {
  all_traits <- c(
    "affection_level", "energy_level", "intelligence", "dog_friendly",
    "child_friendly", "stranger_friendly", "adaptability", "grooming",
    "shedding_level", "social_needs", "vocalisation"
  )
  if (is.null(traits) || !length(traits)) traits <- all_traits

  new_ggplot_transform_block(
    function(id, data) {
      moduleServer(id, function(input, output, session) {
        r_traits <- reactiveVal(traits)
        r_breeds <- reactiveVal(breeds)

        observeEvent(data(), {
          updateSelectInput(
            session, "traits",
            choices = intersect(all_traits, names(data())),
            selected = r_traits()
          )
          updateSelectInput(
            session, "breeds",
            choices = sort(unique(as.character(data()$name))),
            selected = r_breeds()
          )
        })
        observeEvent(input$traits, r_traits(input$traits), ignoreNULL = FALSE)
        observeEvent(input$breeds, {
          r_breeds(if (is.null(input$breeds)) character() else input$breeds)
        }, ignoreNULL = FALSE)

        list(
          expr = reactive({
            tr <- r_traits()
            if (!length(tr)) tr <- all_traits
            br <- r_breeds()
            bquote(
              local({
                traits <- intersect(.(tr), names(data))
                d <- data
                if (length(.(br))) {
                  d <- d[as.character(d$name) %in% .(br), ]
                }
                long <- tidyr::pivot_longer(
                  d[, c("name", traits)],
                  cols = tidyr::all_of(traits),
                  names_to = "trait", values_to = "value"
                )
                ord <- names(sort(tapply(long$value, long$name, mean,
                                         na.rm = TRUE)))
                long$name <- factor(long$name, levels = ord)
                long$trait <- factor(long$trait, levels = traits)
                ggplot2::ggplot(
                  long, ggplot2::aes(x = trait, y = name, fill = value)
                ) +
                  ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
                  ggplot2::scale_fill_gradient(
                    low = "#deebf7", high = "#08519c", name = "Score",
                    limits = c(1, 5)
                  ) +
                  ggplot2::labs(x = NULL, y = NULL) +
                  ggplot2::theme_minimal(base_size = 9) +
                  ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
                    panel.grid = ggplot2::element_blank()
                  )
              }),
              list(tr = tr, br = br)
            )
          }),
          state = list(traits = r_traits, breeds = r_breeds)
        )
      })
    },
    function(id) {
      tagList(
        selectInput(NS(id, "traits"), "Traits", choices = all_traits,
                    selected = traits, multiple = TRUE),
        selectInput(NS(id, "breeds"), "Breeds (none = all)", choices = NULL,
                    selected = NULL, multiple = TRUE)
      )
    },
    class = "trait_matrix_block",
    allow_empty_state = "breeds",   # empty selection = show every breed
    ...
  )
}

#' @importFrom blockr.core block_ui
#' @importFrom shiny plotOutput NS tags
#' @exportS3Method
block_ui.trait_matrix_block <- function(id, x, ...) {
  # a tall plot inside a scroll box, so the rows stay legible with many breeds
  # (a plain 400px plotOutput squashes 67 rows); the panel scrolls if needed.
  tagList(
    tags$div(
      style = "height:100%; overflow-y:auto;",
      plotOutput(NS(id, "result"), height = "1000px")
    )
  )
}
