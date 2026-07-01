#' Cat breeds data
#'
#' Reads the cat breeds dataset shipped with the package and keeps the breeds at
#' or above a dog-friendliness floor. The data is bundled (not fetched live) so
#' the block never hits The Cat API at runtime. Rebuild the bundled CSV from the
#' API on demand with `source("data-raw/cat_breeds.R")`.
#'
#' @param min_dog Integer 1-5. Keep breeds with `dog_friendly >= min_dog`.
#' @param rare Logical. If `TRUE`, keep only breeds flagged `rare`.
#' @return A data frame with one row per breed: identity and text fields
#'   (`name`, `origin`, `country_code`, `temperament`, `description`,
#'   `alt_names`, `wikipedia_url`, `image_url`), the origin-country
#'   centroid (`lat`, `lng`), numeric `weight_kg` and `life_span_yrs`, the full
#'   set of 1-5 trait scores (affection, energy, intelligence, dog/child/stranger
#'   friendliness, adaptability, grooming, shedding, social needs, vocalisation,
#'   health issues) and the 0/1 flags (`indoor`, `lap`, `hypoallergenic`,
#'   `hairless`, `rare`).
#' @export
catbreeds_fetch <- function(min_dog = 1L, rare = FALSE) {
  path <- system.file(
    "extdata", "cat_breeds.csv",
    package = "blockr.catbreeds", mustWork = TRUE
  )
  out <- utils::read.csv(path, stringsAsFactors = FALSE)
  out <- out[!is.na(out$dog_friendly) & out$dog_friendly >= min_dog, ]
  if (isTRUE(rare)) {
    out <- out[!is.na(out$rare) & out$rare == 1L, ]
  }
  out
}
