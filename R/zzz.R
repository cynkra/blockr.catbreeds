.onLoad <- function(libname, pkgname) {
  # `package` is derived automatically from the constructor's namespace, so it
  # is intentionally not passed here.
  register_block(
    ctor = "new_catbreeds_block",
    name = "Cat breeds",
    description = "Live cat breed data from The Cat API",
    category = "input",
    package = pkgname
  )
  register_block(
    ctor = "new_correlation_block",
    name = "Trait correlation",
    description = "Correlation matrix of numeric columns, in long form",
    category = "transform",
    package = pkgname
  )
  register_block(
    ctor = "new_temperament_cloud_block",
    name = "Temperament word cloud",
    description = "Word cloud of the comma-separated temperament words",
    category = "plot",
    package = pkgname
  )
  register_block(
    ctor = "new_breed_card_block",
    name = "Breed card",
    description = "Photo, temperament and description of one breed",
    category = "display",
    package = pkgname
  )
  register_block(
    ctor = "new_breed_stats_block",
    name = "Breed ranking cards",
    description = "Value cards for the picked breed vs the others",
    category = "display",
    package = pkgname
  )
  register_block(
    ctor = "new_breed_flags_block",
    name = "Breed special-traits badges",
    description = "Lap/hypoallergenic/hairless/rare/indoor badges for a breed",
    category = "display",
    package = pkgname
  )
  register_block(
    ctor = "new_similar_breeds_block",
    name = "Similar breeds",
    description = "Nearest breeds to the picked one by trait distance",
    category = "transform",
    package = pkgname
  )
  register_block(
    ctor = "new_trait_matrix_block",
    name = "Trait matrix",
    description = "Breeds-by-traits heatmap of the 1-5 scores",
    category = "plot",
    package = pkgname
  )
  invisible()
}
