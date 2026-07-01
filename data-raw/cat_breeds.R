# Rebuild the bundled cat breeds dataset from The Cat API.
#
# Run on demand (NOT at build, install, or in CI), so the package never hits
# the API at runtime and we don't worry about rate limits:
#
#   source("data-raw/cat_breeds.R")
#
# Reads the public breeds endpoint, parses the weight and life-span ranges to
# numeric midpoints, attaches an origin-country centroid (jittered so breeds
# from the same country don't stack on a map), and writes the processed frame
# to inst/extdata/cat_breeds.csv. That CSV is what `catbreeds_fetch()` reads.
#
# Requires: jsonlite (Suggests).

build_cat_breeds <- function(url = "https://api.thecatapi.com/v1/breeds") {
  breeds <- jsonlite::fromJSON(url)

  # "3 - 5" -> 4, "14 - 15" -> 14.5
  midpoint <- function(x) {
    parts <- strsplit(gsub("[^0-9.]+", " ", x), "\\s+")
    vapply(
      parts,
      function(p) round(mean(as.numeric(p[nzchar(p)])), 1),
      numeric(1)
    )
  }

  # `col(x)` pulls a field if the API returns it, else a column of NA, so a
  # missing field never breaks the build.
  col <- function(nm, na = NA) {
    if (is.null(breeds[[nm]])) rep(na, nrow(breeds)) else breeds[[nm]]
  }

  coords <- origin_coords()
  hit <- match(breeds$origin, coords$origin)

  img_id <- col("reference_image_id", NA_character_)
  image_url <- ifelse(
    is.na(img_id) | !nzchar(img_id),
    NA_character_,
    paste0("https://cdn2.thecatapi.com/images/", img_id, ".jpg")
  )

  out <- data.frame(
    name              = col("name"),
    origin            = col("origin"),
    country_code      = col("country_code"),
    temperament       = col("temperament"),
    description       = col("description"),
    alt_names         = col("alt_names"),
    wikipedia_url     = col("wikipedia_url"),
    image_url         = image_url,
    lat               = coords$lat[hit],
    lng               = coords$lng[hit],
    weight_kg         = midpoint(breeds$weight$metric),
    life_span_yrs     = midpoint(breeds$life_span),
    affection_level   = col("affection_level"),
    energy_level      = col("energy_level"),
    intelligence      = col("intelligence"),
    dog_friendly      = col("dog_friendly"),
    child_friendly    = col("child_friendly"),
    stranger_friendly = col("stranger_friendly"),
    adaptability      = col("adaptability"),
    grooming          = col("grooming"),
    shedding_level    = col("shedding_level"),
    social_needs      = col("social_needs"),
    vocalisation      = col("vocalisation"),
    health_issues     = col("health_issues"),
    indoor            = col("indoor"),
    lap               = col("lap"),
    hypoallergenic    = col("hypoallergenic"),
    hairless          = col("hairless"),
    rare              = col("rare"),
    stringsAsFactors  = FALSE
  )

  # Spread breeds of the same country into a small ring around the origin
  # centroid, so a map shows all of them instead of one stacked dot. The offset
  # is a deterministic function of row position (golden angle), so markers never
  # jump between rebuilds.
  idx <- seq_len(nrow(out))
  ang <- ((idx * 137.508) %% 360) * pi / 180
  rad <- 0.5 + (idx %% 9) * 0.13
  out$lat <- round(out$lat + rad * sin(ang), 3)
  out$lng <- round(out$lng + rad * cos(ang), 3)

  out
}

# Approximate latitude/longitude for every country The Cat API uses as a breed
# origin, so each breed can be placed on a map.
origin_coords <- function() {
  data.frame(
    origin = c(
      "Australia", "Burma", "Canada", "China", "Cyprus", "Egypt", "France",
      "Greece", "Iran (Persia)", "Isle of Man", "Japan", "Norway", "Russia",
      "Singapore", "Somalia", "Thailand", "Turkey", "United Arab Emirates",
      "United Kingdom", "United States"
    ),
    lat = c(
      -25.0, 21.91, 56.13, 35.86, 35.13, 26.82, 46.60, 39.07, 32.43, 54.24,
      36.20, 60.47, 61.52, 1.35, 5.15, 15.87, 38.96, 23.42, 55.38, 37.09
    ),
    lng = c(
      133.0, 95.96, -106.35, 104.20, 33.43, 30.80, 2.20, 21.82, 53.69, -4.55,
      138.25, 8.47, 105.32, 103.82, 46.20, 100.99, 35.24, 53.85, -3.44, -95.71
    ),
    stringsAsFactors = FALSE
  )
}

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
breeds <- build_cat_breeds()
utils::write.csv(breeds, "inst/extdata/cat_breeds.csv", row.names = FALSE)
message("Wrote inst/extdata/cat_breeds.csv: ", nrow(breeds), " breeds")
