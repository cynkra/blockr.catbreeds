# Run the cat-breeds reference board against LOCAL source checkouts, i.e. with
# your latest uncommitted changes to any blockr package. This is the
# pkgload::load_all() counterpart of the shipped, library()-based
# inst/examples/app.R -- the same dual dev/deploy split as
# blockr.insurance/dev/treaty-pricer.R. There is ONE board definition (in
# inst/examples/app.R); this wrapper just flips the loader and sources it, so
# the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.catbreeds/dev/app.R")
#
# (End users without the source checkouts run the shipped copy instead:
#   shiny::runApp(system.file("examples", "app.R", package = "blockr.catbreeds")))

options(shiny.port = 3838, shiny.host = "0.0.0.0")

dev_local <- TRUE
source("blockr.catbreeds/inst/examples/app.R")
