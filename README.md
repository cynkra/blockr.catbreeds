# blockr.catbreeds

A small [blockr](https://blockr.site/) extension built around [The Cat
API](https://thecatapi.com/) breeds data: a data block that serves the bundled
breeds dataset (rebuilt from the API on demand via `data-raw/`), plus transform,
plot and display blocks built around it.

Originally written for the useR! 2026 blockr workshop, now standalone so it can
be reused.

## Installation

```r
pak::pak("cynkra/blockr.catbreeds")
```

## Usage

Loading the package registers its blocks with the blockr registry, so they show
up in the block picker of any board:

```r
library(blockr.core)
library(blockr.catbreeds)

serve(new_dock_board())
```

You can also call the fetcher directly:

```r
breeds <- blockr.catbreeds::catbreeds_fetch()
```

## Demo apps

Two ready-to-run boards from the useR! 2026 talk ship under `inst/examples`:

```r
# full reference board (cat-breeds analysis across three views)
shiny::runApp(system.file("examples", "app.R", package = "blockr.catbreeds"))

# near-empty starter used for the live build-from-scratch demo
shiny::runApp(system.file("examples", "app-build.R", package = "blockr.catbreeds"))
```

They pull in the wider blockr stack (`blockr.dock`, `blockr.dplyr`, `blockr.ggplot`,
`blockr.viz`, `blockr.leaflet`, `blockr.echarts`, `blockr.dag`, `blockr.ai`,
`blockr.assistant`, `blockr.session`); install those first. The AI pieces use
blockr's default LLM, so set `OPENAI_API_KEY` before launching.

## Blocks

| Block | Category | Description |
| --- | --- | --- |
| Cat breeds | input | Cat breed data (bundled CSV, built from The Cat API) |
| Trait correlation | transform | Correlation matrix of numeric columns, in long form |
| Similar breeds | transform | Nearest breeds to the picked one by trait distance |
| Temperament word cloud | plot | Word cloud of the comma-separated temperament words |
| Trait matrix | plot | Breeds-by-traits heatmap of the 1-5 scores |
| Breed card | display | Photo, temperament and description of one breed |
| Breed ranking cards | display | Value cards for the picked breed vs the others |
| Breed special-traits badges | display | Lap/hypoallergenic/hairless/rare/indoor badges |
