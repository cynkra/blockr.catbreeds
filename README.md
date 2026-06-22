# blockr.catbreeds

A small [blockr](https://blockr.site/) extension that wraps [The Cat
API](https://thecatapi.com/) breeds endpoint as a set of blocks: a data block
that fetches live cat breed data, plus transform, plot and display blocks built
around it.

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

## Blocks

| Block | Category | Description |
| --- | --- | --- |
| Cat breeds | input | Live cat breed data from The Cat API |
| Trait correlation | transform | Correlation matrix of numeric columns, in long form |
| Similar breeds | transform | Nearest breeds to the picked one by trait distance |
| Temperament word cloud | plot | Word cloud of the comma-separated temperament words |
| Trait matrix | plot | Breeds-by-traits heatmap of the 1-5 scores |
| Breed card | display | Photo, temperament and description of one breed |
| Breed ranking cards | display | Value cards for the picked breed vs the others |
| Breed special-traits badges | display | Lap/hypoallergenic/hairless/rare/indoor badges |
