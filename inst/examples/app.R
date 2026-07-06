# Reference blockr board shown (recorded) in the useR2026 talk.
#
# Ships with blockr.catbreeds. Run it with:
#   shiny::runApp(system.file("examples", "app.R", package = "blockr.catbreeds"))
#
# A full cat-breeds analysis, end to end. The data source is this package's
# `new_catbreeds_block()` (bundled CSV, built from The Cat API by data-raw/), so
# the "no code" demo and the "no limits" custom-block section are the same cats.
#
# The board answers a stack of questions from the breed traits
#   weight_kg, life_span_yrs, affection_level, energy_level, intelligence,
#   dog_friendly, child_friendly
# across themed views:
#   - Build      DAG + cat-breeds input + the AI assistant
#   - My cat     pick a breed -> trait radar (vs other breeds) + ranking cards,
#                that breed highlighted on the scatter and the map
#   - Global cat headline KPIs, correlations, compatibility, lifespan, breeds
#                per origin, friendliest origins, a temperament word cloud, and
#                a chart-filter -> drilldown-table pair (blockr.viz)
#
# "Pick a breed" is a plain filter (default: name == "American Wirehair"): the
# breed name lives only there. A small custom block (`flag`) adds a `highlight`
# column to the full set from the pick, so the picked breed shows among the
# others on the scatter, map and radar (no join, no empty columns).
#
# AI is wired in two ways:
#   - blockr.assistant : a board-level chat extension ("Assistant" view) that
#     can read the board and stage block/link edits.
#   - blockr.ai        : per-block AI controls. ai_ctrl_block() is a global
#     ctrl_block plugin, so every blockr.dplyr block (arrange, slice, mutate,
#     filter, summarize, pivot) swaps its native picker UI for an AI chat
#     ("keep breeds heavier than 5 kg"). The ggplot and catbreeds blocks keep
#     their native controls.
#
# blockr.session::manage_project() adds project save/load/version (the navbar
# title, workflow switcher and history), backed by a pins board.
#
# LLM backend: left at blockr's default (OpenAI GPT-4.1). Set OPENAI_API_KEY
# before launching. To use Claude instead, uncomment the options() call below
# and set ANTHROPIC_API_KEY.

# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from a wrapper
# that sets it first, see dev/app.R -- to load every blockr package from its
# LOCAL source checkout with pkgload::load_all(), which picks up your latest
# uncommitted changes. Same dual style as blockr.insurance/dev/treaty-pricer.R.
# The load_all path uses bare dir names, so run it from the workspace root.
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.core",
  "blockr.dock",
  "blockr.dag",
  "blockr.io",
  "blockr.extra",     # function blocks + the HTML table-preview glue (option below)
  "blockr.dplyr",
  "blockr.ggplot",
  "blockr.assistant",
  "blockr.ai",
  "blockr.session",
  "blockr.leaflet",   # custom "map block" we built (markers per origin)
  "blockr.viz",       # BI blocks: tile scorecards, chart, drilldown + shaded tables
  "blockr.catbreeds"  # this package: catbreeds data block + card/flags/stats/similar
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# Swap the default DT previews for the shared blockr.ui HTML table preview on
# plain data / transform blocks. blockr.extra (loaded above) reads this at
# render time; specialised blocks (viz tables/charts, ggplot, leaflet) keep
# their own output since they dispatch on their own class.
options(blockr.html_table_preview = TRUE)

# Work around a blockr.core bbquote() bug (>= 0.1.3): in process_splices(), a
# `function(...)` definition inside a bquoted expression has a trailing NULL
# srcref slot; `e_list[[i]] <- process_splices(...)` assigns NULL, which *drops*
# that slot (length 4 -> 3), so the later `names(e_list) <- names_e` throws
# "'names' attribute [4] must be the same length as the vector [3]". This breaks
# every custom block whose quoted expr contains an anonymous function (breed
# stats/flags, similar, wordcloud, ...). Fix: use `e_list[i] <- list(...)`, which
# preserves NULL slots. The proper fix belongs upstream in blockr.core.
local({
  ns <- asNamespace("blockr.core")
  if (!exists("bbquote", envir = ns, inherits = FALSE)) {
    return(invisible())
  }
  src <- paste(deparse(get("bbquote", ns)), collapse = "\n")
  fixed <- sub(
    "e_list[[i]] <- process_splices(e_list[[i]])",
    "e_list[i] <- list(process_splices(e_list[[i]]))",
    src,
    fixed = TRUE
  )
  if (identical(fixed, src)) {
    return(invisible())
  }
  patched <- eval(parse(text = fixed))
  environment(patched) <- ns
  utils::assignInNamespace("bbquote", patched, ns = "blockr.core")
  # bbquote is imported (by value) into every package that calls it, so the
  # blocks see their own stale copy. Replace it in each loaded namespace and in
  # each namespace's imports environment.
  for (nm in loadedNamespaces()) {
    target <- asNamespace(nm)
    for (env in list(target, parent.env(target))) {
      if (
        exists("bbquote", envir = env, inherits = FALSE) &&
          !identical(get("bbquote", envir = env), patched)
      ) {
        if (environmentIsLocked(env) && bindingIsLocked("bbquote", env)) {
          unlockBinding("bbquote", env)
        }
        assign("bbquote", patched, envir = env)
      }
    }
  }
})

# To switch the assistant / per-block AI from the default OpenAI model to
# Claude, uncomment (and set ANTHROPIC_API_KEY):
# options(blockr.llm_model = ellmer::chat_anthropic(model = "claude-sonnet-4-6"))

# blockr.catbreeds is loaded above; its .onLoad() registers `new_catbreeds_block`
# and the breed card/flags/stats/similar/correlation/trait-matrix/cloud blocks.
# Install with: pak::pak("cynkra/blockr.catbreeds")

board <- new_dock_board(
  blocks = c(
    breeds = new_catbreeds_block(block_name = "Cat breeds (live API)"),

    # --- Pick a breed: a plain mutate, no custom block ----------------------
    # The breed name lives ONLY here. `highlight` is "Selected" for the picked
    # breed and "Other breeds" for the rest, so the scatter, map and radar can
    # show the pick among the others. Change the name here (or ask the
    # assistant) to switch cats. This is just a blockr.dplyr mutate.
    flag = new_mutate_block(
      mutations = list(
        list(
          name = "highlight",
          expr = "factor(ifelse(name == \"American Wirehair\", \"Selected\", \"Other breeds\"), levels = c(\"Selected\", \"Other breeds\"))"
        )
      ),
      visible = "inputs",
      block_name = "Pick a breed"
    ),
    # the picked breed on its own (one row) for the card + badges: a filter on
    # the flag column, so the breed name is not repeated.
    pick = new_filter_block(
      conditions = list(list(
        type = "expr",
        expr = "highlight == \"Selected\""
      )),
      operator = "&",
      block_name = "Picked breed"
    ),

    # --- My cat charts: scatter, map and radar, all keyed on `highlight` -----
    # blockr.viz chart block in scatter mode: life span vs weight for every
    # breed; clicking a point filters downstream on `name` (drill = "name"), so
    # the scatter doubles as a breed picker.
    scatter = new_chart_block(
      chart_type = "scatter",
      x = "weight_kg",
      y = "life_span_yrs",
      series = "name",
      drill = "name",
      visible = "inputs",
      block_name = "Life span vs weight"
    ),
    map = new_leaflet_markers_block(
      label = "name",
      color_by = "highlight",
      visible = "outputs",
      block_name = "Cat origins map"
    ),
    # blockr.viz radar wants long input (one row per spoke), so pivot the five
    # 1-5 traits to a `trait`/`value` pair first.
    radar_long = new_pivot_longer_block(
      cols = list(
        "affection_level",
        "energy_level",
        "intelligence",
        "dog_friendly",
        "child_friendly"
      ),
      names_to = "trait",
      values_to = "value",
      block_name = "Traits (long)"
    ),
    # radar: spokes = trait (group), one polygon per highlight (color), each
    # vertex = mean(value). Replaces the echarts radar block.
    radar = new_chart_block(
      chart_type = "radar",
      group = "trait",
      color = "highlight",
      metric = "value",
      agg_fn = "mean",
      visible = "inputs",
      block_name = "Trait radar (vs other breeds)"
    ),

    # --- My cat: photo + personality of the picked breed (custom block) -----
    card = new_breed_card_block(
      visible = "outputs",
      block_name = "Meet the breed"
    ),

    # --- My cat: the picked breed's special traits as badges (custom block) -
    flags = new_breed_flags_block(
      visible = "outputs",
      block_name = "Special traits"
    ),

    # --- My cat: headline numbers with a live comparison to the others ------
    # The KPI block's subtitles are static, so a dynamic "x% vs others" needs a
    # small custom card block (recomputes when the pick changes).
    stats = new_breed_stats_block(
      visible = "outputs",
      block_name = "How it ranks"
    ),

    # --- My cat: the breeds most similar by trait distance (custom block) ---
    similar = new_similar_breeds_block(block_name = "Nearest breeds"),
    # blockr.viz horizontal bar of % match; clicking a bar is a selectable
    # filter on `name` (drill), so you can pick a breed straight from it.
    similar_bar = new_chart_block(
      chart_type = "bar",
      group = "name",
      metric = "similarity",
      agg_fn = "sum",
      orientation = "horizontal",
      drill = "name",
      visible = "inputs",
      block_name = "Cats like yours"
    ),

    # --- Data view: trait correlation matrix as a diverging shaded table -----
    # new_correlation_block emits long (x, y, corr); pivot to a wide trait x
    # trait matrix and let blockr.viz shade the cells diverging around 0 -- a
    # table "heatmap" that replaces the echarts heatmap block.
    corr = new_correlation_block(block_name = "Trait correlation"),
    corr_wide = new_pivot_wider_block(
      names_from = "y",
      values_from = "corr",
      block_name = "Correlation matrix (wide)"
    ),
    heatmap = new_table_block(
      rowname = "x",
      shadings = list(list(mode = "diverging", cols = list())),
      digits = 2L,
      visible = "outputs",
      block_name = "Do traits move together?"
    ),

    # --- Data view: breeds per country of origin, as a treemap ---------------
    count_origin = new_summarize_block(
      summaries = list(list(type = "simple", name = "n", func = "n")),
      by = list("origin"),
      block_name = "Count breeds per origin"
    ),
    # blockr.viz treemap (replaces the echarts treemap); fed the per-origin
    # counts, one tile per origin sized by n.
    treemap = new_chart_block(
      chart_type = "treemap",
      group = "origin",
      metric = "n",
      agg_fn = "sum",
      visible = "inputs",
      block_name = "Breeds per origin"
    ),

    # --- Global cat: is "affectionate" the same as "good with kids"? --------
    compat_count = new_summarize_block(
      summaries = list(list(type = "simple", name = "n", func = "n")),
      by = list("child_friendly", "affection_level"),
      block_name = "Count by kid + affection score"
    ),
    # The echarts heatmap has no axis titles, so prefix the category values to
    # say which axis is which ("child 1..5" on x, "affection 1..5" on y).
    compat_label = new_mutate_block(
      mutations = list(
        list(
          name = "child_friendly",
          expr = "paste0(\"child \", child_friendly)"
        ),
        list(
          name = "affection_level",
          expr = "paste0(\"affection \", affection_level)"
        )
      ),
      block_name = "Label heatmap axes"
    ),
    # Pivot the labelled counts to a wide child x affection grid (fill empty
    # combos with 0) and shade sequentially -- a table heatmap replacing the
    # echarts one.
    compat_wide = new_pivot_wider_block(
      names_from = "affection_level",
      values_from = "n",
      values_fill = 0,
      block_name = "Kid x affection (wide)"
    ),
    compat = new_table_block(
      rowname = "child_friendly",
      shadings = list(list(mode = "sequential", cols = list())),
      visible = "outputs",
      block_name = "Affectionate & good with kids?"
    ),

    # --- Global cat: how long do cats live? ---------------------------------
    life_density = new_ggplot_block(
      type = "density",
      x = "life_span_yrs",
      visible = "outputs",
      block_name = "How long do cats live?"
    ),

    # --- Global cat: a numeric trait across origins, on a continuous scale ---
    # Each breed sits near its country of origin (jittered), coloured by the
    # chosen numeric column. Switch "Colour by" to weight, life span, any 1-5
    # trait, ... to hunt for geographic patterns. Numeric -> viridis gradient.
    geo_map = new_leaflet_markers_block(
      label = "name",
      color_by = "life_span_yrs",
      radius = 6,
      cluster = FALSE,
      visible = "outputs",
      block_name = "Trait map (by origin)"
    ),

    # --- Global cat: every breed's 1-5 traits as a matrix (custom block) -----
    # breeds x traits tile heatmap, rows ordered by mean score (ratings-style).
    trait_matrix = new_trait_matrix_block(
      visible = "outputs",
      block_name = "Trait matrix"
    ),

    # --- Global cat: where do the friendliest cats come from? ---------------
    origin_friendly = new_summarize_block(
      summaries = list(
        list(
          type = "simple",
          name = "avg_child",
          func = "mean",
          col = "child_friendly"
        ),
        list(type = "simple", name = "n", func = "n")
      ),
      by = list("origin"),
      block_name = "Average kid-friendliness by origin"
    ),
    origin_keep = new_filter_block(
      conditions = list(list(type = "expr", expr = "n >= 2")),
      operator = "&",
      block_name = "Origins with >= 2 breeds"
    ),
    origin_rank = new_mutate_block(
      mutations = list(
        list(
          name = "origin",
          expr = "factor(origin, levels = origin[order(avg_child)])"
        )
      ),
      block_name = "Order origins by score"
    ),
    origin_bar = new_ggplot_block(
      type = "bar",
      x = "avg_child",
      y = "origin",
      visible = "outputs",
      block_name = "Friendliest origins"
    ),

    # --- Global cat: interactive BI pair from blockr.viz ---------------------
    # Chart block as a visual filter: a bar of breed count per origin; click a
    # bar to filter downstream (drill = "origin"). The drilldown table then
    # shows the breeds of the clicked origin (it shares the same click-filter
    # contract as the chart, so the two compose).
    vfilter = new_chart_block(
      group = "origin",
      metric = ".count",
      agg_fn = "count",
      chart_type = "bar",
      drill = "origin",
      block_name = "Filter breeds"
    ),
    pivot = new_table_block(
      rowname = "name",
      values = c("life_span_yrs", "weight_kg"),
      digits = 1L,
      block_name = "Breeds in selection"
    ),

    # --- Global cat: personality word cloud (custom echarts cloud block) ----
    wordcloud = new_temperament_cloud_block(
      visible = "outputs",
      block_name = "Personality words"
    ),

    # --- Global cat: headline numbers across all breeds (blockr.viz tile) ----
    # The tile is a pure renderer: it labels each card with the measure column's
    # NAME, so the summary already names them the way they should read. One row
    # in -> one number tile per measure (replaces the deprecated new_kpi_block).
    kpi_data = new_summarize_block(
      summaries = list(
        list(type = "simple", name = "Breeds", func = "n"),
        list(
          type = "simple",
          name = "Countries",
          func = "n_distinct",
          col = "origin"
        ),
        list(
          type = "simple",
          name = "Avg life span",
          func = "mean",
          col = "life_span_yrs"
        ),
        list(
          type = "simple",
          name = "Avg weight",
          func = "mean",
          col = "weight_kg"
        )
      ),
      by = list(),
      block_name = "Headline numbers"
    ),
    kpis = new_tile_block(
      value = c("Breeds", "Countries", "Avg life span", "Avg weight"),
      format = "number", # default: one decimal (used by the averages)
      measures = list(
        # counts as whole numbers ("compact" rounds, dropping the .0); the
        # averages keep the "number" default and add a unit
        Breeds = list(format = "compact"),
        Countries = list(format = "compact"),
        `Avg life span` = list(unit = "yrs"),
        `Avg weight` = list(unit = "kg")
      ),
      visible = "inputs",
      block_name = "At a glance"
    )
  ),
  links = c(
    new_link("breeds", "flag", "data"),
    new_link("flag", "pick", "data"),

    new_link("flag", "scatter", "data"),
    new_link("flag", "map", "data"),
    new_link("flag", "radar_long", "data"),
    new_link("radar_long", "radar", "data"),
    new_link("flag", "similar", "data"),
    new_link("similar", "similar_bar", "data"),

    new_link("flag", "stats", "data"),
    new_link("pick", "card", "data"),
    new_link("pick", "flags", "data"),

    new_link("breeds", "corr", "data"),
    new_link("corr", "corr_wide", "data"),
    new_link("corr_wide", "heatmap", "data"),

    new_link("breeds", "count_origin", "data"),
    new_link("count_origin", "treemap", "data"),

    new_link("breeds", "compat_count", "data"),
    new_link("compat_count", "compat_label", "data"),
    new_link("compat_label", "compat_wide", "data"),
    new_link("compat_wide", "compat", "data"),

    new_link("breeds", "life_density", "data"),
    new_link("breeds", "geo_map", "data"),
    new_link("breeds", "trait_matrix", "data"),

    new_link("breeds", "origin_friendly", "data"),
    new_link("origin_friendly", "origin_keep", "data"),
    new_link("origin_keep", "origin_rank", "data"),
    new_link("origin_rank", "origin_bar", "data"),

    new_link("breeds", "vfilter", "data"),
    new_link("vfilter", "pivot", "data"),

    new_link("breeds", "wordcloud", "data"),

    new_link("breeds", "kpi_data", "data"),
    new_link("kpi_data", "kpis", "data")
  ),
  extensions = list(
    dag = new_dag_extension()
  ),
  layouts = list(
    Build = dock_layout(
      "dag_extension",
      "breeds",
      sizes = c(0.4, 0.6)
    ),
    # Three columns. Related panels are stacked as TABS (panels()) rather than
    # all shown at once, so the view reads as "identity | ranking | explore"
    # instead of an eight-panel wall.
    mycat = dock_layout(
      # Left -- the picked breed: photo up top, badges and the picker behind tabs.
      group(
        "card",
        panels("flags", "flag", active = "flags"),
        sizes = c(0.58, 0.42)
      ),
      # Middle -- how it ranks, then the trait radar below it.
      group("stats", "radar", sizes = c(0.4, 0.6)),
      # Right -- the interactive charts and the origins map, one tab at a time.
      panels("scatter", "similar_bar", "map", active = "scatter"),
      sizes = c(0.26, 0.42, 0.32),
      name = "My cat"
    ),
    # KPI tile row on top; below it three columns, each a tab strip of related
    # panels, with the chart-filter -> drilldown-table pair kept side by side.
    global = dock_layout(
      "kpis",
      group(
        # The two "matrix" heatmaps.
        panels("trait_matrix", "heatmap", active = "trait_matrix"),
        # Distributions and personality.
        panels("wordcloud", "life_density", "compat", active = "wordcloud"),
        # Origins: the summary views as tabs, plus the live filter -> table pair.
        group(
          panels("treemap", "origin_bar", "geo_map", active = "treemap"),
          panels("vfilter", "pivot", active = "vfilter"),
          sizes = c(0.5, 0.5)
        ),
        sizes = c(0.3, 0.32, 0.38)
      ),
      orientation = "vertical",
      sizes = c(0.22, 0.78), # tile scorecards need a fair bit of height
      name = "Global cat"
    )
  )
)

# NOTE: do NOT add `loader = rack_loader()` here. This is a PRE-POPULATED
# reference board; rack_loader() returns clear_board(default) on a cold load
# (no `?id`/`?board_name`), which would wipe the reference workflow to a blank
# canvas. rack_loader() is only for boards that start empty (see app-build.R).
# The default local_loader() returns the populated board on a cold load.
serve(
  board,
  plugins = custom_plugins(list(
    ai_ctrl_block(), # blockr.ai: per-block AI controls
    manage_project() # blockr.session: project save / load / versions
  ))
)
