# Empty ("blank canvas") version of the cat-breeds reference board.
#
# Same package stack, options and setup as inst/examples/app.R, but the board
# starts with NO blocks and NO links -- just the DAG panel -- so you can build a
# cat-breeds workflow from scratch (by hand, or with the per-block AI controls).
# Every catbreeds block (the data block plus the breed card/flags/stats/similar/
# correlation/trait-matrix/cloud blocks) is registered and ready to add.
#
# Ships with blockr.catbreeds. Run it with:
#   shiny::runApp(system.file("examples", "app-empty.R", package = "blockr.catbreeds"))

# ---- Package loading (dual: installed vs local source) ---------------------
# `dev_local = FALSE` (the default, and what ships) attaches the INSTALLED
# packages with library(). Set it to TRUE -- or source this file from a wrapper
# that sets it first -- to load every blockr package from its LOCAL source
# checkout with pkgload::load_all(). Identical to app.R (see there for details).
if (!exists("dev_local")) dev_local <- FALSE

blockr_pkgs <- c(
  "blockr.core",
  "blockr.dock",
  "blockr.dag",
  "blockr.io",
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

# ---- Curate the block browser ----------------------------------------------
# Keep ONLY `dataset` and `glue` from blockr.core; drop its low-level / noise
# blocks (subset, merge, rbind, head, scatter, csv, filebrowser, upload) via
# unregister_blocks(), selecting by the registry `package` attribute so only
# core blocks are affected.
core_keep <- c("dataset_block", "glue_block")
core_drop <- setdiff(
  names(Filter(
    function(entry) identical(attr(entry, "package"), "blockr.core"),
    available_blocks()
  )),
  core_keep
)
unregister_blocks(core_drop)

# Swap the default DT previews for the shared blockr.ui HTML table preview on
# plain data / transform blocks. Specialised blocks (viz tables/charts,
# ggplot, leaflet) keep their own output since they dispatch on their own class.
options(blockr.tabular_display = blockr.ui::html_table_display)

# Work around a blockr.core bbquote() bug (>= 0.1.3): in process_splices(), a
# `function(...)` definition inside a bquoted expression has a trailing NULL
# srcref slot; `e_list[[i]] <- process_splices(...)` assigns NULL, which *drops*
# that slot (length 4 -> 3), so the later `names(e_list) <- names_e` throws
# "'names' attribute [4] must be the same length as the vector [3]". This breaks
# every custom block whose quoted expr contains an anonymous function (breed
# stats/flags, similar, wordcloud, ...). Kept here so those blocks work once you
# add them. Fix: use `e_list[i] <- list(...)`, which preserves NULL slots. The
# proper fix belongs upstream in blockr.core.
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

# To switch the per-block AI from the default OpenAI model to Claude, uncomment
# (and set ANTHROPIC_API_KEY):
# options(blockr.llm_model = ellmer::chat_anthropic(model = "claude-sonnet-4-6"))

# An empty board: same DAG extension as app.R, but no blocks and no links. The
# Build view shows just the DAG canvas, ready for you to add the cat-breeds data
# block and build out from there.
board <- new_dock_board(
  extensions = list(
    dag = new_dag_extension()
  ),
  grids = list(
    Build = dock_grid(
      # Addressed by mount key via ext(): extension ids are container-owned,
      # so the class-derived "dag_extension" resolves to nothing and the view
      # built from this grid comes out empty.
      ext("dag")
    )
  )
)

# This board starts EMPTY, so manage_project()'s restore needs the rack loader
# (see the NOTE in app.R): the default local_loader() ignores the `?board_name`
# query that restore navigates to, which would reload the board blank. For an
# empty starting board that is exactly the desired cold-load behaviour.
serve(
  board,
  plugins = custom_plugins(list(
    ai_ctrl_block(), # blockr.ai: per-block AI controls
    manage_project() # blockr.session: project save / load / versions
  )),
  loader = blockr.session::rack_loader()
)
