# Predefined "My cat" board used to RECORD the build-up video for the talk.
#
# The full My cat view (scoped to the blocks the slide needs) is defined up
# front in its final layout. A headless chromote driver then fades the panels
# in one by one (data -> pick a breed -> meet the breed -> trait radar -> life
# span vs weight) and finishes with a live breed change, recording the whole
# thing into assets/video/. Nothing is built through the (fragile) interactive
# add-block UI; the "appearing in place" effect is pure opacity reveal so the
# layout never jumps.

library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.dplyr)
library(blockr.ggplot)
library(blockr.assistant)
library(blockr.ai)
library(blockr.session)
library(blockr.leaflet) # custom "map block" we built (markers per origin)
library(blockr.echarts) # radar, gauge, heatmap, treemap (no blockr.viz equivalent)
library(blockr.viz) # BI blocks: tile scorecards, chart, drilldown table (Global cat)
library(blockr.catbreeds) # this package: the catbreeds data block + breed card/flags/stats/similar blocks

# blockr.catbreeds .onLoad() registers `new_catbreeds_block` and the breed
# card/flags/stats/similar/correlation/trait-matrix/cloud blocks.
# Install with: pak::pak("cynkra/blockr.catbreeds")

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
      if (exists("bbquote", envir = env, inherits = FALSE) &&
          !identical(get("bbquote", envir = env), patched)) {
        if (environmentIsLocked(env) && bindingIsLocked("bbquote", env)) {
          unlockBinding("bbquote", env)
        }
        assign("bbquote", patched, envir = env)
      }
    }
  }
})

# Work around a blockr.dock startup race that empties every view. On session
# init, sync_layouts_to_board() reads each dock's *live* dockview layout through
# live_view_data() (after a 250ms debounce). If that fires before a dock has
# finished restoring its panels, the live layout is empty, gets written back
# over the board's real layout (apply_views_mod), and then reconcile_views()
# tears every panel down: all views show the "Start by adding a panel" prompt.
# Guard: treat a view as "not ready" (suppress the sync) while its live dockview
# is empty but its dock is still meant to hold panels (live_panels() non-empty),
# which is exactly the race and is distinct from a genuinely empty view (where
# live_panels() is empty too). The proper fix belongs upstream in blockr.dock.
local({
  ns <- asNamespace("blockr.dock")
  if (!exists("live_view_data", envir = ns, inherits = FALSE)) {
    return(invisible())
  }
  patched <- function(client_views, docks, client_active) {
    reactive({
      state <- client_views()
      v_list <- lapply(names(state), function(v_id) {
        dk <- docks[[v_id]]
        if (is.null(dk)) {
          return(NULL)
        }
        ly <- dk$layout()
        if (is.null(ly)) {
          return(NULL)
        }
        out <- dockview_to_layout(ly)
        nm <- view_name(state[[v_id]])
        if (!is.null(nm)) {
          view_name(out) <- nm
        }
        out
      })
      if (any(lgl_ply(v_list, is.null))) {
        return(NULL)
      }
      not_ready <- vapply(
        seq_along(v_list),
        function(i) {
          dk <- docks[[names(state)[[i]]]]
          lp <- tryCatch(
            isolate(dk$live_panels()),
            error = function(e) character()
          )
          length(layout_panel_ids(v_list[[i]])) == 0L && length(lp) > 0L
        },
        logical(1)
      )
      if (any(not_ready)) {
        return(NULL)
      }
      res <- reconstruct_dock_layouts(set_names(v_list, names(state)))
      ca <- client_active()
      if (!is.null(ca)) {
        active_view(res) <- ca
      }
      res
    })
  }
  environment(patched) <- ns
  utils::assignInNamespace("live_view_data", patched, ns = "blockr.dock")
})

board <- new_dock_board(
  extensions = list(
    dag = new_dag_extension(),
    assistant = new_assistant_extension()
  ),
  layouts = list(
    Build = dock_layout(
      "assistant_extension",
      "dag_extension",
      sizes = c(0.5, 0.5)
    )
  )
)

serve(
  board,
  plugins = custom_plugins(list(
    ai_ctrl_block(), # blockr.ai: per-block AI controls
    manage_project() # blockr.session: project save / load / versions
  ))
)
