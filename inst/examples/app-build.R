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
library(blockr.io)
library(blockr.dplyr)
library(blockr.ggplot)
library(blockr.assistant)
library(blockr.ai)
library(blockr.dm)
library(blockr.session)
library(blockr.leaflet) # custom "map block" we built (markers per origin)
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
