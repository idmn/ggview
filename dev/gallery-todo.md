# Gallery: next session

- Decide whether a plain ggplot should acquire a default canvas when placed in
  a gallery. **Recommendation:** no. Keep gallery thumbnail defaults in the
  renderer, as they are now; a canvas should remain an explicit promise about
  a plot's normal preview/export dimensions.

- Revisit the list contract and the possible `map_gallery()` helper.
  `gallery` is already a list in the base-R sense (`is.list(g)` is `TRUE`).
  Adding `"list"` as a secondary S3 class would make `inherits(g, "list")`
  true, but would not remove the methods that preserve the gallery class on
  `[` or validate assignments. **Recommendation:** add it only if its
  compatibility signal proves useful; do not expect it to simplify the core
  implementation. Do not add `map_gallery()` yet: `as.gallery(lapply(g, f))`
  and `as.gallery(purrr::map(g, f))` are clear already. If repeated use shows
  it is needed, make it direct (not recursively magical), name-preserving, and
  return a gallery.

- Ensure gallery-wide ggplot additions have one simple rule: `g + element`
  applies the element recursively to every plot leaf, preserves folders and
  names, and otherwise behaves like adding that element to each ggplot. This
  is worth supporting because it extends familiar ggplot vocabulary without
  new gallery verbs.

- Think through batch saving from nested galleries, but do not add a
  gallery-specific helper. `flatten_gallery()` would turn hierarchy into
  slash-delimited names, which is lossy and feels less list-like; a
  `save_gallery()` would impose output and directory conventions. Keep saving
  as an operation on individual plots and revisit only if a clear, minimal
  pattern emerges from real use.

- Decide whether the session-local render cache is worth implementing now.
  See `gallery-render-cache.md`. It is most useful for repeated plots and
  nested galleries, but should stay session-local initially.

- Revisit the folder tile and size slider as a single visual system.
  **Recommendation:** replace the platform-dependent emoji folder with a
  small monochrome icon, and try a transparent slider container so it feels
  less like a floating control. Keep the control unobtrusive and unlabelled.
  Consider remembering thumbnail size while moving between folders: it is a
  viewing preference, not gallery data.
  (Iaroslav) Actually it's clear we should remember thumbnail size. The 
  current behaviour is undesireable: our adjust scale -> open a plot ->
  click "back" -> scale is reset.

- Improve the canvas background entry in the info card. **Recommendation:**
  show a small colour swatch plus a readable value (for example, `#f2f2f2`);
  retain a text description for non-colour or inherited backgrounds.

- Do a full code-quality pass over the R methods, HTML templates, navigation,
  cleanup, and error messages. Simplify anything that does not earn its
  complexity, then run package checks and manually inspect both RStudio and
  Positron viewers. In particular, check every mutation route, including
  `names(g) <- ...`, so the unique-name rule cannot be bypassed accidentally.

- Finish release-facing documentation: NEWS, README, and a small gallery
  animation. **Recommendation:** defer a vignette unless the examples outgrow
  the README; this feature should stay easy to discover without one.

- Decide the first-release boundary: list-like construction/extraction,
  nesting, browsing, and gallery-wide ggplot additions are the core. Defer
  search/tags, persistent render caching, and gallery-level output dimensions
  until a concrete workflow justifies them.
