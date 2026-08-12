# Gallery render cache

If the same plot appears more than once in a gallery tree, render it once and
reuse its PNG for every matching tile.

Start with a cache scoped to one preview session. A `hash(plot)` key is enough
for the current gallery renderer because canvas settings are stored on the plot
and galleries always render PNGs with fixed fallbacks.

Do not make the cache persistent across R sessions without more work. Rendering
can also depend on the graphics device, fonts, `theme_set()`, and package
versions; a persistent cache would need a renderer-context key and eviction
policy.
