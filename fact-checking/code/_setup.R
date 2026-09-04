# ==============================================================================
# _setup.R — run this first, from inside fact-checking/code/.
#
# Two things replication needs that the analysis scripts assume:
#   1. every package the pipeline touches is installed;
#   2. the "Avenir" typeface used throughout the figures resolves to *something*.
#      Avenir ships with macOS only. On Linux/Windows the graphics device
#      otherwise aborts with "invalid font type" partway through a figure.
# ==============================================================================

REQUIRED <- c(
  # core
  "dplyr", "tidyr", "stringr", "ggplot2", "scales",
  # models
  "lme4", "lmerTest", "emmeans", "ordinal", "MCMCglmm", "fixest", "estimatr",
  "sandwich", "lmtest", "clubSandwich", "car", "mediation", "boot",
  # effect sizes / fit
  "performance", "effectsize", "rcompanion", "broom", "broom.mixed",
  # tables / plotting extras
  "patchwork", "cowplot", "ggdist", "ggbeeswarm", "ggeffects", "RColorBrewer",
  "cobalt", "tableone", "stargazer", "knitr", "ragg", "systemfonts"
)

missing <- REQUIRED[!vapply(REQUIRED, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# --- font fallback ------------------------------------------------------------
# Figures request family = "Avenir". Where it is unavailable, alias it to the
# system sans-serif so scripts run unchanged; figures then differ only in
# typeface, not in any plotted value.
if (requireNamespace("systemfonts", quietly = TRUE)) {
  fams <- unique(systemfonts::system_fonts()$family)
  if (!("Avenir" %in% fams)) {
    sub <- intersect(c("Nimbus Sans", "DejaVu Sans", "Arial", "Helvetica",
                       "Liberation Sans"), fams)
    if (length(sub)) {
      systemfonts::register_variant(name = "Avenir", family = sub[1])
      message("Avenir not found; aliased to '", sub[1], "'.")
    } else {
      message("Avenir not found and no substitute located; figures may fail. ",
              "Install a sans-serif font or edit family = \"Avenir\" in the scripts.")
    }
  }
}

# --- default graphics device -------------------------------------------------
# Several scripts call print(p) on a ggplot without opening a device. Under
# Rscript the default is pdf(), which supports only a fixed set of font
# families and aborts with "invalid font type" on Avenir. Route the default
# device to ragg, which handles system fonts; figures the scripts save
# explicitly are unaffected.
if (requireNamespace("ragg", quietly = TRUE)) {
  options(device = function(...) ragg::agg_png(
    filename = tempfile(fileext = ".png"), width = 1800, height = 1400, res = 200))
}

# Figures are written to ../figures/ ; make sure it exists.
dir.create("../figures", showWarnings = FALSE, recursive = TRUE)
