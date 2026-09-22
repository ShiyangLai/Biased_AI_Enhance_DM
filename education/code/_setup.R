# ==============================================================================
# _setup.R — run this first, from inside education/code/.
#
# Three things replication needs that the analysis scripts assume:
#   1. every package the pipeline touches is installed;
#   2. the "Avenir" typeface used throughout the figures resolves to *something*.
#      Avenir ships with macOS only. On Linux/Windows the graphics device
#      otherwise aborts with "invalid font type" partway through a figure;
#   3. DATA_DIR and FIG_DIR, which every script uses to locate its inputs and
#      write its outputs, plus three small helpers (rd, FONT, save_png) that the
#      scripts share. Scripts are otherwise independent of each other, with one
#      exception noted in run_all.R.
# ==============================================================================

REQUIRED <- c(
  # core
  "dplyr", "tidyr", "readr", "ggplot2", "scales", "patchwork", "rlang",
  # models
  "emmeans", "sandwich", "lmtest", "lme4", "lmerTest", "ordinal", "MCMCglmm",
  "coda", "MASS",
  # effect sizes / fit / tidy output
  "performance", "broom", "broom.mixed",
  # plotting extras
  "ggpattern", "ragg", "systemfonts"
)

missing <- REQUIRED[!vapply(REQUIRED, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# --- where the data and figures live -----------------------------------------
# Scripts read with file.path(IN, "<file>.csv") and write to file.path(OUT, ...),
# where each script sets IN <- DATA_DIR; OUT <- FIG_DIR. Regression tables are
# written to FIG_DIR as .md alongside the figures; two regenerated CSVs
# (balance_education.csv, descriptives_table_education.csv) go to DATA_DIR.
DATA_DIR <- "../data"
FIG_DIR  <- "../figures"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
rd <- function(f) read.csv(file.path(DATA_DIR, f), check.names = FALSE, stringsAsFactors = FALSE)

# --- font fallback ------------------------------------------------------------
# Figures request family = "Avenir". Where it is unavailable, alias it to the
# system sans-serif so scripts run unchanged; figures then differ only in
# typeface, not in any plotted value.
FONT <- "Avenir"
if (requireNamespace("systemfonts", quietly = TRUE)) {
  fams <- unique(systemfonts::system_fonts()$family)
  if (!("Avenir" %in% fams)) {
    sub <- intersect(c("Nimbus Sans", "DejaVu Sans", "Arial", "Helvetica",
                       "Liberation Sans"), fams)
    if (length(sub)) {
      systemfonts::register_variant(name = "Avenir", family = sub[1])
      message("Avenir not found; aliased to '", sub[1], "'.")
    } else {
      FONT <- "sans"
      message("Avenir not found and no substitute located; falling back to 'sans'.")
    }
  }
}

# --- PNG writer ----------------------------------------------------------------
# On macOS the quartz device renders Avenir natively; elsewhere ragg resolves the
# alias registered above. Every script saves through this one helper.
USE_QUARTZ <- isTRUE(capabilities("aqua")[[1]])
save_png <- function(path, plot, width, height, dpi = 500) {
  if (USE_QUARTZ)
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi,
                    device = grDevices::png, type = "quartz")
  else if (requireNamespace("ragg", quietly = TRUE))
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi,
                    device = ragg::agg_png)
  else ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi)
}

# --- default graphics device -------------------------------------------------
# Several scripts print a ggplot at top level without opening a device. Under
# Rscript the default is pdf(), which aborts with "invalid font type" on Avenir.
# Route the default device to ragg, which handles system fonts; figures the
# scripts save explicitly are unaffected.
if (requireNamespace("ragg", quietly = TRUE)) {
  options(device = function(...) ragg::agg_png(
    filename = tempfile(fileext = ".png"), width = 1800, height = 1400, res = 200))
}
