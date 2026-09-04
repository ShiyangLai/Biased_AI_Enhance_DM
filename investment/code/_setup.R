# ==============================================================================
# _setup.R — run this first, from inside investment/code/.
#
# Three things replication needs that the analysis scripts assume:
#   1. every package the pipeline touches is installed;
#   2. the "Avenir" typeface used throughout the figures resolves to *something*.
#      Avenir ships with macOS only. On Linux/Windows the graphics device
#      otherwise aborts with "invalid font type" partway through a figure;
#   3. DATA_DIR and FIG_DIR, which every script uses to locate its inputs and
#      write its outputs. Scripts are otherwise independent of each other.
# ==============================================================================

REQUIRED <- c(
  # core
  "dplyr", "tidyr", "stringr", "ggplot2", "scales",
  # models
  "emmeans", "MCMCglmm", "nnet", "quantreg", "MASS",
  "sandwich", "lmtest", "clubSandwich", "estimatr",
  # effect sizes / fit
  "performance", "effectsize", "broom",
  # plotting extras
  "patchwork", "cowplot", "ggdist", "ggbeeswarm", "RColorBrewer",
  "knitr", "ragg", "systemfonts"
)

missing <- REQUIRED[!vapply(REQUIRED, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# --- where the data and figures live -----------------------------------------
# Scripts read with rd("<file>.csv") and write with ggsave(file.path(FIG_DIR, ...)).
DATA_DIR <- "../data"
FIG_DIR  <- "../figures"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

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

# --- PDF device fallback ------------------------------------------------------
# One figure requests cairo_pdf. capabilities("cairo") can report TRUE while the
# underlying X11 libraries are absent, in which case the device fails at write
# time. Probe it for real and fall back to the standard pdf() device, which
# substitutes the font but is otherwise identical.
.cairo_ok <- tryCatch({
  f <- tempfile(fileext = ".pdf"); grDevices::cairo_pdf(f, width = 1, height = 1)
  plot.new(); grDevices::dev.off(); unlink(f); TRUE
}, error = function(e) FALSE, warning = function(w) FALSE)
if (!.cairo_ok) {
  # pdf() resolves font families from its own database and errors on "Avenir";
  # map it onto Helvetica metrics before aliasing the device.
  try(grDevices::pdfFonts(Avenir = grDevices::pdfFonts()$Helvetica), silent = TRUE)
  cairo_pdf <- function(filename, width, height, ...)
    grDevices::pdf(file = filename, width = width, height = height)
  message("cairo_pdf unavailable; aliased to pdf() with Avenir mapped to Helvetica.")
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
