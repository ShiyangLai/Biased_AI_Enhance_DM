# ==============================================================================
# dual_ai_decomposition_v2.R — DRAFT v2: persuasion/backfire decomposition for
# the DUAL-AI experiment with the pair configuration classified FIRST:
#
#   Consensus (both AIs on the same side of the user; "one tied" folded in):
#     -> classic Persuasion / Backfire x Positive / Negative (4 cells)
#   Split (one AI ahead, one behind):
#     -> advisor SELECTION, not persuasion: followed better vs followed worse
#   Tied pairs and no-shift observations are excluded (counted in footnote).
#
# Six cells partition all classifiable substantive shifts; within each arm the
# six cell %s sum to 100. Arms: Dual Default / Balanced / Opposition.
# Self-contained; saves Images/dual_ai_decomposition_v2.png (ragg for emoji).
# ==============================================================================
suppressMessages({library(dplyr); library(stringr); library(ggplot2)})

# ---- 1. Data + six-cell taxonomy ---------------------------------------------
d <- read.csv("../data/encrypted_ai2.csv",
              stringsAsFactors = FALSE)
adj <- function(code, conf) ifelse(code == -1, (1 - conf) * 0.5,
                             ifelse(code == 1, 0.5 + conf * 0.5, 0.5))
code_of <- function(s) case_when(s == "Republican" ~ 1, s %in% c("Neutral", "Default") ~ 0,
                                 s == "Democrat" ~ -1, TRUE ~ NA_real_)
d <- d %>% mutate(
  NormalizedTruthCode = (TruthCode + 1) / 2,
  PrePerformance  = 1 - abs(NormalizedTruthCode - adj(PreEvaCode,  PreConfCode)),
  PostPerformance = 1 - abs(NormalizedTruthCode - adj(PostEvaCode, PostConfCode)),
  AI1S = str_extract(AIStanceLabel_S, "(?<=\\(').*?(?=', ')"),
  AI2S = str_extract(AIStanceLabel_S, "(?<=', ').*?(?=')"),
  AI1C = code_of(AI1S), AI2C = code_of(AI2S),
  UC   = case_when(UStanceLabel_S == "Republican" ~ 1, UStanceLabel_S == "Independent" ~ 0,
                   UStanceLabel_S == "Democrat" ~ -1, TRUE ~ NA_real_),
  Arm = case_when(
    AI1S == "Default" & AI2S == "Default" ~ "Default",
    AI1C != AI2C & UC > pmin(AI1C, AI2C) & UC < pmax(AI1C, AI2C) ~ "Balanced",
    UC != 0 & AI1C != 0 & AI2C != 0 &
      sign(AI1C) == -sign(UC) & sign(AI2C) == -sign(UC) ~ "Opposition",
    TRUE ~ NA_character_)
) %>% dplyr::filter(!is.na(Arm), !is.na(PostPerformance), !is.na(PrePerformance),
             !is.na(AI1Correctness), !is.na(AI2Correctness))

tau <- 0.1
d <- d %>% mutate(
  d1 = AI1Correctness - PrePerformance, d2 = AI2Correctness - PrePerformance,
  regime = case_when(
    (d1 >= tau & d2 <= -tau) | (d1 <= -tau & d2 >= tau) ~ "split",
    pmax(d1, d2) >= tau  & pmin(d1, d2) > -tau ~ "cons_ahead",   # >=1 ahead, none behind
    pmin(d1, d2) <= -tau & pmax(d1, d2) <  tau ~ "cons_behind",  # >=1 behind, none ahead
    TRUE ~ "tied"),
  delta = PostPerformance - PrePerformance,
  cat6 = case_when(
    abs(delta) < tau ~ "noshift",
    regime == "cons_ahead"  & delta >=  tau ~ "PP",
    regime == "cons_behind" & delta <= -tau ~ "NP",
    regime == "cons_behind" & delta >=  tau ~ "PB",
    regime == "cons_ahead"  & delta <= -tau ~ "NB",
    regime == "split" & delta >=  tau ~ "SB",   # followed the BETTER advisor
    regime == "split" & delta <= -tau ~ "SW",   # followed the WORSE advisor
    TRUE ~ "tied_shift"))
s <- d %>% dplyr::filter(cat6 %in% c("PP", "NP", "PB", "NB", "SB", "SW"))

pers_pct <- 100 * mean(s$cat6 %in% c("PP", "NP"))
back_pct <- 100 * mean(s$cat6 %in% c("PB", "NB"))
conf_pct <- 100 * mean(s$cat6 %in% c("SB", "SW"))
pos_pct  <- 100 * mean(s$cat6 %in% c("PP", "PB", "SB"))
cell_tab <- 100 * prop.table(table(s$Arm, s$cat6), margin = 1)
n_split  <- sum(s$cat6 %in% c("SB", "SW"))

# ---- 2. Figure geometry -------------------------------------------------------
ARMS <- c("Balanced", "Default", "Opposition")
arm_cols <- c(Balanced = "#4E79A7", Default = "#5F5A57", Opposition = "#E15759")
EMOJI <- "Apple Color Emoji"

cells <- list(PP = c(1.2, 6.3),  PB = c(11.0, 6.3),  SB = c(20.8, 6.3),
              NP = c(1.2, 3.0),  NB = c(11.0, 3.0),  SW = c(20.8, 3.0))
LINE_L <- 0.6; LINE_R <- 5.4

el_line <- el_pt <- el_txt <- el_seg <- el_box <- NULL
add_txt <- function(x, y, lab, size = 3, face = "plain", col = "black", ang = 0, hj = 0.5, fam = "Avenir")
  el_txt <<- rbind(el_txt, data.frame(x, y, lab, size, face, col, ang, hj, fam))

for (k in names(cells)) {
  x0 <- cells[[k]][1]; y0 <- cells[[k]][2]
  el_line <- rbind(el_line, data.frame(x = x0 + LINE_L, xend = x0 + LINE_R, y = y0, yend = y0))
  el_pt   <- rbind(el_pt, data.frame(x = c(x0 + LINE_L, x0 + LINE_R), y = y0))
  add_txt(x0 + LINE_L, y0 - 0.5, "Completely Wrong",   size = 2.4, col = "gray25", hj = 0)
  add_txt(x0 + LINE_R, y0 - 0.5, "Completely Correct", size = 2.4, col = "gray25", hj = 1)
  yI <- y0 + 0.62
  if (k == "PP") {        # consensus pair correct side; user moves toward it
    add_txt(x0 + 4.95, yI, "\U0001F916\U0001F916", size = 4.6, fam = EMOJI)
    add_txt(x0 + 1.55, yI, "\U0001F469", size = 3.6, fam = EMOJI)
    el_seg <- rbind(el_seg, data.frame(x = x0 + 2.05, xend = x0 + 3.55, y = yI, yend = yI))
    add_txt(x0 + 4.05, yI, "\U0001F469", size = 4.6, fam = EMOJI)
  } else if (k == "NP") { # consensus pair wrong side; user moves toward it
    add_txt(x0 + 1.0, yI, "\U0001F916\U0001F916", size = 4.6, fam = EMOJI)
    add_txt(x0 + 2.1, yI, "\U0001F469", size = 4.6, fam = EMOJI)
    el_seg <- rbind(el_seg, data.frame(x = x0 + 3.5, xend = x0 + 2.6, y = yI, yend = yI))
    add_txt(x0 + 4.0, yI, "\U0001F469", size = 3.6, fam = EMOJI)
  } else if (k == "PB") { # consensus pair wrong side; user moves away (improves)
    add_txt(x0 + 1.0, yI, "\U0001F916\U0001F916", size = 4.6, fam = EMOJI)
    add_txt(x0 + 2.25, yI, "\U0001F469", size = 3.6, fam = EMOJI)
    el_seg <- rbind(el_seg, data.frame(x = x0 + 2.75, xend = x0 + 4.2, y = yI, yend = yI))
    add_txt(x0 + 4.7, yI, "\U0001F469", size = 4.6, fam = EMOJI)
  } else if (k == "NB") { # consensus pair correct side; user moves away (worsens)
    add_txt(x0 + 1.3, yI, "\U0001F469", size = 4.6, fam = EMOJI)
    el_seg <- rbind(el_seg, data.frame(x = x0 + 2.8, xend = x0 + 1.8, y = yI, yend = yI))
    add_txt(x0 + 3.3, yI, "\U0001F469", size = 3.6, fam = EMOJI)
    add_txt(x0 + 4.95, yI, "\U0001F916\U0001F916", size = 4.6, fam = EMOJI)
  } else if (k == "SB") { # split pair: one robot each end; user moves to the BETTER one
    add_txt(x0 + 1.0, yI, "\U0001F916", size = 4.6, fam = EMOJI)
    add_txt(x0 + 4.95, yI, "\U0001F916", size = 4.6, fam = EMOJI)
    add_txt(x0 + 2.45, yI, "\U0001F469", size = 3.6, fam = EMOJI)
    el_seg <- rbind(el_seg, data.frame(x = x0 + 2.9, xend = x0 + 3.75, y = yI, yend = yI))
    add_txt(x0 + 4.15, yI, "\U0001F469", size = 4.6, fam = EMOJI)
  } else {                # SW: split pair; user moves to the WORSE one
    add_txt(x0 + 1.0, yI, "\U0001F916", size = 4.6, fam = EMOJI)
    add_txt(x0 + 4.95, yI, "\U0001F916", size = 4.6, fam = EMOJI)
    add_txt(x0 + 3.55, yI, "\U0001F469", size = 3.6, fam = EMOJI)
    el_seg <- rbind(el_seg, data.frame(x = x0 + 3.1, xend = x0 + 2.25, y = yI, yend = yI))
    add_txt(x0 + 1.8, yI, "\U0001F469", size = 4.6, fam = EMOJI)
  }
  el_box <- rbind(el_box, data.frame(xmin = x0 + 5.8, xmax = x0 + 9.0, ymin = y0 - 0.65, ymax = y0 + 1.15))
  pcts <- cell_tab[ARMS, k]
  for (j in seq_along(ARMS)) {
    yj <- y0 + 0.85 - (j - 1) * 0.62
    is_max <- pcts[j] == max(pcts)
    add_txt(x0 + 5.95, yj, paste0(ARMS[j], ":"), size = 2.9, hj = 0,
            col = unname(arm_cols[ARMS[j]]), face = ifelse(is_max, "bold", "plain"))
    add_txt(x0 + 8.85, yj, sprintf("%.1f%%", pcts[j]), size = 2.9, hj = 1,
            col = unname(arm_cols[ARMS[j]]), face = ifelse(is_max, "bold", "plain"))
  }
}

hdr <- data.frame(xmin = c(1.2, 11.0, 20.8), xmax = c(10.05, 19.85, 29.65), ymin = 8.35, ymax = 9.75)
hx <- (hdr$xmin + hdr$xmax) / 2
add_txt(hx[1], 9.35, "Persuasion Effect", size = 4.2, face = "bold")
add_txt(hx[1], 8.75, sprintf("(consensus pair, %.1f%%)", pers_pct), size = 3.2)
add_txt(hx[2], 9.35, "Backfire Effect", size = 4.2, face = "bold")
add_txt(hx[2], 8.75, sprintf("(consensus pair, %.1f%%)", back_pct), size = 3.2)
add_txt(hx[3], 9.35, "Selection Effect", size = 4.2, face = "bold")
add_txt(hx[3], 8.75, sprintf("(conflicting pair: one ahead, one behind, %.1f%%)", conf_pct), size = 3.2)

rowband <- data.frame(xmin = -0.75, xmax = 0.55, ymin = c(5.35, 2.05), ymax = c(7.65, 4.35))
add_txt(-0.1, mean(c(5.35, 7.65)), sprintf("Positive\n(%.1f%%)", pos_pct), size = 3.2, face = "bold", ang = 90)
add_txt(-0.1, mean(c(2.05, 4.35)), sprintf("Negative\n(%.1f%%)", 100 - pos_pct), size = 3.2, face = "bold", ang = 90)

# footnote
add_txt(29.65, 1.65,
        sprintf("Selection (conflicting-pair) shifts: n = %d (selected better = %d, worse = %d). Tied pairs (n = %d shifts) and no-shift excluded.",
                n_split, sum(s$cat6 == "SB"), sum(s$cat6 == "SW"), sum(d$cat6 == "tied_shift")),
        size = 2.3, col = "gray40", hj = 1)

p <- ggplot() +
  geom_rect(data = hdr,     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "gray88") +
  geom_rect(data = rowband, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "gray88") +
  geom_rect(data = el_box,  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "white", color = "gray55", linetype = "22", linewidth = 0.45) +
  geom_segment(data = el_line, aes(x = x, xend = xend, y = y, yend = yend),
               color = "gray40", linewidth = 0.8) +
  geom_point(data = el_pt, aes(x = x, y = y), color = "gray40", size = 1.8) +
  geom_segment(data = el_seg, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(6, "pt"), type = "closed"),
               color = "gray20", linewidth = 0.7) +
  geom_text(data = el_txt, aes(x = x, y = y, label = lab, size = size, fontface = face,
                               color = col, angle = ang, hjust = hj, family = fam)) +
  scale_size_identity() + scale_color_identity() +
  coord_fixed(xlim = c(-0.8, 29.9), ylim = c(1.4, 9.9), expand = FALSE) +
  theme_void()

out <- path.expand("../figures/dual_ai_decomposition_v2.png")
ragg::agg_png(out, width = 18.4, height = 5.1, units = "in", res = 300, scaling = 1.65)
print(p)
dev.off()
cat("Saved:", out, "\n")
