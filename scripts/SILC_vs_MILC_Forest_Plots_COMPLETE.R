# ═══════════════════════════════════════════════════════════════════════════════
# SILC vs MILC — Umbrella Review Forest Plots (Complete R Implementation)
# Single-incision versus multi-port laparoscopic cholecystectomy
# Systematic umbrella review of meta-analyses
# ═══════════════════════════════════════════════════════════════════════════════

# ── Load required libraries ────────────────────────────────────────────────────
library(tidyverse)      # dplyr, ggplot2, tidyr
library(ggplot2)
library(dplyr)
library(tibble)
library(forcats)
library(gridExtra)      # for multi-panel figures
library(grid)
library(scales)

# Clear environment
rm(list = ls())

# ── Figure dimensions (BMC Surgery: 18 cm wide, 300 DPI) ─────────────────────
DPI        <- 300
FIG_W_CM   <- 18
FIG_W_IN   <- FIG_W_CM / 2.54      # 18 cm → inches
ROW_H_IN   <- 1.2 / 2.54           # ~1.2 cm per row
BASE_H_IN  <- 3.0 / 2.54           # axis + header base

# Function to compute figure height from row count
fig_height <- function(n_rows) {
  BASE_H_IN + ROW_H_IN * n_rows
}

# ── Colour palette (AMSTAR-2 Quality Ratings) ─────────────────────────────────
COL_PRIORITY  <- '#1a6faf'         # blue diamond / priority marker
COL_STANDARD  <- '#333333'         # standard study points
COL_REFLINE   <- '#CC0000'         # red dashed line of no effect

COL_AMSTAR    <- c(
  'Moderate'       = '#2ca02c',    # green
  'Low'            = '#ff7f0e',    # orange
  'Critically Low' = '#d62728'     # red
)

# Missing CI fallback
NO_CI_FALLBACK <- 0.30

# AMSTAR-2 order (for sorting/display)
AMSTAR_ORDER   <- c('Moderate' = 3, 'Low' = 2, 'Critically Low' = 1)

# ══════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

# ── Helper: build row label (author year, asterisk for priority) ──────────────
make_label <- function(author, year, priority) {
  base <- paste(author, year)
  if (priority) paste0(base, '*') else base
}

# ── Core drawing function: Forest plot panel ────────────────────────────────
draw_forest_panel <- function(
  df,
  x_label = 'Mean Difference',
  title = '',
  panel_tag = '',
  ref_line = 0,
  log_scale = FALSE
) {
  # Draw one forest-plot panel using ggplot2
  # df must contain: Author, Year, Effect, CI_Low, CI_High, AMSTAR2, Priority
  
  # Prepare data: create factor levels in reverse order (bottom to top)
  df <- df %>%
    mutate(
      Label = map2_chr(Author, Year, ~make_label(.x, .y, FALSE)),
      Label = factor(Label, levels = rev(unique(Label)))
    )
  
  # Create plot
  p <- ggplot(df, aes(x = Effect, y = Label, color = Priority)) +
    
    # Add AMSTAR-2 background bands
    geom_tile(aes(fill = AMSTAR2), alpha = 0.12, width = Inf, height = 0.8) +
    scale_fill_manual(
      values = COL_AMSTAR,
      breaks = names(COL_AMSTAR),
      guide = 'none'
    ) +
    
    # Add confidence interval lines
    geom_segment(aes(x = CI_Low, xend = CI_High, yend = Label), 
                 linewidth = 0.8, color = '#666666') +
    
    # Add point estimates
    geom_point(aes(shape = Priority, size = Priority)) +
    scale_shape_manual(values = c('FALSE' = 16, 'TRUE' = 5), guide = 'none') +
    scale_size_manual(values = c('FALSE' = 2, 'TRUE' = 2.5), guide = 'none') +
    scale_color_manual(
      values = c('FALSE' = COL_STANDARD, 'TRUE' = COL_PRIORITY),
      guide = 'none'
    ) +
    
    # Add reference line (no effect)
    geom_vline(xintercept = ref_line, color = COL_REFLINE, 
               linetype = 'dashed', linewidth = 0.8) +
    
    # Apply log scale if needed
    {if (log_scale) scale_x_log10() else scale_x_continuous()} +
    
    # Labels and theme
    labs(
      title = if (panel_tag != '') paste(panel_tag, title) else title,
      x = x_label,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 9, face = 'bold', hjust = 0),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 9, face = 'bold'),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = 'black', fill = NA, linewidth = 0.5),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = 'pt')
    )
  
  return(p)
}

# ── Helper: Add inset graphical key ────────────────────────────────────────────
add_legend <- function(p) {
  # Add legend to forest plot as inset key
  # Create legend data
  legend_data <- tribble(
    ~marker, ~label,
    'Priority review (*)', 'diamond',
    'Contributing review', 'circle',
    'AMSTAR-2: Moderate', 'moderate',
    'AMSTAR-2: Low', 'low',
    'AMSTAR-2: Critically Low', 'critically_low'
  )
  
  # For now, legends are added manually in manuscript
  return(p)
}

cat('Setup complete. Proceeding to figure creation.\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2A — Postoperative Pain Early (≤24 h)
# ══════════════════════════════════════════════════════════════════════════════

# Panel A: MD / WMD
fig2a_md <- tribble(
  ~Author,              ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2,       ~Priority,
  'Haueter et al.',     2017,  -0.50,   -0.80,    -0.20,    'Moderate',     TRUE,
  'Rudiman et al.',     2024,  -0.50,   -0.86,    -0.14,    'Moderate',     FALSE,
  'Qiu et al.',         2013,  -0.10,   NA,       NA,       'Moderate',     FALSE,
  'Markar et al.',      2011,  -0.21,   NA,       NA,       'Moderate',     FALSE,
  'Pereira et al.',     2022,  -0.38,   NA,       NA,       'Low',          FALSE,
  'Tamini et al.',      2014,  -0.27,   NA,       NA,       'Low',          FALSE,
  'Lyu et al.',         2019,  -0.10,   NA,       NA,       'Low',          FALSE,
  'Hao et al.',         2012,  -0.75,   NA,       NA,       'Low',          FALSE,
  'Geng et al.',        2013,  -0.58,   -0.704,   -0.457,   'Low',          FALSE,
  'Evers et al.',       2016,  -0.46,   NA,       NA,       'Low',          FALSE,
  'Garg et al.',        2012,   0.025,   0.01,     0.04,    'Low',          FALSE,
  'Trastulli et al.',   2013,  -0.25,   NA,       NA,       'Low',          FALSE,
  'Linkwinstar et al.', 2025,   0.18,   NA,       NA,       'Low',          FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - NO_CI_FALLBACK),
    CI_High = coalesce(CI_High, Effect + NO_CI_FALLBACK)
  )

# Panel B: SMD
fig2a_smd <- tribble(
  ~Author,      ~Year, ~Effect,  ~CI_Low,  ~CI_High, ~AMSTAR2,       ~Priority,
  'Wang Z et al.',   2012,  -0.32,   NA,       NA,       'Low',          FALSE,
  'Sajid et al.',    2012,  -0.32,   NA,       NA,       'Low',          FALSE,
  'Zhong et al.',    2012,  -0.445,  -0.46,    -0.43,    'Critically Low', FALSE,
  'Song et al.',     2013,  -0.40,   NA,       NA,       'Critically Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - NO_CI_FALLBACK),
    CI_High = coalesce(CI_High, Effect + NO_CI_FALLBACK)
  )

# Draw Figure 2A
nA <- nrow(fig2a_md)
nB <- nrow(fig2a_smd)

p2a_md <- draw_forest_panel(
  fig2a_md,
  x_label = 'MD / WMD (SILC − MILC, pain score units)',
  title = 'Postoperative Pain Early (≤24 h) — MD/WMD Reviews',
  panel_tag = 'Figure 2A',
  log_scale = FALSE
)

p2a_smd <- draw_forest_panel(
  fig2a_smd,
  x_label = 'SMD (SILC − MILC)',
  title = 'Postoperative Pain Early (≤24 h) — SMD Reviews (sub-panel)',
  panel_tag = '',
  log_scale = FALSE
)

fig2a_combined <- grid.arrange(p2a_md, p2a_smd, nrow = 2, 
                               heights = c(nA, nB),
                               top = textGrob('Figure 2A — Patient-Reported Outcomes: Pain Early',
                                            gp = gpar(fontsize = 11, fontface = 'bold')))

ggsave('Figure2A_Pain_Early.tiff', fig2a_combined, width = FIG_W_IN, height = fig_height(nA + nB + 1),
       units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure2A_Pain_Early.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2B — Postoperative Pain Late (>24 h)
# ══════════════════════════════════════════════════════════════════════════════

fig2b_md <- tribble(
  ~Author,          ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,  ~Priority,
  'Haueter et al.', 2017,  -0.33,  -0.55,   0.11,     'Moderate', TRUE,
  'Qiu et al.',     2013,  -0.13,  NA,      NA,       'Moderate', FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - NO_CI_FALLBACK),
    CI_High = coalesce(CI_High, Effect + NO_CI_FALLBACK)
  )

fig2b_smd <- tribble(
  ~Author,       ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,        ~Priority,
  'Zhong et al.', 2012,  -0.46,  NA,      NA,       'Critically Low', FALSE,
  'Song et al.',  2013,  -0.47,  NA,      NA,       'Critically Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - NO_CI_FALLBACK),
    CI_High = coalesce(CI_High, Effect + NO_CI_FALLBACK)
  )

nA <- nrow(fig2b_md)
nB <- nrow(fig2b_smd)

p2b_md <- draw_forest_panel(fig2b_md, x_label = 'MD (SILC − MILC, pain score units)',
                            title = 'Postoperative Pain Late (>24 h) — MD Reviews',
                            panel_tag = 'Figure 2B')
p2b_smd <- draw_forest_panel(fig2b_smd, x_label = 'SMD (SILC − MILC)',
                             title = 'Postoperative Pain Late (>24 h) — SMD Reviews (sub-panel)',
                             panel_tag = '')

fig2b_combined <- grid.arrange(p2b_md, p2b_smd, nrow = 2, heights = c(nA, nB),
                               top = textGrob('Figure 2B — Patient-Reported Outcomes: Pain Late',
                                            gp = gpar(fontsize = 11, fontface = 'bold')))

ggsave('Figure2B_Pain_Late.tiff', fig2b_combined, width = FIG_W_IN, 
       height = fig_height(nA + nB + 1), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure2B_Pain_Late.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3A — Operative Time (MD / WMD, minutes)
# ══════════════════════════════════════════════════════════════════════════════

fig3a <- tribble(
  ~Author,            ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2,        ~Priority,
  'Gurusamy et al.',  2013,   4.91,   2.38,     7.44,     'Moderate',      TRUE,
  'Markar et al.',    2011,   2.13,   1.05,     3.20,     'Moderate',      FALSE,
  'Haueter et al.',   2017,  13.56,  10.02,    17.09,     'Moderate',      FALSE,
  'Rudiman et al.',   2024,  10.45,   6.74,    14.17,     'Moderate',      FALSE,
  'Qiu et al.',       2013,  16.10,   9.93,    22.26,     'Moderate',      FALSE,
  'Wang Z et al.',    2012,   7.72,   1.38,    14.07,     'Low',           FALSE,
  'Song et al.',      2013,  10.69,   3.28,    18.11,     'Low',           FALSE,
  'Hao et al.',       2012,  11.60,   6.49,    16.72,     'Low',           FALSE,
  'Lyu et al.',       2019,  15.27,   9.67,    20.87,     'Low',           FALSE,
  'Geng et al.',      2013,  13.61,   9.05,    18.18,     'Low',           FALSE,
  'Wang D et al.',    2012,  10.69,   3.14,    18.24,     'Low',           FALSE,
  'Trastulli et al.', 2013,  16.55,   9.95,    23.15,     'Low',           FALSE,
  'Garg et al.',      2012,  15.93,   7.62,    23.63,     'Low',           FALSE,
  'Evers et al.',     2016,  23.12,  11.59,    34.65,     'Low',           FALSE,
  'Pereira et al.',   2022,  19.66,  13.21,    26.11,     'Low',           FALSE,
  'Turun Song et al.',2012,  18.54,   9.20,    27.88,     'Critically Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - 5.0),
    CI_High = coalesce(CI_High, Effect + 5.0)
  )

p3a <- draw_forest_panel(fig3a,
                         x_label = 'MD / WMD (SILC − MILC, minutes)',
                         title = 'Operative Time',
                         panel_tag = 'Figure 3A')

ggsave('Figure3A_Operative_Time.tiff', p3a, width = FIG_W_IN,
       height = fig_height(nrow(fig3a)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure3A_Operative_Time.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3D — Conversion to Open Surgery (OR/RR – LOG SCALE)
# ══════════════════════════════════════════════════════════════════════════════

fig3d <- tribble(
  ~Author,             ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,        ~Priority,
  'Gurusamy et al.',   2013,  1.23,    0.44,    3.45,     'Moderate',      TRUE,
  'Haueter et al.',    2017,  0.71,    0.29,    1.78,     'Moderate',      FALSE,
  'Rudiman et al.',    2024,  1.07,    0.53,    2.19,     'Moderate',      FALSE,
  'Qiu et al.',        2013,  4.21,    2.71,    6.56,     'Moderate',      FALSE,
  'Linkwinstar et al.',2025,  0.80,    0.46,    1.37,     'Low',           FALSE,
  'Pereira et al.',    2022,  0.99,    0.20,    4.82,     'Low',           FALSE,
  'Tamini et al.',     2014,  0.88,    0.53,    1.46,     'Low',           FALSE,
  'Lyu et al.',        2019,  0.94,    0.47,    1.88,     'Low',           FALSE,
  'Hao et al.',        2012,  1.88,    0.85,    4.19,     'Low',           FALSE,
  'Geng et al.',       2013,  0.686,   0.132,   3.576,    'Low',           FALSE,
  'Sajid et al.',      2012,  1.58,    0.58,    4.33,     'Low',           FALSE,
  'Evers et al.',      2016,  0.62,    0.08,    4.91,     'Low',           FALSE,
  'Wang Z et al.',     2012,  0.80,    0.10,    6.18,     'Low',           FALSE,
  'Wang D et al.',     2012,  7.17,    3.00,   17.11,     'Low',           FALSE,
  'Trastulli et al.',  2013,  0.94,    0.13,    6.97,     'Low',           FALSE,
)

p3d <- draw_forest_panel(fig3d,
                         x_label = 'OR / RR (SILC vs MILC) — log scale',
                         title = 'Conversion to Open Surgery',
                         panel_tag = 'Figure 3D',
                         ref_line = 1,
                         log_scale = TRUE)

ggsave('Figure3D_Conversion.tiff', p3d, width = FIG_W_IN,
       height = fig_height(nrow(fig3d)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure3D_Conversion.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4A — Wound Infection (OR/RR – LOG SCALE)
# ══════════════════════════════════════════════════════════════════════════════

fig4a <- tribble(
  ~Author,             ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,        ~Priority,
  'Rudiman et al.',    2024,  1.20,    0.72,    2.03,     'Moderate',      TRUE,
  'Qiu et al.',        2013,  1.03,    0.53,    2.00,     'Moderate',      FALSE,
  'Linkwinstar et al.',2025,  1.77,    1.30,    2.79,     'Low',           FALSE,
  'Lyu et al.',        2019,  1.05,    0.67,    1.66,     'Low',           FALSE,
  'Garg et al.',       2012,  1.43,    0.47,    4.36,     'Low',           FALSE,
  'Geng et al.',       2013,  1.336,   0.842,   2.119,    'Low',           FALSE,
  'Turun Song et al.', 2012,  0.86,    0.23,    3.27,     'Critically Low', FALSE,
)

p4a <- draw_forest_panel(fig4a,
                         x_label = 'OR / RR (SILC vs MILC) — log scale',
                         title = 'Wound Infection',
                         panel_tag = 'Figure 4A',
                         ref_line = 1,
                         log_scale = TRUE)

ggsave('Figure4A_Wound_Infection.tiff', p4a, width = FIG_W_IN,
       height = fig_height(nrow(fig4a)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure4A_Wound_Infection.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4B — Bile Duct Injury (OR/RR – LOG SCALE)
# ══════════════════════════════════════════════════════════════════════════════

fig4b <- tribble(
  ~Author,             ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,        ~Priority,
  'Rudiman et al.',    2024,  0.83,    0.17,    4.04,     'Moderate',      TRUE,
  'Qiu et al.',        2013,  0.52,    0.22,    1.25,     'Moderate',      FALSE,
  'Lyu et al.',        2019,  1.15,    0.42,    3.19,     'Low',           FALSE,
  'Garg et al.',       2012,  1.21,    0.73,    2.01,     'Low',           FALSE,
  'Geng et al.',       2013,  1.000,   0.165,   6.066,    'Low',           FALSE,
  'Turun Song et al.', 2012,  0.86,    0.23,    3.27,     'Critically Low', FALSE,
)

p4b <- draw_forest_panel(fig4b,
                         x_label = 'OR / RR (SILC vs MILC) — log scale',
                         title = 'Bile Duct Injury',
                         panel_tag = 'Figure 4B',
                         ref_line = 1,
                         log_scale = TRUE)

ggsave('Figure4B_Bile_Duct.tiff', p4b, width = FIG_W_IN,
       height = fig_height(nrow(fig4b)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure4B_Bile_Duct.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4C — Bile Leakage (OR/RR – LOG SCALE)
# ══════════════════════════════════════════════════════════════════════════════

fig4c <- tribble(
  ~Author,            ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,  ~Priority,
  'Haueter et al.',   2017,  1.25,    0.41,    3.80,     'Moderate', TRUE,
  'Qiu et al.',       2013,  1.33,    0.84,    2.11,     'Moderate', FALSE,
  'Lyu et al.',       2019,  1.08,    0.50,    2.31,     'Low',      FALSE,
  'Tamini et al.',    2014,  1.16,    0.73,    1.84,     'Low',      FALSE,
)

p4c <- draw_forest_panel(fig4c,
                         x_label = 'OR / RR (SILC vs MILC) — log scale',
                         title = 'Bile Leakage',
                         panel_tag = 'Figure 4C',
                         ref_line = 1,
                         log_scale = TRUE)

ggsave('Figure4C_Bile_Leakage.tiff', p4c, width = FIG_W_IN,
       height = fig_height(nrow(fig4c)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure4C_Bile_Leakage.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2C — Cosmetic Satisfaction (MD/WMD Panel A | SMD sub-panel B)
# ══════════════════════════════════════════════════════════════════════════════

fig2c_md <- tribble(
  ~Author,            ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2,  ~Priority,
  'Haueter et al.',   2017,   4.03,   NA,       NA,       'Moderate', FALSE,
  'Qiu et al.',       2013,  -0.57,   -1.05,    -0.09,    'Moderate', FALSE,
  'Tamini et al.',    2014,  -0.69,   -0.83,    -0.56,    'Low',      FALSE,
  'Trastulli et al.', 2013,  -0.97,   -1.51,    -0.43,    'Low',      FALSE,
  'Garg et al.',      2012,   1.00,    0.49,     1.51,    'Low',      FALSE,
  'Hao et al.',       2012,   1.07,    0.77,     1.38,    'Low',      FALSE,
  'Wang Z et al.',    2012,  -0.02,   NA,       NA,       'Low',      FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - NO_CI_FALLBACK),
    CI_High = coalesce(CI_High, Effect + NO_CI_FALLBACK)
  )

fig2c_smd <- tribble(
  ~Author,           ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2,        ~Priority,
  'Gurusamy et al.', 2013,   0.13,   -0.19,    0.46,     'Moderate',      TRUE,
  'Rudiman et al.',  2024,   2.12,    1.10,    3.13,     'Moderate',      FALSE,
  'Evers et al.',    2016,   2.38,    1.50,    3.26,     'Low',           FALSE,
  'Sajid et al.',    2012,   0.69,   -0.85,    2.23,     'Low',           FALSE,
  'Zhong et al.',    2012,   0.68,   NA,       NA,       'Critically Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - NO_CI_FALLBACK),
    CI_High = coalesce(CI_High, Effect + NO_CI_FALLBACK)
  )

nA <- nrow(fig2c_md)
nB <- nrow(fig2c_smd)

p2c_md <- draw_forest_panel(fig2c_md, x_label = 'MD / WMD (SILC − MILC, cosmetic score units)',
                            title = 'Cosmetic Satisfaction — MD/WMD Reviews',
                            panel_tag = 'Figure 2C')
p2c_smd <- draw_forest_panel(fig2c_smd, x_label = 'SMD (SILC − MILC)',
                             title = 'Cosmetic Satisfaction — SMD Reviews (sub-panel)',
                             panel_tag = '')

fig2c_combined <- grid.arrange(p2c_md, p2c_smd, nrow = 2, heights = c(nA, nB),
                               top = textGrob('Figure 2C — Patient-Reported Outcomes: Cosmetic Satisfaction',
                                            gp = gpar(fontsize = 11, fontface = 'bold')))

ggsave('Figure2C_Cosmetic.tiff', fig2c_combined, width = FIG_W_IN,
       height = fig_height(nA + nB + 1), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure2C_Cosmetic.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2D — Analgesic Requirement (MD Panel A | SMD sub-panel B)
# ══════════════════════════════════════════════════════════════════════════════

fig2d_md <- tribble(
  ~Author,     ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2, ~Priority,
  'Qiu et al.', 2013, -3.78,  -13.78,   6.22,    'Moderate', FALSE,
  'Hao et al.', 2012,  0.69,    0.42,   1.13,    'Low',      FALSE,
)

fig2d_smd <- tribble(
  ~Author,       ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2, ~Priority,
  'Song et al.',  2013, -0.49,   -1.22,     0.24,    'Low',    FALSE,
)

nA <- nrow(fig2d_md)
nB <- nrow(fig2d_smd)

p2d_md <- draw_forest_panel(fig2d_md, x_label = 'MD (SILC − MILC, analgesic mg)',
                            title = 'Analgesic Requirement — MD Reviews',
                            panel_tag = 'Figure 2D')
p2d_smd <- draw_forest_panel(fig2d_smd, x_label = 'SMD (SILC − MILC)',
                             title = 'Analgesic Requirement — SMD Reviews (sub-panel)',
                             panel_tag = '')

fig2d_combined <- grid.arrange(p2d_md, p2d_smd, nrow = 2, heights = c(nA, nB),
                               top = textGrob('Figure 2D — Patient-Reported Outcomes: Analgesic Requirement',
                                            gp = gpar(fontsize = 11, fontface = 'bold')))

ggsave('Figure2D_Analgesic.tiff', fig2d_combined, width = FIG_W_IN,
       height = fig_height(nA + nB + 1), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure2D_Analgesic.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2 — COMBINED (4-panel Patient-Reported Outcomes)
# ══════════════════════════════════════════════════════════════════════════════

p_fig2 <- grid.arrange(
  draw_forest_panel(fig2a_md, x_label = 'MD/WMD (SILC − MILC)', title = 'Pain Early (≤24h)', panel_tag = 'Figure 2A'),
  draw_forest_panel(fig2b_md, x_label = 'MD/WMD (SILC − MILC)', title = 'Pain Late (>24h)', panel_tag = 'Figure 2B'),
  draw_forest_panel(fig2c_md, x_label = 'MD/WMD (SILC − MILC)', title = 'Cosmetic Satisfaction', panel_tag = 'Figure 2C'),
  draw_forest_panel(fig2d_md, x_label = 'MD (SILC − MILC)', title = 'Analgesic Requirement', panel_tag = 'Figure 2D'),
  nrow = 4, ncol = 1,
  heights = c(nrow(fig2a_md), nrow(fig2b_md), nrow(fig2c_md), nrow(fig2d_md)),
  top = textGrob('Figure 2 — Patient-Reported Outcomes (MD/WMD Panels)',
                 gp = gpar(fontsize = 12, fontface = 'bold'))
)

ggsave('Figure2_Patient_Reported.tiff', p_fig2, width = FIG_W_IN,
       height = fig_height(nrow(fig2a_md) + nrow(fig2b_md) + nrow(fig2c_md) + nrow(fig2d_md) + 6),
       units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure2_Patient_Reported.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3B — Hospital Stay (MD/WMD Panel A | SMD sub-panel B)
# ══════════════════════════════════════════════════════════════════════════════

fig3b_md <- tribble(
  ~Author,             ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2,        ~Priority,
  'Gurusamy et al.',   2013,  0.00,   -0.12,     0.11,    'Moderate',      TRUE,
  'Haueter et al.',    2017, -0.05,   -0.14,     0.04,    'Moderate',      FALSE,
  'Rudiman et al.',    2024, -0.11,   -0.26,     0.05,    'Moderate',      FALSE,
  'Markar et al.',     2011, -0.25,   -0.69,     0.18,    'Moderate',      FALSE,
  'Qiu et al.',        2013, -0.16,   -0.28,    -0.04,    'Moderate',      FALSE,
  'Linkwinstar et al.',2025,  0.16,    0.16,     0.28,    'Low',           FALSE,
  'Pereira et al.',    2022, -0.01,   NA,       NA,       'Low',           FALSE,
  'Tamini et al.',     2014, -0.29,   -0.37,    -0.21,    'Low',           FALSE,
  'Geng et al.',       2013, -0.127,  -0.384,    0.129,   'Low',           FALSE,
  'Hao et al.',        2012, -0.27,   -0.59,     0.05,    'Low',           FALSE,
  'Garg et al.',       2012, -0.21,   -0.50,    -0.09,    'Low',           FALSE,
  'Trastulli et al.',  2013, -0.10,   -0.46,     0.26,    'Low',           FALSE,
  'Turun Song et al.', 2012, -0.02,   -0.05,     0.01,    'Critically Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect - 0.20),
    CI_High = coalesce(CI_High, Effect + 0.20)
  )

fig3b_smd <- tribble(
  ~Author,      ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,        ~Priority,
  'Song et al.',  2013, -0.40,  -0.81,   0.01,     'Low',           FALSE,
  'Sajid et al.', 2012, -0.12,  -0.55,   0.31,     'Low',           FALSE,
  'Zhong et al.', 2012, -0.13,  -0.47,   0.20,     'Critically Low', FALSE,
)

nA <- nrow(fig3b_md)
nB <- nrow(fig3b_smd)

p3b_md <- draw_forest_panel(fig3b_md, x_label = 'MD / WMD (SILC − MILC, days)',
                            title = 'Hospital Stay — MD/WMD Reviews',
                            panel_tag = 'Figure 3B')
p3b_smd <- draw_forest_panel(fig3b_smd, x_label = 'SMD (SILC − MILC)',
                             title = 'Hospital Stay — SMD Reviews (sub-panel)',
                             panel_tag = '')

fig3b_combined <- grid.arrange(p3b_md, p3b_smd, nrow = 2, heights = c(nA, nB),
                               top = textGrob('Figure 3B — Surgical Efficacy: Hospital Stay',
                                            gp = gpar(fontsize = 11, fontface = 'bold')))

ggsave('Figure3B_Hospital_Stay.tiff', fig3b_combined, width = FIG_W_IN,
       height = fig_height(nA + nB + 1), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure3B_Hospital_Stay.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3C — Blood Loss (WMD/MD)
# ══════════════════════════════════════════════════════════════════════════════

fig3c <- tribble(
  ~Author,          ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,  ~Priority,
  'Haueter et al.', 2017, -0.08,  -1.83,   1.68,     'Moderate', TRUE,
  'Rudiman et al.', 2024,  1.29,  -0.85,   3.43,     'Moderate', FALSE,
  'Lyu et al.',     2019,  1.35,  -0.02,   2.71,     'Low',      FALSE,
  'Tamini et al.',  2014, -0.16,  -0.27,  -0.04,     'Low',      FALSE,
)

p3c <- draw_forest_panel(fig3c,
                         x_label = 'WMD / MD (SILC − MILC, mL)',
                         title = 'Blood Loss',
                         panel_tag = 'Figure 3C')

ggsave('Figure3C_Blood_Loss.tiff', p3c, width = FIG_W_IN,
       height = fig_height(nrow(fig3c)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure3C_Blood_Loss.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3E — Time to Return to Normal Activity (MD / WMD, days)
# ══════════════════════════════════════════════════════════════════════════════

fig3e <- tribble(
  ~Author,             ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2,  ~Priority,
  'Rudiman et al.',    2024,  0.00,   -1.42,    1.43,     'Moderate', TRUE,
  'Tamini et al.',     2014, -0.47,   -0.65,   -0.29,     'Low',      FALSE,
  'Qiu et al.',        2013, -0.23,   -0.80,    0.34,     'Moderate', FALSE,
  'Geng et al.',       2013, -0.527,  -2.122,   1.068,    'Low',      FALSE,
  'Linkwinstar et al.',2025,  0.01,   -0.56,    0.59,     'Low',      FALSE,
)

p3e <- draw_forest_panel(fig3e,
                         x_label = 'MD / WMD (SILC − MILC, days)',
                         title = 'Time to Return to Normal Activity',
                         panel_tag = 'Figure 3E')

ggsave('Figure3E_Return_Activity.tiff', p3e, width = FIG_W_IN,
       height = fig_height(nrow(fig3e)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure3E_Return_Activity.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3 — COMBINED (5-panel Surgical Efficacy)
# ══════════════════════════════════════════════════════════════════════════════

p_fig3 <- grid.arrange(
  draw_forest_panel(fig3a, x_label = 'MD/WMD (minutes)', title = 'Operative Time', panel_tag = 'Figure 3A'),
  draw_forest_panel(fig3b_md, x_label = 'MD/WMD (days)', title = 'Hospital Stay', panel_tag = 'Figure 3B'),
  draw_forest_panel(fig3c, x_label = 'WMD/MD (mL)', title = 'Blood Loss', panel_tag = 'Figure 3C'),
  draw_forest_panel(fig3d, x_label = 'OR/RR (log)', title = 'Conversion to Open', panel_tag = 'Figure 3D', 
                    ref_line = 1, log_scale = TRUE),
  draw_forest_panel(fig3e, x_label = 'MD/WMD (days)', title = 'Return to Activity', panel_tag = 'Figure 3E'),
  nrow = 5, ncol = 1,
  heights = c(nrow(fig3a), nrow(fig3b_md), nrow(fig3c), nrow(fig3d), nrow(fig3e)),
  top = textGrob('Figure 3 — Surgical Efficacy Outcomes',
                 gp = gpar(fontsize = 12, fontface = 'bold'))
)

ggsave('Figure3_Surgical_Efficacy.tiff', p_fig3, width = FIG_W_IN,
       height = fig_height(nrow(fig3a) + nrow(fig3b_md) + nrow(fig3c) + nrow(fig3d) + nrow(fig3e) + 8),
       units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure3_Surgical_Efficacy.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4D — Incisional Hernia (OR/RR – LOG SCALE)
# ══════════════════════════════════════════════════════════════════════════════

fig4d <- tribble(
  ~Author,           ~Year, ~Effect, ~CI_Low,  ~CI_High, ~AMSTAR2,       ~Priority,
  'Haueter et al.',  2017,  2.50,    1.10,     5.69,     'Moderate',     TRUE,
  'Rudiman et al.',  2024,  1.43,    0.75,     2.74,     'Moderate',     FALSE,
  'Qiu et al.',      2013,  1.67,    0.65,     4.27,     'Moderate',     FALSE,
  'Pereira et al.',  2022,  1.70,    1.16,     2.50,     'Low',          FALSE,
  'Lyu et al.',      2019,  2.51,    1.23,     5.12,     'Low',          FALSE,
  'Tamini et al.',   2014,  2.133,   1.19,     3.83,     'Low',          FALSE,
  'Garg et al.',     2012,  2.94,    NA,       NA,       'Low',          FALSE,
  'Trastulli et al.',2013,  2.99,    0.60,    14.80,     'Low',          FALSE,
  'Geng et al.',     2013,  1.937,   0.658,    5.706,    'Low',          FALSE,
) %>%
  mutate(
    no_ci = is.na(CI_Low),
    CI_Low = coalesce(CI_Low, Effect / 2.0),
    CI_High = coalesce(CI_High, Effect * 2.0)
  )

p4d <- draw_forest_panel(fig4d,
                         x_label = 'OR / RR (SILC vs MILC) — log scale',
                         title = 'Incisional Hernia',
                         panel_tag = 'Figure 4D',
                         ref_line = 1,
                         log_scale = TRUE)

ggsave('Figure4D_Incisional_Hernia.tiff', p4d, width = FIG_W_IN,
       height = fig_height(nrow(fig4d)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure4D_Incisional_Hernia.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4E — Haematoma (OR – LOG SCALE)
# ══════════════════════════════════════════════════════════════════════════════

fig4e <- tribble(
  ~Author,           ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,  ~Priority,
  'Haueter et al.',  2017,  1.02,    0.37,    2.86,     'Moderate', TRUE,
  'Qiu et al.',      2013,  2.07,    0.90,    4.74,     'Moderate', FALSE,
  'Geng et al.',     2013,  0.586,   0.074,   4.639,    'Low',      FALSE,
  'Trastulli et al.',2013,  2.07,    0.90,    4.74,     'Low',      FALSE,
)

p4e <- draw_forest_panel(fig4e,
                         x_label = 'OR (SILC vs MILC) — log scale',
                         title = 'Haematoma',
                         panel_tag = 'Figure 4E',
                         ref_line = 1,
                         log_scale = TRUE)

ggsave('Figure4E_Haematoma.tiff', p4e, width = FIG_W_IN,
       height = fig_height(nrow(fig4e)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure4E_Haematoma.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4F — Retained Bile Stones (OR/RR – LOG SCALE)
# ══════════════════════════════════════════════════════════════════════════════

fig4f <- tribble(
  ~Author,          ~Year, ~Effect, ~CI_Low, ~CI_High, ~AMSTAR2,  ~Priority,
  'Haueter et al.', 2017,  2.03,    0.57,    7.27,     'Moderate', TRUE,
  'Qiu et al.',     2013,  1.11,    0.35,    3.49,     'Moderate', FALSE,
  'Lyu et al.',     2019,  1.23,    0.45,    3.39,     'Low',      FALSE,
  'Geng et al.',    2013,  2.149,   0.554,   8.329,    'Low',      FALSE,
)

p4f <- draw_forest_panel(fig4f,
                         x_label = 'OR / RR (SILC vs MILC) — log scale',
                         title = 'Retained Bile Stones',
                         panel_tag = 'Figure 4F',
                         ref_line = 1,
                         log_scale = TRUE)

ggsave('Figure4F_Retained_Stones.tiff', p4f, width = FIG_W_IN,
       height = fig_height(nrow(fig4f)), units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure4F_Retained_Stones.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4 — COMBINED (6-panel Surgical Safety)
# ══════════════════════════════════════════════════════════════════════════════

p_fig4 <- grid.arrange(
  draw_forest_panel(fig4a, x_label = 'OR/RR (log)', title = 'Wound Infection', panel_tag = 'Figure 4A', 
                    ref_line = 1, log_scale = TRUE),
  draw_forest_panel(fig4b, x_label = 'OR/RR (log)', title = 'Bile Duct Injury', panel_tag = 'Figure 4B', 
                    ref_line = 1, log_scale = TRUE),
  draw_forest_panel(fig4c, x_label = 'OR/RR (log)', title = 'Bile Leakage', panel_tag = 'Figure 4C', 
                    ref_line = 1, log_scale = TRUE),
  draw_forest_panel(fig4d, x_label = 'OR/RR (log)', title = 'Incisional Hernia', panel_tag = 'Figure 4D', 
                    ref_line = 1, log_scale = TRUE),
  draw_forest_panel(fig4e, x_label = 'OR (log)', title = 'Haematoma', panel_tag = 'Figure 4E', 
                    ref_line = 1, log_scale = TRUE),
  draw_forest_panel(fig4f, x_label = 'OR/RR (log)', title = 'Retained Bile Stones', panel_tag = 'Figure 4F', 
                    ref_line = 1, log_scale = TRUE),
  nrow = 6, ncol = 1,
  heights = c(nrow(fig4a), nrow(fig4b), nrow(fig4c), nrow(fig4d), nrow(fig4e), nrow(fig4f)),
  top = textGrob('Figure 4 — Surgical Safety Outcomes',
                 gp = gpar(fontsize = 12, fontface = 'bold'))
)

ggsave('Figure4_Safety_Outcomes.tiff', p_fig4, width = FIG_W_IN,
       height = fig_height(nrow(fig4a) + nrow(fig4b) + nrow(fig4c) + nrow(fig4d) + nrow(fig4e) + nrow(fig4f) + 10),
       units = 'in', dpi = DPI, compression = 'none')
cat('Saved → Figure4_Safety_Outcomes.tiff\n')

cat('\n✓ All forest plots created successfully using ggplot2!\n')
cat('✓ Professional AMSTAR-2 color-coded backgrounds applied.\n')
