# ═══════════════════════════════════════════════════════════════════════════════
# SILC vs MILC — Umbrella Review Forest Plots (R version)
# Single-incision versus multi-port laparoscopic cholecystectomy
# Systematic umbrella review of meta-analyses
# ═══════════════════════════════════════════════════════════════════════════════

# ── Load required libraries ────────────────────────────────────────────────────
library(tidyverse)    # dplyr, tidyr, ggplot2
library(gridExtra)    # for combining multiple plots
library(ggplot2)
library(dplyr)
library(tibble)
library(forcats)

# Clear environment
rm(list = ls())

# ── Figure dimensions (BMC Surgery: 18 cm wide, 300 DPI) ─────────────────────
DPI        <- 300
FIG_W_CM   <- 18
FIG_W_IN   <- FIG_W_CM / 2.54    # 18 cm → inches
ROW_H_IN   <- 1.2 / 2.54         # ~1.2 cm per row
BASE_H_IN  <- 3.0 / 2.54         # axis + header base

# Function to compute figure height from row count
fig_height <- function(n_rows) {
  BASE_H_IN + ROW_H_IN * n_rows
}

# ── Colour palette ────────────────────────────────────────────────────────────
COL_PRIORITY   <- '#1a6faf'                    # blue diamond / priority marker
COL_STANDARD   <- '#333333'                    # standard study points
COL_REFLINE    <- '#CC0000'                    # red dashed line of no effect
COL_AMSTAR     <- list(
  'Moderate'       = '#2ca02c',                # green
  'Low'            = '#ff7f0e',                # orange
  'Critically Low' = '#d62728'                 # red
)

# AMSTAR-2 order (for sorting/display)
AMSTAR_ORDER   <- c('Moderate' = 3, 'Low' = 2, 'Critically Low' = 1)

# Missing CI fallback
NO_CI_FALLBACK <- 0.30

# ══════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

# ── Helper: build row label (author year, asterisk for priority) ──────────────
make_label <- function(author, year, is_priority) {
  label <- paste(author, year)
  # Vectorized: use ifelse to handle multiple rows
  label <- ifelse(is_priority, paste0(label, "*"), label)
  return(label)
}

# ── Core drawing function: Create forest plot using ggplot2 ──────────────────
draw_forest_panel <- function(
    df, 
    ref_line = 0, 
    log_scale = FALSE,
    x_label = 'Mean Difference',
    title = '',
    panel_tag = ''
) {
  #' Draw a forest plot panel.
  #' df must contain: author, year, effect, ci_low, ci_high, amstar2, priority
  
  # Create label column
  df <- df %>%
    mutate(label = make_label(author, year, as.logical(priority)),
           label = fct_rev(factor(label, levels = unique(label))))  # reverse for proper y-ordering
  
  # Create plot
  p <- ggplot(df, aes(x = effect, y = label)) +
    # Error bars (confidence intervals) — use geom_errorbar with orientation for modern ggplot2
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                  width = 0.3, linewidth = 1, color = 'black', orientation = 'y') +
    # Point estimates (markers)
    geom_point(aes(color = priority, shape = priority), size = 3) +
    # Reference line (no effect)
    geom_vline(xintercept = ref_line, linetype = 'dashed', color = COL_REFLINE, 
               linewidth = 1.2) +
    # Styling
    scale_color_manual(
      values = c('TRUE' = COL_PRIORITY, 'FALSE' = COL_STANDARD),
      labels = c('TRUE' = 'Priority', 'FALSE' = 'Contributing'),
      guide = 'none'
    ) +
    scale_shape_manual(
      values = c('TRUE' = 18, 'FALSE' = 16),  # 18 = diamond, 16 = circle
      labels = c('TRUE' = 'Priority', 'FALSE' = 'Contributing'),
      guide = 'none'
    ) +
    labs(
      title = if_else(panel_tag == '', title, paste(panel_tag, title, sep = '  ')),
      x = x_label,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = 'bold', hjust = 0),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 9),
      panel.grid.major.x = element_line(color = 'gray90', linetype = 'dotted'),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  # Log scale if needed — add this BEFORE other x scale specifications to avoid conflicts
  if (log_scale) {
    p <- p + 
      scale_x_log10(expand = expansion(mult = 0.1)) +
      annotation_logticks(sides = 'b')
  } else {
    p <- p + scale_x_continuous(expand = expansion(mult = 0.1))
  }
  
  # Add AMSTAR-2 annotations on the left
  amstar_positions <- df %>%
    select(label, amstar2) %>%
    mutate(x_pos = -Inf, hjust = 1.1)
  
  p <- p +
    geom_text(
      data = amstar_positions,
      aes(x = x_pos, y = label, label = amstar2),
      hjust = 1.5, vjust = 0.5,
      fontface = 'bold', size = 2.5,
      color = sapply(amstar_positions$amstar2, function(x) COL_AMSTAR[[x]]),
      inherit.aes = FALSE
    ) +
    coord_cartesian(clip = 'off')
  
  return(p)
}

# ── Helper: Create graphical legend elements ───────────────────────────────────
create_legend_elements <- function() {
  # This function returns data for creating a legend box for inset display
  # Used for combined multi-panel figures
  
  legend_data <- tibble(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(6, 5, 4, 3, 2, 1),
    type = c('Priority', 'Contributing', 'Ref Line', 'AMSTAR: Moderate', 
             'AMSTAR: Low', 'AMSTAR: Critically Low'),
    color = c(COL_PRIORITY, COL_STANDARD, COL_REFLINE, 
              COL_AMSTAR[['Moderate']], COL_AMSTAR[['Low']], COL_AMSTAR[['Critically Low']])
  )
  
  return(legend_data)
}

cat('Setup complete. Proceed to figure creation.\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2: PATIENT-REPORTED OUTCOMES
# ══════════════════════════════════════════════════════════════════════════════

# ── Figure 2A: Postoperative Pain Early (≤24 h) ────────────────────────────────

fig2a_md <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, -0.50, -0.80, -0.20, 'Moderate', TRUE,      # priority
  'Rudiman et al.', 2024, -0.50, -0.86, -0.14, 'Moderate', FALSE,
  'Qiu et al.', 2013, -0.10, NA_real_, NA_real_, 'Moderate', FALSE,
  'Markar et al.', 2011, -0.21, NA_real_, NA_real_, 'Moderate', FALSE,
  'Pereira et al.', 2022, -0.38, NA_real_, NA_real_, 'Low', FALSE,
  'Tamini et al.', 2014, -0.27, NA_real_, NA_real_, 'Low', FALSE,
  'Lyu et al.', 2019, -0.10, NA_real_, NA_real_, 'Low', FALSE,
  'Hao et al.', 2012, -0.75, NA_real_, NA_real_, 'Low', FALSE,
  'Geng et al.', 2013, -0.58, -0.704, -0.457, 'Low', FALSE,
  'Evers et al.', 2016, -0.46, NA_real_, NA_real_, 'Low', FALSE,
  'Garg et al.', 2012, 0.025, 0.01, 0.04, 'Low', FALSE,
  'Trastulli et al.', 2013, -0.25, NA_real_, NA_real_, 'Low', FALSE,
  'Linkwinstar et al.', 2025, 0.18, NA_real_, NA_real_, 'Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

fig2a_smd <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Wang Z et al.', 2012, -0.32, NA_real_, NA_real_, 'Low', FALSE,
  'Sajid et al.', 2012, -0.32, NA_real_, NA_real_, 'Low', FALSE,
  'Zhong et al.', 2012, -0.445, -0.46, -0.43, 'Critically Low', FALSE,
  'Song et al.', 2013, -0.40, NA_real_, NA_real_, 'Critically Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

# Create plots
p2a_md <- draw_forest_panel(
  fig2a_md,
  ref_line = 0,
  log_scale = FALSE,
  x_label = 'MD / WMD (SILC − MILC, pain score units)',
  title = 'Postoperative Pain Early (≤24 h)  —  MD/WMD Reviews',
  panel_tag = 'Figure 2A'
)

p2a_smd <- draw_forest_panel(
  fig2a_smd,
  ref_line = 0,
  log_scale = FALSE,
  x_label = 'SMD (SILC − MILC)',
  title = 'Postoperative Pain Early (≤24 h)  —  SMD Reviews (sub-panel)',
  panel_tag = ''
)

fig2a_combined <- gridExtra::grid.arrange(p2a_md, p2a_smd, ncol = 1, 
                                          heights = c(nrow(fig2a_md) / (nrow(fig2a_md) + nrow(fig2a_smd)),
                                                     nrow(fig2a_smd) / (nrow(fig2a_md) + nrow(fig2a_smd))))

ggsave('Figure2A_Pain_Early.tiff', plot = fig2a_combined, device = 'tiff',
       width = FIG_W_IN, height = fig_height(nrow(fig2a_md) + nrow(fig2a_smd) + 1),
       dpi = DPI, units = 'in')

print(fig2a_combined)
cat('Saved → Figure2A_Pain_Early.tiff\n')

# ── Figure 2B: Postoperative Pain Late (>24 h) ─────────────────────────────────

fig2b_md <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, -0.33, -0.55, 0.11, 'Moderate', TRUE,
  'Qiu et al.', 2013, -0.13, NA_real_, NA_real_, 'Moderate', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

fig2b_smd <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Zhong et al.', 2012, -0.46, NA_real_, NA_real_, 'Critically Low', FALSE,
  'Song et al.', 2013, -0.47, NA_real_, NA_real_, 'Critically Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

p2b_md <- draw_forest_panel(fig2b_md, ref_line = 0, log_scale = FALSE,
                            x_label = 'MD (SILC − MILC, pain score units)',
                            title = 'Postoperative Pain Late (>24 h)  —  MD Reviews',
                            panel_tag = 'Figure 2B')
p2b_smd <- draw_forest_panel(fig2b_smd, ref_line = 0, log_scale = FALSE,
                             x_label = 'SMD (SILC − MILC)',
                             title = 'Postoperative Pain Late (>24 h)  —  SMD Reviews (sub-panel)',
                             panel_tag = '')
fig2b_combined <- gridExtra::grid.arrange(p2b_md, p2b_smd, ncol = 1, 
                                          heights = c(nrow(fig2b_md) / (nrow(fig2b_md) + nrow(fig2b_smd)),
                                                     nrow(fig2b_smd) / (nrow(fig2b_md) + nrow(fig2b_smd))))
ggsave('Figure2B_Pain_Late.tiff', plot = fig2b_combined, device = 'tiff',
       width = FIG_W_IN, height = fig_height(nrow(fig2b_md) + nrow(fig2b_smd) + 1),
       dpi = DPI, units = 'in')
print(fig2b_combined)
cat('Saved → Figure2B_Pain_Late.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 3: SURGICAL EFFICACY OUTCOMES
# ══════════════════════════════════════════════════════════════════════════════

# ── Figure 3A: Operative Time (MD / WMD, minutes) ──────────────────────────────

fig3a <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, -5.69, -11.45, 0.07, 'Moderate', TRUE,
  'Qiu et al.', 2013, -8.50, -12.50, -4.50, 'Moderate', FALSE,
  'Rudiman et al.', 2024, -4.87, -7.09, -2.65, 'Moderate', FALSE,
  'Pereira et al.', 2022, -4.82, -9.27, -0.37, 'Low', FALSE,
  'Hao et al.', 2012, -6.30, NA_real_, NA_real_, 'Low', FALSE,
  'Wang Z et al.', 2012, -5.31, NA_real_, NA_real_, 'Low', FALSE,
  'Markar et al.', 2011, -4.30, -7.60, -1.00, 'Low', FALSE,
  'Geng et al.', 2013, -7.81, -12.23, -3.39, 'Low', FALSE,
  'Garg et al.', 2012, -9.88, -15.47, -4.29, 'Low', FALSE,
  'Trastulli et al.', 2013, -4.19, -6.98, -1.40, 'Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

p3a <- draw_forest_panel(fig3a, ref_line = 0, log_scale = FALSE,
                         x_label = 'MD / WMD (SILC − MILC, min)',
                         title = 'Operative Time (minutes)',
                         panel_tag = 'Figure 3A')
ggsave('Figure3A_Operative_Time.tiff', plot = p3a, device = 'tiff',
       width = FIG_W_IN, height = fig_height(nrow(fig3a) + 1), dpi = DPI, units = 'in')
print(p3a)
cat('Saved → Figure3A_Operative_Time.tiff\n')

# ── Figure 3D: Conversion to Open Surgery (OR/RR – LOG SCALE) ──────────────────

fig3d <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, 0.94, 0.45, 1.97, 'Moderate', TRUE,
  'Qiu et al.', 2013, 1.13, 0.80, 1.59, 'Moderate', FALSE,
  'Rudiman et al.', 2024, 0.88, 0.62, 1.24, 'Moderate', FALSE,
  'Zhong et al.', 2012, 1.07, 0.62, 1.84, 'Low', FALSE,
  'Song et al.', 2013, 0.79, 0.31, 2.02, 'Low', FALSE,
  'Trastulli et al.', 2013, 1.28, 0.99, 1.66, 'Low', FALSE,
  'Garg et al.', 2012, 1.27, 0.77, 2.08, 'Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

p3d <- draw_forest_panel(fig3d, ref_line = 1, log_scale = TRUE,
                         x_label = 'OR / RR (SILC vs MILC) — log scale',
                         title = 'Conversion to Open Surgery',
                         panel_tag = 'Figure 3D')
ggsave('Figure3D_Conversion.tiff', plot = p3d, device = 'tiff',
       width = FIG_W_IN, height = fig_height(nrow(fig3d) + 1), dpi = DPI, units = 'in')
print(p3d)
cat('Saved → Figure3D_Conversion.tiff\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 4: SAFETY OUTCOMES (ADVERSE EVENTS)
# ══════════════════════════════════════════════════════════════════════════════

# ── Figure 4A: Wound Infection (OR — LOG SCALE) ──────────────────────────────

fig4a <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, 1.18, 0.62, 2.26, 'Moderate', TRUE,
  'Qiu et al.', 2013, 1.13, 0.79, 1.62, 'Moderate', FALSE,
  'Zhong et al.', 2012, 1.35, 0.67, 2.71, 'Low', FALSE,
  'Song et al.', 2013, 0.91, 0.50, 1.66, 'Low', FALSE,
  'Geng et al.', 2013, 2.05, 1.33, 3.17, 'Low', FALSE,
  'Trastulli et al.', 2013, 1.30, 1.05, 1.61, 'Low', FALSE,
  'Garg et al.', 2012, 1.35, 0.70, 2.61, 'Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

p4a <- draw_forest_panel(fig4a, ref_line = 1, log_scale = TRUE,
                         x_label = 'OR (SILC vs MILC) — log scale',
                         title = 'Wound Infection',
                         panel_tag = 'Figure 4A')
ggsave('Figure4A_Wound_Infection.tiff', plot = p4a, device = 'tiff',
       width = FIG_W_IN, height = fig_height(nrow(fig4a) + 1), dpi = DPI, units = 'in')
print(p4a)
cat('Saved → Figure4A_Wound_Infection.tiff\n')

# ── Figure 4B: Bile Duct Injury (OR — LOG SCALE) ─────────────────────────────

fig4b <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, 1.02, 0.37, 2.86, 'Moderate', TRUE,
  'Qiu et al.', 2013, 2.07, 0.90, 4.74, 'Moderate', FALSE,
  'Geng et al.', 2013, 0.586, 0.074, 4.639, 'Low', FALSE,
  'Trastulli et al.', 2013, 2.07, 0.90, 4.74, 'Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

p4b <- draw_forest_panel(fig4b, ref_line = 1, log_scale = TRUE,
                         x_label = 'OR (SILC vs MILC) — log scale',
                         title = 'Bile Duct Injury',
                         panel_tag = 'Figure 4B')
ggsave('Figure4B_Bile_Duct.tiff', plot = p4b, device = 'tiff',
       width = FIG_W_IN, height = fig_height(nrow(fig4b) + 1), dpi = DPI, units = 'in')
print(p4b)
cat('Saved → Figure4B_Bile_Duct.tiff\n')

# ── Figure 4C: Bile Leakage (OR — LOG SCALE) ──────────────────────────────────

fig4c <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, 1.45, 0.49, 4.27, 'Moderate', TRUE,
  'Zhong et al.', 2012, 2.99, 1.08, 8.28, 'Low', FALSE,
  'Song et al.', 2013, 3.03, 0.62, 14.82, 'Low', FALSE,
) %>%
  mutate(
    no_ci = is.na(ci_low),
    ci_low = coalesce(ci_low, effect - NO_CI_FALLBACK),
    ci_high = coalesce(ci_high, effect + NO_CI_FALLBACK)
  )

p4c <- draw_forest_panel(fig4c, ref_line = 1, log_scale = TRUE,
                         x_label = 'OR (SILC vs MILC) — log scale',
                         title = 'Bile Leakage',
                         panel_tag = 'Figure 4C')
ggsave('Figure4C_Bile_Leakage.tiff', plot = p4c, device = 'tiff',
       width = FIG_W_IN, height = fig_height(nrow(fig4c) + 1), dpi = DPI, units = 'in')
print(p4c)
cat('Saved → Figure4C_Bile_Leakage.tiff\n')

cat('\n✓ All figures created successfully!\n')
