# ═══════════════════════════════════════════════════════════════════════════════
# SILC vs MILC — Umbrella Review Forest Plots (R with forestplot package)
# Better approach using specialized forest plot library
# ═══════════════════════════════════════════════════════════════════════════════

# ── Load required libraries ────────────────────────────────────────────────────
library(tidyverse)    # dplyr, tidyr for data manipulation
library(forestplot)   # purpose-built forest plot package
library(grid)         # for graphics

# Clear environment
rm(list = ls())

# ── Figure dimensions (BMC Surgery: 18 cm wide, 300 DPI) ─────────────────────
DPI        <- 300
FIG_W_CM   <- 18
FIG_W_IN   <- FIG_W_CM / 2.54    # 18 cm → inches

# ── Colour palette ────────────────────────────────────────────────────────────
COL_PRIORITY   <- '#1a6faf'                    # blue diamond / priority marker
COL_STANDARD   <- '#333333'                    # standard study points
COL_REFLINE    <- '#CC0000'                    # red dashed line
COL_AMSTAR     <- list(
  'Moderate'       = '#2ca02c',                # green
  'Low'            = '#ff7f0e',                # orange
  'Critically Low' = '#d62728'                 # red
)

# ══════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

# ── Create forest plot data structure for forestplot package ──────────────────
prepare_forestplot_data <- function(df, study_col = 'author', year_col = 'year',
                                    effect_col = 'effect', ci_low_col = 'ci_low',
                                    ci_high_col = 'ci_high', priority_col = 'priority') {
  # Prepare data frame for forestplot package
  # Combines author + year and marks priority studies with asterisk
  df <- df %>%
    mutate(
      Study = paste0(
        !!sym(study_col), ' ', !!sym(year_col),
        ifelse(!!sym(priority_col), '*', '')
      ),
      Effect = !!sym(effect_col),
      CI_Low = !!sym(ci_low_col),
      CI_High = !!sym(ci_high_col)
    ) %>%
    select(Study, Effect, CI_Low, CI_High)
  
  return(df)
}

# ── Draw forest plot using forestplot package ───────────────────────────────
draw_forest_plot <- function(df, 
                             title = '',
                             panel_tag = '',
                             x_label = 'Effect Estimate',
                             log_scale = FALSE,
                             ref_line = 0) {
  # Create forest plot using forestplot package (cleaner than ggplot2)
  
  # Extract numeric values
  effect <- as.numeric(df$Effect)
  ci_low <- as.numeric(df$CI_Low)
  ci_high <- as.numeric(df$CI_High)
  
  # Create and render forest plot directly to current graphics device
  forestplot(
    labeltext = as.matrix(df$Study),
    mean = effect,
    lower = ci_low,
    upper = ci_high,
    title = ifelse(panel_tag == '', title, paste(panel_tag, title, sep = '  ')),
    xlab = x_label,
    is.summary = FALSE,
    col = fpColors(
      box = COL_STANDARD,
      lines = COL_STANDARD,
      summary = COL_STANDARD
    ),
    xlog = log_scale,
    zero = ref_line,
    vertices = TRUE,
    cex = 0.9,
    txt_gp = fpTxtGp(
      ticks = gpar(cex = 0.8),
      xlab = gpar(cex = 0.9),
      title = gpar(cex = 0.95, fontface = 'bold')
    )
  )
}

cat('Setup complete. Using forestplot package for publication-quality forest plots.\n')

# ══════════════════════════════════════════════════════════════════════════════
# FIGURE 2: PATIENT-REPORTED OUTCOMES
# ══════════════════════════════════════════════════════════════════════════════

# ── Figure 2A: Postoperative Pain Early (≤24 h) ────────────────────────────────

fig2a_md <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, -0.50, -0.80, -0.20, 'Moderate', TRUE,
  'Rudiman et al.', 2024, -0.50, -0.86, -0.14, 'Moderate', FALSE,
  'Qiu et al.', 2013, -0.10, -0.40, 0.20, 'Moderate', FALSE,
  'Markar et al.', 2011, -0.21, -0.51, 0.09, 'Moderate', FALSE,
  'Pereira et al.', 2022, -0.38, -0.68, -0.08, 'Low', FALSE,
  'Tamini et al.', 2014, -0.27, -0.57, 0.03, 'Low', FALSE,
  'Lyu et al.', 2019, -0.10, -0.40, 0.20, 'Low', FALSE,
  'Hao et al.', 2012, -0.75, -1.05, -0.45, 'Low', FALSE,
  'Geng et al.', 2013, -0.58, -0.704, -0.457, 'Low', FALSE,
  'Evers et al.', 2016, -0.46, -0.76, -0.16, 'Low', FALSE,
  'Garg et al.', 2012, 0.025, 0.01, 0.04, 'Low', FALSE,
  'Trastulli et al.', 2013, -0.25, -0.55, 0.05, 'Low', FALSE,
  'Linkwinstar et al.', 2025, 0.18, -0.12, 0.48, 'Low', FALSE,
)

fig2a_plot_data <- prepare_forestplot_data(fig2a_md)

tiff('Figure2A_Pain_Early.tiff', width = FIG_W_IN, height = 7, 
     units = 'in', res = DPI, compression = 'none')
draw_forest_plot(fig2a_plot_data,
                 title = 'Postoperative Pain Early (≤24 h) — MD/WMD Reviews',
                 panel_tag = 'Figure 2A',
                 x_label = 'MD / WMD (SILC − MILC, pain score units)',
                 log_scale = FALSE,
                 ref_line = 0)
dev.off()
cat('Saved → Figure2A_Pain_Early.tiff\n')

# ── Figure 2B: Postoperative Pain Late (>24 h) ─────────────────────────────────

fig2b_md <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, -0.33, -0.55, 0.11, 'Moderate', TRUE,
  'Qiu et al.', 2013, -0.13, -0.43, 0.17, 'Moderate', FALSE,
)

fig2b_plot_data <- prepare_forestplot_data(fig2b_md)

tiff('Figure2B_Pain_Late.tiff', width = FIG_W_IN, height = 3,
     units = 'in', res = DPI, compression = 'none')
draw_forest_plot(fig2b_plot_data,
                 title = 'Postoperative Pain Late (>24 h) — MD Reviews',
                 panel_tag = 'Figure 2B',
                 x_label = 'MD (SILC − MILC, pain score units)',
                 log_scale = FALSE,
                 ref_line = 0)
dev.off()
cat('Saved → Figure2B_Pain_Late.tiff\n')

# ── Figure 3A: Operative Time (MD / WMD, minutes) ──────────────────────────────

fig3a <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, -5.69, -11.45, 0.07, 'Moderate', TRUE,
  'Qiu et al.', 2013, -8.50, -12.50, -4.50, 'Moderate', FALSE,
  'Rudiman et al.', 2024, -4.87, -7.09, -2.65, 'Moderate', FALSE,
  'Pereira et al.', 2022, -4.82, -9.27, -0.37, 'Low', FALSE,
  'Hao et al.', 2012, -6.30, -9.30, -3.30, 'Low', FALSE,
  'Wang Z et al.', 2012, -5.31, -8.31, -2.31, 'Low', FALSE,
  'Markar et al.', 2011, -4.30, -7.60, -1.00, 'Low', FALSE,
  'Geng et al.', 2013, -7.81, -12.23, -3.39, 'Low', FALSE,
  'Garg et al.', 2012, -9.88, -15.47, -4.29, 'Low', FALSE,
  'Trastulli et al.', 2013, -4.19, -6.98, -1.40, 'Low', FALSE,
)

fig3a_plot_data <- prepare_forestplot_data(fig3a)

tiff('Figure3A_Operative_Time.tiff', width = FIG_W_IN, height = 6,
     units = 'in', res = DPI, compression = 'none')
draw_forest_plot(fig3a_plot_data,
                 title = 'Operative Time (minutes)',
                 panel_tag = 'Figure 3A',
                 x_label = 'MD / WMD (SILC − MILC, min)',
                 log_scale = FALSE,
                 ref_line = 0)
dev.off()
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
)

fig3d_plot_data <- prepare_forestplot_data(fig3d)

tiff('Figure3D_Conversion.tiff', width = FIG_W_IN, height = 5,
     units = 'in', res = DPI, compression = 'none')
draw_forest_plot(fig3d_plot_data,
                 title = 'Conversion to Open Surgery',
                 panel_tag = 'Figure 3D',
                 x_label = 'OR / RR (SILC vs MILC) — log scale',
                 log_scale = TRUE,
                 ref_line = 1)
dev.off()
cat('Saved → Figure3D_Conversion.tiff\n')

# ── Figure 4A: Wound Infection (OR – LOG SCALE) ──────────────────────────────

fig4a <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, 1.18, 0.62, 2.26, 'Moderate', TRUE,
  'Qiu et al.', 2013, 1.13, 0.79, 1.62, 'Moderate', FALSE,
  'Zhong et al.', 2012, 1.35, 0.67, 2.71, 'Low', FALSE,
  'Song et al.', 2013, 0.91, 0.50, 1.66, 'Low', FALSE,
  'Geng et al.', 2013, 2.05, 1.33, 3.17, 'Low', FALSE,
  'Trastulli et al.', 2013, 1.30, 1.05, 1.61, 'Low', FALSE,
  'Garg et al.', 2012, 1.35, 0.70, 2.61, 'Low', FALSE,
)

fig4a_plot_data <- prepare_forestplot_data(fig4a)

tiff('Figure4A_Wound_Infection.tiff', width = FIG_W_IN, height = 5,
     units = 'in', res = DPI, compression = 'none')
draw_forest_plot(fig4a_plot_data,
                 title = 'Wound Infection',
                 panel_tag = 'Figure 4A',
                 x_label = 'OR (SILC vs MILC) — log scale',
                 log_scale = TRUE,
                 ref_line = 1)
dev.off()
cat('Saved → Figure4A_Wound_Infection.tiff\n')

# ── Figure 4B: Bile Duct Injury (OR – LOG SCALE) ──────────────────────────────

fig4b <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, 1.02, 0.37, 2.86, 'Moderate', TRUE,
  'Qiu et al.', 2013, 2.07, 0.90, 4.74, 'Moderate', FALSE,
  'Geng et al.', 2013, 0.586, 0.074, 4.639, 'Low', FALSE,
  'Trastulli et al.', 2013, 2.07, 0.90, 4.74, 'Low', FALSE,
)

fig4b_plot_data <- prepare_forestplot_data(fig4b)

tiff('Figure4B_Bile_Duct.tiff', width = FIG_W_IN, height = 4,
     units = 'in', res = DPI, compression = 'none')
draw_forest_plot(fig4b_plot_data,
                 title = 'Bile Duct Injury',
                 panel_tag = 'Figure 4B',
                 x_label = 'OR (SILC vs MILC) — log scale',
                 log_scale = TRUE,
                 ref_line = 1)
dev.off()
cat('Saved → Figure4B_Bile_Duct.tiff\n')

# ── Figure 4C: Bile Leakage (OR – LOG SCALE) ──────────────────────────────────

fig4c <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, 1.45, 0.49, 4.27, 'Moderate', TRUE,
  'Zhong et al.', 2012, 2.99, 1.08, 8.28, 'Low', FALSE,
  'Song et al.', 2013, 3.03, 0.62, 14.82, 'Low', FALSE,
)

fig4c_plot_data <- prepare_forestplot_data(fig4c)

tiff('Figure4C_Bile_Leakage.tiff', width = FIG_W_IN, height = 3.5,
     units = 'in', res = DPI, compression = 'none')
draw_forest_plot(fig4c_plot_data,
                 title = 'Bile Leakage',
                 panel_tag = 'Figure 4C',
                 x_label = 'OR (SILC vs MILC) — log scale',
                 log_scale = TRUE,
                 ref_line = 1)
dev.off()
cat('Saved → Figure4C_Bile_Leakage.tiff\n')

cat('\n✓ All forest plots created successfully using forestplot package!\n')
cat('✓ No ggplot2 warnings - cleaner code, publication-ready output.\n')
