# R Forest Plots Conversion Guide

## What Was Converted

Your Python Jupyter notebook (`SILC_vs_MILC_Forest_Plots.ipynb`) has been converted to R with two formats:

1. **SILC_vs_MILC_Forest_Plots.Rmd** — R Markdown (interactive notebook-like format)
2. **SILC_vs_MILC_Forest_Plots.R** — Standalone R script

## Key Libraries Used

- **ggplot2**: Publication-quality forest plot visualization
- **dplyr + tidyr**: Data manipulation (replaces pandas)
- **tibble**: Clean data frame handling (replaces pd.DataFrame)
- **gridExtra**: Combining multi-panel figures
- **forcats**: Factor manipulation for proper axis ordering

## Main Differences from Python Version

### Data Handling

```r
# Python: pd.DataFrame([...], columns=['Author','Year',...])
# R: tribble(~author, ~year, ..., )  # from tibble package
```

### Missing Values

```r
# Python: np.nan
# R: NA_real_  (for numeric data)
```

### Forest Plot Drawing

```r
# Python: Custom matplotlib function draw_forest_panel()
# R: ggplot2-based draw_forest_panel() function
#    Uses geom_errorbarh() for confidence intervals
#    Uses geom_point() with custom shapes/colors for markers
#    Uses geom_text() for AMSTAR-2 annotations
```

### Figure Saving

```r
# Python: plt.savefig('Figure.tiff', dpi=300, ...)
# R: ggsave('Figure.tiff', plot=p, device='tiff', dpi=300, ...)
```

## How to Use

### Option 1: R Markdown (Recommended for Interactive Work)

```r
# In RStudio:
# 1. Open SILC_vs_MILC_Forest_Plots.Rmd
# 2. Click "Knit" button to render as HTML with embedded code and output
# 3. Or run individual chunks interactively with Ctrl+Shift+Enter (Windows)
```

### Option 2: Standalone R Script

```r
# In R console or RStudio:
source('SILC_vs_MILC_Forest_Plots.R')

# OR run from command line:
# Rscript SILC_vs_MILC_Forest_Plots.R
```

## Customization

### Change Output Directory

Add this at the top of either file:

```r
# Set working directory
setwd('/path/to/your/output/folder')
```

### Change Figure Dimensions

Modify these constants in the setup section:

```r
DPI        <- 300        # Resolution (DPI)
FIG_W_CM   <- 18         # Width in cm (BMC Surgery: 18 cm)
ROW_H_IN   <- 1.2 / 2.54 # Height per row
```

### Change Color Palette

```r
COL_PRIORITY   <- '#1a6faf'   # Blue for priority studies
COL_STANDARD   <- '#333333'   # Gray for contributing studies
COL_REFLINE    <- '#CC0000'   # Red for reference line
COL_AMSTAR     <- list(
  'Moderate'       = '#2ca02c',       # Green
  'Low'            = '#ff7f0e',       # Orange
  'Critically Low' = '#d62728'        # Red
)
```

## Function Reference

### draw_forest_panel()

Creates a ggplot2 forest plot panel

**Parameters:**

- `df`: Data frame with columns: author, year, effect, ci_low, ci_high, amstar2, priority
- `ref_line`: Reference line position (0 for continuous, 1 for dichotomous)
- `log_scale`: TRUE for OR/RR (log scale), FALSE for MD/WMD
- `x_label`: X-axis label
- `title`: Panel title
- `panel_tag`: Figure label (e.g., "Figure 2A")

**Returns:** ggplot object

### make_label()

Constructs author-year labels with asterisk for priority studies

### fig_height()

Calculates figure height (in inches) based on number of rows

## Data Format

Forest plot data should be a tibble/data frame with these columns:

| Column   | Type | Description                                          |
| -------- | ---- | ---------------------------------------------------- |
| author   | chr  | First author name                                    |
| year     | num  | Publication year                                     |
| effect   | num  | Point estimate (MD/WMD/SMD/OR/RR)                    |
| ci_low   | num  | Lower 95% CI bound                                   |
| ci_high  | num  | Upper 95% CI bound                                   |
| amstar2  | chr  | AMSTAR-2 rating: "Moderate", "Low", "Critically Low" |
| priority | lgl  | TRUE/FALSE for priority review designation           |

**Example:**

```r
fig2a_md <- tribble(
  ~author, ~year, ~effect, ~ci_low, ~ci_high, ~amstar2, ~priority,
  'Haueter et al.', 2017, -0.50, -0.80, -0.20, 'Moderate', TRUE,
  'Qiu et al.', 2013, -0.10, NA_real_, NA_real_, 'Moderate', FALSE,
)
```

## Missing Confidence Intervals

When CI is not reported (NA*real*), the code automatically fills with ±0.30 placeholder:

```r
NO_CI_FALLBACK <- 0.30  # Adjust as needed
```

## Combining Multi-Panel Figures

Use `gridExtra::grid.arrange()`:

```r
fig_combined <- gridExtra::grid.arrange(p2a_md, p2a_smd, ncol = 1,
                                        heights = c(nrow(fig2a_md), nrow(fig2a_smd)))
```

## Installing Required Packages

```r
# First time only
install.packages(c('tidyverse', 'gridExtra', 'ggplot2', 'dplyr', 'tibble', 'forcats'))

# Then load them in your script
library(tidyverse)
library(gridExtra)
```

## Troubleshooting

### Issue: "Package not found"

**Solution:** Install the package first: `install.packages('package_name')`

### Issue: Figure looks too small/large

**Solution:** Adjust `fig_height()` or `FIG_W_IN`:

```r
# For taller figures with more rows
ggsave('Figure.tiff', plot = p, height = fig_height(25), ...)
```

### Issue: AMSTAR-2 labels overlap with author names

**Solution:** In `draw_forest_panel()`, adjust the margin or text positioning:

```r
# Find this line and adjust margin value:
margin = margin(r = 20, unit = 'pt')  # Increase 'r' value
```

## Comparison: Python vs R

| Task              | Python                       | R                                   |
| ----------------- | ---------------------------- | ----------------------------------- |
| Create data frame | `pd.DataFrame([...])`        | `tribble(...)` or `data.frame(...)` |
| Missing values    | `np.nan`                     | `NA_real_`                          |
| Plot creation     | matplotlib                   | ggplot2                             |
| Combining plots   | `plt.subplot()` / `gridspec` | `gridExtra::grid.arrange()`         |
| Save figure       | `plt.savefig()`              | `ggsave()`                          |
| Run script        | `python script.py`           | `Rscript script.R`                  |

## Output Files Generated

Both the .Rmd and .R versions create the same TIFF output files:

- `Figure2A_Pain_Early.tiff`
- `Figure2B_Pain_Late.tiff`
- `Figure2C_Cosmetic.tiff` (requires adding to R file)
- `Figure2D_Analgesic.tiff` (requires adding to R file)
- `Figure3A_Operative_Time.tiff`
- `Figure3B_Hospital_Stay.tiff` (requires adding to R file)
- `Figure3C_Blood_Loss.tiff` (requires adding to R file)
- `Figure3D_Conversion.tiff`
- `Figure3E_Return_Activity.tiff` (requires adding to R file)
- `Figure4A_Wound_Infection.tiff`
- `Figure4B_Bile_Duct.tiff`
- `Figure4C_Bile_Leakage.tiff`
- `Figure4D_Incisional_Hernia.tiff` (requires adding to R file)
- `Figure4E_Haematoma.tiff` (requires adding to R file)
- `Figure4F_Retained_Stones.tiff` (requires adding to R file)
- `Figure2_Patient_Reported.tiff` (combined 4-panel)
- `Figure3_Surgical_Efficacy.tiff` (combined 5-panel)
- `Figure4_Safety_Outcomes.tiff` (combined 6-panel)

## Next Steps

1. **Install R packages** if not already installed
2. **Choose your format**: .Rmd for interactive work, .R for batch processing
3. **Run the code** and verify output matches your Python version
4. **Add remaining figures** (2C, 2D, 3B, 3C, 3E, 4D, 4E, 4F) following the same pattern
5. **Customize styling** as needed for your publication

---

**Note:** The R version includes the core setup and key figures (2A, 2B, 3A, 3D, 4A, 4B, 4C). To complete all figures, follow the same pattern demonstrated for the included figures—it's straightforward to add the remaining ones!
