# TORV Agronomic Reports — Claude Code Reference

This file is the technical and stylistic reference for generating TORV agronomic
reports. Read `ai-report-prompt.md` for writing rules and prose templates.
This file covers pipeline architecture, LaTeX formatting, known issues, and
client-specific customization patterns.

---

## 1. Pipeline Architecture

### How a report is generated

1. **`eric.R`** — call `generate_report()` with client-specific parameters
2. **`aux-scripts/generate-report.R`** — defines `generate_report()`; builds
   `input_params` list; renders `report/report.Rmd` to PDF via `lualatex`
3. **`report/report.Rmd`** — main Rmd; sources `templates/setup.Rmd`; iterates
   over soil types, OM types, water sources; calls `knitr::knit_expand()` on
   each template; embeds the `.txt` prose file for the executive summary
4. **`report/templates/`** — one Rmd per section type (see below)
5. **Output** — `generated-reports/[folder]/[report].tex` + `.pdf`

### Key template files

| File | Purpose |
|:-----|:--------|
| `setup.Rmd` | Colors, fonts, themes, database filtering, `input_params` validation |
| `soil_testing_table.Rmd` | Nutrient analysis table |
| `soil_testing_row.Rmd` | Individual nutrient row |
| `om_testing.Rmd` | Organic matter charts + historical table |
| `sand_fraction_testing.Rmd` | Particle size charts + table |
| `water_testing.Rmd` | Water analytics pages (one expansion per source) |
| `trendline_figure.Rmd` | Trendline charts (expanded by each section template) |

### Source of truth for prose

The executive summary prose lives in a `.txt` file in the report output folder:
`generated-reports/[report-folder]/[report-name].txt`

This file is embedded into the `.tex` via `cat(readLines(txt_path))` in
`report.Rmd`. **All prose edits must go in the `.txt` file** — the `.tex` is
regenerated each pipeline run and manual edits to it will be overwritten.
Template/style changes must go in the relevant `.Rmd` template.

### Compile without re-running the pipeline

To recompile the PDF after editing the `.tex` directly (e.g. after prose edits
to the `.txt` that have already been baked in):

```bash
cd "/Users/ericfoerster/Desktop/ torv-reports v4/report"
lualatex -interaction=nonstopmode \
  -output-directory="../generated-reports/[report-folder]" \
  "../generated-reports/[report-folder]/[report-name].tex"
```

Run from `report/` so relative paths to figures and assets resolve correctly.

---

## 2. `generate_report()` Parameters

```r
generate_report(
  .site_name              = "The Briarwood",        # must match MASTER_DATABASE site field exactly
  .site_name_abbr         = "84398-briarwood",      # used in output filenames (no spaces)
  .date_sample_submitted  = "2026-05-07",           # YYYY-MM-DD; most recent sample date
  .start_date             = "2017-01-01",           # earliest date to include in trendlines
  .om_seasons             = "season",               # "season" = spring+fall only; "all" = all dates
  .om_stats               = "average",              # "average" or "median" for OM period summaries
  .warm_or_cool           = "cool",                 # "cool" or "warm" — selects grass-type MLSN refs
  .acid_extract           = "Mehlich",              # "Mehlich" or "Olsen" — soil extraction method
  .include_results_interpretation = "No",           # "Yes" adds a lab interpretation page
  .include_sand_fraction  = "No",                   # "Yes" includes particle size / sand fraction section
  .beeswarm               = TRUE,                   # TRUE = beeswarm dots; FALSE = centered dot strips
  .suppress_water_sources = c("Spring"),            # character vector — client-specific only
  .output                 = c("pdf")               # "pdf", "html", or c("pdf", "html")
)
```

**Parameter notes:**

- **`.site_name`** — must match the `site` field in MASTER_DATABASE exactly (case-sensitive).
  Check with `unique(full_database$site)` if uncertain.
- **`.om_seasons`** — `"season"` limits OM trendlines to spring (Apr–May) and fall (Sep–Oct)
  samples, which is the standard agronomic comparison. Use `"all"` only if interim samples exist.
- **`.warm_or_cool`** — selects the correct MLSN minimum thresholds for the grass type.
  Cool-season: creeping bentgrass, Poa, Kentucky bluegrass. Warm-season: bermudagrass, zoysia.
- **`.acid_extract`** — Mehlich-3 is the standard for most US labs. Olsen is used for
  high-pH/calcareous soils. Confirm with the lab's test code before setting.
- **`.beeswarm`** — when TRUE, individual sample dots are spread horizontally (beeswarm) to
  avoid overplotting. FALSE centers them on the x-axis. Use beeswarm when there are many
  samples per period; centered strips are cleaner for sparse data.
- **`.suppress_water_sources`** — filters named water sources from the pipeline before template
  expansion. Use for sources with one data point (trendline meaningless) or sources not part
  of irrigation. Filter is case-insensitive. Default `NULL` = no suppression.
- **`.output`** — `"pdf"` renders via lualatex. `"html"` renders as an interactive HTML report.
  Charts use `girafe()` interactive tooltips in HTML mode; static `ggplot2` in PDF mode.

Always `source(here::here("aux-scripts/generate-report.R"))` before calling
`generate_report()` to ensure the in-memory function matches the file.

---

## 3. LaTeX / PDF Formatting Rules

### Section headers

All section headers in the executive summary narrative use this format:

```latex
\noindent{\large\textbf{\textcolor{torvgray}{Section Name}}}
```

`torvgray` is aliased to `customgray` (`#5A5A5A`). It is defined in
`report.Rmd` header-includes:
```yaml
- \colorlet{torvgray}{customgray}
```

This alias must be present in the preamble. If you see
`Undefined color 'torvgray'` errors, check that the `\colorlet` line is in
the header-includes of `report.Rmd`.

In analytics templates, section headers use the same format. Current status:
- `om_testing.Rmd` line 275: `\textcolor{torvgray}{Historical Results by Depth}` ✓
- `sand_fraction_testing.Rmd` line 368: `\textcolor{torvgray}{Particle Size Trends}` ✓

### Available colors

| Name | Hex | Use |
|:-----|:----|:----|
| `successgreen` | `#698960` | No action / pass |
| `warningorange` | `#D08C47` | Monitor / watch |
| `deficitred` | `#A65D57` | Action required / fail |
| `customgray` / `torvgray` | `#5A5A5A` | Section headers, body text |
| `torv_orange` (R) | `#c88544` | Soil trendline period average |
| `torv_blue` (R) | `#147891` | Water trendline / site historic average |
| `torv_purple` (R) | `#756b96` | Linear trend line |
| `torv_gray` (R) | `#595959` | Individual sample dots |
| `torv_gray_light` (R) | `#b0b0b0` | TORV average line (OM charts) |

### Callout box environments

Two environments available (PDF only, requires `\tcbuselibrary{breakable}`):

```latex
\begin{insight}
\textbf{Title}\\[4pt]
Content — use for protocols, action thresholds, specs to adopt.
\end{insight}

\begin{context}
\textbf{Title}\\[4pt]
Content — use for interpretive caveats, background context, clarifications.
\end{context}
```

`insight` = moss green left rule (`#6D7F56`). `context` = grey left rule.
Maximum 2 callout boxes per report total. If a box splits across a page
boundary, condense to two sentences.

### Brace escaping in `cat()` output

Pandoc escapes a leading `{` in `cat(..., results='asis')` output to `\{`.
**Never use `{...}` grouping in `cat()` output.** Use `\begingroup...\endgroup`
instead:

```r
# Wrong — pandoc escapes the leading {
cat("{\small text}")

# Correct
cat("\\begingroup\\small text\\endgroup")
```

This affects the liability waiver block in `report.Rmd` and any `cat()` calls
that output LaTeX grouping.

### Raw LaTeX blocks in `.txt` prose

Use `{=latex}` fenced blocks for raw LaTeX in the prose `.txt` file:

````
```{=latex}
\newpage
```
````

This is the correct way to insert page breaks, callout boxes, and table
environments into the `.txt` prose. Pandoc passes these through unmodified.

---

## 4. Executive Summary

### Status table format

```latex
\begin{tabular}{p{2.8cm}p{2.4cm}p{9.2cm}}
\toprule
\textbf{Section} & \textbf{Status} & \textbf{Key finding} \\
\midrule
Nutrient Analysis & \textcolor{successgreen}{No action} & [one-line finding] \\
Organic Matter    & \textcolor{warningorange}{Monitor}   & [one-line finding] \\
Water             & \textcolor{successgreen}{No action} & [one-line finding] \\
\bottomrule
\end{tabular}
```

Status values: `\textcolor{successgreen}{No action}` /
`\textcolor{warningorange}{Monitor}` /
`\textcolor{deficitred}{Action required}`

### Executive summary spacing

```latex
\vspace{2mm}    % between header tile and status table
\medskip        % after \end{table}
\vspace{2mm}    % before first section header (Nutrient Analysis etc.)
```

### Page breaks in the executive summary

Insert a `\newpage` in a raw LaTeX block in the `.txt` file where needed.
Example: page break before the FAO water quality table (currently in the
Briarwood `.txt` before `**Quality assessment**`).

---

## 5. Water Section — Executive Summary

### Section structure (in `.txt` file)

```
\noindent{\large\textbf{\textcolor{torvgray}{Water}}}

[Intro sentence about sources tested.]

```{=latex}
\begin{context}
\textbf{[Source name] --- summary}\\[4pt]
[One sentence verdict per source.]
\end{context}
```

**Source Comparison --- Current Results**
[Markdown comparison table]

```{=latex}
\newpage
```

**Quality assessment**
[FAO table — raw LaTeX block]

**Year-over-year changes**
[Year-over-year prose]
```

### FAO water quality table format

Column widths: `p{5.5cm}rp{2cm}p{3.5cm}`
Threshold column: two boundary values only — `0.7 / 3.0` not `<0.7 / 0.7–3.0 / >3.0`

```latex
\begin{tabular}{p{5.5cm}rp{2cm}p{3.5cm}}
\toprule
\textbf{Parameter} & \textbf{Value} & \textbf{Thresholds} & \textbf{Rating} \\
\midrule
EC\textsubscript{w} (mmhos/cm) & [X] & 0.7 / 3.0   & \textcolor{...}{...} \\
TDS (ppm)                      & [X] & 450 / 2,000 & \textcolor{...}{...} \\
pH                             & [X] & 6.5--8.4    & \textcolor{...}{...} \\
SAR                            & [X] & 3 / 9       & \textcolor{...}{...} \\
RSC (meq/L)                    & [X] & 1.25 / 2.50 & \textcolor{...}{...} \\
Sodium (ppm)                   & [X] & 70 / 400    & \textcolor{...}{...} \\
Chloride (ppm)                 & [X] & 105 / 355   & \textcolor{...}{...} \\
Boron (ppm)                    & [X] & 0.5 / 3.0   & \textcolor{...}{...} \\
Bicarbonate (ppm)              & [X] & 92 / 520    & \textcolor{...}{...} \\
\bottomrule
\end{tabular}
```

Rating values: `\textcolor{successgreen}{None}` /
`\textcolor{warningorange}{Slight--Moderate}` /
`\textcolor{deficitred}{Severe}`

### FAO table footnote format

```latex
\par\vspace{3pt}
\begingroup\footnotesize\color{customgray}\itshape
Degree of restriction on use. Source: FAO Irrigation and Drainage Paper No.~29,
Ayers and Westcott (1985). Thresholds show the
\textcolor{successgreen}{None}/\textcolor{deficitred}{Severe} breakpoints;
values between are \textcolor{warningorange}{Slight--Moderate}.
RSC~<~0 indicates calcium and magnesium dominance at the exchange sites ---
beneficial for permeability.
\endgroup
```

The FAO table is generated dynamically by `water_testing.Rmd` for the
analytics pages (currently suppressed — `eval=FALSE, include=FALSE`). The
executive summary version lives in the `.txt` prose file and is hand-authored
per client. Update the `.txt` values when new water results arrive.

---

## 6. Trendline Chart Legends

### PDF inline legend format

All trendline charts output an inline legend in PDF mode instead of a
"How to read this chart" link. The legend is generated at the end of each
chart chunk with `results='asis', include=!html`.

**`trendline_figure.Rmd`** (used by Soil and Water sections):
```r
point_color_hex <- if(exists("sample_type_for_color") && sample_type_for_color == "Soil") "c88544" else "147891"
cat(paste0(
  '\\noindent\\begingroup\\small\\color{customgray}\\itshape ',
  'Dots --- individual samples; ',
  '\\textcolor[HTML]{', point_color_hex, '}{circle} --- period average; ',
  '\\textcolor[HTML]{756b96}{trend line} --- linear trend; ',
  '\\textcolor[HTML]{147891}{dashed line} --- site historic average',
  '\\endgroup'
))
```

**`om_testing.Rmd`** (OM section has an additional TORV average line):
```r
om_point_color_hex <- if(exists("sample_type_for_color") && sample_type_for_color == "Soil") "c88544" else "147891"
cat(paste0(
  '\\noindent\\begingroup\\small\\color{customgray}\\itshape ',
  'Dots --- individual samples; ',
  '\\textcolor[HTML]{', om_point_color_hex, '}{circle} --- period average; ',
  '\\textcolor[HTML]{756b96}{trend line} --- linear trend; ',
  '\\textcolor[HTML]{147891}{dashed line} --- site historic average; ',
  '\\textcolor[HTML]{b0b0b0}{dashed line} --- TORV average',
  '\\endgroup'
))
```

### Color logic — point color

The period-average dot color matches `torv_color` in the chart:
- `sample_type == "Soil"` → `torv_orange` (`#c88544`)
- All other types (Water, Physical/OM246) → `torv_blue` (`#147891`)

The legend condition mirrors this: `== "Soil"` → orange, else → blue.
**Do not use `== "Water"` as the condition** — Physical (OM246) would
incorrectly get orange.

### OM chart reference lines

The OM trendline has two horizontal dashed lines (both `linetype = "31"`):
- **Blue** (`torv_blue` / `#147891`): site historic average
- **Light grey** (`torv_gray_light` / `#b0b0b0`): TORV-wide average

Both are always present in the OM chart. Both must be in the legend.

---

## 7. Suppressed Pages

Some analytics pages may be suppressed per client or permanently. The pattern
for permanent suppression (all reports) is `eval=FALSE` on the render chunk.
For client-specific suppression, use `.suppress_water_sources` in `eric.R`.

### Currently suppressed (all reports)

| What | Where | How |
|:-----|:------|:----|
| Water bar chart | `water_testing.Rmd` | Render chunk: `eval=FALSE`. Header tile moved to suppressed chunk with `include=FALSE`. Raw `\newpage` and `\vspace` removed. |
| FAO analytics page | `water_testing.Rmd` | Chunk: `eval=FALSE, include=FALSE`. Table lives in executive summary `.txt` instead. |
| How to Read page | `report.Rmd` | Three chunks set to `include=FALSE`. Raw LaTeX wrapped in HTML comments. |

To restore any of these: find the suppression comment in the template and
re-enable as documented in the comment.

### Client-specific suppression (Briarwood)

Spring water source suppressed because only one data point exists — a trendline
is not meaningful and the spring is not part of the irrigation system.

In `eric.R`:
```r
.suppress_water_sources = c("Spring")
```

This filters `water_sample_descriptions` in `report.Rmd` before the template
loop, so no Spring pages are generated at all.

---

## 8. New Client Setup

### Adding a new site to `eric.R`

1. Confirm the exact `site` name in MASTER_DATABASE: `unique(full_database$site)`
2. Confirm the client number for the `site_name_abbr` (e.g., `"84398-briarwood"`)
3. Determine which soil extraction method the lab uses (Mehlich or Olsen)
4. Confirm grass type (warm or cool season)
5. Check whether a sand fraction test has been run (`.include_sand_fraction`)
6. Check water sources: `unique(full_database$water_sample_description[full_database$site == "..."])`
7. If any water source has only one data point, add it to `.suppress_water_sources`
8. Set `.start_date` to the date of the earliest sample in the database for that site

### Checklist for first report at a new site

- [ ] Site name verified against MASTER_DATABASE (exact case match)
- [ ] MLSN grass type confirmed
- [ ] Extraction method confirmed with lab
- [ ] Water sources reviewed — single-sample sources added to `.suppress_water_sources`
- [ ] `.start_date` set to earliest sample date
- [ ] Test pipeline run with `.output = c("pdf")` before client delivery
- [ ] Executive summary `.txt` file authored (pipeline creates a blank one; prose is hand-written)

---

## 9. Inline Emoji / Icon Pattern

For a positive closing statement, use an inline smiley face image:

```latex
`\raisebox{-2pt}{\includegraphics[width=0.022\textwidth]{\detokenize{/Users/ericfoerster/Desktop/ torv-reports v4/report/assets/1-smiley-face.png}}}`{=latex}
```

This goes at the end of the relevant sentence in the `.txt` prose file, inline with the text.
The `\detokenize{}` wrapper is required when the path contains spaces.

Available icons are in `report/assets/`. Use sparingly — one per report maximum.

---

## 10. Report Footer Format

```latex
\fancyfoot[L]{\footnotesize\textcolor{gray}{TORV, LLC • [Month Year] • [Venue] • [Report ID]}}
\fancyfoot[R]{\footnotesize\textcolor{gray}{\thepage}}
```

Both sides use `\footnotesize`. Footer is set in `report.Rmd` and auto-populated from
`input_params`. Do not override it in the `.tex` directly — it will be regenerated.

---

## 11. Common Issues

### Duplicate chunk labels
Templates expanded multiple times (one per water source, soil type, etc.) will
error if chunk labels are named. All chunks in `water_testing.Rmd` and other
iterated templates must be **anonymous** (`{r}` not `{r chunk-name}`).

### `torvgray` undefined
`torvgray` is not in the base template — it is aliased in `report.Rmd`
header-includes:
```yaml
- \colorlet{torvgray}{customgray}
```
If you see `Package xcolor Error: Undefined color 'torvgray'`, confirm this
line is present.

### Negative values in water tables
Format negative values with LaTeX en-dash, not a hyphen:
```r
val_fmt = str_replace(val_fmt, "^-", "--")
```
This is already in the `water_testing.Rmd` FAO chunk.

### Footer sizing
Both footer lines use `\footnotesize`:
```latex
\fancyfoot[L]{\footnotesize\textcolor{gray}{TORV, LLC • ...}}
\fancyfoot[R]{\footnotesize\textcolor{gray}{\thepage}}
```

---

## 12. Style Consistency Checklist

Before delivering any report, verify:

- [ ] All section headers use `\textcolor{torvgray}{...}` wrapper
- [ ] Status table uses correct color macros (successgreen / warningorange / deficitred)
- [ ] No "How to read this chart" links in PDF output — inline legend on every chart
- [ ] OM legend includes both the blue (site) and grey (TORV) dashed line entries
- [ ] Water section header reads "Water" not "Water Quality"
- [ ] FAO table threshold column uses `X / Y` format (two breakpoints only)
- [ ] FAO footnote uses colored keywords (None / Slight–Moderate / Severe)
- [ ] Page break before "Quality assessment" in water executive summary
- [ ] Footer is `\footnotesize` on both left and right
- [ ] No orphan analytics pages (bar charts without content, title-only header tiles)
- [ ] Liability waiver uses `\begingroup...\endgroup` not `{...}` grouping
- [ ] Report compiles clean with no errors in `.log` file

---

## 13. Writing Style Reference

See `ai-report-prompt.md` for the full TUFTE × TORV writing rules, section
templates, and data requirements. Key principles:

- Conclusions before evidence — open every section with the finding
- Every section ends with a verdict, not a summary
- Numbers where the claims are — no vague recommendations
- No hedging unless uncertainty is genuinely the finding
- Callout boxes: maximum 2 per report (`insight` for protocols, `context` for caveats)
- FAO water thresholds: the `context` box explaining what actually warrants
  amendment should appear in every water section where sodium/SAR is discussed
