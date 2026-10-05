# scale_comp

![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![R](https://img.shields.io/badge/R-Shiny-276DC3)

A Shiny app that illustrates how **mapping latent values onto a few response categories** can change variances. It compares a visual analogue scale (VAS) from 0 to 100 with a seven-point and a five-point Likert-type scale. All formats measure the same latent value, their ends correspond, and responses are compared after linear rescaling to the interval from 0 to 1. The app illustrates a possible mechanism only.

**App:** https://donvollb.github.io/scale_comp/

---

## Tabs

| Tab | Description |
|---|---|
| 1 Measurement per Person | One response per person; means, variances, and shares at the ends in each format's own metric and after rescaling |
| 50 Measurements per Person | 50 responses per person; within-person variance, variance of the person means, and ICC; responses of three example persons |

In both tabs, the thresholds between the Likert-type categories can be equidistant or random.

---

## Running the App Locally

The app runs in the browser via [shinylive](https://posit-dev.github.io/r-shinylive/), so no installation is needed. To run it locally in R:

```r
install.packages(c("shiny", "shinythemes"))
shiny::runApp("app.R")
```

---

## Repository Structure

| File | Description |
|---|---|
| `app.R` | The Shiny app (single source of the code) |
| `index.qmd` | Web page with explanation; includes `app.R` in a shinylive chunk |
| `docs/` | Rendered web page for GitHub Pages |
| `_extensions/` | Quarto shinylive extension |

If you clone this repository and want to rebuild the web page after changing `app.R` or `index.qmd`, run `quarto render` in the repository folder. This requires [Quarto](https://quarto.org/) and the R package `shinylive`.

---

## References

Vollbracht, D., Ottenstein, C., Ecker, S., & Lischetzke, T. (2026). Slider versus Likert scales: Psychometric properties in ambulatory assessment. *Behavior Research Methods*, *58*(4), Article 97. https://doi.org/10.3758/s13428-026-02992-4

---

## License

The code and text in this repository are licensed under the MIT License, see [LICENSE](LICENSE). The folder `docs/index_files/` also contains third-party software bundled by shinylive (e.g., webR and R packages), which remains under its own licenses.
