# [provBookR](docs/architecture.md)

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-2ea44f?style=flat-square&logo=r&logoColor=white)](https://github.com/End-to-end-provenance/provBookR/actions/workflows/R-CMD-check.yaml)
[![Frontend & E2E Tests](https://img.shields.io/badge/Frontend_%26_E2E-passing-2ea44f?style=flat-square&logo=playwright&logoColor=white)](https://github.com/End-to-end-provenance/provBookR/actions/workflows/frontend.yaml)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-0969da?style=flat-square)](LICENSE)
[![Svelte 5](https://img.shields.io/badge/Svelte-v5.0-ff3e00?style=flat-square&logo=svelte&logoColor=white)](inst/codex_template)

> Generates interactive, web-native digital booklet codices of R computational provenance.

## Nature of Provenance
How scientific results come to be is the product of various domain-specific, economic, historical, social, and technical factors. Information about how these factors shaped and produced these results is essential to both artistic trade and scientific reproducibility. Whether these results are material or digital, **provenance** provides the historical accounts of objects: paintings, bones, essays, scientific tables, and plots.

## Advancing Scientific Reproducibility
For ecologists and the wider scientific community, the reproducibility of computational experiments is a critical challenge. Modern research relies heavily on complex data pipelines, and understanding exactly how a result was derived is essential for verifying, replicating, and building upon scientific claims. 

**provBookR** addresses this by bridging formal provenance research with practical software engineering workflows. It utilizes End-to-end-provenance tools (such as `rdtLite`) to automatically collect the execution history of R scripts and packages that provenance into an interactive, static web booklet built with **SvelteKit**.

This allows researchers to visually explore the lineage of their data pipelines and specific R objects (e.g., plots, models, variables) through a browser-based reading experience. By making provenance transparent and easily shareable without requiring specialized backend infrastructure, `provBookR` empowers the scientific community to elevate the standard of computational reproducibility.

![provBookR Main Interface](docs/provbook_example_chapter_4.png)

## Features

- **Interactive Lineage Graph**: Directed acyclic graph (DAG) visualization of data derivation, tracking operations (functions) and entities (variables, dataframes, files).
- **Cryptographic File Hash Integrity**: MD5 hash validation of generated file artifacts to guarantee authenticity and reproducibility across environments.
- **W3C PROV-JSON Standard Compliance**: Full compatibility with the W3C PROV-JSON standard and End-to-End Extended Provenance specification.
- **Sub-second Execution Timeline**: Granular operation duration and temporal ordering tracking for R workflows.
- **Standalone Static Export**: Zero-backend deployment — compiles to plain HTML/CSS/JS ready for GitHub Pages, Netlify, or local offline reading.

## Ecosystem

`provBookR` is part of the **[End-to-End Provenance](https://github.com/End-to-end-provenance)** (E2E) ecosystem. It seamlessly integrates with the suite's specialized tools:

- **`rdtLite`**: Traces script execution and captures fine-grained provenance graphs.
- **`provSummarizeR` / `provExplainR`**: Complements textual summaries and run-to-run diffing by providing an interactive visual reading experience.
- **`provDebugR`**: Complements terminal-based time-travel debugging with high-level visual lineage navigation.

---

# Installation
Install from GitHub:
```R
# install.packages("devtools")
devtools::install_github("End-to-end-provenance/provBookR")
```

Once installed, load the package:
```R
library(provBookR)
```

# Usage

## 1. Publishing an Interactive Web Codex
The primary method for generating a provenance booklet is through the `publish_codex` function. This automates the process of tracing the script, compiling the frontend interface, and exporting a static HTML directory.

```R
# Automatically trace myscript.R and build a static web booklet
provBookR::publish_codex("myscript.R", output_dir = "my_provbook")
```
This generates a `my_provbook/` folder containing the static HTML, CSS, and JS files. The `index.html` file can be opened directly in a browser or hosted via standard static site hosting services (e.g., GitHub Pages).

## 2. Local Booklet Preview (`preview_codex`)
To view the compiled booklet locally without running into browser Same-Origin (CORS) restrictions on local `file://` URLs, use `preview_codex`:

```R
# Launch a local server to preview your built codex
provBookR::preview_codex(output_dir = "my_provbook", port = 8000)
```

> **Note on Legacy Prototypes**: Early 2019 terminal-based interactive CLI prototypes (`provBookR(..., mode="full")`) are archived in [`inst/legacy/`](inst/legacy/README.md) for historical reference.

---

## Ecosystem & Citation

`provBookR` is developed within the **End-to-End Provenance Project**, a multi-institutional research initiative supported by Mount Holyoke College, Harvard University, the University of British Columbia, and the National Science Foundation (NSF).

If you use `provBookR` in your research, please cite the End-to-End Provenance project:

> Lerner, B., Boose, E., & Perez, F. *Using End-to-End Provenance to Support Reproducibility in R*. The End-to-End Provenance Project. https://github.com/End-to-end-provenance
