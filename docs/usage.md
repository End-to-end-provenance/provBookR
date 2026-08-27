# provBookR Usage Guide

`provBookR` provides a modern R API for collecting computational provenance from R scripts and generating interactive, web-native digital booklets ("codices").

---

## 1. Publishing a Codex (`publish_codex`)

The primary function of `provBookR` is `publish_codex()`. It performs the following end-to-end pipeline:
1. Executes your R script and captures fine-grained W3C-compliant PROV trace data using `rdtLite`.
2. Scaffolds the SvelteKit digital booklet template.
3. Injects the provenance graph and source code payload.
4. Compiles the static web bundle using Node.js/Vite into an output directory.

### Basic Usage

```R
library(provBookR)

# Publish a codex for an R analysis script
publish_codex(
  script = "path/to/analysis.R",
  output_dir = "my_codex_build"
)
```

### Parameters
* `script`: The file path to the target `.R` script.
* `output_dir`: The directory where the compiled HTML/JS/CSS digital booklet will be saved (default: `"codex_build"`).

---

## 2. Previewing a Codex (`preview_codex`)

Because modern web browsers enforce strict Same-Origin Policy (CORS) restrictions on local `file://` URLs, `provBookR` includes a built-in preview server.

```R
# Launch a local server to preview the built codex in your browser
preview_codex(
  output_dir = "my_codex_build",
  port = 8000
)
```

### Parameters
* `output_dir`: The directory containing the published digital booklet (default: `"codex_build"`).
* `port`: The local HTTP server port number (default: `8000`).

---

## 3. Interactive Codex Features

Once generated, the digital booklet provides an interactive reactive interface featuring:
* **Page Flip Book Mode**: Book-like chapter navigation separating Overview, Flow, Source Code, Lineage Graph, Data Entities, and Execution Timelines.
* **Overview Sorter (Graph View)**: Macroscopic spread overview allowing one-click jumping to any chapter.
* **Lineage DAG Explorer**: Interactive node graph rendering operations, data objects, and execution dependencies.
* **Custom Typography & Appearance**: Configurable reading modes (Light/Detailed), serif/sans fonts, and font size stepping.
