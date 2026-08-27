# provBookR Showcase Notebooks

This directory contains showcase notebooks demonstrating practical, real-world workflows using `provBookR` to collect, visualize, and publish computational provenance.

## Showcase Catalog

| Notebook & Script | Domain & Workflow | Key Provenance Features Showcase |
| :--- | :--- | :--- |
| **[`01_everyday_data_pipeline.ipynb`](01_everyday_data_pipeline.ipynb)** / [`01_everyday_data_pipeline.R`](01_everyday_data_pipeline.R) | Everyday Data Science & Visualization | Ingestion, filtering, summary stats, ggplot rendering, and variable derivation tracking. |
| **[`02_hpc_development_story.ipynb`](02_hpc_development_story.ipynb)** / [`02_hpc_development_story.R`](02_hpc_development_story.R) | HPC & High-Performance Computing | Iterative benchmarking, memory allocation, sub-second step execution timing, and performance stories. |
| **[`03_reproducible_proposal_workflow.ipynb`](03_reproducible_proposal_workflow.ipynb)** / [`03_reproducible_proposal_workflow.R`](03_reproducible_proposal_workflow.R) | Collaborative Research & Publishing | Multi-reviewer feedback integration, document drafting, MD5 hash verification, and PDF artifact compilation. |
| **[`04_botany_leaf_identification.ipynb`](04_botany_leaf_identification.ipynb)** / [`04_botany_leaf_identification.R`](04_botany_leaf_identification.R) | Botany & Plant Specimen Identification | Specimen morphometrics (leaf length/width, aspect ratio, vein density), feature engineering, and species scatter plots. |
| **[`05_forest_ecology_monitoring.ipynb`](05_forest_ecology_monitoring.ipynb)** / [`05_forest_ecology_monitoring.R`](05_forest_ecology_monitoring.R) | Forest Ecology & Ecosystem Dynamics | Longitudinal plot surveys (DBH, canopy cover %), allometric aboveground tree biomass (AGB), and Shannon diversity indices ($H'$). |

---

## How to Run & Publish Any Notebook

To generate an interactive `provBook` from any of these notebooks, execute the following commands in R:

```R
library(provBookR)

# 01. Publish Everyday Data Science notebook:
publish_codex("notebooks/01_everyday_data_pipeline.R", output_dir = "books/everyday_data_pipeline")

# 02. Publish HPC Performance Story notebook:
publish_codex("notebooks/02_hpc_development_story.R", output_dir = "books/hpc_development_story")

# 03. Publish Collaborative Proposal Workflow notebook:
publish_codex("notebooks/03_reproducible_proposal_workflow.R", output_dir = "books/proposal_workflow")

# 04. Publish Botany & Leaf Identification notebook:
publish_codex("notebooks/04_botany_leaf_identification.R", output_dir = "books/botany_leaf_identification")

# 05. Publish Forest Ecology & Ecosystem Monitoring notebook:
publish_codex("notebooks/05_forest_ecology_monitoring.R", output_dir = "books/forest_ecology_monitoring")
```

Each command generates a static, standalone web booklet in `books/<notebook_name>/index.html` that can be viewed locally or deployed to GitHub Pages!
