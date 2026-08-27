# Deploying & Hosting ProvBook Codices

`provBookR` compiles computational provenance booklets into static Single Page Applications (SPAs). Because the compiled codex relies zero backend servers or databases, hosting is straightforward.

---

## 1. Hosting Options

### Option A: GitHub Pages (Recommended)

You can easily publish compiled codices directly to GitHub Pages.

#### Method 1: Automatic Publishing via GitHub Actions
Create `.github/workflows/deploy-codex.yaml` in your repository:

```yaml
name: Deploy Codex to GitHub Pages

on:
  push:
    branches: [main]

permissions:
  contents: read
  pages: write
  id-token: write

jobs:
  deploy:
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - name: Install provBookR & dependencies
        run: |
          Rscript -e 'install.packages("devtools")'
          Rscript -e 'devtools::install_github("End-to-end-provenance/provBookR")'
      - name: Generate Codex
        run: |
          Rscript -e 'provBookR::publish_codex("script.R", output_dir = "public")'
      - uses: actions/upload-pages-artifact@v3
        with:
          path: 'public'
      - id: deployment
        uses: actions/deploy-pages@v4
```

---

### Option B: Local Preview Server (`preview_codex`)

To view a generated booklet locally without hitting browser Same-Origin Policy (CORS) security restrictions on `file://` paths:

```R
library(provBookR)

# Launches local web server at http://localhost:8000
preview_codex("codex_build", port = 8000)
```

---

### Option C: Embedding in Quarto / R Markdown / Lab Websites

You can embed a published codex directly into an R Markdown HTML report or lab website using an HTML `<iframe>`:

```html
<iframe 
  src="path/to/codex_build/index.html" 
  width="100%" 
  height="700px" 
  style="border: none; border-radius: 8px;"
></iframe>
```
