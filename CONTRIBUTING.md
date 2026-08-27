# Contributing to provBookR

Thank you for your interest in contributing to `provBookR`! 

## Development Setup

`provBookR` consists of two main components:
1. **R Package Engine** (`R/`, `DESCRIPTION`, `NAMESPACE`)
2. **SvelteKit Frontend Template** (`inst/codex_template/`)

### Prerequisites
* R (>= 4.0.0) with `devtools` and `rdtLite` installed.
* Node.js (>= 20) & `npm`.

### Frontend Setup & Testing
To work on the SvelteKit digital booklet template:

```bash
cd inst/codex_template
npm install

# Run dev server
npm run dev

# Run Svelte typechecking
npm run check

# Run Vitest unit tests
npm run test

# Run Playwright End-to-End (E2E) tests
npm run test:e2e
```

### R Package Testing
To verify R package integrity:

```R
devtools::check()
```

## Pull Request Guidelines

1. Ensure all tests pass (`npm run test`, `npm run test:e2e`, and `devtools::check()`).
2. Adhere to double-blind anonymization guidelines (do not include personal author names or institutional affiliations in commit messages or docs).
