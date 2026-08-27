# ==============================================================================
# Notebook 03: Collaborative Research Proposal & Artifact Publication Workflow
# Showcase: Multi-reviewer feedback, document compilation, and artifact MD5 hashing
# ==============================================================================

# 1. Proposal Draft Preparation
proposal_title <- "Interactive Computational Provenance in High-Performance Computing"
draft_intro <- "Computational experiments require fine-grained provenance to ensure reproducibility across multi-institutional teams."
draft_method <- "We employ W3C PROV-JSON tracing and web-native SvelteKit codex rendering."

# 2. Peer Reviewer Input Integration
reviewers <- c(r1 = "Dr. Alice Vance (MIT)", r2 = "Dr. Bob Miller (Harvard)")
reviews <- c(
  r1 = "Approved with minor edits on metadata tracing.",
  r2 = "Methodology is rigorous and scalable."
)

# 3. Final Proposal Text Synthesis
proposal_document <- paste(
  paste0("# ", proposal_title),
  "## Introduction",
  draft_intro,
  "## Methodology",
  draft_method,
  "## Peer Review Sign-off",
  paste("-", reviewers, ":", reviews, collapse = "\n"),
  sep = "\n\n"
)

# 4. Export Artifact & Compute MD5 Hash Proof
writeLines(proposal_document, "reproducible_proposal_final.md")
proposal_hash <- tools::md5sum("reproducible_proposal_final.md")

cat("Proposal compiled successfully. MD5 Hash Proof:", proposal_hash, "\n")
