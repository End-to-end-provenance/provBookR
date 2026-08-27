# proposal_workflow.R
# A simple, everyday example of drafting a proposal and generating a final PDF

# 1. Simulate drafting the proposal content
draft_v1 <- "Introduction: This is the first draft of the Scientific Proposal."
draft_v2 <- paste(draft_v1, "Methodology: We will use a mixed-methods computational approach.")

# 2. Assign reviewers and gather feedback
reviewer_1 <- "Dr. Smith"
reviewer_2 <- "Dr. Jones"
feedback <- c(reviewer_1 = "Needs more background context", reviewer_2 = "Methodology looks solid")

# 3. Incorporate feedback into the final text
final_text <- paste(
  draft_v2, 
  "\n\nReviewer Notes:", 
  paste(feedback, collapse = " | ")
)

# 4. Output the final proposal as a PDF
pdf("scientific_proposal_demo.pdf", width = 8, height = 11)
plot.new()
text(x = 0.5, y = 0.8, labels = "SCIENTIFIC PROPOSAL DEMO", cex = 2, font = 2)
text(x = 0.5, y = 0.5, labels = final_text, cex = 1)
text(x = 0.5, y = 0.2, labels = "Status: FINAL", col = "red")
dev.off()

print("Proposal scientific_proposal_demo.pdf has been generated successfully!")

