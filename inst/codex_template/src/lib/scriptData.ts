export interface ScriptMeta {
  id: string;
  name: string;
  category: 'Documents' | 'Everyday Math' | 'Data & Stats' | 'HPC & Systems';
  icon: string;
  goal: string;
  variables: string[];
  operations: string[];
  outputFile: string;
  code: string;
}

export const scriptMetadata: Record<string, ScriptMeta> = {
  "proposal_workflow.R": {
    id: "proposal_workflow.R",
    name: "proposal_workflow.R",
    category: "Documents",
    icon: "document",
    goal: "Drafts a research proposal, incorporates comments, and outputs a PDF.",
    variables: ["draft_v1", "draft_v2", "feedback", "final_text"],
    operations: ["paste() text merging", "c() vector creation", "pdf() device export"],
    outputFile: "proposal.pdf",
    code: `# Proposal Workflow\n# 1. Draft content\ndraft_v1 <- 'Intro: First draft of proposal'\ndraft_v2 <- paste(draft_v1, 'Methodology: Mixed-methods')\n\n# 2. Gather reviewer feedback\nfeedback <- c(rev1 = 'Add context', rev2 = 'Solid')\nfinal_text <- paste(draft_v2, paste(feedback, collapse='; '))\n\n# 3. Export PDF artifact\npdf('proposal.pdf')\nplot.new()\ntext(0.5, 0.5, final_text)\ndev.off()`
  },
  "coffee_tracker.R": {
    id: "coffee_tracker.R",
    name: "coffee_tracker.R",
    category: "Everyday Math",
    icon: "coffee",
    goal: "Tracks weekly daily espresso cups, calculates total cost, and exports a CSV summary.",
    variables: ["daily_cups", "price_per_cup", "weekly_spent"],
    operations: ["sum() aggregation", "multiplication *", "write.csv() export"],
    outputFile: "coffee_summary.csv",
    code: `# Weekly Coffee Tracker\n# 1. Record daily cup count\ndaily_cups <- c(Mon=2, Tue=3, Wed=2, Thu=4, Fri=1)\nprice_per_cup <- 4.50\n\n# 2. Calculate weekly total expense\nweekly_spent <- sum(daily_cups) * price_per_cup\n\n# 3. Export summary table\nwrite.csv(weekly_spent, 'coffee_summary.csv')`
  },
  "weather_analysis.R": {
    id: "weather_analysis.R",
    name: "weather_analysis.R",
    category: "Data & Stats",
    icon: "chart",
    goal: "Analyzes daily temperatures, identifies above-average days, and exports a chart.",
    variables: ["temps", "days", "avg_temp", "hot_days"],
    operations: ["mean() calculation", "subsetting >", "pdf() barplot export"],
    outputFile: "weather_chart.pdf",
    code: `# Weather Analysis\n# 1. Weekly temperatures\ntemps <- c(72, 68, 75, 80, 77, 65, 70)\ndays  <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')\n\n# 2. Find days above average\navg_temp <- mean(temps)\nhot_days <- days[temps > avg_temp]\n\n# 3. Export chart\npdf('weather_chart.pdf')\nbarplot(temps, names.arg = days, col = 'steelblue')\ndev.off()`
  },
  "recipe_scaling.R": {
    id: "recipe_scaling.R",
    name: "recipe_scaling.R",
    category: "Everyday Math",
    icon: "cookie",
    goal: "Takes a cookie recipe and scales it by 3x for a party.",
    variables: ["recipe", "scale_factor", "scaled"],
    operations: ["data.frame() creation", "vectorized multiplication *", "write.csv() export"],
    outputFile: "party_recipe.csv",
    code: `# Recipe Scaling\n# 1. Base recipe ingredients\nrecipe <- data.frame(ing = c('flour','sugar','eggs'), amt = c(2, 1, 3))\nscale_factor <- 3\n\n# 2. Scale amounts by 3x factor\nscaled <- recipe\nscaled$amt <- recipe$amt * scale_factor\n\n# 3. Export party recipe CSV\nwrite.csv(scaled, 'party_recipe.csv')`
  },
  "student_grades.R": {
    id: "student_grades.R",
    name: "student_grades.R",
    category: "Data & Stats",
    icon: "table",
    goal: "Calculates class averages and assigns letter grades.",
    variables: ["students", "scores", "grades", "class_avg"],
    operations: ["data.frame() creation", "ifelse() evaluation", "mean() calculation"],
    outputFile: "In-memory dataframe",
    code: `# Student Grading\n# 1. Student roster and exam scores\nstudents <- c('Alice','Bob','Carol','Dan')\nscores   <- c(92, 85, 78, 96)\n\n# 2. Assign letter grades & compute average\ngrades <- data.frame(name = students, score = scores)\ngrades$letter <- ifelse(grades$score >= 90, 'A', 'B')\nclass_avg <- mean(grades$score)`
  }
};
