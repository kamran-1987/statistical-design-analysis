# Statistical Analysis Portfolio Project
# Demonstrating Experimental Design & Multivariate Analysis in R
# Author: Ali Fadakar
# Repository: https://github.com/kamran-1987/statistical-design-analysis

# Load required libraries
library(readxl)
library(agricolae)
library(cluster)
library(factoextra)

cat("=== STATISTICAL ANALYSIS PORTFOLIO PROJECT ===\n\n")

# Example: Randomized Block Design Analysis
cat("1. EXPERIMENTAL DESIGN ANALYSIS\n")
cat("--------------------------------\n")

# Simulate some example data for demonstration
set.seed(123)
example_data <- data.frame(
  treatment = factor(rep(1:3, each = 20)),
  block = factor(rep(1:4, each = 15)),
  response = rnorm(60, mean = 50, sd = 10)
)

# ANOVA analysis
cat("Randomized Block Design ANOVA:\n")
block_anova <- aov(response ~ treatment + block, data = example_data)
print(summary(block_anova))

# Post-hoc test
cat("\nPost-hoc LSD Test:\n")
lsd_result <- LSD.test(block_anova, "treatment", console = TRUE)

# Example: Multivariate Analysis
cat("\n2. MULTIVARIATE ANALYSIS\n")
cat("-------------------------\n")

# Use built-in dataset for demonstration
multivariate_data <- iris[, 1:4]

# Factor Analysis
cat("Factor Analysis Results:\n")
fa_result <- factanal(multivariate_data, factors = 2, scores = "regression")
print(fa_result)

# Cluster Analysis
cat("\nK-means Clustering:\n")
set.seed(123)
kmeans_result <- kmeans(multivariate_data, centers = 3)
cat("Cluster sizes:\n")
print(table(kmeans_result$cluster))

cat("\n=== ANALYSIS COMPLETE ===\n")
