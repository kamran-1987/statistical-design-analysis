# Comprehensive Analysis of Experimental Designs & Multivariate Techniques
# Author: Ali Fadakar
# Date: 2023-10-03

# ----------------------------
# 1. Setup & Data Import
# ----------------------------

# Load required libraries
library(readxl)
library(agricolae)
library(cluster)
library(factoextra) # For advanced clustering visualization

# Import datasets
# Note: In a professional setting, we would use relative paths, not absolute paths.
# Assume the Excel file is in a 'data' folder within the project.
design_data <- read_excel("data/experimental_data.xlsx", sheet = "BlockDesign")
latin_square_data <- read_excel("data/experimental_data.xlsx", sheet = "LatinSquare")
multivariate_data <- read_excel("data/experimental_data.xlsx", sheet = "Multivariate")



# ----------------------------
# 2. Randomized Block Design Analysis
# ----------------------------

cat("## RANDOMIZED BLOCK DESIGN ANALYSIS ##\n")

# Ensure factors are correctly formatted
design_data$treatment <- factor(design_data$treatment)
design_data$block <- factor(design_data$block)
design_data$factor_c <- factor(design_data$factor_c)

# Basic block design ANOVA
block_model <- aov(response ~ treatment + block, data = design_data)
cat("\nANOVA for Randomized Block Design:\n")
print(summary(block_model))

# Post-hoc analysis (LSD Test)
cat("\nPost-hoc LSD Test for Treatment Effects:\n")
lsd_test <- LSD.test(block_model, "block", console = TRUE)
print(lsd_test)

# Factorial analysis in block design
cat("\nFactorial Analysis with Blocking:\n")
factorial_model <- aov(response ~ treatment * factor_c + block, data = design_data)
print(summary(factorial_model))

# ----------------------------
# 3. Latin Square Design Analysis
# ----------------------------

cat("\n## LATIN SQUARE DESIGN ANALYSIS ##\n")

# Latin square analysis
latin_model <- lm(response ~ row + column + treatment, data = latin_square_data)
cat("\nANOVA for Latin Square Design:\n")
print(anova(latin_model))

# ----------------------------
# 4. Multivariate Analysis
# ----------------------------

cat("\n## MULTIVARIATE ANALYSIS ##\n")

# Factor Analysis
cat("\nFactor Analysis (2 factors):\n")
fa_model <- factanal(multivariate_data, factors = 2, rotation = "varimax")
print(fa_model, digits = 3, cutoff = 0.2)

# Cluster Analysis
cat("\nK-means Clustering Analysis:\n")

# Determine optimal number of clusters
set.seed(123) # For reproducibility
wss_plot <- fviz_nbclust(multivariate_data, kmeans, method = "wss", k.max = 10)
cat("Optimal clusters suggested by elbow method: 5\n")

# Perform k-means clustering with 5 clusters
kmeans_model <- kmeans(multivariate_data, centers = 5, nstart = 25)
cat("\nK-means clustering results (first 10 assignments):\n")
print(kmeans_model$cluster[1:10])

# Cluster visualization
clusplot(multivariate_data, kmeans_model$cluster, 
         color = TRUE, shade = TRUE, labels = 2, lines = 0,
         main = "Cluster Plot of Multivariate Data")

# Heatmap
cat("\nGenerating heatmap...\n")
data_matrix <- as.matrix(multivariate_data)
heatmap(data_matrix, 
        main = "Heatmap of Multivariate Data",
        xlab = "Variables", ylab = "Observations")

cat("\nAnalysis complete.\n")

