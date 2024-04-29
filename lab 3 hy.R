# Function to simulate student features data
simulate_student_features <- function(n = 100) {
  set.seed(260923)
  # Generate sequential student IDs
  student_ids <- seq(1, n)
  # Simulate student engagement scores
  student_engagement <- rnorm(n, mean = 50, sd = 10)
  # Simulate student performance scores
  student_performance <- rnorm(n, mean = 60, sd = 15)
  # Create a dataframe with student features
  student_features <- data.frame(
    student_id = student_ids,
    student_engagement = student_engagement,
    student_performance = student_performance
  )
  return(student_features)
}

# Generate simulated student features data
student_features <- simulate_student_features(n = 100)

# Perform dimensionality reduction using Principal Component Analysis (PCA)
pca_result <- prcomp(student_features[, -1], scale. = TRUE)

# Cluster the data using K-Means algorithm
kmeans_result <- kmeans(pca_result$x, centers = 3)

# Interpret the clustering results
cluster_labels <- kmeans_result$cluster
# Calculate means of features for each cluster
cluster_means <- aggregate(. ~ cluster_labels, data = cbind(student_features, Cluster = cluster_labels), mean)
cluster_means <- cluster_means[, -1] # Remove the cluster label column
print(cluster_means)

# Plot the clusters
plot(pca_result$x[,1:2], col = cluster_labels, pch = 19,
     main = "K-Means Clustering of Student Features",
     xlab = "Principal Component 1", ylab = "Principal Component 2")
points(kmeans_result$centers[,1:2], col = 1:3, pch = 8, cex = 2)
legend("topright", legend = paste("Cluster", 1:3), col = 1:3, pch = 8, cex = 1.2)
