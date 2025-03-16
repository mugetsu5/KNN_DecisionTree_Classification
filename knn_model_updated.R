
# Load necessary libraries
install.packages("wakefield")
library(wakefield)
library(caTools)
library(class)

# Set seed for reproducibility
set.seed(2101743)

# Generate synthetic data
data <- r_data(1000)
data$Died <- as.factor(data$Died)

# Split the data into training and testing sets
sample <- sample.split(data$Died, SplitRatio = 0.8)
trainData <- subset(data, sample == TRUE)
testData <- subset(data, sample == FALSE)

# Separate features and labels for training and testing
trainDataY <- trainData$Died
trainDataX <- trainData[, c("Age", "Race", "Sex", "Hour", "IQ", "Height")]
testDataY <- testData$Died
testDataX <- testData[, c("Age", "Race", "Sex", "Hour", "IQ", "Height")]

# Convert categorical variables to numeric
trainDataX$Race <- as.numeric(as.factor(trainDataX$Race))
trainDataX$Sex <- as.numeric(as.factor(trainDataX$Sex))
testDataX$Race <- as.numeric(as.factor(testDataX$Race))
testDataX$Sex <- as.numeric(as.factor(testDataX$Sex))

# Normalize the data
trainDataX <- scale(trainDataX)
testDataX <- scale(testDataX)

# Perform KNN with varying values of k to find the optimal k
k.optm <- numeric(50)
for (i in 1:50) {
  knn.mod <- knn(trainDataX, testDataX, trainDataY, k = i)
  k.optm[i] <- 100 * sum(knn.mod == testDataY) / NROW(testDataY)
  cat(i, '=', k.optm[i], '\n')
}

# Plot accuracy vs k
plot(k.optm, type = "b", xlab = "k Value", ylab = "Accuracy", 
     main = "Accuracy based on k value", col="blue", pch=19)

# Determine the optimal k value
optimal_k <- which.max(k.optm)
cat("Optimal k value:", optimal_k, "\n")

# Run KNN with the optimal k value
knn.model <- knn(trainDataX, testDataX, trainDataY, k = optimal_k)

# Evaluate performance with confusion matrix and accuracy
confusion_matrix <- table(knn.model, testDataY)
print(confusion_matrix)
accuracy <- mean(knn.model == testDataY)
cat("Accuracy: ", accuracy, "\n")


library(caTools)
set.seed(2101743)
sample <- sample.split(data$Died, SplitRatio = 0.8)
trainData <- subset(data, sample == TRUE)
testData <- subset(data, sample == FALSE)
install.packages("rpart")
>library(rpart)
>model <- rpart(Died ~ Age + Race + Sex + Hour + IQ + Height, data = trainData, method = "class",  parms = list(split = "information"))

