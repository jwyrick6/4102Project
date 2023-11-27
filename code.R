#install.packages("tidyverse")
library(tidyverse)
library(dslabs)
library(dplyr)
library(caret)
library(lubridate)
library(tidytext)
library("RColorBrewer")
library(randomForest)
library(tictoc)
library(e1071)
library(ggpubr)

# Read the data into R from the .csv file
heart_data <- read.csv("./heart.csv")

# Display the first few rows of the data for a quick overview
head(heart_data)

# Display the structure of the dataset
str(heart_data)

# Display summary statistics
summary(heart_data)

# Check unique values for each column
lapply(heart_data, unique)

# printing the column names 
print(colnames(heart_data))

# Print data types for each column
print(sapply(heart_data, class))

####################################################
# Distinct values
####################################################

heart_data %>% summarise(n_age = n_distinct(Age), n_sex = n_distinct(Sex),
                                 n_cp = n_distinct(ChestPainType), n_trestbps = n_distinct(RestingBP),
                                 n_chol = n_distinct(Cholesterol), n_fbs = n_distinct(FastingBS),
                                 n_restecg = n_distinct(RestingECG), n_MaxHR = n_distinct(MaxHR),
                                 n_exang = n_distinct(ExerciseAngina), n_oldpeak = n_distinct(Oldpeak),
                                 n_slope = n_distinct(ST_Slope), n_ca = n_distinct(HeartDisease))


####################################################
# Disease distribution for age. 0 no disease and 1 disease
####################################################

heart_data %>% group_by(Age, HeartDisease) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Age, count,   fill = as.factor(HeartDisease)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "HeartDisease")


####################################################
# Chest pain type for diseased people
# 0: typical angina 1: atypical angina  Value 2: non-anginal pain Value 3: asymptomatic (Majority)
####################################################

heart_data %>% filter(HeartDisease == 1) %>% group_by(Age, ChestPainType) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Age, count,   fill = as.factor(ChestPainType)),stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "HeartDisease") + 
  ggtitle("Age vs. Count (disease only) for various chest pain conditions") +
  scale_fill_manual(values=c("red", "blue", "green", "black"))


####################################################
# condition sex wise
####################################################
options(repr.plot.width = 20, repr.plot.height = 8) 

heart_data %>% ggballoonplot(x = "Age", y = "Sex",
                                     size = "Cholesterol", size.range = c(5, 30), fill = "HeartDisease",show.label = FALSE,
                                     ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Age vs. Sex Map") + labs(fill = "HeartDisease")

options(repr.plot.width = 20, repr.plot.height = 8) 

####################################################
# condition sex wise
####################################################
library(ggpubr)
library(ggplot2)
library(viridis)

heart_data %>%
  ggballoonplot(x = "Age", y = "ChestPainType",
                size = "Cholesterol", size.range = c(5, 30), fill = "Sex",show.label = FALSE,
                ggtheme = theme_bw()) +
  scale_fill_viridis_d(option = "C") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Age vs. Chest Pain Map") + labs(fill = "Sex")

<<<<<<< HEAD
##############################
# Heart disease by age column Pie plot
#############################
library(ggplot2)

# Summarize the counts for each level of heartdisease
heartdisease_counts <- as.data.frame(table(heart_data$HeartDisease))
heartdisease_counts

# Plotting
ggplot(heartdisease_counts, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Distribution of Heart Disease", fill = "Heart Disease Type")

# Box plot 
ggplot(heart_data, aes(x = as.factor(HeartDisease), y = Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Heart Disease", x = "Heart Disease", y = "Age") +
  theme_minimal()

# violin plot 
ggplot(heart_data, aes(x = as.factor(HeartDisease), y = Age)) +
  geom_violin() +
  labs(title = "Violin Plot of Age by Heart Disease", x = "Heart Disease", y = "Age") +
  theme_minimal()

############################################
# pie plot for the ChestPainType, Sex, RestingECG, ExerciseAngina, and ST_Slope column
#############################################

# Function to generate pie plot for a given column
create_pie_plot <- function(data, column_name) {
  counts <- as.data.frame(table(data[[column_name]]))
  
  ggplot(counts, aes(x = "", y = Freq, fill = Var1)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    labs(title = paste0("Distribution of ", column_name), fill = column_name) +
    theme_minimal()
}

# Create pie plots
pie_ChestPainType <- create_pie_plot(heart_data, "ChestPainType")
pie_Sex <- create_pie_plot(heart_data, "Sex")
pie_RestingECG <- create_pie_plot(heart_data, "RestingECG")
pie_ExerciseAngina <- create_pie_plot(heart_data, "ExerciseAngina")
pie_ST_Slope <- create_pie_plot(heart_data, "ST_Slope")

# Display one of the pie plots (for demonstration purposes, displaying the pie plot for "ChestPainType")
print(pie_ChestPainType)
print(pie_Sex)
print(pie_RestingECG)
print(pie_ExerciseAngina)
print(pie_ST_Slope)

################################
# BOX Plot for Numerical Column
################################

# Filter out only the numeric columns
numeric_data <- heart_data[sapply(heart_data, is.numeric)]

# Create a long format for ggplot
long_data <- reshape2::melt(numeric_data)

# Create the boxplot
ggplot(long_data, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Boxplots for Numerical Columns", x="", y="Value") +
  scale_fill_brewer(palette="Set3")

##########################################
# Histogram to check the data distribution 
##########################################
library(gridExtra)

# Create a list to store individual histograms and individual plots
hist_list <- list()
plot_list <- list()

# Loop through each column and create a histogram or bar plot depending on the column type
for (col_name in names(heart_data)) {
  if (is.numeric(heart_data[[col_name]])) { # Check if the column is numeric
    p <- ggplot(heart_data, aes_string(col_name)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      theme_minimal() +
      labs(title = paste("Histogram of", col_name), x = col_name, y = "Count")
  } else { # For non-numeric columns, create a bar plot
    p <- ggplot(heart_data, aes_string(col_name)) +
      geom_bar(fill = "salmon", color = "black") +
      theme_minimal() +
      labs(title = paste("Bar Plot of", col_name), x = col_name, y = "Count")
  }
  plot_list[[col_name]] <- p
}

# Arrange the plots in a grid layout
do.call(gridExtra::grid.arrange, c(plot_list, ncol = 2))

# Create a line plot for the Age column
ggplot(heart_data, aes(x = 1:nrow(heart_data), y = Age)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Line Plot of Age", x = "Index", y = "Age")


heart_data <- read.csv("./heart.csv")
head(heart_data)

# Convert the 'Sex' column to factor for better labeling (assuming 0 for female and 1 for male)
#heart_data$Sex <- factor(heart_data$Sex, levels = c(0, 1), labels = c("Female", "Male"))

# Calculate average age for each sex
avg_age <- heart_data %>%
  group_by(Sex) %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE))

# Create a bar plot for Average Age across different Sex categories
bar_plot <- ggplot(avg_age, aes(x = Sex, y = Average_Age, fill = Sex)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Age by Sex", x = "Sex", y = "Average Age")

# Display the plot
bar_plot

# Convert all categorical columns (factors and characters) to numeric using factorization
heart_data_numeric <- heart_data
for (col_name in names(heart_data)) {
  if (is.character(heart_data[[col_name]]) || is.factor(heart_data[[col_name]])) {
    heart_data_numeric[[col_name]] <- as.numeric(as.factor(heart_data[[col_name]]))
  }
}

# View the first few rows of the converted dataframe
head(heart_data_numeric)

# Install and load the corrplot package if not already done
# install.packages("corrplot")
library(corrplot)

# Calculate the correlation matrix
cor_matrix <- cor(heart_data_numeric, use = "complete.obs")  # 'complete.obs' ignores missing values

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle")

########################
# Correlation plot 
#######################

#install.packages("GGally")
library(GGally)

# Create a pair plot using ggpairs and display it
pair_plot <- ggpairs(heart_data_numeric)
pair_plot

# Calculate the number of missing values for each column
missing_values <- colSums(is.na(heart_data))

# Create a bar plot for missing values
library(ggplot2)
missing_plot <- ggplot(data = data.frame(Columns = names(missing_values), MissingCount = missing_values), aes(x = Columns, y = MissingCount)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Missing Values per Column", x = "Columns", y = "Number of Missing Values")

# Display the plot for missing values
missing_plot

#########################################################################
#########################################################################
# Hierarchical clustering with dendrogram
#########################################################################
#########################################################################

# Install necessary libraries
install.packages('hclust')
install.packages("dendextend", dependencies=TRUE)

# Step 1: Load necessary libraries
library(dplyr)
library(dendextend)

# Step 2: Read the data
heart_data <- read.csv("./heart.csv")

# Step 3: Convert categorical variables to factors
heart_data[] <- lapply(heart_data, as.factor)

# Step 4: Convert categorical variables to dummy variables (one-hot encoding)
heart_data_encoded <- as.data.frame(model.matrix(~ . - 1, data = heart_data))

# Step 5: Calculate the distance matrix
dist_matrix <- dist(heart_data_encoded, method = "euclidean")

# Step 6: Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "average")

# Step 7: Plot the dendrogram
dend <- as.dendrogram(hc)
plot(dend)
#########################################################################
#########################################################################
# preprocessing 
#########################################################################
#########################################################################

# Step 1: Install and Load necessary libraries and data
install.packages("dplyr")
library(dplyr)

# Step 2: Convert all columns to numeric using label encoding
heart_data_numeric <- heart_data %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Display the first few rows of the numeric data
head(heart_data_numeric)

##########################################################################
# K-means clustering 
##########################################################################

# Step 1: Preparation
#library(dplyr)

# Convert all data to numeric
heart_data_numeric <- heart_data %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Step 2: Normalization
scaled_data <- scale(heart_data_numeric)

# Step 3: K-means Clustering
# assuming we want 3 clusters.
set.seed(123)  # Setting seed for reproducibility
kmeans_result <- kmeans(scaled_data, centers = 3)

# Step 4: Inspect the Clusters
# View cluster assignments
print(kmeans_result$cluster)

# View cluster centroids
print(kmeans_result$centers)

####################################################
# plot the centroids 
#####################################################

# Install and load necessary libraries
install.packages(c("dplyr", "ggplot2", "gganimate", "transformr"))
library(dplyr)
library(ggplot2)
library(gganimate)
library(transformr)

# Convert all data to numeric
heart_data_numeric <- heart_data %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Normalize the data
scaled_data <- scale(heart_data_numeric)

# Implement k-means clustering
set.seed(123)  # Setting seed for reproducibility
kmeans_result <- kmeans(scaled_data, centers = 3)

# Add cluster assignments to the data
data_with_clusters <- as.data.frame(scaled_data)
data_with_clusters$cluster <- as.factor(kmeans_result$cluster)

# Extract cluster centroids
centroids <- as.data.frame(kmeans_result$centers)
centroids$cluster <- as.factor(1:nrow(centroids))

# Create animated plot
anim_plot <- ggplot(data_with_clusters, aes(x = Age, y = Cholesterol)) + 
  geom_point(aes(color = cluster), alpha = 0.6) +
  geom_point(data = centroids, aes(x = Age, y = Cholesterol, color = cluster), size = 5, shape = 8) +
  labs(title = 'Animated Centroids of K-means Clusters', x = 'Age', y = 'Cholesterol') +
  transition_states(cluster, transition_length = 2, state_length = 1) +
  enter_fade() + 
  exit_fade()

# Save the animated plot
anim_save("animated_centroids.gif", anim_plot)
getwd()  # to check the animated plot path 

####################################################
# elbow scree plot 
#####################################################
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Convert all data to numeric
heart_data_numeric <- heart_data %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Normalize the data
scaled_data <- scale(heart_data_numeric)

# Compute total within-cluster sum of square
wss <- numeric(15)
for (k in 1:15) {
  kmeans_result <- kmeans(scaled_data, centers = k)
  wss[k] <- kmeans_result$tot.withinss
}

# Plot the elbow method scree plot
elbow_plot <- ggplot(data.frame(k = 1:15, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method Scree Plot for Heart Data", x = "Number of clusters (k)", y = "Total within-cluster sum of squares") +
  theme_minimal()

print(elbow_plot)

################################################################
# pie plot of the cluster 
#################################################################
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Convert all data to numeric
heart_data_numeric <- heart_data %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Normalize the data
scaled_data <- scale(heart_data_numeric)

# K-means clustering with 4 clusters
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4)

# Create a table of cluster assignments
cluster_counts <- table(kmeans_result$cluster)

# Plot a pie chart
pie(cluster_counts, main="Distribution of Data Points Among 4 Clusters", col=rainbow(4), labels=paste(names(cluster_counts), "\n", cluster_counts, " points"))

#########################################################################
#########################################################################
# AI Classification Here
# splitting the data into training and testing 
#########################################################################
#########################################################################

# Load necessary libraries
library(dplyr)

# Step 2: Convert all data to numeric
heart_data_numeric <- heart_data %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Step 3: Split the data into training and testing sets
set.seed(123)  # Setting seed for reproducibility

sample_size <- floor(0.7 * nrow(heart_data_numeric))
train_indices <- sample(seq_len(nrow(heart_data_numeric)), size = sample_size)

train_data <- heart_data_numeric[train_indices, ]
test_data <- heart_data_numeric[-train_indices, ]

# Verifying the split
cat("Number of rows in training data:", nrow(train_data), "\n")
cat("Number of rows in testing data:", nrow(test_data), "\n")

##########################################################################
# Random Forest classifier 
##########################################################################

# Install necessary libraries, comment out when done
install.packages(c("randomForest", "e1071", "pROC", "caret"))

# load necessary libraries
library(randomForest)
library(e1071)
library(pROC)
library(caret)

# Random Forest Model Training
set.seed(123)
rf_model <- randomForest(HeartDisease ~ ., data = train_data, ntree = 100)

# Training and Testing Accuracy
train_predictions <- predict(rf_model, train_data)
train_accuracy <- sum(train_predictions == train_data$HeartDisease) / nrow(train_data)

test_predictions <- predict(rf_model, test_data)
test_accuracy <- sum(test_predictions == test_data$HeartDisease) / nrow(test_data)

cat("Training accuracy:", train_accuracy, "\n")
cat("Testing accuracy:", test_accuracy, "\n")

# Confusion Matrix & Classification Report
conf_matrix <- table(test_predictions, test_data$HeartDisease)
print(conf_matrix)

# ROC and AUC Plot
roc_obj <- roc(test_data$HeartDisease, as.numeric(test_predictions))
auc_obj <- auc(roc_obj)
plot(roc_obj, main = paste("ROC Curve\nAUC:", round(auc_obj, 3)))

# Visualize Confusion Matrix using caret
confusionMatrixPlot(conf_matrix)

##########################################################################
##########################################################################
# Random Forest classifier with model tuning 
##########################################################################
##########################################################################

# Install and load necessary libraries, same as before
#install.packages(c("randomForest", "e1071", "pROC", "caret","vcd"))
library(randomForest)
library(e1071)
library(pROC)
library(caret)
library(vcd)  #confusion matrix library

# Convert all data to numeric
heart_data_numeric <- heart_data %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Convert the HeartDisease column to a factor
heart_data_numeric$HeartDisease <- as.factor(heart_data_numeric$HeartDisease)

# Normalize the data
scaled_data <- scale(heart_data_numeric)

# Split the data into training and testing sets
set.seed(123)  # Setting seed for reproducibility
sample_size <- floor(0.7 * nrow(heart_data_numeric))
train_indices <- sample(seq_len(nrow(heart_data_numeric)), size = sample_size)
train_data <- heart_data_numeric[train_indices, ]
test_data <- heart_data_numeric[-train_indices, ]

# Hyperparameter Grid Setup
tune_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10))

# Grid Search with Cross-Validation
tuned_rf <- train(
  HeartDisease ~ ., 
  data = train_data, 
  method = "rf", 
  metric = "Accuracy", 
  tuneGrid = tune_grid, 
  trControl = trainControl(method = "cv", number = 5)
)

# Print the best hyperparameters
print(tuned_rf$bestTune)

# Evaluate Best Model's Performance
best_rf_model <- randomForest(HeartDisease ~ ., data = train_data, mtry = tuned_rf$bestTune$mtry, ntree = 100)
train_predictions <- predict(best_rf_model, train_data)
test_predictions <- predict(best_rf_model, test_data)

train_accuracy <- sum(train_predictions == train_data$HeartDisease) / nrow(train_data)
test_accuracy <- sum(test_predictions == test_data$HeartDisease) / nrow(test_data)

cat("Training accuracy with tuned parameters:", train_accuracy, "\n")
cat("Testing accuracy with tuned parameters:", test_accuracy, "\n")

# ROC Curve
roc_obj <- roc(as.numeric(test_data$HeartDisease), as.numeric(test_predictions))
plot(roc_obj, main = paste("ROC Curve for Tuned Random Forest Model"))

# Confusion Matrix using caret
conf_matrix <- table(test_predictions, test_data$HeartDisease)

# Visualize the confusion matrix
fourfoldplot(conf_matrix, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")


##########################################################################
##########################################################################
# Decision tree classifier 
##########################################################################
##########################################################################
# Install and load necessary libraries
#install.packages(c("rpart", "rpart.plot", "e1071", "pROC"))
library(rpart)
library(rpart.plot)
library(e1071)
library(pROC)

# Assuming the data has been loaded, converted to numeric, and split into train_data and test_data

# Step 1: Decision Tree Model Training
tree_model <- rpart(HeartDisease ~ ., data = train_data, method = "class")

# Step 2: Training and Testing Accuracy
train_predictions <- predict(tree_model, train_data, type = "class")
train_accuracy <- sum(train_predictions == train_data$HeartDisease) / nrow(train_data)

test_predictions <- predict(tree_model, test_data, type = "class")
test_accuracy <- sum(test_predictions == test_data$HeartDisease) / nrow(test_data)

cat("Training accuracy:", train_accuracy, "\n")
cat("Testing accuracy:", test_accuracy, "\n")

# Step 3: Confusion Matrix
conf_matrix <- table(test_predictions, test_data$HeartDisease)
print(conf_matrix)
# Visualize the confusion matrix
fourfoldplot(conf_matrix, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")


# Step 4: ROC Curve
test_probabilities <- predict(tree_model, test_data, type = "prob")[,2]  # Probabilities of class 1
roc_obj <- roc(test_data$HeartDisease, test_probabilities)
plot(roc_obj, main = "ROC Curve for Decision Tree Model")

##########################################################################
##########################################################################
# Decision tree classifier with model tuning 
##########################################################################
##########################################################################

# Install and load necessary libraries
#install.packages(c("rpart", "e1071", "pROC", "caret"))
library(rpart)
library(e1071)
library(pROC)
library(caret)

# Assuming the data has been loaded, converted to numeric, and split into train_data and test_data

# 1. Decision Tree Model Training with Hyperparameter Tuning
# Hyperparameter Grid Setup
tune_grid <- expand.grid(cp = seq(0.001, 0.05, by = 0.002))

# Grid Search with Cross-Validation
set.seed(123)
tuned_tree <- train(
  HeartDisease ~ ., 
  data = train_data, 
  method = "rpart", 
  metric = "Accuracy", 
  tuneGrid = tune_grid, 
  trControl = trainControl(method = "cv", number = 5)
)

# Print the best hyperparameters
print(tuned_tree$bestTune)

# 2. Training and Testing Accuracy
best_tree_model <- rpart(HeartDisease ~ ., data = train_data, method = "class", control = rpart.control(cp = tuned_tree$bestTune$cp))
train_predictions <- predict(best_tree_model, train_data, type = "class")
test_predictions <- predict(best_tree_model, test_data, type = "class")

train_accuracy <- sum(train_predictions == train_data$HeartDisease) / nrow(train_data)
test_accuracy <- sum(test_predictions == test_data$HeartDisease) / nrow(test_data)

cat("Training accuracy with tuned parameters:", train_accuracy, "\n")
cat("Testing accuracy with tuned parameters:", test_accuracy, "\n")

# 3. Confusion Matrix
conf_matrix <- table(test_predictions, test_data$HeartDisease)
print(conf_matrix)

# Visualize the confusion matrix
fourfoldplot(conf_matrix, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")


# 4. ROC Curve
test_probabilities <- predict(best_tree_model, test_data, type = "prob")[,2]  # Probabilities of class 1
roc_obj <- roc(test_data$HeartDisease, test_probabilities)
plot(roc_obj, main = "ROC Curve for Tuned Decision Tree Model")

##########################################################################
##########################################################################
# Logistic Regression 
##########################################################################
##########################################################################


# Install and load necessary libraries
#install.packages(c("e1071", "pROC", "caret"))
library(e1071)
library(pROC)
library(caret)

# Assuming the data has been loaded, converted to numeric, and split into train_data and test_data

# 1. Logistic Regression Model Training
logistic_model <- glm(HeartDisease ~ ., data = train_data, family = binomial())

# 2. Training and Testing Accuracy
train_probabilities <- predict(logistic_model, train_data, type = "response")
train_predictions <- ifelse(train_probabilities > 0.5, 1, 0)
train_accuracy <- sum(train_predictions == train_data$HeartDisease) / nrow(train_data)

test_probabilities <- predict(logistic_model, test_data, type = "response")
test_predictions <- ifelse(test_probabilities > 0.5, 1, 0)
test_accuracy <- sum(test_predictions == test_data$HeartDisease) / nrow(test_data)

cat("Training accuracy:", train_accuracy, "\n")
cat("Testing accuracy:", test_accuracy, "\n")

# 3. Confusion Matrix
conf_matrix <- table(test_predictions, test_data$HeartDisease)
print(conf_matrix)
# Visualize the confusion matrix
fourfoldplot(conf_matrix, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")


# 4. ROC Curve
roc_obj <- roc(test_data$HeartDisease, test_probabilities)
plot(roc_obj, main = "ROC Curve for Logistic Regression Model")

##########################################################################
##########################################################################
# Logistic Regression with model tuning 
##########################################################################
##########################################################################

# Install and load necessary libraries
#install.packages(c("glmnet", "pROC", "caret"))
library(glmnet)
library(pROC)
library(caret)

# Assuming the data has been loaded, converted to numeric, and split into train_data and test_data

# Convert data to matrix form (required by glmnet)
x_train <- as.matrix(train_data[, -which(names(train_data) == "HeartDisease")])
y_train <- train_data$HeartDisease
x_test <- as.matrix(test_data[, -which(names(test_data) == "HeartDisease")])
y_test <- test_data$HeartDisease

# 1. Regularized Logistic Regression Model Training with Hyperparameter Tuning
set.seed(123)
cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)  # Lasso regularization (alpha=1)
best_lambda <- cv_fit$lambda.min
logistic_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)

# 2. Training and Testing Accuracy
train_probabilities <- predict(logistic_model, s = best_lambda, newx = x_train, type = "response")
train_predictions <- ifelse(train_probabilities > 0.5, 1, 0)
train_accuracy <- sum(train_predictions == y_train) / length(y_train)

test_probabilities <- predict(logistic_model, s = best_lambda, newx = x_test, type = "response")
test_predictions <- ifelse(test_probabilities > 0.5, 1, 0)
test_accuracy <- sum(test_predictions == y_test) / length(y_test)

cat("Training accuracy with tuned parameters:", train_accuracy, "\n")
cat("Testing accuracy with tuned parameters:", test_accuracy, "\n")

# 3. Confusion Matrix
conf_matrix <- table(test_predictions, y_test)
print(conf_matrix)
# Visualize the confusion matrix
fourfoldplot(conf_matrix, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")


# 4. ROC Curve
roc_obj <- roc(y_test, test_probabilities)
plot(roc_obj, main = "ROC Curve for Tuned Logistic Regression Model")