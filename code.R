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
