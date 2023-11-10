# loding the Library 
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
# Read the data into R
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
# Disease distribution for age. 
# 0 - no disease
# 1 - disease
####################################################

heart_data %>% group_by(Age, HeartDisease) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Age, count,   fill = as.factor(HeartDisease)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "HeartDisease")


####################################################
# Chest pain type for diseased people
# You can see - Majority as condition 3 type
# 0: typical angina 1: atypical angina  Value 2: non-anginal pain Value 3: asymptomatic
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
