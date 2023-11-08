library(shiny)
library(randomForest)
library(ggplot2)

# 1. Read the data
heart_data <- read.csv("C:/Users/0513/Desktop/TK123816 - R -Language - Smart Health Prediction/heart.csv")

# 2. Preprocess and encode categorical variables
categorical_columns <- c("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope")
heart_data[categorical_columns] <- lapply(heart_data[categorical_columns], as.factor)

# Split data into training and testing sets
set.seed(42)
trainIndex <- sample(1:nrow(heart_data), 0.8*nrow(heart_data))
train_data <- heart_data[trainIndex,]
test_data <- heart_data[-trainIndex,]

# 3. Train random forest classifier
trained_model <- randomForest(HeartDisease ~ ., data = train_data, ntree = 100)

# Shiny app
ui <- fluidPage(
  titlePanel("Heart Disease Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      # Input fields for features
      numericInput("age", "Age", 50, min = 1, max = 100),
      selectInput("sex", "Sex", choices = c("M", "F")),
      selectInput("chestPainType", "Chest Pain Type", choices = c("ATA", "NAP", "ASY", "TA")),
      numericInput("restingBP", "Resting Blood Pressure", 120, min = 80, max = 200),
      numericInput("cholesterol", "Cholesterol", 200, min = 50, max = 400),
      numericInput("fastingBS", "Fasting Blood Sugar", 0, min = 0, max = 1),
      selectInput("restingECG", "Resting ECG", choices = c("Normal", "ST", "LVH")),
      numericInput("maxHR", "Max Heart Rate", 150, min = 50, max = 220),
      selectInput("exerciseAngina", "Exercise Induced Angina", choices = c("Y", "N")),
      numericInput("oldpeak", "Oldpeak", 0, min = 0, max = 5),
      selectInput("stSlope", "ST Slope", choices = c("Up", "Flat", "Down")),
      actionButton("predict", "Predict")
    ),
    