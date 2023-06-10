# Import the Library

library(ggplot2)
library(readxl)
library(forecast)
library(caret)
library(tidyr)
library(neuralnet)


# Load the Dataset
electrical_dataset <- read_xlsx("uow-consumption.xlsx")

# Display the Dimensions of Electrical Dataset
cat("Dimensions of Dataset is : ",dim(electrical_dataset))



#                             SubTask No 1

# Define the Time Delayes for Input Matrix
time_delaye <- 5

# Create the new_dataset from electrical_dataset with the requirements
new_dataset <- electrical_dataset[,4]

# Display the Dimensions of new_dataset
cat("Dimensions of New Dataset is : ",dim(new_dataset))


# Create the Input/Output Matrix
input_matrix <- matrix(NA, nrow = nrow(new_dataset) - time_delaye, ncol = time_delaye)
output_matrix <- as.matrix(new_dataset[(time_delaye + 1):nrow(new_dataset),])

for (i in 1:time_delaye) {
  input_matrix[,i] <- as.matrix(new_dataset[(time_delaye - i + 1):(nrow(new_dataset) - i),1])
}

# Normalize/Standardize the input/output Matrix
input_matrix  <- apply(input_matrix , 2, function(x) (x - min(x)) / (max(x) - min(x)))
output_matrix <- apply(output_matrix, 2, function(x) (x - min(x)) / (max(x) - min(x)))

# Splitting Dataset into Training and Testing Part 
split_size <- floor(0.8 * nrow(new_dataset))
AR_input_train  <- input_matrix[1:split_size, ]
AR_input_test   <-   input_matrix[(split_size+1):nrow(input_matrix), ]
AR_output_train <- output_matrix[1:split_size, ]
AR_output_test  <- output_matrix[(split_size+1):nrow(output_matrix), ]

# Apply the AR Neural Net Model 


# Define the All Error Functions
sMAPE <- function(actual, predicted) {
  return(100/length(actual) * sum(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))))
}
MAPE <- function(actual, predicted) {
  return (mean(abs((actual - predicted) / actual)) * 100)
}
RMSE <- function(actual, predicted) {
  return (sqrt(1/length(actual) * sum((actual - predicted)^2)))
}
MAE <- function(actual, predicted) {
  return (mean(abs(actual - predicted)))
}

# Define the Variables
RMSEA <- c()
MAEA <- c()
MAPEA <- c()
SMAPEA <- c()
first_weight <- c()
second_weight <- c()
first_predict <- c()
second_predict <- c()

# Define the Parameters of NN Model
layers <- c(1,2)
nodes <- c(3,5,8,10,12,14)

index <- 0
layer1 <- 0
layer2 <- 0
for(l in layers){
  for(n in nodes){
    # Create the Object of the AR Model with neuralnet
    model <- neuralnet(AR_output_train ~. ,data = data.frame(AR_input_train , AR_output_train) , 
                       hidden =  c(l,n) , act.fct = function(x){x} , linear.output = TRUE) 
    
    # Predict the Result with AR NN Model
    predict_res <- predict(model , AR_input_test)
    
    # Calulate and Add the Erros
    index <- index + 1
    RMSEA[index]  <-  RMSE(AR_output_test, predict_res)
    MAEA[index]   <- MAE(AR_output_test, predict_res)
    MAPEA[index]  <- MAPE(AR_output_test, predict_res)
    SMAPEA[index] <- sMAPE(AR_output_test, predict_res)
    
    # Calculate and Add the Weights/Prediction Result
    if (l == 1){
      layer1 <- layer1 + 1
      first_predict[layer1] <- list(predict_res)
      first_weight[layer1] <- (ncol(AR_input_train) * n) + (n * ncol(AR_input_test))
    }else{
      layer2 <- layer2 + 1
      second_weight[layer2] <- (ncol(AR_input_train) * n) + (n * 5) + (5 * ncol(AR_input_test))
      second_predict[layer2] <- list(predict_res)
    }
  }
}

# Define the Comparison of Statistically Results
comparison_armodel <- data.frame(
  RMSE  = RMSEA, 
  MAE   = MAEA, 
  MAPE  = MAPEA, 
  SMAPE = SMAPEA
)
print(comparison_armodel)


#                               SubTask No 2


# Create again New Dataset
new_dataset <- electrical_dataset[,c(2,3,4)]

# Display the Dimensions of New Dataset
cat("Dimensions of New Dataset is : ",dim(new_dataset))

# Create the Input/Output Matrix
Ninput_matrix <- as.matrix(new_dataset[1:465,c(1,2)])
output_matrix <- as.matrix(new_dataset[,c(3)])

# Normalize/Standardize the input/output Matrix
Ninput_matrix  <- apply(Ninput_matrix , 2, function(x) (x - min(x)) / (max(x) - min(x)))
output_matrix <- apply(output_matrix, 2, function(x) (x - min(x)) / (max(x) - min(x)))

# concatenation Current input matrix with Previous input matrix
input_matrix <- cbind(input_matrix , Ninput_matrix)


# Splitting Dataset into Training and Testing Part 
split_size <- floor(0.8 * nrow(new_dataset))
NX_input_train  <- input_matrix[1:split_size, ]
NX_input_test   <- input_matrix[(split_size+1):nrow(input_matrix), ]
NX_output_train <- output_matrix[1:split_size, ]
NX_output_test  <- output_matrix[(split_size+1):nrow(output_matrix), ]


# Apply the NARX Neural Net Model 

# Define the Variables
RMSEX <- c()
MAEX <- c()
MAPEX <- c()
SMAPEX <- c()
models_weight <- c()
models_predict <- c()

# Define the Parameters of the NARX NN Model
nodes <- c(10,20,30,40)
index <- 0

for(n in nodes){
  # Create the Object of NARX NN Model
  model <- nnetar(NX_output_train, repeats = 50 , size=n)
  
  # Predict the Future outcomes with Forecast
  predict_res <- forecast(model , h = 94)
  
  # Calculate and Add the Erros
  index <- index + 1
  RMSEX[index]  <-  RMSE(NX_output_test, as.numeric(predict_res$mean))
  MAEX[index]   <- MAE(NX_output_test, as.numeric(predict_res$mean))
  MAPEX[index]  <- MAPE(NX_output_test, as.numeric(predict_res$mean))
  SMAPEX[index] <- sMAPE(NX_output_test, as.numeric(predict_res$mean))
  
  
  # Add the Predict Result and Model Weights
  models_weight[index]  <- length(model$model[[1]]$wts)
  models_predict[index] <- list(as.numeric(predict_res$mean))
}

# Define the Comparison of Statistically Results
comparison_narxmodel <- data.frame(
  RMSE  = RMSEX, 
  MAE   = MAEX, 
  MAPE  = MAPEX, 
  SMAPE = SMAPEX
)
print(comparison_narxmodel)



#                                SubTask No 3 (Graphically Results)


# AR NN Model Graphically Results

plot(AR_output_test , col = "green", pch = 15, main = "AR NN Model vs Dataset")
points(as.matrix(second_predict[[3]]), col = "black", pch = 15)
legend("topright", legend = c("Dataset", "Predicted"), col = c("green", "black"), pch = 15)

AR_stat <- data.frame(
  Model = ("Model Stat Info."),
  RMSE  = RMSEA[3], 
  MAE   = MAEA[3],
  MAPE  = MAPEA[3],
  SMAPE = SMAPEA[3]
)
dataframe <- pivot_longer(AR_stat, -Model, names_to = "Metric", values_to = "Value")
ggplot(dataframe, 
       aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison Stat Data",
       x = "Model",
       y = "Value",
       fill = "Metric")


# NARX NN Model Graphically Results

plot(NX_output_test , col = "green", pch = 15, main = "NARX NN Model vs Dataset")
points(as.matrix(models_predict[[2]]), col = "black", pch = 15)
legend("topright", legend = c("Dataset", "Predicted"), col = c("green", "black"), pch = 15)

NARX_stat <- data.frame(
  Model = ("Model Stat Info."),
  RMSE  = RMSEX[2], 
  MAE   = MAEX[2],
  MAPE  = MAPEX[2],
  SMAPE = SMAPEX[2]
)
dataframe <- pivot_longer(NARX_stat, -Model, names_to = "Metric", values_to = "Value")
ggplot(dataframe, 
       aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison Stat Data",
       x = "Model",
       y = "Value",
       fill = "Metric")
