library(ggplot2)
library(corrplot)
library(caret)
library(plotly)

# Preprocessing function
preprocess <- function(data) {
    chess <- data
    chess$rated <- as.logical(chess$rated)
    chess$increment_code <- as.character(chess$increment_code)
    # everthing before the plus sign
    chess$increment_code1 <- strsplit(chess$increment_code, split = "\\+")
    chess$increment_code1 <- sapply(chess$increment_code1, function(x) x[1])
    # everything after the plus sign
    chess$increment_code2 <- strsplit(chess$increment_code, split = "\\+")
    chess$increment_code2 <- sapply(chess$increment_code2, function(x) x[2])

    chess$increment_code1 <- as.numeric(chess$increment_code1)
    chess$increment_code2 <- as.numeric(chess$increment_code2)

    chess$increment_code1_bin <- cut(chess$increment_code1, breaks = c(0,
    20, 50, 100, 200, 1000))
    chess$increment_code2_bin <- cut(chess$increment_code2, breaks = c(0, 20,
    50, 100, 200, 1000))
    data <- chess
    data$rating_diff <- data$white_rating - data$black_rating

    # Remove the draws
    data <- subset(data, winner != "draw")
    # Convert the winner column to 1 and 0
    data$winner <- ifelse(data$winner == "white", 1, 0)
    # Remove the columns that we don't need
    data <- data[, c("winner", "white_rating", "black_rating",
     "rating_diff", "increment_code1_bin", "increment_code2_bin",
     "rated")]
    # Remove the rows with NA values
    data <- na.omit(data)
    # Return the preprocessed data
    return(data)
}

train_model <- function(data, verbose = FALSE) {
  # Split the data into train and test
  train_index <- createDataPartition(data$winner, p = 0.7, list = FALSE)
  train <- data[train_index, ]
  test <- data[-train_index, ]
  # Train the model
  model <- glm(winner ~ ., data = train, family = "binomial")
  # Predict the test data
  pred <- predict(model, test)
  if (verbose) {
    print(model)
    print(summary(model))
    # Print the confusion matrix
    print(table(test$winner, pred > 0.5))
    # Print the accuracy
    print(mean(test$winner == (pred > 0.5)))

  }
  # return the model and the mean accuracy
  return(list(model = model, accuracy = mean(test$winner == (pred > 0.5))))
}
#A function to get the chi-square test
chisq_test <- function(data, var1, var2) {
  # Create a contingency table
  table <- table(data[, var1], data[, var2])
  # Perform the chi-square test
  chi <- chisq.test(table)
  # Return the p-value
  return(chi$p.value)
}
# A function to get the Anova test results
anova_test <- function(data, var1, var2) {
  # Perform the Anova test
  anova <- aov(data[, var1] ~ data[, var2])
  # Return the p-value
  summ <- summary(anova)
  return(as.numeric(unlist(summ)["Pr(>F)1"]))
}



iterative_sampling_num <- function(data, numerical_vars) {
  selected_vars <- c()
  accuracy <- -1
  # Run the below code iteratively while adding variable if
  # the mean accuracy is greater than the previous one
  while (length(numerical_vars) > 0) {
    # Run the chi-square test on the numerical variables and
    # select the variable with the least p-value
    anova_pvals <- sapply(numerical_vars,
      function(x) anova_test(data, "winner", x))
    # Select the variable with the least p-value
    anova_var <- numerical_vars[which.min(anova_pvals)]
    # Run the model with the selected variable with the
    # train_model function which will return the model and the accuracy
    selected_vars <- c(selected_vars, anova_var)
    model <- train_model(data[, c("winner", selected_vars)], verbose = TRUE)
    # If the accuracy is greater than the previous one,
    # then add the variable to the model
    numerical_vars <- numerical_vars[-which(numerical_vars == anova_var)]
    if (model$accuracy > accuracy) {
      accuracy <- model$accuracy
    } else {
      # removez the last variable added to the model
      selected_vars <- selected_vars[-length(selected_vars)]
    }
  }
  return(selected_vars)
}

#iterative sampling for categorical variables
iterative_sampling_cat <- function(data, categorical_vars) {
  selected_vars <- c()
  accuracy <- -1
  # Run the below code iteratively while adding variable if
  # the mean accuracy is greater than the previous one
  while (length(categorical_vars) > 0) {
    # Run the chi-square test on the categorical variables and
    # select the variable with the least p-value
    chi_pvals <- sapply(categorical_vars,
      function(x) chisq_test(data, "winner", x))
    # Select the variable with the least p-value
    chi_var <- categorical_vars[which.min(chi_pvals)]
    # Run the model with the selected variable with the
    # train_model function which will return the model and the accuracy
    selected_vars <- c(selected_vars, chi_var)
    model <- train_model(data[, c("winner", selected_vars)], verbose = TRUE)
    # If the accuracy is greater than the previous one,
    # then add the variable to the model
    categorical_vars <- categorical_vars[-which(categorical_vars == chi_var)]
    if (model$accuracy > accuracy) {
      accuracy <- model$accuracy
    } else {
      # removez the last variable added to the model
      selected_vars <- selected_vars[-length(selected_vars)]
    }
  }
  return(selected_vars)
}

# Preprocess the data
chess <- read.csv("games.csv")
chess_pros <- preprocess(chess)

numerical_vars <- c("white_rating", "black_rating", "rating_diff")
selected_num_vars <- iterative_sampling_num(chess_pros, numerical_vars)

categorical_vars <- c("increment_code1_bin", "increment_code2_bin", "rated")
selected_cat_vars <- iterative_sampling_cat(chess_pros, categorical_vars)

selected_vars <- c(selected_num_vars, selected_cat_vars)

# Train the model
model <- train_model(chess_pros[,
 c("winner", selected_vars)], verbose = TRUE)

# Save the model
saveRDS(model$model, "chess_model.rds")