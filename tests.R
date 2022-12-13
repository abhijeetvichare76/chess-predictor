library(testthat)
source("model_training.R")
# We'll test the dataset
# Write a test function for the dataset
test_dataset <- function(file) {
  # Load the dataset
  data <- read.csv(file)
  # Check if the dataset is not empty
  test_that("Dataset is not empty", {
    expect_true(nrow(data) > 0)
  })
  # Check if the dataset has the right number of columns
  test_that("Dataset has the right number of columns", {
    expect_true(ncol(data) == 16)
  })

  test_that("Dataset has the expected columns", {
    expect_true(sum(colnames(data) == c("id", "rated", "created_at",
     "last_move_at", "turns", "victory_status", "winner",
     "increment_code", "white_id", "white_rating", "black_id",
     "black_rating", "moves", "opening_eco", "opening_name",
     "opening_ply")) == 16)
  })

}

# We'll test the file model_training.R
test_preprocess <- function(file) {
    # Load the dataset
    data <- read.csv(file)
    # Preprocess the dataset
    data_pros <- preprocess(data) # nolint
    # Check if the dataset is not empty
    test_that("Dataset is not empty", {
        expect_true(nrow(data_pros) > 0)
    })
    # Check if the dataset has the right number of columns
    test_that("Dataset has the right number of columns", {
        expect_true(ncol(data_pros) == 7)
    })
    test_that("Dataset has the expected columns", {
        expect_true(sum(colnames(data_pros) == c("winner", "white_rating",
         "black_rating", "rating_diff", "increment_code1_bin",
         "increment_code2_bin", "rated")) == 7)
    })
    # Check if the winner column has only 0 and 1
    test_that("Winner column is binary", {
        expect_true(sum(
            data_pros$winner == 0 |
            data_pros$winner == 1) == nrow(data_pros)
            )
    })
    # Check if the rated column has only TRUE and FALSE
    test_that("Rated column is binary", {
        expect_true(sum(
            data_pros$rated == TRUE |
            data_pros$rated == FALSE) == nrow(data_pros)
            )
    })
    # Check if there are no null values
    test_that("No null values", {
        expect_true(sum(is.na(data_pros)) == 0)
    })

}

# Test the function model_training
test_train_model <- function(file) {
    # Load the dataset
    data <- read.csv(file)
    # Preprocess the dataset
    data_pros <- preprocess(data) # nolint
    # Train the model
    data_pros <- data_pros %>% select(-rating_diff) # nolint
    model <- train_model(data_pros) # nolint
    # Check if the model is not empty
    test_that("Model is not empty", {
        expect_true(length(model) > 0)
    })
    # Check if the model is a random forest
    test_that("Model is glm", {
        expect_true(class(model$model)[1] == "glm")
    })
}
# Test the chi-squared test
test_chisq_test <- function(file) {
    # Load the dataset
    data <- read.csv(file)
    # Preprocess the dataset
    data_pros <- preprocess(data) # nolint
    # Run the chi-squared test
    p_value <- chisq_test(data_pros,"winner", "rated") # nolint
    # Check if the p-value is not empty
    test_that("P-value is not empty", {
        expect_true(length(p_value) > 0)
    })
    # Check if the p-value is a number
    test_that("P-value is a number", {
        expect_true(is.numeric(p_value))
    })
    # Check if the p-value is between 0 and 1
    test_that("P-value is between 0 and 1", {
        expect_true(p_value >= 0 & p_value <= 1)
    })
}

# Check the anova test
test_anova_test <- function(file) {
    # Load the dataset
    data <- read.csv(file)
    # Preprocess the dataset
    data_pros <- preprocess(data) # nolint
    # Run the anova test
    p_value <- anova_test(data_pros,"winner", "white_rating") # nolint
    # Check if the p-value is not empty
    test_that("P-value is not empty", {
        expect_true(length(p_value) > 0)
    })
    # Check if the p-value is a number
    test_that("P-value is a number", {
        expect_true(is.numeric(p_value))
    })
    # Check if the p-value is between 0 and 1
    test_that("P-value is between 0 and 1", {
        expect_true(p_value >= 0 & p_value <= 1)
    })
}

# Check the iterative sampling function for numerical values
test_iterative_sampling_num <- function(file) {
    # Load the dataset
    data <- read.csv(file)
    # Preprocess the dataset
    data_pros <- preprocess(data) # nolint
    # Run the iterative sampling function
    # Test that iterative_sampling_num doesn't throw an error
    expect_no_error(iterative_sampling_num(data_pros, "white_rating")) #nolint
    selected_var <- iterative_sampling_num(data_pros, "white_rating") # nolint
    # Check if the selected variable is not empty
    test_that("Selected variable is not empty", {
        expect_true(length(selected_var) > 0)
    })
    # Check if the selected variable is a character
    test_that("Selected variable is a character", {
        expect_true(is.character(selected_var))
    })
    # Check if the selected variable is in the dataset
    test_that("Selected variable is in the dataset", {
        expect_true(selected_var %in% colnames(data_pros))
    })
}

# Check the iterative sampling function for categorical values
test_iterative_sampling_cat <- function(file) {
    # Load the dataset
    data <- read.csv(file)
    # Preprocess the dataset
    data_pros <- preprocess(data) # nolint
    # Run the iterative sampling function
    expect_no_error(selected_var <- iterative_sampling_cat(data_pros, "rated")) #nolint
    # Check if the selected variable is not empty
    test_that("Selected variable is not empty", {
        expect_true(length(selected_var) > 0)
    })
    # Check if the selected variable is a character
    test_that("Selected variable is a character", {
        expect_true(is.character(selected_var))
    })
    # Check if the selected variable is in the dataset
    test_that("Selected variable is in the dataset", {
        expect_true(selected_var %in% colnames(data_pros))
    })
}

# run the function test_dataset on the file games.csv
test_dataset("games.csv")

# run the function test_preprocess on the file games.csv
test_preprocess("games.csv")

# run the function test_model_train on the file games.csv
test_train_model("games.csv")

# run the function test_chisq_test on the file games.csv
test_chisq_test("games.csv")

# run the function test_anova_test on the file games.csv
test_anova_test("games.csv")

# run the function test_iterative_sampling_num on the file games.csv
test_iterative_sampling_num("games.csv")

# run the function test_iterative_sampling_cat on the file games.csv
test_iterative_sampling_cat("games.csv")
