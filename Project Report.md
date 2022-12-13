# Project Report
Intro to Statistical Computing - STAT 610
Chess Outcome Prediction

Notes:

The github link is: https://github.com/abhijeetvichare76/chess-predictor

To run the test files, run the code: 
testthat::test_dir("."))
## Introduction
The goal of the project is to create a predictive model that predicts the outcome of the chess game based on the information before the game such 
as the players’ ratings, the type of match, the tournament. The data set is available at https://www.kaggle.com/datasnaek/chess. The data set contains
information about 20,000 chess games. 

<!-- describe the objectives, describe the design decisions, provide evidence that your code does what it should, and give a description of the final output. -->

Objectives
- To create a predictive model that predicts the outcome of the chess game based on the information before the game such as the players’ ratings, the type of match, the tournament.
- To create a pipeline that uses the data and performs the following steps:
  - Data cleaning
  - Data exploration
  - Data visualization
  - Model building
  - Model evaluation

Design Decisions
I have created a EDA notebook that performs the following steps:

1. Data Wrangling
    a. Data importing
    b. Checking the data types of variables and performing necessary steps to change the data type based on relevancy.
    c. Univariate Statistics of the variables
    d. Dealing with null values
    e. Outlier analysis
2. Data Visualization
    a. Bivariate analysis
    b. Correlation analysis
3. Feature
    a. Chi square test
    b. ANOVA test
    c. Correlation test
4. Linear Regression
    a. Linear Regression assumption validation
    b. Model training
    c. Hyperparameter tuning
    d. Regularization
    e. Result visualization

I have created a file "model_training.R" that performs the following steps:
- Prepare the data for model training
- Perform iterative feature selection for categorical and numerical variables
- Train the model using the selected features

I've also created a file "tests.R" that performs the following tests on the model training pipeline:
- Test the dataset that has been downloaded and has the required columns and data types
- Test the preprocessing function and the outputs of the function
- The chisquare test and the anova test
- The iterative sampling function for both the categorical and numerical variables
- The linear regression model training function


Finally the output is the model that is saved in the "model.RData" file. The EDA is also saved in the pdf output. 