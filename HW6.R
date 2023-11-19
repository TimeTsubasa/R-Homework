# Q1 Conceptual questions
# a. (True or False? Explain your answer.) 
# L2 loss (sometime with a regularization penalty) is widely used because
# it usually reflects the actual loss function for applications in business and science.

# This statement is often true. L2 loss, also known as mean squared error (MSE), measures the average squared difference between the predicted and actual values.
# However, while L2 loss is widely used and can reflect the actual loss function in many applications, it is not always the best choice and should be carefully considered in each context.

# b. (True or False? Explain your answer.) Ridge regression is ‘scale invariant’ in the sense that test set
# prediction accuracy is unchanged if one rescales the independent variables.

# False.

# Ridge regression is not "scale invariant" in the sense that test set prediction accuracy is not unchanged if one rescales the independent variables.

# When the independent variables are rescaled, the ridge regression coefficients will also be affected. In fact, the coefficients are directly proportional to the scale of the independent variables. 
# Rescaling the independent variables will lead to a different scaling of the coefficients, which in turn affects the test set predictions.
# However, the L2 penalty term in ridge regression is invariant to scaling of the independent variables. That is, the regularization parameter lambda is not affected by scaling of the independent variables, 
# and therefore the relative importance of the penalty term remains the same.  But the scaling of the independent variables still affects the coefficients and the predictions.

#C. Which one performs better, ridge or Lasso, when a few predictors have large coefficients (and the rest have
#very small coefficients)? How about when a lot of predictors have large coefficients? And what if all the
#predictors’ coefficients are roughly the same magnitude? Note any assumptions you make and explain your reasoning.
# When a few predictors have large coefficients (and the rest have very small coefficients), Lasso performs better than Ridge. 
# The Lasso's penalty term has a tendency to shrink the coefficients of the less important predictors towards zero, resulting in a sparse model that only includes the most relevant predictors. 

# On the other hand, Ridge regression performs better when a lot of predictors have large coefficients. This is because Ridge's penalty term shrinks all the coefficients towards zero, including the ones that are more important. 
# Therefore, Ridge is less likely to exclude important predictors from the model, and it can be useful when all the predictors are relevant and the model needs to be regularized to avoid overfitting.
# When all the predictors' coefficients are roughly the same magnitude, Ridge and Lasso perform similarly. In this case, both Ridge and Lasso will have similar effects on the model coefficients, 
# and the choice between the two methods may depend on other factors, such as interpretability or computational efficiency.

# When all the predictors' coefficients are roughly the same magnitude, Ridge and Lasso perform similarly. In this case, both Ridge and Lasso will have similar effects on the model coefficients, 
# and the choice between the two methods may depend on other factors, such as interpretability or computational efficiency.

# It's worth noting that these conclusions are based on some assumptions about the data and the models. For example, the performance of Ridge and Lasso may depend on the correlation structure between the predictors, the strength of the signal-to-noise ratio, and the choice of the regularization parameter. 
# Therefore, it's important to test and compare different models on the specific data at hand to determine which method performs better in a given situation.

#Q2 Applied question on KNN
# a. (3)
# Load the data and check the summary statistics. For this data, we will only keep 7 predictors: Age, Sex,
# Occupation, Account.Balance, Credit.amount, Length.of.current.employment (Duration) and Purpose. The
# response is Creditability, a binary response. Create an appropriate data frame for this subset.
library(readr)
GermanCredit <- read_csv("GermanCredit.csv")
CreditSubset<-GermanCredit[, c(Age = "Age (years)", Sex = "Sex & Marital Status", "Occupation", "Account Balance", "Credit Amount", 
                               Duration = "Length of current employment", "Purpose", "Creditability")]
summary(CreditSubset)
str(CreditSubset)
CreditSubset <- as.data.frame(CreditSubset) # Convert the tibble into Dataframe
str(CreditSubset)

#b. Do we need to standardize the data before using KNN? If yes, show it in your code and explain why, if not, explain why not.

# It's good to standardize the predictors before using KNN. 
# It helps to handle features that are on different scales: KNN uses the distance between data points to find the nearest neighbors. If some features are on a much larger scale than others, 
# they could dominate the distance calculation, giving them more weight in the determination of the nearest neighbors. 
# By standardizing the data, we transform all the features to the same scale, which can help to reduce the bias towards features with a larger scale.
ScaleCreditSubset = scale(CreditSubset)

#C. Use a fixed seed value “529” to split the data into a train (75%) and a test set (25%) using base R sub-setting operations, 
# call the resulting data subsets xtrain, xtest, ytrain, ytest. Show the length of your xtrain set. (Hint: sample() might be useful.)
set.seed(529)
n = nrow(ScaleCreditSubset)
# Split the data into train and test sets
train_index <- sample(1:n, size = 0.75*n)
xtrain <- ScaleCreditSubset[train_index, -8] # exclude response Creditability
xtest <- ScaleCreditSubset[-train_index, -8]
ytrain <- ScaleCreditSubset[train_index, 8]
ytest <- ScaleCreditSubset[-train_index, 8]
# Check the length of xtrain
nrow(xtrain)

# d. Sometimes a value of K is chosen to be the square root of total number of observations. We will use the
# knn() function from the “class” package. Fit the knn model using K=floor(sqrt(1000)). For the test set,
# show the confusion matrix (a 2x2 matrix where the diagonal elements indicate correct predictions, while the
# off-diagonals represent incorrect predictions) and the total test error rate. Use base R functions.
library(class)
# Fit the KNN model with k = floor(sqrt(1000))
k1 <- floor(sqrt((1000)))
knn_model <- knn(train = xtrain, test = xtest, ytrain, k = k1) # knn_model is the prediction
# Create the confusion matrix
confusion_matrix <- table(Actual = ytest, Predicted = knn_model)
confusion_matrix
# Calculate the total test error rate
aux = prop.table(table(Actual = ytest, Predicted = knn_model))
ErrorRate <- 1 - sum(diag(aux))
ErrorRate # 0.244, so the test error rate is small. 

# E. Put your code for part d) into a loop (or a function if you like). The loop structure is given. 
# Fill in the blanks. Identify the value your code returns for best K and plot K values vs. error rates. Explain why this looping approach is effective here.

# Define the range of K values to try
try_K = c(seq(21,31,2), seq(33,66,3))
try_K
error_rate = rep(0, length(try_K))#18
for (ik in 1:length(try_K)){
  # get the prediction for y
  ypred = knn(xtrain, xtest, ytrain,k =ik)
  aux2 = prop.table(table(Actual = ytest, ypred))
# get the error rate for this k
confusion_matrix = table(Actual = ytest, ypred)
error_rate[ik] = 1-sum(diag(aux2))
}
error_rate
best_idx = which.min(error_rate)
bestK = best_idx # The optimal k is also the index for the smallest element in error_rate vector.
bestK
# plot K values vs. error rates
par(mar = c(2,2,2,2))
xaxis = 1:length(try_K)
plot(error_rate,xlab = "No. of neighbors(K values)", ylab = "Error Rate")
# loop function is effective here because the best number of k is large, which is more than 10 (13). If we compute the k hand by hand, it will be time-consuming. 
# If the best K is more than 20 or 30, then it will be terrible for us to find the best k by hand. In this way, the loop function is quite useful. 

# Q3 Applied question on bootstrap (20pts)

# Step 1.
# Use a fixed seed value “529”. Create a ‘data’ item by drawing 500 observations 
# from a normal distribution with mean 10 and stdev 4, round each observation to the nearest integer.
set.seed(529)
data <- round(rnorm(500, mean=10, sd=4))
# Step 2.
# Create 30 (sets of) bootstrap samples with lapply()
BootSamples <- lapply(1:30, function(i)sample(data, replace = T))
# Calculate the median for each bootstrap sample using sapply() to generate a bootstrap distribution.
medians <- sapply(BootSamples, median)
# Show the mean of the first (set) of the bootstrap samples.
medians_mat <- matrix(medians, ncol = 1)
First_Mean <- mean(medians_mat[, 1])
First_Mean
#Compute summary statistics for the distribution of the medians
summary(medians)

# Step 3.
# Calculate the standard deviation of the distribution of medians.
sd(medians) # 0.4660916 in calculation vs sd = 4 in normal distribution. 

# Now compare
# R has many packages that implement bootstrapping. Pick one library and use their functions to do Q3 again.
# Do you get simliar results? Report your statistics. (for example, you can use the boot() function from the “boot” library.)
library(boot)
# create a function
boot_medians <- function(data, index) {
  return(median(data[index]))
}

boot_samples <- boot(data, boot_medians, R=30)
sd_boot_medians <- sd(boot_samples$t)

# Comparison
print(paste0("Standard deviation of medians using sapply(): ", sd(medians)))
print(paste0("Standard deviation of medians using boot(): ", sd_boot_medians))
# The results may not be exactly the same due to the random nature of the bootstrap procedure, but they should be similar.
# The standard deviation of medians using supply() is 0.466 while that of medians using boot function is 0.382. 