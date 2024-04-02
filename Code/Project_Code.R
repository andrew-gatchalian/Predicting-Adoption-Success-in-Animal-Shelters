# Predictive Analytics Final Project
# BA 288 Winter 2024
# Team 2: Andrew Gatchalian, Davidson Rajasekar, 
# Devin Xiang Tian, Kaidi Huang, Yuanhui Yao

setwd("/Users/andrewgatchalian/Documents/UCI MSBA 24/Winter Quarter/BA 288 Predictive Analytics/Final Project/data")

####################################################
######## EXPLORATORY DATA ANALYSIS #################
###################################################
data <- read.csv("cat_data_cleaned_updated_EDA.csv")

library(dlookr)
library(dplyr)
library(psych)
library(ggplot2)

#shape
print(ncol(data))
print(nrow(data))

#sample
data_sample <- data %>% sample_n(5)
print(data_sample)

# Get column names and data types
col_names <- names(data)
col_names
col_types <- sapply(data, class)

# Get non-null counts for each column
non_null_counts <- sapply(data, function(x) sum(!is.na(x)))

# Create a dataframe to display the summary
df_info <- data.frame(Column_Name = col_names,
                      Data_Type = col_types,
                      Non_Null_Count = non_null_counts)

print(df_info)

#Our independent variable is whether the cat is adopted or not. 
#Thus, we will use that as the focus of our visualisations

# Count plot of adopted vs not adopted
plot1 <- ggplot(data = data, aes(x = is_adopted, fill = is_adopted)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Not Adopted" = "red", "Adopted" = "green")) +
  theme_minimal() +
  ggtitle("Outcome Type Counts") +
  labs(y = "Outcome Type", x = "Count") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

# Show the plot
print(plot1)

# Function to create plot for each column that isn't time related
create_plot <- function(column) {
  plot_data <- data.frame(column = data[[column]], is_adopted = data$is_adopted)
  if (is.numeric(plot_data$column)) {
    plot <- ggplot(plot_data, aes(x = column, fill = is_adopted)) +
      geom_bar(position = "dodge", color = "black") +
      labs(title = paste("Adoption Status by", column),
           x = column,
           y = "Count") +
      theme_minimal() +
      geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
      scale_x_continuous(labels = scales::comma) +  # Format numeric labels with comma
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  } else {
    plot <- ggplot(plot_data, aes(x = factor(column), fill = is_adopted)) +
      geom_bar(position = "dodge", color = "black") +
      labs(title = paste("Adoption Status by", column),
           x = column,
           y = "Count") +
      theme_minimal() +
      geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  return(plot)
}

# Create plots for each column
plots <- lapply(c("sex", "Spay.Neuter", "Cat.Kitten..outcome.", 
                  "breed1", "cfa_breed", 
                  "coat_pattern", "coat", "has_name",
                  "is_shorthair", "is_solid_pattern", "color"), create_plot)

# Print plots
print(plots)

#ADOPTION BY HOUR
# Filter data to include only rows where the cat is adopted
adopted_data <- data[data$is_adopted == "Adopted", ]

# Create plot data
plot_data <- data.frame(outcome_hour = adopted_data$outcome_hour)

# Aggregate data by hour and count the number of adopted cats
hour_counts <- aggregate(is_adopted ~ outcome_hour, adopted_data, FUN = length)

# Create line plot
plot_hour <- ggplot(hour_counts, aes(x = outcome_hour, y = is_adopted)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Adopted Cats by Hour",
       x = "Hour of the Day",
       y = "Number of Adopted Cats") +
  theme_minimal()

# Print plot
print(plot_hour)


#ADOPTION BY WEEKDAY
library(ggplot2)

# Filter data to include only rows where the cat is adopted
adopted_data <- data[data$is_adopted == "Adopted", ]

# Create plot data
plot_data <- data.frame(outcome_weekday = adopted_data$outcome_weekday)

# Aggregate data by weekday and count the number of adopted cats
weekday_counts <- aggregate(is_adopted ~ outcome_weekday, adopted_data, FUN = length)

# Reorder weekdays to display in the correct order
weekday_counts$outcome_weekday <- factor(weekday_counts$outcome_weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Create bar plot
plot_weekday <- ggplot(weekday_counts, aes(x = outcome_weekday, y = is_adopted)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = is_adopted), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Number of Adopted Cats by Weekday",
       x = "Weekday",
       y = "Number of Adopted Cats") +
  theme_minimal()

# Print plot
print(plot_weekday)

#ADOPTION BY SEASON

# Filter data to include only rows where the cat is adopted
adopted_data <- data[data$is_adopted == "Adopted", ]

# Create plot data
plot_data <- data.frame(season = adopted_data$season)

# Aggregate data by season and count the number of adopted cats
season_counts <- aggregate(is_adopted ~ season, adopted_data, FUN = length)

# Reorder seasons to display in the correct order
season_counts$season <- factor(season_counts$season, levels = c("Spring", "Summer", "Fall", "Winter"))

# Create bar plot with count labels
plot_season <- ggplot(season_counts, aes(x = season, y = is_adopted)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = is_adopted), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Number of Adopted Cats by Season",
       x = "Season",
       y = "Number of Adopted Cats") +
  theme_minimal()

# Print plot
print(plot_season)

#OUTCOME AGE YEARS

#ADOPTED

# Filter data to include only rows where the cat is adopted
adopted_data <- data[data$is_adopted == "Adopted", ]

# Create violin plot
plot_age_violin <- ggplot(adopted_data, aes(x = is_adopted, y = outcome_age_.years., fill = is_adopted)) +
  geom_violin() +
  labs(title = "Age Distribution of Adopted Cats",
       x = "Adoption Status",
       y = "Age (Years)") +
  scale_fill_manual(values = c("Adopted" = "green")) +
  theme_minimal()

# Print plot
print(plot_age_violin)

#NOT ADOPTED

# Filter data to include only rows where the cat is not adopted
adopted_data <- data[data$is_adopted != "Adopted", ]

# Create violin plot
plot_age_violin <- ggplot(adopted_data, aes(x = is_adopted, y = outcome_age_.years., fill = is_adopted)) +
  geom_violin() +
  labs(title = "Age Distribution of Adopted Cats",
       x = "Adoption Status",
       y = "Age (Years)") +
  scale_fill_manual(values = c("Not Adopted" = "grey")) +
  theme_minimal()

# Print plot
print(plot_age_violin)

# Create an empty list to store chi-squared test results
chi_squared_results <- list()

# List of categorical variables
categorical_variables <- c("sex", "Spay.Neuter", "Cat.Kitten..outcome.", "outcome_weekday", 
                           "breed1", "cfa_breed", "coat_pattern", "coat", "has_name", 
                           "season", "is_weekend", "time_of_day", "is_shorthair", 
                           "is_solid_pattern", "color")

# Perform chi-squared test for each categorical variable
for (variable in categorical_variables) {
  # Create a contingency table
  contingency_table <- table(data[[variable]], data$is_adopted)
  
  # Perform chi-squared test
  chi_squared_results[[variable]] <- chisq.test(contingency_table)
}

# Print the results
for (variable in categorical_variables) {
  print(paste("Chi-squared test for", variable))
  print(chi_squared_results[[variable]])
}

# Chi-squared test results summary

# Adoption status showed significant associations with all tested variables, 
#including sex, spay/neuter status, cat/kitten outcome, outcome weekday, breed, CFA breed, 
#coat pattern, coat color, name availability, season, weekend, time of day, shorthair status, 
#solid coat pattern, and color (p < 0.05).


###############################################
############ ANALYSIS ########################
##############################################

# Predicting Adoption Success in Animal Shelters

set.seed(123)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(dplyr)
library(fpp3)

dat <- read.csv("cat_data_cleaned_updated.csv")

# PRE-PROCESSING

#remove animal_id column
dat$animal_id <- NULL 
names(dat)

# Categorical variables
# "season"
# "time of day"
# "color"

# remove one variable for each categorical variable
dat$season_Fall <- NULL 
dat$time_of_day_Closed <- NULL 
dat$color_other <- NULL 

# summary of all variables in data
summary(dat)

# dependent variable: is_adopted
table(dat$is_adopted)
hist(dat$is_adopted, breaks=c(-0.5, 0.5, 1.5, 0.4), 
     col="beige", main="Histogram of Cat Adoptions",
     xlab="is_adopted", ylab="Frequency", xaxt='n')
axis(1, at=c(0, 1), labels=c("0 = All Else", "1 = Adopted"))


#  Look at the correlations of all variables with the 
#    Adoption (is_adoption) variable 1 = cat is adopted;  
#    0 = cat is not adopted (either euthanized or transferred)
#
cor_matrix <- data.frame(cor(dat)[2,])
ggcorrplot(cor_matrix, type = "full", lab = TRUE) +
  ggtitle("Correlation Plot")
# highly correlated variables:
# has_name, spay_neuter

# general logistic regression model (no training)
lr.all <- glm(is_adopted ~ . , data = dat, 
                    family = "binomial")
sum.lr.all <- summary(lr.all)
sum.lr.all


### TRAINING AND TESTING
#  Create a 50/50 training set
table(dat$is_adopted)
dat.A <- dat[dat$is_adopted == 1,]
dat.notA <- dat[dat$is_adopted == 0,]
train.A <- sample(nrow(dat.A),1000)
train.notA <- sample(nrow(dat.notA),1000)
dat.train <- rbind(dat.A[train.A,],
                   dat.notA[train.notA,])
table(dat.train$is_adopted)
# Create a test data set similar to the training set
dat.notA.notsel <- dat.notA[-train.notA,]
dat.A.notsel <- dat.A[-train.A,]
test.notA <- sample(nrow(dat.notA.notsel),nrow(dat.A.notsel))
dat.test <- rbind(dat.A[-train.A,], 
                  dat.notA.notsel[test.notA,])
# Check distribution
mean(dat.test$is_adopted)
mean(dat$is_adopted)
table(dat.test$is_adopted)/nrow(dat.test)
table(dat$is_adopted)/nrow(dat)
#  Remove unneeded objects
rm(dat.A, dat.notA, dat.notA.notsel)
rm(test.notA, train.A, train.notA)
#######

# Logistic Regression TRAINING Model
lr.all.train <- glm(is_adopted ~ . , data = dat.train, 
                    family = "binomial")
sum.lr.all.train <- summary(lr.all.train)
sum.lr.all

#  Compute a percentage reduction in deviance using 
#    the null and residual deviance

#  Null deviance = Deviance (value of -2 Log Likelihood) 
#    when the "naive" model is fit, that is, the model 
#    with just the intercept and no predictors
lr.naive.train <- glm(is_adopted ~ 1 , data = dat.train, 
                      family = "binomial")
lr.naive.train$deviance
lr.all.train$null.deviance
summary(lr.naive.train)

#  Residual deviance = Deviance with the current model
#    including all the predictors
DevRed <- 1 - (lr.all.train$deviance / 
                 lr.all.train$null.deviance)
DevRed

#  According this measure, the model reduced the 
#    null deviance by about 43.7%.  Remember this 
#    is not an R-squared measure-so it 
#    can be not be interpreted as such

#  Try a simpler version of the logistic regression 
#    using most of the highly significant variables 
#    from the "all in" model.
sum.lr.all
sum.lr.all.train

# Logistic Regression MODEL 2 (simplified)
lr.2.train <- glm(is_adopted ~ . - cfa_approved - season_Summer
                  - is_shorthair - color_black - color_white,
                  data = dat.train, 
                  family = "binomial")
sum.lr.2.train <- summary(lr.2.train)
sum.lr.2.train

lr.all.train$deviance 
lr.2.train$deviance 
# slightly higher deviance in model 2

lr.all.train$df.null 
lr.all.train$df.residual
lr.2.train$df.residual 
# slightly higher residiual in model 2


#  The Likelihood Ratio Test
#     Ho:  Models fit equally well
#     Ha:  Model 2 is just as good as Model 1
anova(lr.2.train, lr.all.train, test = "Chisq")

# p-value: 0.2316
# FAIL to reject the null hypothesis
# Simple model is just as good as all in model


#  Check performance of these two models on training data
yhat.all.train <- predict(lr.all.train, dat.train, 
                          type = "response")  
yhat.all.train.cl <- ifelse(yhat.all.train > 0.5, 1, 0)
tab.all.train <- table(dat.train$is_adopted, 
                       yhat.all.train.cl, 
                       dnn = c("Actual","Predicted"))
tab.all.train
train.all.err <- mean(yhat.all.train.cl != 
                        dat.train$is_adopted)
train.all.err # 0.1945, 19.45%
# Model 1 All-in training accuracy: 80.55%

#  Create confusion matrix and error on the training 
#    data for model 2
yhat.2.train <- predict(lr.2.train, dat.train, 
                        type = "response")  
yhat.2.train.cl <- ifelse(yhat.2.train > 0.5, 1, 0)
tab.2.train <- table(dat.train$is_adopted, yhat.2.train.cl, 
                     dnn = c("Actual","Predicted"))
tab.2.train
train.2.err <- mean(dat.train$is_adopted != yhat.2.train.cl)
train.2.err # 0.1935, 19.35%
# Model 2 Training Accuracy: 80.65%

#  Now, create confusion matrix and compute the error 
#    on the TEST data.  First use the "all-in" model:
yhat.all.test <- predict(lr.all.train, dat.test, 
                         type = "response")
yhat.all.test.cl <- ifelse(yhat.all.test > 0.5, 1, 0)
tab.all.test <- table(dat.test$is_adopted, yhat.all.test.cl, 
                      dnn = c("Actual","Predicted"))
tab.all.test
test.all.err <- mean(dat.test$is_adopted != 
                       yhat.all.test.cl)
test.all.err # 0.1886217, 18.86%
# Model 1 All-in Testing Accuracy: 81.14%

#  Compute confusion matrix and error for test data using Model 2
yhat.2.test <- predict(lr.2.train, dat.test, 
                       type = "response")
yhat.2.test.cl <- ifelse(yhat.2.test > 0.5, 1, 0)
tab.2.test <- table(dat.test$is_adopted, yhat.2.test.cl, 
                    dnn = c("Actual","Predicted"))
tab.2.test
test.2.err <- mean(dat.test$is_adopted != yhat.2.test.cl)
test.2.err # 0.1861164, 18.61%
# Model 2 Testing Accuracy: 81.39%

# All-in model shows training and test errors at 19.45% and 18.86% respectively.
# Model 2 shows training and test errors at 19.35% and 18.61% respectively.
# Model 2 (simplified) is BETTER

##### RIDGE REGRESSION and LASSO
library(glmnet)
set.seed(123)

X.train <- model.matrix(is_adopted~., dat.train)[,-1]
y.train <- dat.train$is_adopted
X.test <- model.matrix(is_adopted~., dat.test)[,-1]
y.test <- dat.test$is_adopted

#### for plotting ridge regression
y <- dat$is_adopted
X <- model.matrix(is_adopted~., dat)[,-1]

grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(X.train, y.train, alpha = 0, family = "binomial",
                    lambda = grid, thresh = 1e-12)

ridge.coeff <- matrix(0, nrow = ncol(X), ncol = 100)
ridge.pred <- matrix(0,nrow = length(y.test), ncol = 100)
testerr <- matrix(0, nrow = 100, ncol = 1)

for (j in 1:100) {
  ridge.coeff[,j] <- ridge.mod$beta[,j]
  ridge.pred[,j] <- predict(ridge.mod, s = grid[j], type = "response",
                            newx = X.test)
  testerr[j] <- mean((ridge.pred[,j] - y.test)^2)
}

plot(testerr, xlab = "Model Number", 
     ylab = "Test Mean Suqare Error",
     main = "Ridge Regression",
     )
which.min(testerr)
# model 100 has the lowest MSE
ridge.mod$lambda[100] 
# Lambda = 0.01

##################

# Let's use cross-validation to determine the best lambda to use in ridge regression
cv.out <- cv.glmnet(X.train, y.train, alpha = 0, family = "binomial")
bestlam <- cv.out$lambda.min
bestlam
# best lambda = 0.02544792

# fit the ridge regression model
ridge.mod.cv <- glmnet(X.train, y.train, alpha = 0, family = "binomial", 
                    lambda = bestlam)

# predict on the testing data
ridge.pred.cv <- predict(ridge.mod.cv, s=bestlam, newx = X.test, type = "response")

MSE.R.CV <- mean((ridge.pred.cv-y.test)^2)
RMSE.R.CV <- MSE.R.CV^0.5
RMSE.R.CV
# RMSE.R.CV = 0.359061

#  Compute confusion matrix and error for ridge regression using best lambda
yhat.ridge.cl <- ifelse(ridge.pred.cv > 0.5, 1, 0)
tab.4.test <- table(dat.test$is_adopted, yhat.ridge.cl, 
                    dnn = c("Actual","Predicted"))
tab.4.test
test.4.err <- mean(dat.test$is_adopted != yhat.ridge.cl)
test.4.err # 0.1888039, 18.88%
# Ridge Regression (best lambda) Testing Accuracy: 81.12%
# Logistic Regression Model 2 Testing Accuracy: 81.39%

### LASSO regression
# plotting LASSO coefficients that are being driven to 0
lasso.mod <- glmnet(X.train, y.train, alpha=1,  family = "binomial", 
                    lambda=grid, thresh = 1e-12)
plot(lasso.mod, xvar="lambda", label = FALSE, lwd = 1.5,
     xlim=c(-4.2, 1.5), main = "LASSO Coefficients")

var_names <- colnames(X.train)
legend("topright", legend = var_names, col = 1:ncol(coef(lasso.mod)), lty = 1,
       cex = 0.8, lwd = 4)

# Let's use cross-validation to determine the best lambda for LASSO
cv.out1 <- cv.glmnet(X.train, y.train,  family = "binomial", alpha = 1)
coef(cv.out1) # for reference on which variables were driven to 0 by LASSO
# plot LASSO
plot(cv.out1, main = "")

# find best lambda for LASSO
bestlam1 <- cv.out1$lambda.min
bestlam1

# predict on testing data
lasso.pred.cv <- predict(lasso.mod, s=bestlam1, type = "response",
                      newx = X.test)
MSE.L.CV <- mean((lasso.pred.cv-y.test)^2)
RMSE.L.CV <- MSE.L.CV^0.5
RMSE.L.CV
# RMSE.L.CV (lasso) = 0.3576907
RMSE.R.CV
# RMSE.R.CV (ridge) = 0.359061

# RMSE is around the same for LASSO and Ridge. LASSO slightly better.

#  Compute confusion matrix and error for LASSO using best lambda
yhat.lasso.cl <- ifelse(lasso.pred.cv > 0.5, 1, 0)
tab.5.test <- table(dat.test$is_adopted, yhat.lasso.cl, 
                    dnn = c("Actual","Predicted"))
tab.5.test
test.5.err <- mean(dat.test$is_adopted != yhat.lasso.cl)
test.5.err # 0.1902615, 19.03%
# LASSO Regression (best lambda) Testing Accuracy: 80.97%
# Ridge Regression (best lambda) Testing Accuracy: 81.12%
# Logistic Regression Model 2 Testing Accuracy: 81.39%

# Logistic Regression Model 1 (All-in)
test.all.err 
# Logistic Regression Model 2 (simplified model)
test.2.err 
# Ridge Regression
test.4.err 
# LASSO Regression
test.5.err

# Both models perform the same:
# These are the coefficients being driven to 0 by LASSO
coef(cv.out1)
# season_Summer, color_black, color_white

#### CONCLUSION
# According to the results, the simplified logistic regression gives us the best accuracy
# and trade off for model complexity and interpretability.

sum.lr.2.train

# BEST MODEL!!!
# Logistic Regression Model 2 Simplified (10 predictors)
coef(lr.2.train)

####### FORECASTING ########
## ETS
setwd("/Users/andrewgatchalian/Documents/UCI MSBA 24/Winter Quarter/BA 288 Predictive Analytics/Final Project/data")

dat.f <- read.csv("cat_data_cleaned_updated_forecast.csv")
dat.f <- dat.f[-53, ] # last month is outlier, # of observations ends
dat.f$Date <- as.Date(dat.f$Date, format = "%m/%d/%Y")
ADPts <- ts(dat.f[,2], start = c(2013, 10), frequency = 12)
ADPtts <- as_tsibble(ADPts)
names(ADPtts)[2] <- "Adopted"
str(ADPtts)

autoplot(ADPtts, Adopted) +
  labs(y = "Adopted", title = "Animal Shelter Cat Adoptions",
       x = "Month") 

# best ETS model
fit_ETS <- model(ADPtts, ETS(Adopted))
fit_ETS
accuracy(fit_ETS) # 105

# parameters
report(fit_ETS)

gg_tsresiduals(fit_ETS) +
  ggtitle("fit_ETS")

# Display the model components (chart)
aug_ETS <- augment(fit_ETS)
autoplot(aug_ETS, Adopted) +
  autolayer(aug_ETS,.fitted, colour = "Red") +
  autolayer(aug_ETS,.resid, colour = "Green") +
  labs(y = "Adopted", title = "ETS Residuals",
       x = "Month")

# forecast next year
forc_ETS <- forecast(fit_ETS, h = 12)
forc_ETS

######## ARIMA
library(fpp3)

fit_ARIMA <- model(ADPtts, ARIMA(Adopted))
accuracy(fit_ARIMA) # RMSE 104
report(fit_ARIMA)

gg_tsresiduals(fit_ARIMA)+
  ggtitle("fit_ARIMA")

aug_ARIMA <- augment(fit_ARIMA)
aug_ARIMA

autoplot(aug_ARIMA, Adopted) +
  autolayer(aug_ARIMA,.fitted, colour = "Red") +
  autolayer(aug_ARIMA,.resid, colour = "Green") +
  labs(y = "Adopted", title = "ARIMA Residuals",
       x = "Month")

forc_ARIMA <- forecast(fit_ARIMA, h = 12)

# plot ARIMA forecast
autoplot(forc_ARIMA, ADPtts, level = NULL, colour = "Red") +
  labs(y = "Adopted", title = "Cat Adoptions Forecast (ARIMA)",
       x = "Month")

# plot ETS forecast
autoplot(forc_ETS, ADPtts, level = NULL, colour = "Blue") +
  labs(y = "Adopted", title = "Cat Adoptions Forecast (ETS)",
       x = "Month")

# plot both forecast
autoplot(forc_ETS, ADPtts, level = NULL, colour = "Blue") +
  autolayer(forc_ARIMA, ADPtts, level = NULL, colour = "Red") + 
  labs(y = "Adopted", title = "Cat Adoptions 1 Year Forecast",
       x = "Month", subtitle = "ARIMA (Red) & ETS (Blue)")

forc_ARIMA # 230 adoptions
forc_ETS # 214 adoptions

fit_ARIMA # <ARIMA(0,0,0) w/ mean>
fit_ETS # <ETS(A,N,N)>

accuracy(fit_ARIMA)
accuracy(fit_ETS)

#################
#### Tree
#tree model without any feature selection and parameter tunning 
library(tree)
dat.train[,2] <- as.factor(dat.train[,2])
dat.test[,2] <- as.factor(dat.test[,2])
str(dat.train)

tree1 <- tree(is_adopted~., data = dat.train)
summary(tree1)
#number of terminal nodes is 8
#residual mean deviance is around 0.7857
#error rate is 0.1955
plot(tree1)
text(tree1, pretty = 0)

#spay_neuter is the variable has the most predicting power 
#the ranking of the importance of features are 
# 1. spay_neuter
# 2. has_name
# 3. is_kitten
# 4. sex_male
# 5. is_weekend 

tree1.pred.tr <- predict(tree1, dat.train, type = "class")
table(dat.train$is_adopted, tree1.pred.tr,
      dnn = c("Actual", "Predicted"))
err.tree.tr <- mean(dat.train$is_adopted != tree1.pred.tr) #0.2
err.tree.tr # 0.1955, 19.55%
# Tree TRAINING Accuracy 80.45%

tree.pred.tst <- predict(tree1, dat.test, type = "class")
table(dat.test$is_adopted, tree.pred.tst,
      dnn = c("Actual", "Predicted"))
err.tree.tst <- mean(dat.test$is_adopted != tree.pred.tst) #0.198
err.tree.tst #0.1932677, 19.33%
# Tree TESTING Accuracy 80.67%

# Lets see if we can improve tree by pruning 
prune1 <- prune.misclass(tree1)
names(prune1)

plot(prune1)
plot(prune1$size, prune1$dev, xlab = "Size of Tree",
     ylab = "Deviation")

prune.tree1 <- prune.misclass(tree1, best=5 )
summary(prune.tree1)
prune.tree1
plot(prune.tree1)
text(prune.tree1, pretty = 0)

tree2.pred.tr <- predict(tree1, dat.train, type = "class")
table(dat.train$is_adopted, tree2.pred.tr,
      dnn = c("Actual", "Predicted"))
err.ptree.tr <- mean(dat.train$is_adopted != tree2.pred.tr)
err.ptree.tr #0.1955, 19.55%
# Pruned Tree TRAINING Accuracy 80.45% (same as un-pruned tree)

tree2.pred.tst <- predict(tree1, dat.test, type = "class")
table(dat.test$is_adopted, tree.pred.tst,
      dnn = c("Actual", "Predicted"))
err.ptree.tst <- mean(dat.test$is_adopted != tree2.pred.tst)
err.ptree.tst # 0.1932677, 19.33%
# Pruned Tree TESTING Accuracy 80.67% (same as un-pruned tree)

# BOTH Trees have the same accuracy.

############## RANDOM FOREST
library(randomForest)
library(caret)
library(future)
set.seed(123)

metric = 'Accuracy'
# Create a custom RF to optimize mtry and ntree
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:6), .ntree=c(50,100,150))
custom <- train(is_adopted~., data=dat.train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom) # best mtry = 3, ntree = 50

# RF model using best parameters
rf.model <- randomForest(is_adopted ~ ., data = dat.train, ntree = 50, mtry = 3, nodesize = 5)
rf.model     

# Feature importance plot
varImpPlot(rf.model)

# Predict on testing data
rf.pred.tst <- predict(rf.model, newdata = dat.test)
table(dat.test$is_adopted, rf.pred.tst,
      dnn = c("Actual", "Predicted"))
err.rf.tst <- mean(dat.test$is_adopted != rf.pred.tst)
err.rf.tst #0.1755033, 17.55%
# Random Forest TESTING Accuracy 82.45%

##### Support Vector Machines SVM
# Build model
library(e1071)
svm.model <- svm(is_adopted ~ ., data = dat.train, kernel = 'linear', cost = 10)
# Prediction on training data
svm.pred.tr <- predict(svm.model, dat.train)
table(dat.train$is_adopted, svm.pred.tr,
      dnn = c("Actual", "Predicted"))
err.svm.tr <- mean(dat.train$is_adopted != svm.pred.tr)
err.svm.tr #0.19, 19%
# SVM TRAINING Accuracy 81% 

# Prediction on testing data
svm.pred.tst <- predict(svm.model, dat.test)
table(dat.test$is_adopted, svm.pred.tst,
      dnn = c("Actual", "Predicted"))
err.svm.tst <- mean(dat.test$is_adopted != svm.pred.tst)
err.svm.tst #0.1875285, 18.75%
# SVM TESTING Accuracy 81.25% 

err.svm.tst # SVM TESTING Accuracy 81.25% 

########
#### we have explored the accuracy of all of our models
#### the models are all similar in performance
#### we have selected our favorite models which we feel have a good
#### trade off for fit and interpretability:
#### Random Forest and Decision Tree

# with this in mind, lets try slightly changing our data to fit our model
# and use a CONTINOUS variable for AGE instead of binary


#### TREE using continous variable
dat <- read.csv("cat_data_cleaned_updated_continous_age.csv")
#remove animal_id
dat$animal_id <- NULL 
# remove one variable for each categorical variable
dat$season_Fall <- NULL 
dat$time_of_day_Closed <- NULL 
dat$color_other <- NULL 
set.seed(123)
#build the tree with all dataset and features 
library(tree)
#set up dependent variable as factor
dat$is_adopted <- as.factor(dat$is_adopted)
tree.all <- tree(is_adopted~., dat)
summary(tree.all)

#splitting training and testing dataset 
table(dat$is_adopted)
dat.A <- dat[dat$is_adopted == 1,]
dat.notA <- dat[dat$is_adopted == 0,]
train.A <- sample(nrow(dat.A),1000)
train.notA <- sample(nrow(dat.notA),1000)
dat.train <- rbind(dat.A[train.A,],
                   dat.notA[train.notA,])
table(dat.train$is_adopted)
# Create a test data set similar to the training set
dat.notA.notsel <- dat.notA[-train.notA,]
dat.A.notsel <- dat.A[-train.A,]
test.notA <- sample(nrow(dat.notA.notsel),nrow(dat.A.notsel))
dat.test <- rbind(dat.A[-train.A,], 
                  dat.notA.notsel[test.notA,])
table(dat.test$is_adopted)

#  Remove unneeded objects
rm(dat.A, dat.notA, dat.notA.notsel)
rm(test.notA, train.A, train.notA)

#build a tree on training dataset
tree.train.1 <-tree(is_adopted~., dat.train)
summary(tree.train.1)

plot(tree.train.1)
text(tree.train.1, pretty = 0)

sum.tree.train.1 <- summary(tree.train.1)
sum.tree.train.1$misclass
err.tree.train.1 <- sum.tree.train.1$misclass[1]/
  sum.tree.train.1$misclass[2]
err.tree.train.1 #0.1555

#tree splits at the spay_neuter first, if the cat not neutered and does not have a name,
#the cat is 100% would not be adopted according to this tree model. 

#at the age node, it seems like if a cat is yonger than half years, then there is a big
#chance of this cat being adopted. 

#model's performance on training dataset
yhat.tree.train.1 <- predict(tree.train.1, dat.train)
head(yhat.tree.train.1)
yhat.tree.train.1.cl <- 
  ifelse(yhat.tree.train.1[,2] > 0.5, 1, 0)
tab.tree.train.1 <- table(dat.train$is_adopted, yhat.tree.train.1.cl,
                          dnn = c("Actual", "Predicted"))
tab.tree.train.1
mean(dat.train$is_adopted != yhat.tree.train.1.cl) #0.1555

#model's performance on testing dataset 
yhat.tree.test.1 <- predict(tree.train.1, dat.test)
yhat.tree.test.1.cl <- 
  ifelse(yhat.tree.test.1[,2] > 0.5, 1, 0)
tab.tree.test.1 <- table(dat.test$is_adopted, yhat.tree.test.1.cl,
                         dnn = c("Actual", "Predicted"))
tab.tree.test.1
mean(dat.test$is_adopted != yhat.tree.test.1.cl) #0.1612007
# TREE with continous variable Testing Accuracy: 83.88%


### RANDOM FOREST using continous variable
#Random Forest Models 
#number of features selected in each tree 
#sqrt of 15 , which is around 4
rf.train.1 <- randomForest(is_adopted ~ ., data = dat.train, 
                           mtry = 4, ntree = 100,  
                           importance = TRUE)
rf.train.1
yhat.rf.1 <- predict(rf.train.1, dat.test)
tab.rf.1 <- table(dat.test$is_adopted, yhat.rf.1)
tab.rf.1
err.rf.1 <- mean(dat.test$is_adopted != yhat.rf.1)
err.rf.1 #0.1499499 

rf.train.2 <- randomForest(is_adopted ~ ., data = dat.train, 
                           mtry = 4, ntree = 1000,  
                           importance = TRUE)
rf.train.2
yhat.rf.2 <- predict(rf.train.2, dat.test)
tab.rf.2 <- table(dat.test$is_adopted, yhat.rf.2)
tab.rf.2
err.rf.2 <- mean(dat.test$is_adopted != yhat.rf.2)
err.rf.2 #0.1497221
# RANDOM FOREST with continous variable Testing Accuracy: 85%

sort(importance(rf.train.2)[,3], decreasing = TRUE)[1:5]
#spay_neuter, outcome_age_.years, has_name, is_weekend, time_of_day_afternoon

#Gini importance 
sort(importance(rf.train.2)[,4], decreasing = TRUE)[1:5]
#outcome_age_.years, spay_neuter, has_name,time_of_day_afternoon,is_weekend



######
## As we can see adding a continous variable improves the accuracy of our models
## Whether these models are "better" can be up to choice, to determine the best
## trade-off for fit and interpretability.
## Overall, we prefer the Random Forest model with a continous age variable.

