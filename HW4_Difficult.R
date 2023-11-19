# (1)
library(readr)
CaliHousing <- read.csv("~/Graduate/ISE 529 Predicable Analysis/housing_10k.csv")
head(CaliHousing)
ocean_proximity = as.factor(CaliHousing$ocean_proximity)
str(CaliHousing)
names(CaliHousing)

#(2)
par(mfrow= c(3,3))
par(mar = c(2, 2, 2, 2))
hist(CaliHousing$longitude,main = 'Longitude',xlab = 'longitude')
hist(CaliHousing$latitude, main = 'Latitude',xlab = 'latitude')
hist(CaliHousing$housing_median_age,main = 'Median Age',xlab = 'median age')# Axis Labels
hist(CaliHousing$total_rooms,main = 'Total Rooms',xlab = 'total rooms')
hist(CaliHousing$total_bedrooms,main = 'Total Bedrooms',xlab = 'total bedrooms')
hist(CaliHousing$population,main = 'Population',xlab = 'population')
hist(CaliHousing$households,main = 'Households',xlab = 'households')
hist(CaliHousing$median_income,main = 'Median Income',xlab = 'median income')
hist(CaliHousing$median_house_value,main = 'Median Value',xlab = 'median value')
barplot(table(CaliHousing$ocean_proximity),main="Ocean Proximity")

summary(CaliHousing)
# The predictors who have outliers are: total_rooms, total_bedrooms, population, households and median_house_value.
# The maximum value of these predictors are much higher than the third quartile, so these maximum values must be outliers. 

# Comment on all plot features that we have discussed in class
# Distribution
# From the plot, we see that Longitude and Latitude have bimodal distribution.
# Median Age is nearly normal distribution, so it is nearly symmetrical. 
# The others are right-tails.
# The mode of Longitude Latitude,population and household are much higher than other observations, as they have outliers in the data. 

# Discuss any features that stand out as potentially problematic for linear regression.
#   One must be the outlier which could have a significant impact on the linear regression model to change its trend.
#   Nonlinearity: Linear regression assumes a linear relationship between the predictors and the response variable. 
# However, some of the predictors such as housing_median_age may not have a linear relationship with median_house_value. 
# In such cases, linear regression may not be the most appropriate model.
#   Multicollinearity: Some of the predictors in the dataset, such as total_rooms and total_bedrooms, are highly correlated with each other.
#This could lead to multicollinearity, which can make it difficult to interpret the coefficients of the linear regression model.
#   Categorical predictors: The ocean_proximity variable in the dataset is a categorical variable. 
#To include categorical variables in a linear regression model, they need to be converted to factors. 

#(3)
FitModel <- lm(median_house_value~longitude+latitude+housing_median_age+
                 total_rooms+total_bedrooms+population+households+ 
                 median_income+ocean_proximity,data = CaliHousing)
summary(FitModel)
# From summary, we see that the p-value of categorical variable of Near Ocean is larger than 0.05, which means it has no significant relationship with 1H Ocean and medium_housing_value.
# The P-values of all other predictors are small, so they are statistically significant on the medium_house_value. 

# Predictors such as housing_median_age,total_bedrooms,households and median_income,Ocean_proximityISLAND are positively affect median household value,
# while longitude,latitude,total_rooms,population,Ocean ProximityINlAND, ProximityNear Bay proximity are negatively affect median_house_value. 

pairs(~ median_house_value + longitude+latitude+housing_median_age+
        total_rooms+total_bedrooms+population+households+ median_income+as.factor(ocean_proximity),data = CaliHousing)

plot(longitude~latitude,data = CaliHousing) # Check
#(4)
library(carData)
library(car)
vif(FitModel) # The rule of thumb for VIF is that if any variable has a VIF larger than 5, then there might be issues with multicollinearity in the model.
#  if any variable has a VIF larger than 10, it is extremely likely that there is strong multicollinearity in the model.
# The variables whose VIFs are greater than 5 are: total_bedrooms and households. 

# "A simple way to detect collinearity is to look at the correlation matrix of the predictors".
# “Instead of inspecting the correlation matrix,a better way to assess multicollinearity is to compute the variance inﬂation factor(VIF).”
# "When faced with the problem of collinearity,there are two simple solutions.
# The ﬁrst is to drop one of the problematic variables from the regression."
#"The second solution is to combine the collinear variables together in to a single predictor."

#(5)
# I choose to drop one variable which is households. 
FitModel2 <- lm(median_house_value~longitude+latitude+housing_median_age+
                              total_rooms+total_bedrooms+population+ 
                              median_income+ocean_proximity,data = CaliHousing)
summary(FitModel2)
vif(FitModel2)
# At this time, the vif of total_bedroom drop to 3 < 5. All VIFs of all variables are smaller than 5, which indicates that there are no significant multicollinearity in the model.
# Also, the multiple Rsquare drops from 0.6506 to 0.6498. 

# (6)
par(mar = c(5, 4, 4, 5))
plot(fitted(FitModel2),resid(FitModel2),xlab = 'Fitted values',ylab = 'Residuals')
abline(0,0,col="red")

# from the residuals vs ﬁtted plot, we see
# There is a clear curve in the plot, indicating that the relationship between the predictors and the response may not be linear.
# There is a slight funnel shape in the plot, which indicates that the variance of the residuals may not be constant across the range of the predictors.
# There are a few outliers in the upper left corner of the plot, which may have a large influence on the regression coefficients.
# Therefore, the new model can be improved.
# Improvements:
# Add polynomial terms or interaction terms to account for the curvature in the relationship between the predictors and the response.
# Use a transformation such as log-transform or square-root transform to make the relationship more linear.
qqnorm(resid(FitModel2))
qqline(resid(FitModel2),col='red')

FitModel3 <- lm(median_house_value~longitude+latitude+housing_median_age+
                  total_rooms+total_bedrooms+population+ 
                  median_income+ocean_proximity+
                  longitude*latitude*housing_median_age*total_rooms*total_bedrooms*population*median_income*as.factor(ocean_proximity),data = CaliHousing)
plot(FitModel3)
par(mar = c(1,1,1,1))
plot(fitted(FitModel3),resid(FitModel3),xlab = 'Fitted values',ylab = 'Residuals')
abline(0,0,col="red")
# It is a little better. Anyway. 
qqnorm(resid(FitModel3))
qqline(resid(FitModel3),col='red')

#(7)
remove.packages('olsrr')
library(usethis)
library(devtools)
library(olsrr)

ols_step_forward_p(FitModel2, penter = 0.01, penterv = 0.1)
ForwardModel <- ols_step_forward_p(FitModel2, penter = 0.01, penterv = 0.1)
#stepwise forward regression plot
plot(ForwardModel)
# final model
ForwardModel$FitModel2

# I choose the FitModel2 which has been crated above, and I choose 0.01 p threshold because it is stricker than 0.05.
# Use LATEX to express the final model in the knowable world (including coefficient estimates from R).
$$\operatorname{\widehat{median_house_value}} = -2473782 - 29105(\operatorname{longitude}) -27446(\operatorname{latitude}) + 1060(\operatorname{housing_median_age})
-6(\operatorname{total_rooms}) +142(\operatorname{total_bedrooms}) - 37(\operatorname{population}) + 39287(\operatorname{median_income})
-38484(\operatorname{ocean_proximityINLAND}) +121816(\operatorname{ocean_proximityISLAND})$$
  
summary(FitModel2)
# Using the summary, I find that coefficients estimates are nearly the same with the final model using the ols_step_forward_p()function,except for the two variables ocean_proximityNear Bay and ocean_proximityNear Ocean.
# This is because I thoose the 0.01 p threshold. For those two variables, although their P-values are smaller than 0.05, they are larger than 0.01, so they do not appear in my model using the ols_step_forward_p() function. 

# All model Regression 
library(leaps)
# Define the response variable and predictor variables
response_var <- "median_house_value"
predictor_vars <- setdiff(names(CaliHousing), response_var)

# Fit all possible regression models using leaps
all_models <- regsubsets(as.formula(paste(response_var, paste(predictor_vars, collapse="+"), sep="~")), data=CaliHousing)

# Display the best models based on different criteria
summary(all_models)

# Extract the best model based on adjusted R-squared
best_model <- which.max(summary(all_models)$adjr2)
coef(all_models, id=best_model)