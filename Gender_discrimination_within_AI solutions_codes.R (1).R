# setwd('/Users/zuzanna/Desktop/Studia/Advanced_Econometrics/finalProject')

################################################################################
# skip this stage when csv data set is available 
################################################################################


# Load the data set 
# data <- read.csv("1dataset.txt")

# Remove the first column (id)
# data <- data[, -1]

# Change the column names
# colnames(data) <- c("Dept", "Gender", "Clin", "Cert", "Prate", "Exper", "Rank", "Sal94", "Sal95")

# Save as csv
# write.csv(data, "data.csv", row.names = FALSE)


################################################################################
# packages
################################################################################

requiredPackages = c( "dplyr",
                      "Matrix",
                      "stats",
                      "lmtest",
                      "sandwich",
                      "pscl",
                      "stargazer",
                      "mfx",
                      "glmtoolbox",
                      "DescTools",
                      "corrplot")# list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 

################################################################################
# data recognition and preparation
################################################################################

data <- read.csv('data.csv')

glimpse(data)
summary(data)
str(data)

# our sample is balanced in terms of the gender
# we do not observe any enormous outliers for salaries
# next step: find the literature for the possibilities of AI in the specific dept

# Function to perform min-max normalization
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Normalize Sal94, Sal95, Prate, and Exper
data$Sal94 <- min_max_normalize(data$Sal94)
data$Sal95 <- min_max_normalize(data$Sal95)
data$Prate <- min_max_normalize(data$Prate)
data$Exper <- min_max_normalize(data$Exper)

summary(data)



########## AI Implementation ############

# Creating a new column in our data frame
data$AI_Implementation <- NA

# Assign values to the "AI_Implementation" column. If the scientific literature 
# exists and proves the reasonability of AI implementation, set the value to 
# 1; otherwise, set it to 0
data$AI_Implementation <- ifelse(data$Dept %in% c(3, 4, 5), 1, 0)

summary(data)

###############################################################################
#                  EDA
###############################################################################


# Create a matrix to arrange the plots in a grid
par(mfrow = c(3, 2))

# Histogram for AI_Implementation
hist(data$AI_Implementation, main = "AI Implementation", xlab = "AI Implementation")

# Histogram for Gender
hist(data$Gender, main = "Gender", xlab = "Gender")

# Histogram for Clin
hist(data$Clin, main = "Clin", xlab = "Clin")

# Histogram for Prate
hist(data$Prate, main = "Prate", xlab = "Prate")

# Histogram for Exper
hist(data$Exper, main = "Exper", xlab = "Exper")

# Histogram for Sal95
hist(data$Sal95, main = "Sal95", xlab = "Sal95")


par(mfrow = c(2, 2))
# Boxplot for Prate
boxplot(data$Prate, main = "Prate", ylab = "Prate")

# Boxplot for Exper
boxplot(data$Exper, main = "Exper", ylab = "Exper")

# Boxplot for Sal95
boxplot(data$Sal95, main = "Sal95", ylab = "Sal95")



# Create a bar plot for 'AI_Implementation' based on 'Gender'
barplot(table(data$AI_Implementation, data$Exper),
        main = "Bar Plot of AI Implementation by Gender",
        xlab = "Gender",
        ylab = "Frequency",
        col = c("lightblue", "pink"),
        legend = c("No AI Implementation", "AI Implementation"))

# Create a bar plot for 'AI_Implementation' based on 'Clin'
barplot(table(data$AI_Implementation, data$Clin),
        main = "Bar Plot of AI Implementation by Clin",
        xlab = "Clin",
        ylab = "Frequency",
        col = c("lightblue", "pink"),
        legend = c("No AI Implementation", "AI Implementation"))

# Create a bar plot for 'Gender' based on 'Clin'
barplot(table(data$Gender, data$Clin),
        main = "Bar Plot of Gender by Clin",
        xlab = "Clin",
        ylab = "Frequency",
        col = c("lightblue", "pink"),
        legend = c("Female", "Male"))


par(mar = c(5, 4, 4, 10))
# Calculate the correlation matrix
cor_matrix <- cor(data[, c("Gender", "Clin", "Prate", "Exper", "Sal95")])

# Print the correlation matrix with values
print(cor_matrix)

# Create a heatmap of the correlation matrix
corrplot(cor_matrix, 
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         col = colorRampPalette(c("darkblue", "white", "blue"))(100),
         main = "Correlation Heatmap",
         mar = c(5, 4, 2, 2))

################################################################################
# a) estimation of linear probability model, logit model and probit model,
#    selection of significant variables
################################################################################

# We do not take into consideration the debt variable in advance, as it is
# already simplified to AI_Implementation. We simply divided the departments
# to the ones which have significant business meaning while implementing the AI,
# and those which are not yet developed in this direction, based on the scientific
# literature. (c) general-to-specific method to variables selection)

# OLS
model_ols <- lm(AI_Implementation ~ Gender + Clin + Cert + Prate + Exper + Rank 
                + Sal94 + Sal95,
                 data = data)
summary(model_ols)

# we estimated linear probability model bu he it has many defects, he didn't pass 
# Ramsey RESET test so model have incorrect form, residuals are heteroscedastic so
# LPM's estimators are biased and inconsistent so we will not use it.


# White's estimator of the variance-covariance matrix
robust_vcov = vcovHC(model_ols, data = data, type = "HC")
coeftest(model_ols, vcov.=robust_vcov)

# White's estimator is used to provide robust standard errors that account for 
# heteroscedasticity. It adjusts the standard errors to provide more accurate and 
# reliable inference in the presence of heteroscedasticity.

# to compare the simple lpm and the one with a robust vcov matrix
library("stargazer")
robust.lpm = coeftest(model_ols, vcov.=robust_vcov)
stargazer(model_ols, robust.lpm, type="text")


# LOGIT
model_logit <- glm(AI_Implementation ~ Gender + Clin + Cert + Prate + Exper + Rank 
                   + Sal94 + Sal95,
             data = data, family = binomial(link = "logit"))
summary(model_logit)


# PROBIT 
model_probit <- glm(AI_Implementation ~ Gender + Clin + Cert + Prate + Exper + Rank 
                    + Sal94 + Sal95,
                       data = data, family = binomial(link = "probit"))
summary(model_probit)

# In both models logit and probit, significant variables are: Gender, Clin, Prate and Exper

################################################################################
# b) choice between logit and probit on the basis of information criteria
################################################################################
# Joint insignificance of all variables for logit 
null_logit <- glm(AI_Implementation ~ 1, data = data, family = binomial(link = "logit"))
summary(null_logit)

lrtest(model_logit, null_logit)

# P-value is close to zero so we can reject the null hypothesis about joint insignificance

# Joint insignificance of all variables for probit
null_logit <- glm(AI_Implementation ~ 1, data = data, family = binomial(link = "probit"))
summary(null_logit)

lrtest(model_logit, null_logit)
# P-value is close to zero so we can reject the null hypothesis about joint insignificance

# the conclusion is that we choose logit model, because of smaller AIC.

###############################################################################
# c) general-to-specific method to variables selection
###############################################################################

# LR test - full model vs model without Sal94

logit_restricted1 <- glm(AI_Implementation ~ Gender + Clin + Cert + Prate + Exper 
                         + Rank + Sal95, data = data, family = binomial(link = "logit"))
summary(logit_restricted1)

lrtest(model_logit, logit_restricted1)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Sal94 is better fit.

# LR test  full model vs logit_restricted3 without Rank

logit_restricted2 <- glm(AI_Implementation ~ Gender + Clin + Cert + Prate + Exper 
                         + Sal95, data = data, family = binomial(link = "logit"))
summary(logit_restricted2)

lrtest(logit_restricted1, logit_restricted2)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Sal94 and Rank is better fit.

# LR test - full model vs logit_restricted4 without Cert

logit_restricted3 <- glm(AI_Implementation ~ Gender + Clin + Prate + Exper + Sal95, 
                         data = data, family = binomial(link = "logit"))
summary(logit_restricted3)

lrtest(logit_restricted2, logit_restricted3)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Sal94 and Rank and Cert is better fit and
# now all variables are significant. We can define this model as our logit ready for tranformations - 
# "logit_rft". 

logit_rft <- logit_restricted3
summary(logit_rft)


###############################################################################
# d) nonlinear relationship (variable to a power) and interaction between 
#    variables
###############################################################################

# We will consider introducing of three different iterations: Gender x Prate, Clin x Prate, Gender x Clin.

# Gender x Prate: It can be effect of Prate on AI implementation differs based on the 
# individual's gender. This interaction can capture potential gender-related differences 
# in the relationship between publication rate and AI implementation.

logit_gxp <- glm(AI_Implementation ~ Gender + Clin + Prate + Exper + + Sal95 + Gender:Prate, 
                        data = data, family = binomial(link = "logit"))
summary(logit_gxp)

lrtest(logit_gxp, logit_rft)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Gender x Prate is better fit.

# Gender x Clin:  Including this interaction can capture potential differences in the 
# effect of clinical or research emphasis on AI implementation based on gender. 
# This interaction term can help assess whether the relationship between Clin and AI 
# implementation differs for males and females.

logit_gxc <- glm(AI_Implementation ~ Gender + Clin + Prate + Exper + Sal95 + Gender:Clin, 
                 data = data, family = binomial(link = "logit"))
summary(logit_gxc)

lrtest(logit_gxc, logit_rft)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Gender x Clin is better fit.

# Gender x Sal95: there could be different salary distributions based on gender

logit_gxs <- glm(AI_Implementation ~ Gender + Clin + Prate + Exper+ Sal95 + Gender:Sal95, 
                 data = data, family = binomial(link = "logit"))
summary(logit_gxs)

lrtest(logit_gxs, logit_rft)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Gender x Sal95 is better fit.

# Gender x Exper: can capture the potential interaction effect between gender and years of experience since obtaining an MD

logit_gxe <- glm(AI_Implementation ~ Gender + Clin + Prate + Exper+ Sal95 + Gender:Exper, 
                 data = data, family = binomial(link = "logit"))
summary(logit_gxe)

lrtest(logit_gxe, logit_rft)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Gender x Exper is better fit.

# After testing the other interactions, we didn't find one that improve the model.

# Checking nonlinear relationship

data$Exper_squared <- data$Exper^2

logit_e2 <- glm(AI_Implementation ~ Gender + Clin + Prate + Exper + Exper_squared + Sal95, 
                data = data, family = binomial(link = "logit"))
summary(logit_e2)

lrtest(logit_e2, logit_rft)

# We can see that p-value is higher than significance level so we cannot reject the null 
# hypothesis, so restricted model (2) without Exper variable squared is better fit.

logit_final <- logit_rft
summary(logit_final)


###############################################################################
# e) present the general model (LPM, logit, and probit), the final model 
# (the specif model) in one quality table. If there is space, at least one 
# intermediate model might be presented
###############################################################################

# Generate the summary table using stargazer
stargazer(model_ols, model_probit, model_logit, logit_e2, logit_final,
                           type = "text", header = FALSE)

# stargazer(model_ols, model_logit, model_probit, logit_e2, logit_final,
#           type = "text", header = FALSE, out = "summary_table.html")
# file_path <- "C:\\Users\\karla\\OneDrive\\Pulpit\\AE project\\data\\summary_table.html"
# cat(summary_table, file = file_path)

###############################################################################
# f) calculation and interpretation of marginal effects for the final model 
# (from the general-to- specific approach);
###############################################################################

logitmfx(formula = AI_Implementation ~ Gender + Clin + Prate + Exper + Sal95, 
        data = data, atmean=TRUE)

# Gender: Being female decreases the probability of AI implementation by 21.0 percentage points,
# compared to being male, holding other variables constant.

# Clin: Having primarily research emphasis decreases the probability of AI implementation 
# by 37.6 percentage points compared to having primarily clinical emphasis, holding other variables constant.

# Prate: Additional unit of publication rate decreases the probability of AI implementation by 306.6 percentage points, holding other variables constant.

# Exper: Additional year since obtaining MD increases the probability of AI implementation by 81.1 percentage points, holding other variables constant.

# Sal95: Additional unit of Sal95 (salary after increment to 1994) decreases the probability of AI implementation by 228.3 percentage points, holding other variables constant.

###############################################################################
# g) calculation and interpretation of odds ratios
###############################################################################

# Odds is the ration of the pobability that a particular event will occur to the probability that 
# it will not occur.

# To calculate odds ratios we will extract the coefficient estimates
coeff <- coef(logit_final)

# Now we can calculate the odds ratios

odds_ratios <- exp(summary(logit_final)$coeff[, "Estimate"]) / 
  (1 + exp(summary(logit_final)$coeff[, "Estimate"]))
odds_ratios

###############################################################################
# h) perform the linktest and interpret the result
###############################################################################

# First we need to load the linktest package
source("../data/linktest.R")

# Now we can perform linktest on our final model
linktest_result = linktest(logit_final) 

# Model has correct form if yhat variable is statistically significant and yhat2 is 
# statistically insignificant so we can see that p-value of yhat is close to zero, so
# we reject the null hypothesis about insignificance. P-value of yhat2 is higher than
# significance level so yhat2 is insignificant.

###############################################################################
# i) interpretation of the appropriate R2 statistics (R2 McKelvey-Zavoina, 
#    count R2, and adjusted count R2;
###############################################################################

PseudoR2(model_logit, which = "McFadden")
PseudoR2(model_probit, which = "McFadden")
# calculating R2 McKelvey-Zavoina
PseudoR2(logit_final, which = "McKelveyZavoina")

# If the latent(unobserved) variable was observed then our model would explain 44.72% of 
# its variation.

# calculating count R2
countR2<-function(logit_final) mean(logit_final$y==round(logit_final$fitted.values))

countR2(logit_final)

# Our model correctly predicts 76% of all observations, 

# calculating adjusted count R2
adj.countR2<-function(logit_final) {
  n<-length(logit_final$y)
  k<-max(table(logit_final$y))
  correct<-table(logit_final$y==round(logit_final$fitted.values))[["TRUE"]]
  (correct-k)/(n-k)
}

adj.countR2(logit_final)

# Only 52% of all prediction were correct because of the variation of independent variable.

###############################################################################
# k) perform the Hosmer-Lemeshow and alike tests
###############################################################################

# Goodness-of-Fit Tests

hltest(logit_final, verbose = TRUE)

# There is no replicated data so Hosmer-Lemeshow test does not require replicated data so 
# we can interpret its high p-value as indicating no evidence of lack-of-fit.
