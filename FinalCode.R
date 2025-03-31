################################################################################
## 1. LOAD & VIEW DATA
################################################################################

movies = read.csv('/Users/roycim/Downloads/imdb.csv')
attach(movies)
View(movies)


################################################################################
## 2. CORRELATION CHECKS WITH imdb_score
################################################################################

# Compute correlations with imdb_score
correlations = cor(movies[sapply(movies, is.numeric)], use = "complete.obs")

# Extract correlations with imdb_score and sort them
sorted_correlations = sort(correlations["imdb_score", ], decreasing = TRUE)

# Display the sorted correlations
sorted_correlations


################################################################################
## 3. EXPLORATORY PLOT (duration vs. imdb_score)
################################################################################

library(ggplot2)
plot = ggplot(movies, aes(y = imdb_score, x = duration))
scatter= geom_point() # scatter
plot + scatter


################################################################################
## 4. INITIAL MODELS
################################################################################

# Building first model with multiple genre dummies
model1 = lm(imdb_score ~ duration + action + adventure + scifi + thriller + musical + 
              romance + western + sport + horror + drama + war + animation, 
            data = movies)
plot + scatter
summary(model1)

# Second model with a subset of predictors
model2 = lm(imdb_score ~ duration + action + romance + drama + animation + nb_news_articles + horror + nb_faces + movie_budget)
summary(model2)

# Model with only numerical variables
model_numerical = lm(imdb_score ~ duration + nb_news_articles + nb_faces + movie_budget) 
summary(model_numerical)

# Check linearity of numeric variables
library(car)
residualPlots(model_numerical)


################################################################################
## 5. POLYNOMIAL DEGREE SEARCH (duration, nb_news_articles, movie_budget)
################################################################################

# Expand grid to generate all combinations of polynomial degrees
install.packages("boot")
library(boot)


degree_combinations <- expand.grid(
  duration = 1:5,
  nb_news_articles = 1:5,
  movie_budget = 1:5
)
degree_combinations$MSE = NA
View(degree_combinations)

for (i in 1:nrow(degree_combinations)) {
  current_duration_degree = degree_combinations$duration[i]
  current_nb_news_articles_degree = degree_combinations$nb_news_articles[i]
  current_movie_budget_degree = degree_combinations$movie_budget[i]
  
  fit = glm(imdb_score ~ poly(duration, current_duration_degree) +
              poly(nb_news_articles, current_nb_news_articles_degree) +
              poly(movie_budget, current_movie_budget_degree), 
            data = movies)
  
  MSE_result = cv.glm(movies, fit, K = 965)
  degree_combinations$MSE[i] = MSE_result$delta[1]
}

lowestMSE = degree_combinations[which.min(degree_combinations$MSE), ]
lowestMSE


################################################################################
## 6. BASELINE MODEL TEST
################################################################################

# Compute the mean of imdb_score
mean_pred <- mean(movies$imdb_score, na.rm = TRUE)

# Compute MSE for the baseline model
baseline_mse <- mean((movies$imdb_score - mean_pred)^2, na.rm = TRUE)
print(baseline_mse)  # This is the baseline MSE


################################################################################
## 7. CORRELATION AMONG NUMERIC VARIABLES
################################################################################

# Checking correlation between duration, budget, nb_news_articles, nb_faces
quantvars = movies[, c(5, 9, 15, 25)] # the variables mentioned
corr_matrix = cor(quantvars)
round(corr_matrix, 2)


################################################################################
## 8. ADDITIONAL MODELS (NUMERICAL, CATEGORICAL, COMBINATION)
################################################################################

# Numerical model (again, for clarity)
model_numerical = lm(imdb_score ~ duration + nb_news_articles + nb_faces + movie_budget) 

# Categorical model (genres only)
categorical_model = lm(imdb_score ~ action + romance + drama + animation)

# Combination model (polynomial expansions on duration & budget + categories)
combination_model = lm(imdb_score ~ poly(duration, 2) + nb_news_articles + nb_faces + 
                         poly(movie_budget, 5) + action + romance + drama + animation)

summary(categorical_model)
summary(combination_model)


################################################################################
## 9. RAW SCATTER PLOTS FOR NUMERIC VARIABLES
################################################################################

plot(nb_news_articles, imdb_score)
plot(nb_faces, imdb_score)
plot(movie_budget, imdb_score)
plot(duration, imdb_score)


################################################################################
## 10. LOG TRANSFORMATIONS FOR SKEWED VARIABLES
################################################################################

movies$log_nb_news_articles <- log1p(movies$nb_news_articles)
movies$log_movie_budget     <- log1p(movies$movie_budget)
movies$log_nb_faces         <- log1p(movies$nb_faces)
movies$log_duration         <- log(movies$duration)

# Log-based model
model_log <- lm(
  imdb_score ~ log_nb_news_articles + log_movie_budget + log_nb_faces + log_duration +
    action + romance + drama + animation, 
  data = movies
)
summary(model_log)


################################################################################
## 11. OUTLIER DETECTION & REMOVAL
################################################################################

outlierTest(model_log)

# Removing specific outliers
movies2 = movies[-c(1806, 1581, 395, 191, 1123, 1592), ]

# New log model with polynomial expansions
model_log <- lm(
  imdb_score ~ log_nb_news_articles + poly(log_movie_budget, 4) + log_nb_faces + 
    poly(log_duration, 2) + action + romance + drama + animation, 
  data = movies2
)
summary(model_log)


################################################################################
## 12. HETEROSKEDASTICITY CHECK
################################################################################

install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)

ncvTest(model_log)
coeftest(model_log, vcov=vcovHC(model_log, type = "HC1"))
summary(model_log)


################################################################################
## 13. SECOND POLYNOMIAL LOOP (INCLUDES nb_faces)
################################################################################
library(boot)
degree_combinations2 <- expand.grid(
  duration = 1:5,
  nb_news_articles = 1:5,
  movie_budget = 1:5,
  nb_faces = 1:5
)
degree_combinations2$MSE = NA
View(degree_combinations2)

# Checking the MSE for the log model with possible polynomial nb_faces
for (i in 1:nrow(degree_combinations2)) {
  current_duration_degree = degree_combinations2$duration[i]
  current_nb_news_articles_degree = degree_combinations2$nb_news_articles[i]
  current_movie_budget_degree = degree_combinations2$movie_budget[i]
  current_nb_faces_degree = degree_combinations2$nb_faces[i]
  
  model_log = glm(imdb_score ~ poly(log_nb_news_articles, current_nb_news_articles_degree) + 
                    poly(log_movie_budget, current_movie_budget_degree) +
                    poly(log_duration, current_duration_degree) +
                    poly(log_nb_faces, current_nb_faces_degree) + 
                    action + romance + drama + animation, 
                  data = movies2)
  
  MSE_result = cv.glm(movies2, model_log, K = 60)
  degree_combinations2$MSE[i] = MSE_result$delta[1]
}

lowestMSE = degree_combinations2[which.min(degree_combinations2$MSE), ]
lowestMSE



# Using optimal degrees from loop
model_log <- lm(
  imdb_score ~ poly(log_nb_news_articles, 3) + log_movie_budget + 
    poly(log_nb_faces,2) + poly(log_duration, 3) +
    action + romance + drama + animation + release_year,
  data = movies2
)

# Possibly adding extra variables (like release year) to boost R^2
residualPlots(model_log)
ncvTest(model_log)

coeftest(model_log, vcov = vcovHC(model_log, type = "HC1"))

##### Refixing heterosketasticty for model_log with release_year

# 2. Compute log of absolute residuals vs. log of fitted values
abs_resid <- abs(resid(model_log))                 # absolute residuals
log_abs_resid <- log(abs_resid + 1e-8)             # avoid log(0)
log_fitted <- log(fitted(model_log))               # log of fitted values

# 3. Regress log(abs_resid) on log_fitted to find slope
wls_fit <- lm(log_abs_resid ~ log_fitted)
summary(wls_fit)

# 4. Extract slope g => indicates variance ∝ (fitted)^(2*g)
g <- coef(wls_fit)["log_fitted"]

# 5. Define weights = 1 / (fitted^(2*g))
w <- 1 / (fitted(model_log)^(2 * g))

# 6. Refit using weights
model_wls <- lm(
  imdb_score ~ poly(log_nb_news_articles, 3) + log_movie_budget + 
    poly(log_nb_faces, 2) + poly(log_duration, 3) +
    action + romance + drama + animation + release_year,
  data = movies2,
  weights = w
)

# 7. Summarize the WLS model
summary(model_wls)

# 8. (Optional) Check if heteroskedasticity improved
library(car)
ncvTest(model_wls)       # Should be less significant if WLS helped
residualPlots(model_wls)




### Finding optimal polynomial for release year, news articles, budget, log nb faces, log duration, 
################################################################################
## 1. CREATE A GRID OF DEGREES
################################################################################

degree_combinations3 <- expand.grid(
  log_nb_news_articles_degree = 1:5,
  log_movie_budget_degree     = 1:5,
  log_nb_faces_degree         = 1:5,
  log_duration_degree         = 1:5,
  release_year = 1:5
  
)

degree_combinations3$MSE <- NA  # Column to store MSE results
View(degree_combinations3)
################################################################################
## 2. LOOP OVER EACH COMBINATION AND COMPUTE 20-FOLD CV MSE
################################################################################

for (i in 1:nrow(degree_combinations3)) {
  
  # Extract degrees for each variable
  current_nb_news_articles_degree <- degree_combinations3$log_nb_news_articles_degree[i]
  current_movie_budget_degree     <- degree_combinations3$log_movie_budget_degree[i]
  current_nb_faces_degree         <- degree_combinations3$log_nb_faces_degree[i]
  current_duration_degree         <- degree_combinations3$log_duration_degree[i]
  current_releaseyear_degree = degree_combinations3$release_year[i]
  # Fit a GLM (or LM) with polynomials on the log variables + any other predictors
  model_log <- glm(
    imdb_score ~ poly(log_nb_news_articles, current_nb_news_articles_degree) +
      poly(log_movie_budget, current_movie_budget_degree) +
      poly(log_nb_faces, current_nb_faces_degree) +
      poly(log_duration, current_duration_degree) +
      action + romance + drama + animation + poly(release_year, current_releaseyear_degree),
    data = movies2
  )
  
  # Perform 20-fold CV and store the MSE
  MSE_result <- cv.glm(movies2, model_log, K = 20)
  degree_combinations3$MSE[i] <- MSE_result$delta[1]
}

################################################################################
## 3. FIND THE BEST COMBINATION (LOWEST MSE)
################################################################################

lowestMSE <- degree_combinations3[which.min(degree_combinations3$MSE), ]
lowestMSE

# Fit the model with the best combination of polynomials found

model_combinations3 <- lm(
  imdb_score ~ poly(log_nb_news_articles, 4) + poly(log_movie_budget, 1) +
    poly(log_nb_faces, 2) + poly(log_duration, 3) +
    poly(release_year, 2) + action + romance + drama + animation,
  data = movies2,
  weights = w
)
summary(model_combinations3)





################################################################################
##              ADDING NEW FEATURES TO IMPROVE THE MODEL FURTHER              ##
##      Enhancing our best polynomial model with additional predictors        ## 
################################################################################

# NOTE: Many other variables (e.g., aspect_ratio, language, release_day/month, distributor, production_company) were 
# tested but found insignificant. The following sections include only those retained.


################################################################################
## 1. ASSESSING MATURITY RATING AS A PREDICTOR
################################################################################
movies2$maturity_rating <- as.factor(movies2$maturity_rating)

model_with_maturity <- lm(
  imdb_score ~ poly(log_nb_news_articles, 2) + poly(log_movie_budget, 2) +
    poly(log_nb_faces, 4) + poly(log_duration, 3) +
    poly(release_year, 3) + action + romance + drama + animation +
    maturity_rating,
  data = movies2,
  weights = w
)
summary(model_with_maturity)

# Keeping only statistically significant maturity ratings to avoid unnecessary model complexity
# Create binary dummy variables for the significant maturity ratings
movies2$NC17 <- ifelse(movies2$maturity_rating == "NC-17", 1, 0)
movies2$R <- ifelse(movies2$maturity_rating == "R", 1, 0)
movies2$TV14 <- ifelse(movies2$maturity_rating == "TV-14", 1, 0)
movies2$TVG <- ifelse(movies2$maturity_rating == "TV-G", 1, 0)

model_maturity_filtered <- lm(
  imdb_score ~ poly(log_nb_news_articles, 2) + poly(log_movie_budget, 2) +
    poly(log_nb_faces, 4) + poly(log_duration, 3) +
    poly(release_year, 3) + action + romance + drama + animation +
    NC17 + R + TV14 + TVG,
  data = movies2,
  weights = w
)
summary(model_maturity_filtered)



################################################################################
## 2. ASSESSING ACTOR STAR METER AS A PREDICTOR
################################################################################
# Test various formulations of actor star meter to capture actor influence:
# 2.1 Lowest star meter (best actor rank)
movies2$lowest_star_meter <- apply(
  movies2[, c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter")],
  1,
  min,
  na.rm = TRUE
)

# 2.2 Presence of a top-500 actor (binary)
movies2$top_250_actor <- ifelse(movies2$lowest_star_meter < 250, 1, 0)

# 2.3 Average star meter for the 3 actors (overall star power)
movies2$avg_star_meter <- rowMeans(
  movies2[, c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter")],
  na.rm = TRUE
)

# Test each formulation separately; the best (lowest_star_meter) was selected.
model_star_lowest <- lm(
  imdb_score ~ poly(log_nb_news_articles, 2) + poly(log_movie_budget, 2) +
    poly(log_nb_faces, 4) + poly(log_duration, 3) +
    poly(release_year, 3) + action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2, 
  weights = w
)
summary(model_star_lowest)

# Check multicollinearity (VIF values should be low)
library(car)
vif_values <- vif(model_star_lowest)
print(vif_values)  # VIF indicate no multicollinearity issues

# Check for heteroskedasticity and residual patterns
ncvTest(model_star_lowest)  # Low p-value may indicate heteroskedasticity
plot(model_star_lowest, which = 1, main = "Residuals vs Fitted")  # Mild downward trend observed


# CORRECTING HETEROSKEDASTICITY & NON-LINEARITY for actor star meter

# Compute absolute residuals and log-transform them to estimate the relationship
# between residual magnitude and fitted values.
abs_resid <- abs(resid(model_star_lowest))
log_abs_resid <- log(abs_resid + 1e-8)  # Add small constant to avoid log(0)
log_fitted <- log(fitted(model_star_lowest))

# Regress log(abs(residuals)) on log(fitted values) to estimate the slope (g)
wls_fit <- lm(log_abs_resid ~ log_fitted)
summary(wls_fit)
g <- coef(wls_fit)["log_fitted"]

# Define new weights: 1 / (fitted^(2*g))
w_new <- 1 / (fitted(model_star_lowest)^(2 * g))

# Refit the model with new weights, keeping the same predictors
final_actor_star_model <- lm(
  imdb_score ~ poly(log_nb_news_articles, 2) + poly(log_movie_budget, 2) +
    poly(log_nb_faces, 4) + poly(log_duration, 3) +
    poly(release_year, 3) + action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2,
  weights = w_new
)
summary(final_actor_star_model)

# Compute and display the Mean Squared Error (MSE) for the final model.
mse_final <- mean(final_actor_star_model$residuals^2)
cat("MSE:", mse_final, "\n")

# Current performance: R2 = 0.491 and CV MSE = 0.656





###############################################################################
## 3. ADDING THE VARIABLE "movie_meter_IMDBpro"
###############################################################################

# Check distribution via summary
summary(movies2$movie_meter_IMDBpro)
# Observed: Min = 71, Median = 5398, Max = 849550 (this suggest heavy right skew)

# Histogram to confirm right skew
hist(movies2$movie_meter_IMDBpro, breaks = 100,
     main = "Histogram of movie_meter_IMDBpro",
     xlab = "movie_meter_IMDBpro",
     xlim = c(0, 50000))
# indeed, there is a heavy right skew


# apply log transformation to address heavy skew
movies2$log_movie_meter_IMDBpro <- log1p(movies2$movie_meter_IMDBpro)
summary(movies2$log_movie_meter_IMDBpro)
hist(movies2$log_movie_meter_IMDBpro, breaks = 100,
     main = "Histogram of log_movie_meter_IMDBpro",
     xlab = "log(movie_meter_IMDBpro + 1)")
# Observation: The log transformation has heavily reduced skew.

################################################################################
## STEP 1: FIT PRELIMINARY MODELS (Raw vs. Log-Transformed)
################################################################################

# Model A: Using the raw variable (not log-transformed)
model_IMDBpro_raw <- lm(
  imdb_score ~ poly(log_nb_news_articles, 2) + poly(log_movie_budget, 2) +
    poly(log_nb_faces, 4) + poly(log_duration, 3) +
    poly(release_year, 3) + action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter + movie_meter_IMDBpro,
  data = movies2,
  weights = w_new
)
summary(model_IMDBpro_raw)
# R^2 ~ 0.4894, p-value for movie_meter_IMDBpro ~ 0.45 (not significant)

# Model B: Using the log-transformed variable
model_IMDBpro_log <- lm(
  imdb_score ~ poly(log_nb_news_articles, 2) + poly(log_movie_budget, 2) +
    poly(log_nb_faces, 4) + poly(log_duration, 3) +
    poly(release_year, 3) + action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter + log_movie_meter_IMDBpro,
  data = movies2,
  weights = w_new
)
summary(model_IMDBpro_log)
# R2 ~ 0.5298
# log_movie_meter_IMDBpro is highly significant (p < 2e-16).
# Conclusion: The log-transformed variable improves the model performance.

################################################################################
## STEP 2: CHECK MULTICOLLINEARITY
################################################################################

library(car)
vif_values <- vif(model_IMDBpro_log)
vif_values
# VIF values show no multicollinearity concerns

################################################################################
## STEP 3: CHECK HETEROSKEDASTICITY
################################################################################

library(lmtest)
ncvTest(model_IMDBpro_log)
# p ~ 0.34432 => no significant heteroskedasticity => no new WLS needed

################################################################################
## STEP 4: FINAL MODEL 
################################################################################

# Since the log-transformed variable performs better and no new WLS is required,
# we simply adopt model_IMDBpro_log as our final model.
best_poly_model_IMDBpro <- model_IMDBpro_log

# Compute Mean Squared Error (MSE)
mse_best_poly_IMDBpro <- mean(best_poly_model_IMDBpro$residuals^2)
cat("Final MSE (best_poly_model_IMDBpro):", mse_best_poly_IMDBpro, "\n")
# MSE = 0.616




################################################################################

## NEW GRID SEARCH: FINDING OPTIMAL POLYNOMIAL DEGREES (INCLUDING log_movie_meter_IMDBpro)
## for numeric predictors while retaining categorical variables as-is.
## This grid search is used to determine the best polynomial degrees for:
##   - log_nb_news_articles
##   - log_movie_budget
##   - log_nb_faces
##   - log_duration
##   - release_year
##   - log_movie_meter_IMDBpro (now included as a new variable)
## 20-fold CV is used to compute the MSE for each combination.

################################################################################

# Load required packages
library(boot)   # For cv.glm
library(car)    # For potential VIF checks

# Create a grid over polynomial degrees for each numeric predictor.
# We use a new variable name 'poly_grid' to avoid conflicts.
poly_grid <- expand.grid(
  log_nb_news_articles_degree = 1:5,
  log_movie_budget_degree     = 1:5,
  log_nb_faces_degree         = 1:5,
  log_duration_degree         = 1:5,
  release_year_degree         = 1:5,
  log_movie_meter_IMDBpro_degree = 1:5
)

# Add a column to store the CV MSE for each combination.
poly_grid$MSE <- NA

# Print the initial grid to the console.
print("Initial poly_grid:")
print(poly_grid)

# Total number of combinations.
total_combinations <- nrow(poly_grid)
cat("Total combinations to process:", total_combinations, "\n")

################################################################################
## LOOP OVER EACH COMBINATION AND COMPUTE 20-FOLD CV MSE
################################################################################

for (i in 1:total_combinations) {
  
  # Log progress
  cat("Processing combination", i, "of", total_combinations, "\n")
  
  # Extract current polynomial degrees from the grid.
  current_nb_news_articles_degree <- poly_grid$log_nb_news_articles_degree[i]
  current_movie_budget_degree     <- poly_grid$log_movie_budget_degree[i]
  current_nb_faces_degree         <- poly_grid$log_nb_faces_degree[i]
  current_duration_degree         <- poly_grid$log_duration_degree[i]
  current_release_year_degree     <- poly_grid$release_year_degree[i]
  current_movie_meter_degree      <- poly_grid$log_movie_meter_IMDBpro_degree[i]
  
  # Fit a temporary GLM model with the current combination.
  # The categorical predictors (action, romance, drama, animation,
  # NC17, R, TV14, TVG, lowest_star_meter) remain unchanged.
  temp_model <- glm(
    imdb_score ~ poly(log_nb_news_articles, current_nb_news_articles_degree) +
      poly(log_movie_budget, current_movie_budget_degree) +
      poly(log_nb_faces, current_nb_faces_degree) +
      poly(log_duration, current_duration_degree) +
      poly(release_year, current_release_year_degree) +
      poly(log_movie_meter_IMDBpro, current_movie_meter_degree) +
      action + romance + drama + animation +
      NC17 + R + TV14 + TVG + lowest_star_meter,
    data = movies2
  )
  
  # Perform 20-fold cross-validation.
  cv_result <- cv.glm(movies2, temp_model, K = 20)
  poly_grid$MSE[i] <- cv_result$delta[1]
  

# After processing all combinations, display the updated grid.
print("Completed grid search. poly_grid with MSE values:")
print(poly_grid)

################################################################################
## FIND THE BEST COMBINATION (LOWEST MSE)
################################################################################

best_combination <- poly_grid[which.min(poly_grid$MSE), ]
cat("Best combination of polynomial degrees found:\n")
print(best_combination)


## The following optimal polynomial degrees were determined via this grid search:
##   - log_nb_news_articles: 4
##   - log_movie_budget:     3
##   - log_nb_faces:         2
##   - log_duration:         3
##   - release_year:         4
##   - log_movie_meter_IMDBpro: 4
##
## These values yielded the lowest CV MSE (~0.6224495) with 20-fold CV.

# fit the model
final_poly_model <- lm(
  imdb_score ~ poly(log_nb_news_articles, 4) +
    poly(log_movie_budget, 3) +
    poly(log_nb_faces, 2) +
    poly(log_duration, 3) +
    poly(release_year, 4) +
    poly(log_movie_meter_IMDBpro, 4) +
    action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2,
  weights = w_new  # Use the previously established weights.
)


summary(final_poly_model)

mse_final_poly <- mean(final_poly_model$residuals^2)
cat("Final Model MSE:", mse_final_poly, "\n")


# Final Poly Model MSE: 0.603, R2: 0.537


################################################################################
## RESIDUALS VS. FITTED VALUES PLOT
################################################################################

library(ggplot2)

# 1. Calculate residuals and fitted values
residual_vals <- residuals(final_poly_model)
fitted_vals   <- fitted(final_poly_model)

# 2. Create a data frame for plotting
residual_data <- data.frame(
  Residuals = residual_vals,
  Fitted    = fitted_vals
)

# 3. Plot: Residuals vs. Fitted
ggplot(residual_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +       # semi-transparent points
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals vs. Fitted Values",
    x = "Fitted IMDb Score",
    y = "Residuals (Observed - Fitted)"
  ) +
  theme_minimal(base_size = 14)


# The plot shows residuals evenly scattered around zero, suggesting that 
# the model does not exhibit obvious bias or heteroskedasticity, which 
# indicates that it is making reasonably well-calibrated predictions.






################################################################################
##                              SPLINE MODELS                                 ##
##       Evaluation of spline-based models using quantile knots               ##
################################################################################

# -------------------------------------------------------------------------------
# SECTION: Splines-Only - 3 Knots
# -------------------------------------------------------------------------------

library(splines)
library(boot)

# Compute 3 knots (25th, 50th, 75th percentiles) for each continuous predictor
knots_nb_news_articles <- quantile(movies2$log_nb_news_articles, probs = c(0.25, 0.5, 0.75))
knots_movie_budget      <- quantile(movies2$log_movie_budget,     probs = c(0.25, 0.5, 0.75))
knots_nb_faces          <- quantile(movies2$log_nb_faces,         probs = c(0.25, 0.5, 0.75))
knots_duration          <- quantile(movies2$log_duration,         probs = c(0.25, 0.5, 0.75))
knots_release_year      <- quantile(movies2$release_year,         probs = c(0.25, 0.5, 0.75))
knots_movie_meter_IMDBpro <- quantile(movies2$log_movie_meter_IMDBpro, probs = c(0.25, 0.5, 0.75))

# Fit the full model using the manually specified 3-knots spline transformations
model_3knots <- glm(
  imdb_score ~ 
    bs(log_nb_news_articles, knots = knots_nb_news_articles) +
    bs(log_movie_budget,      knots = knots_movie_budget) +
    bs(log_nb_faces,          knots = knots_nb_faces) +
    bs(log_duration,          knots = knots_duration) +
    bs(log_movie_meter_IMDBpro, knots = knots_movie_meter_IMDBpro) +
    bs(release_year,          knots = knots_release_year) +
    action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2
)

# Evaluate the 3-knots model using 20-fold cross-validation and print the CV MSE
cv_result_3knots <- cv.glm(movies2, model_3knots, K = 20)
cat("Full model with 3 manually entered knots (CV MSE):", cv_result_3knots$delta[1], "\n")

# Refit the final model with lm (for easier coefficient interpretation) using the same 3-knots
final_model_3knots <- lm(
  imdb_score ~ 
    bs(log_nb_news_articles, knots = knots_nb_news_articles) +
    bs(log_movie_budget,      knots = knots_movie_budget) +
    bs(log_nb_faces,          knots = knots_nb_faces) +
    bs(log_duration,          knots = knots_duration) +
    bs(log_movie_meter_IMDBpro, knots = knots_movie_meter_IMDBpro) +
    bs(release_year,          knots = knots_release_year) +
    action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2
)
summary(final_model_3knots)
mse_3knots <- mean(residuals(final_model_3knots)^2)
cat("Final Model MSE:", mse_3knots, "\n")

# Results: R2 = 0.491; CV MSE = 0.58



# -------------------------------------------------------------------------------
# SECTION: Splines-Only - 4 Knots
# -------------------------------------------------------------------------------

library(splines)
library(boot)

# Compute 4 knots (20th, 40th, 60th, 80th percentiles) for each continuous predictor
knots_nb_news_articles <- quantile(movies2$log_nb_news_articles, probs = c(0.2, 0.4, 0.6, 0.8))
knots_movie_budget      <- quantile(movies2$log_movie_budget,     probs = c(0.2, 0.4, 0.6, 0.8))
knots_nb_faces          <- quantile(movies2$log_nb_faces,         probs = c(0.2, 0.4, 0.6, 0.8))
knots_duration          <- quantile(movies2$log_duration,         probs = c(0.2, 0.4, 0.6, 0.8))
knots_release_year      <- quantile(movies2$release_year,         probs = c(0.2, 0.4, 0.6, 0.8))
knots_movie_meter_IMDBpro <- quantile(movies2$log_movie_meter_IMDBpro, probs = c(0.2, 0.4, 0.6, 0.8))

# Fit the full model using the manually specified 4-knots spline transformations
model_4knots <- glm(
  imdb_score ~ 
    bs(log_nb_news_articles, knots = knots_nb_news_articles) +
    bs(log_movie_budget,      knots = knots_movie_budget) +
    bs(log_nb_faces,          knots = knots_nb_faces) +
    bs(log_duration,          knots = knots_duration) +
    bs(log_movie_meter_IMDBpro, knots = knots_movie_meter_IMDBpro) +
    bs(release_year,          knots = knots_release_year) +
    action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2
)

# Evaluate the 4-knots model using 20-fold cross-validation and print the CV MSE
cv_result_4knots <- cv.glm(movies2, model_4knots, K = 20)
cat("Full model with 4 manually entered knots (CV MSE):", cv_result_4knots$delta[1], "\n")

# Refit the final model with lm (for easier coefficient interpretation) using the same 4-knots
final_model_4knots <- lm(
  imdb_score ~ 
    bs(log_nb_news_articles, knots = knots_nb_news_articles) +
    bs(log_movie_budget,      knots = knots_movie_budget) +
    bs(log_nb_faces,          knots = knots_nb_faces) +
    bs(log_duration,          knots = knots_duration) +
    bs(log_movie_meter_IMDBpro, knots = knots_movie_meter_IMDBpro) +
    bs(release_year,          knots = knots_release_year) +
    action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2
)
summary(final_model_4knots)
mse_4knots <- mean(residuals(final_model_4knots)^2)
cat("Final Model MSE:", mse_4knots, "\n")


# Results: R2 = 0.492; CV MSE = 0.58

# -------------------------------------------------------------------------------
# COMMENT: Overall, the splines-only models appear to be inferior to our final_poly_model.
# -------------------------------------------------------------------------------




################################################################################
##                        TRYING HYBRID/MIXED MODELS                          ##
##             COMBINING SPLINE AND POLYNOMIAL TRANSFORMATIONS                ##
################################################################################

################################################################################
# Version 1
# for each continuous predictor, we have two transformation options:
#   - "poly": use a polynomial transformation with optimal degree (from best_poly_model)
#   - "spline": use a spline transformation with 3 knots (25th, 50th, 75th percentiles)
#
# We create a grid of these options for:
#   log_nb_news_articles, log_movie_budget, log_nb_faces, log_duration, release_year, log_movie_meter_IMDBpro
################################################################################

# Create candidate options grid
candidate_options <- expand.grid(
  log_nb_news_articles = c("poly", "spline"),
  log_movie_budget = c("poly", "spline"),
  log_nb_faces = c("poly", "spline"),
  log_duration = c("poly", "spline"),
  release_year = c("poly", "spline"),
  log_movie_meter_IMDBpro = c("poly", "spline")
)
candidate_options$CV_MSE <- NA  # to store CV MSE for each candidate

# Fixed polynomial degrees (from your best_poly_model)
poly_degrees <- list(
  log_nb_news_articles = 2,
  log_movie_budget = 2,
  log_nb_faces = 4,
  log_duration = 3,
  release_year = 3,
  log_movie_meter_IMDBpro = 4
)

# Pre-calculate spline knots (3 knots: 25th, 50th, 75th percentiles)
knots_nb_news_articles <- quantile(movies2$log_nb_news_articles, probs = c(0.25, 0.5, 0.75))
knots_movie_budget      <- quantile(movies2$log_movie_budget,     probs = c(0.25, 0.5, 0.75))
knots_nb_faces          <- quantile(movies2$log_nb_faces,         probs = c(0.25, 0.5, 0.75))
knots_duration          <- quantile(movies2$log_duration,         probs = c(0.25, 0.5, 0.75))
knots_release_year      <- quantile(movies2$release_year,         probs = c(0.25, 0.5, 0.75))
knots_movie_meter_IMDBpro <- quantile(movies2$log_movie_meter_IMDBpro, probs = c(0.25, 0.5, 0.75))

# Loop over each candidate combination
for(i in 1:nrow(candidate_options)) {
  # Extract the transformation choices for each predictor
  trans_nb_news_articles <- candidate_options$log_nb_news_articles[i]
  trans_movie_budget     <- candidate_options$log_movie_budget[i]
  trans_nb_faces         <- candidate_options$log_nb_faces[i]
  trans_duration         <- candidate_options$log_duration[i]
  trans_release_year     <- candidate_options$release_year[i]
  trans_movie_meter_IMDBpro <- candidate_options$log_movie_meter_IMDBpro[i]
  
  # Build formula terms for each predictor based on the chosen transformation
  term_nb_news_articles <- if(trans_nb_news_articles == "poly") {
    paste0("poly(log_nb_news_articles, ", poly_degrees$log_nb_news_articles, ")")
  } else {
    "bs(log_nb_news_articles, knots = knots_nb_news_articles)"
  }
  
  term_movie_budget <- if(trans_movie_budget == "poly") {
    paste0("poly(log_movie_budget, ", poly_degrees$log_movie_budget, ")")
  } else {
    "bs(log_movie_budget, knots = knots_movie_budget)"
  }
  
  term_nb_faces <- if(trans_nb_faces == "poly") {
    paste0("poly(log_nb_faces, ", poly_degrees$log_nb_faces, ")")
  } else {
    "bs(log_nb_faces, knots = knots_nb_faces)"
  }
  
  term_duration <- if(trans_duration == "poly") {
    paste0("poly(log_duration, ", poly_degrees$log_duration, ")")
  } else {
    "bs(log_duration, knots = knots_duration)"
  }
  
  term_release_year <- if(trans_release_year == "poly") {
    paste0("poly(release_year, ", poly_degrees$release_year, ")")
  } else {
    "bs(release_year, knots = knots_release_year)"
  }
  
  term_movie_meter_IMDBpro <- if(trans_movie_meter_IMDBpro == "poly") {
    paste0("poly(log_movie_meter_IMDBpro, ", poly_degrees$log_movie_meter_IMDBpro, ")")
  } else {
    "bs(log_movie_meter_IMDBpro, knots = knots_movie_meter_IMDBpro)"
  }
  
  # Combine the terms into a full model formula (including other predictors)
  formula_str <- paste("imdb_score ~", 
                       term_nb_news_articles, "+", 
                       term_movie_budget, "+", 
                       term_nb_faces, "+", 
                       term_duration, "+", 
                       term_release_year, "+",
                       term_movie_meter_IMDBpro, "+",
                       "action + romance + drama + animation + NC17 + R + TV14 + TVG + lowest_star_meter")
  model_formula <- as.formula(formula_str)
  
  # Fit the candidate model (using glm, no weights here for simplicity)
  candidate_model <- glm(model_formula, data = movies2)
  
  # Evaluate candidate using 10-fold CV (using cv.glm)
  cv_candidate <- cv.glm(movies2, candidate_model, K = 10)
  candidate_options$CV_MSE[i] <- cv_candidate$delta[1]
  
  cat("Candidate", i, ":", formula_str, "\nCV MSE:", cv_candidate$delta[1], "\n\n")
}

# Identify the best candidate (lowest CV_MSE)
best_candidate <- candidate_options[which.min(candidate_options$CV_MSE), ]
print(best_candidate)

# Build the final model using the best candidate transformation options
final_hybrid_model <- lm(
  imdb_score ~ poly(log_nb_news_articles, 2) +
    poly(log_movie_budget, 2) +
    poly(log_nb_faces, 4) +
    poly(log_duration, 3) +
    bs(release_year, knots = knots_release_year) +
    poly(log_movie_meter_IMDBpro, 4) +
    action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2
)
summary(final_hybrid_model)
mse_final_hybrid <- mean(final_hybrid_model$residuals^2)
cat("MSE:", mse_final_hybrid, "\n")


# R2 = 0.48, MSE = 0.59



################################################################################
## Version 2 (more advanced)
## For each continuous predictor, consider either:
##   - A polynomial transformation with degree in {1, 2, 3, 4, 5}, OR
##   - A spline transformation with 4 knots (20th, 40th, 60th, 80th percentiles).
## The grid search uses 10-fold CV to select the configuration with the lowest CV MSE.
################################################################################

# Define candidate options for each predictor: 5 options for poly (degrees 1 to 5) and 1 for spline
candidate_levels <- c("poly1", "poly2", "poly3", "poly4", "poly5", "spline4")

# Create the candidate grid for all six predictors:
hybrid_grid <- expand.grid(
  log_nb_news_articles = candidate_levels,
  log_movie_budget     = candidate_levels,
  log_nb_faces         = candidate_levels,
  log_duration         = candidate_levels,
  release_year         = candidate_levels,
  log_movie_meter_IMDBpro = candidate_levels,
  stringsAsFactors = FALSE
)
hybrid_grid$CV_MSE <- NA  # to store CV MSE for each candidate

# Pre-calculate spline knots for each variable for both 3 and 4 knots options.
# For 3 knots (25th, 50th, 75th percentiles):
knots_nb_news_3       <- quantile(movies2$log_nb_news_articles, probs = c(0.25, 0.5, 0.75))
knots_movie_budget_3    <- quantile(movies2$log_movie_budget, probs = c(0.25, 0.5, 0.75))
knots_nb_faces_3        <- quantile(movies2$log_nb_faces, probs = c(0.25, 0.5, 0.75))
knots_duration_3        <- quantile(movies2$log_duration, probs = c(0.25, 0.5, 0.75))
knots_release_year_3    <- quantile(movies2$release_year, probs = c(0.25, 0.5, 0.75))
knots_movie_meter_3     <- quantile(movies2$log_movie_meter_IMDBpro, probs = c(0.25, 0.5, 0.75))

# For 4 knots (20th, 40th, 60th, 80th percentiles):
knots_nb_news_4       <- quantile(movies2$log_nb_news_articles, probs = c(0.2, 0.4, 0.6, 0.8))
knots_movie_budget_4    <- quantile(movies2$log_movie_budget, probs = c(0.2, 0.4, 0.6, 0.8))
knots_nb_faces_4        <- quantile(movies2$log_nb_faces, probs = c(0.2, 0.4, 0.6, 0.8))
knots_duration_4        <- quantile(movies2$log_duration, probs = c(0.2, 0.4, 0.6, 0.8))
knots_release_year_4    <- quantile(movies2$release_year, probs = c(0.2, 0.4, 0.6, 0.8))
knots_movie_meter_4     <- quantile(movies2$log_movie_meter_IMDBpro, probs = c(0.2, 0.4, 0.6, 0.8))

# Function to build a transformation term for a given predictor based on candidate option.
# 'varname' is the name of the variable (as a string),
# 'option' is one of "poly1", "poly2", "poly3", "poly4", "poly5", or "spline4".
build_term <- function(varname, option) {
  if(grepl("^poly", option)) {
    deg <- as.numeric(sub("poly", "", option))
    term <- paste0("poly(", varname, ", ", deg, ")")
  } else {
    # Only "spline4" is allowed for splines in this version.
    if(varname == "log_nb_news_articles") {
      term <- "bs(log_nb_news_articles, knots = knots_nb_news_4)"
    } else if(varname == "log_movie_budget") {
      term <- "bs(log_movie_budget, knots = knots_movie_budget_4)"
    } else if(varname == "log_nb_faces") {
      term <- "bs(log_nb_faces, knots = knots_nb_faces_4)"
    } else if(varname == "log_duration") {
      term <- "bs(log_duration, knots = knots_duration_4)"
    } else if(varname == "release_year") {
      term <- "bs(release_year, knots = knots_release_year_4)"
    } else if(varname == "log_movie_meter_IMDBpro") {
      term <- "bs(log_movie_meter_IMDBpro, knots = knots_movie_meter_4)"
    }
  }
  return(term)
}

# Set up cross-validation parameters
library(boot)
K_folds <- 10

# Loop over each candidate combination in the hybrid grid
for(i in 1:nrow(hybrid_grid)) {
  opt_nb_news <- hybrid_grid$log_nb_news_articles[i]
  opt_movie_budget <- hybrid_grid$log_movie_budget[i]
  opt_nb_faces <- hybrid_grid$log_nb_faces[i]
  opt_duration <- hybrid_grid$log_duration[i]
  opt_release_year <- hybrid_grid$release_year[i]
  opt_movie_meter <- hybrid_grid$log_movie_meter_IMDBpro[i]
  
  term_nb_news <- build_term("log_nb_news_articles", opt_nb_news)
  term_movie_budget <- build_term("log_movie_budget", opt_movie_budget)
  term_nb_faces <- build_term("log_nb_faces", opt_nb_faces)
  term_duration <- build_term("log_duration", opt_duration)
  term_release_year <- build_term("release_year", opt_release_year)
  term_movie_meter <- build_term("log_movie_meter_IMDBpro", opt_movie_meter)
  
  formula_str <- paste("imdb_score ~", 
                       term_nb_news, "+", 
                       term_movie_budget, "+", 
                       term_nb_faces, "+", 
                       term_duration, "+", 
                       term_release_year, "+",
                       term_movie_meter, "+",
                       "action + romance + drama + animation + NC17 + R + TV14 + TVG + lowest_star_meter")
  candidate_formula <- as.formula(formula_str)
  
  candidate_model <- glm(candidate_formula, data = movies2)
  
  cv_candidate <- cv.glm(movies2, candidate_model, K = K_folds)
  hybrid_grid$CV_MSE[i] <- cv_candidate$delta[1]
  
  cat("Candidate", i, ":", formula_str, "\nCV MSE:", cv_candidate$delta[1], "\n\n")
}

# Identify the best candidate (lowest CV MSE)
best_candidate <- hybrid_grid[which.min(hybrid_grid$CV_MSE), ]
print(best_candidate)

# Rebuild the final hybrid model using the best candidate configuration
best_nb_news <- build_term("log_nb_news_articles", best_candidate$log_nb_news_articles)
best_movie_budget <- build_term("log_movie_budget", best_candidate$log_movie_budget)
best_nb_faces <- build_term("log_nb_faces", best_candidate$log_nb_faces)
best_duration <- build_term("log_duration", best_candidate$log_duration)
best_release_year <- build_term("release_year", best_candidate$release_year)
best_movie_meter <- build_term("log_movie_meter_IMDBpro", best_candidate$log_movie_meter_IMDBpro)

final_formula_str <- paste("imdb_score ~", 
                           best_nb_news, "+", 
                           best_movie_budget, "+", 
                           best_nb_faces, "+", 
                           best_duration, "+", 
                           best_release_year, "+",
                           best_movie_meter, "+",
                           "action + romance + drama + animation + NC17 + R + TV14 + TVG + lowest_star_meter")
final_formula <- as.formula(final_formula_str)

final_hybrid_model <- lm(final_formula, data = movies2, weights = w_new)
summary(final_hybrid_model)
mse_final_hybrid <- mean(final_hybrid_model$residuals^2)
cat("MSE:", mse_final_hybrid, "\n")

# log_nb_news_articles log_movie_budget log_nb_faces log_duration release_year log_movie_meter_IMDBpro
#                poly1            poly2        poly3      spline4        poly4                   poly4



# final_hybrid_model perf: R2 = 0.542	MSE = 0.598
# _____________________________________________________
# versus Final Poly Model: R2 = 0.537 MSE = 0.603



# CONCLUSION
# The final hybrid model achieves an R² of 0.542 and an MSE of 0.598, 
# compared to the final poly model's R² of 0.537 and MSE of 0.603.
# This represents only a marginal improvement (R² increase of 0.005 and MSE reduction of 0.005),
# which does not justify the increased complexity of the hybrid approach.
# Therefore, we choose the simpler final poly model as our final model for making predictions.






####################################################################################
##    Making predictions for the 12 upcoming movies using our final_poly_model    ##
####################################################################################

# 1. Read the test data
test_data_12 <- read.csv('/Users/roycim/Downloads/12_test_data.csv', stringsAsFactors = FALSE)

# 2. Apply the same transformations used in training
test_data_12$log_nb_news_articles <- log1p(test_data_12$nb_news_articles)
test_data_12$log_movie_budget     <- log1p(test_data_12$movie_budget)
test_data_12$log_nb_faces         <- log1p(test_data_12$nb_faces)
test_data_12$log_duration         <- log(test_data_12$duration)
test_data_12$log_movie_meter_IMDBpro <- log1p(test_data_12$movie_meter_IMDBpro)

test_data_12$NC17 <- ifelse(test_data_12$maturity_rating == "NC-17", 1, 0)
test_data_12$R    <- ifelse(test_data_12$maturity_rating == "R",    1, 0)
test_data_12$TV14 <- ifelse(test_data_12$maturity_rating == "TV-14", 1, 0)
test_data_12$TVG  <- ifelse(test_data_12$maturity_rating == "TV-G",  1, 0)

test_data_12$lowest_star_meter <- apply(
  test_data_12[, c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter")],
  1,
  min,
  na.rm = TRUE
)

# 3. Generate Predictions
predicted_scores <- predict(final_poly_model, newdata = test_data_12)
test_data_12$predicted_imdb_score <- predicted_scores

# 4. Rank Movies by Predicted Score
test_data_12 <- test_data_12[order(-test_data_12$predicted_imdb_score), ]
test_data_12$rank <- 1:nrow(test_data_12)

# 5. Output the Predictions and Rankings
print(test_data_12[, c("predicted_imdb_score", "rank")])
write.csv(test_data_12, "/Users/roycim/Downloads/predicted_movies_ranking.csv", row.names = FALSE)

# Predictions:
# predicted_imdb_score    rank
# 5.899998                1
# 5.578331                2
# 5.446489                3
# 5.440979                4
# 5.188210                5
# 5.167653                6
# 5.018651                7
# 5.014472                8
# 4.762248                9
# 4.540813               10
# 4.369035               11
# 4.316898               12

# We notice these predicted scores seem somewhat low compared to typical training fitted values.
# Let's investigate briefly:

# Compare means
cat("MEAN FITTED SCORE (Training)")
cat(mean(fitted(final_poly_model)), "\n") # 6.53 mean

cat("MEAN PREDICTED SCORE (Test Data)")
cat(mean(test_data_12$predicted_imdb_score), "\n") # 5.06 mean


# Compare distributions: training fitted vs. these 12 test predictions
boxplot(
  list(TrainingFitted   = fitted(final_poly_model),
       TestPredictions  = test_data_12$predicted_imdb_score),
  main = "Training Fitted vs Test Predictions",
  ylab = "IMDb Score",
  col  = c("lightpink", "lightblue"))


# It is possible that one of the variables creates a bias
# => check medians of all numeric variables in both sets
cat("Training Set (movies2) Medians \n")
train_numeric <- sapply(movies2, is.numeric)
print(sapply(movies2[, train_numeric], median, na.rm = TRUE))

cat("Test Set (test_data_12) Medians")
test_numeric <- sapply(test_data_12, is.numeric)
print(sapply(test_data_12[, test_numeric], median, na.rm = TRUE))

# Print Model Coefficients to See How Each Predictor Influences IMDb Score
cat("\n=== FINAL MODEL COEFFICIENTS ===\n")
print(coef(final_poly_model))


# # Observed: The test dataset has a much lower median for nb_news_articles,
# # which likely drives the lower predicted scores (the model is strongly influenced
# # by higher news_articles counts). This indicates potential bias in the training
# # data (older/more popular movies got more coverage).

# # Rationale: Removing or adjusting the nb_news_articles predictor may yield
# # predictions more consistent with new releases that haven't accrued as much media.



################################################################################
## MODEL FITTING WITHOUT NEWS_ARTICLES & PREDICTION ON 12 MOVIES
################################################################################

# PART 1: Fit final model without news_articles (optimal degrees hard-coded)
final_model_no_news <- lm(
  imdb_score ~ poly(log_movie_budget, 3) +
    poly(log_nb_faces, 2) +
    poly(log_duration, 3) +
    poly(release_year, 4) +
    poly(log_movie_meter_IMDBpro, 4) +
    action + romance + drama + animation +
    NC17 + R + TV14 + TVG + lowest_star_meter,
  data = movies2,
  weights = w_new
)
summary(final_model_no_news) 
# R2 = 0.52
cat("MSE (without news_articles):", mean(final_model_no_news$residuals^2), "\n") 
# MSE = 0.624

# PART 2: Predict on test data (12 movies)
test_data_12 <- read.csv('/Users/roycim/Downloads/12_test_data.csv', stringsAsFactors = FALSE)
test_data_12$log_movie_budget         <- log1p(test_data_12$movie_budget)
test_data_12$log_nb_faces             <- log1p(test_data_12$nb_faces)
test_data_12$log_duration             <- log(test_data_12$duration)
test_data_12$log_movie_meter_IMDBpro  <- log1p(test_data_12$movie_meter_IMDBpro)
# release_year remains unchanged.
test_data_12$NC17 <- ifelse(test_data_12$maturity_rating == "NC-17", 1, 0)
test_data_12$R    <- ifelse(test_data_12$maturity_rating == "R", 1, 0)
test_data_12$TV14 <- ifelse(test_data_12$maturity_rating == "TV-14", 1, 0)
test_data_12$TVG  <- ifelse(test_data_12$maturity_rating == "TV-G", 1, 0)
test_data_12$lowest_star_meter <- apply(test_data_12[, c("actor1_star_meter", "actor2_star_meter", "actor3_star_meter")],
                                        1, min, na.rm = TRUE)
test_data_12$predicted_imdb_score <- predict(final_model_no_news, newdata = test_data_12)

# PART 3: Rank and output predictions
test_data_12 <- test_data_12[order(-test_data_12$predicted_imdb_score), ]
test_data_12$rank <- seq_len(nrow(test_data_12))
print(test_data_12[, c("predicted_imdb_score", "rank")])
write.csv(test_data_12, "/Users/roycim/Downloads/predicted_movies_ranking_no_news.csv", row.names = FALSE)



# Predicted IMDb scores and their rankings

# The Day the Earth Blew Up   - 6.296547   (Rank 1)
# Novocaine                   - 6.212451   (Rank 2)
# A Working Man               - 5.971847   (Rank 3)
# The Alto Knights            - 5.838457   (Rank 4)
# Black Bag                   - 5.596099   (Rank 5)
# The Woman in the Yard       - 5.587410   (Rank 6)
# Locked                      - 5.539078   (Rank 7)
# Snow White                  - 5.091080   (Rank 8)
# My Love Will Make You Disappear - 5.081821   (Rank 9)
# Ash                         - 5.046448   (Rank 10)
# High Rollers                - 4.734501   (Rank 11)
# O'Dessa                     - 4.724445   (Rank 12)


