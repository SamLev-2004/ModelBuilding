################################################################################
################################# Midterm Code #################################
################################################################################

# Kenitha Chapdelaine (261051765)
# Cara-Li Farrell (261051787)
# Lena Samak (261006632)
# Sara Matin (260984352)
# Alexandra White (261048097)

############################# Loading the dataset ##############################
imdb_data=read.csv("/Users/cara-lifarrell/Desktop/Midterm Project/IMDB_data_Fall_2023.csv")
attach(imdb_data)

############################## Loading librairies ##############################
library(car)
library(caret)
library(plyr)
library(boot)
library(splines)
library(stargazer)
library(ggpubr)
library(ggplot2)
require(methods)
library(dplyr)

######################## Preliminary Data Preprocessing ########################

#### Dropping variables ####

# Reasoning:
# - movie_title, movie_id, and imdb_link are irrelevant for analysis.
# - actor1, actor2, and actor3 are already represented in terms of the 
#   actor_star_meter, which are rankings of the actors and unique identifiers.
# - genres encompasses all the individual genres that were already dummified in 
#   the dataset.
# - plot_keywords would require text analysis, which is out of scope of this course.

# Columns to drop
columns_to_drop <- c(
  "movie_title", "movie_id", "imdb_link",
  "actor1", "actor2", "actor3",
  "genres", "plot_keywords")

# Dropping columns
imdb_data <- imdb_data[, !names(imdb_data) %in% columns_to_drop]

#### Transforming release_month into numerical values ####

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# Loop to replace month names with corresponding numbers
for (i in 1:length(month_names)) {
  imdb_data$release_month[imdb_data$release_month == month_names[i]] <- i
}

# Turn char into numeric 
class(release_month)
imdb_data$release_month = as.numeric(imdb_data$release_month)
attach(imdb_data)

############################## Data Description ################################

#### Dataset overview ####
summary(imdb_data)

#### Testing for collinearity ####

# Correlation Matrix
quantvars=imdb_data[, c(2,3,4,5,6,10,12,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)]
corr_matrix=cor(quantvars)
round(corr_matrix,3)

#### imdb_score (dependent variable) ####
# Variable exploration
summary(imdb_score)
hist(imdb_score)
boxplot(imdb_score)

#### movie_budget ####
# Extra preprocessing: Scaling the movie_budget to millions
imdb_data$movie_budget = (as.numeric(imdb_data$movie_budget)/1000000)
attach(imdb_data)

# Variable exploration
summary(movie_budget)
hist(movie_budget)
boxplot(movie_budget)

plot(movie_budget,imdb_score)
lm.fit=lm(imdb_score~movie_budget)
lm.fit
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
# P-values > 0.1, so linear 

# Variable relationship exploration
# Polynomials
budget1 = lm(imdb_score~movie_budget)
budget2 = lm(imdb_score~poly(movie_budget,2))
budget3 = lm(imdb_score~poly(movie_budget,3))
budget4 = lm(imdb_score~poly(movie_budget,4))
budget5 = lm(imdb_score~poly(movie_budget,5))

names(budget1$coefficients) <- c('Intercept', 'movie_budget')
names(budget2$coefficients) <- c('Intercept', 'movie_budget', 'movie_budget_2')
names(budget3$coefficients) <- c('Intercept', 'movie_budget', 'movie_budget_2', 'movie_budget_3')
names(budget4$coefficients) <- c('Intercept', 'movie_budget', 'movie_budget_2', 'movie_budget_3', 'movie_budget_4')
names(budget5$coefficients) <- c('Intercept', 'movie_budget', 'movie_budget_2', 'movie_budget_3', 'movie_budget_4', 'movie_budget_5')

# Stargazer table
stargazer(budget1, budget2, budget3, budget4, budget5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "movie budget", "(movie budget)<sup>2</sup>", "(movie budget)<sup>3</sup>","(movie budget)<sup>4</sup>","(movie budget)<sup>5</sup>"))



plot = ggplot(imdb_data, aes(y=imdb_score, x=movie_budget))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(budget1, budget2, budget3, budget4, budget5)

# Splines
k1= quantile(movie_budget,.20)
k2= quantile(movie_budget,.40)
k3= quantile(movie_budget,.60)
k4= quantile(movie_budget,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(movie_budget,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse
which.min(mse)
min(mse)

# Polynomial
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(movie_budget,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(movie_budget,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### release_day ####
# Variable exploration
summary(release_day)
hist(release_day)
boxplot(release_day)

plot(release_day,imdb_score)
lm.fit=lm(imdb_score~release_day)
lm.fit
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
# P-values > 0.1, so linear 

# Variable relationship exploration
# Polynomials
rd_1 = lm(imdb_score~release_day)
rd_2 = lm(imdb_score~poly(release_day,2))
rd_3 = lm(imdb_score~poly(release_day,3))
rd_4 = lm(imdb_score~poly(release_day,4))
rd_5 = lm(imdb_score~poly(release_day,5))

names(rd_1$coefficients) <- c('Intercept', 'release_day')
names(rd_2$coefficients) <- c('Intercept', 'release_day', 'release_day_2')
names(rd_3$coefficients) <- c('Intercept', 'release_day', 'release_day_2', 'release_day_3')
names(rd_4$coefficients) <- c('Intercept', 'release_day', 'release_day_2', 'release_day_3', 'release_day_4')
names(rd_5$coefficients) <- c('Intercept', 'release_day', 'release_day_2', 'release_day_3', 'release_day_4', 'release_day_5')

# Stargazer table
stargazer(rd_1, rd_2, rd_3, rd_4, rd_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "release day", "(release day)<sup>2</sup>", "(release day)<sup>3</sup>","(release day)<sup>4</sup>","(release day)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=release_day))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(rd_1, rd_2, rd_3, rd_4, rd_5)
# quadratic model d=2

# Splines
k1= quantile(release_day,.20)
k2= quantile(release_day,.40)
k3= quantile(release_day,.60)
k4= quantile(release_day,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(release_day,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(release_day,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### release_month ####
# Variable exploration
summary(release_month)
hist(release_month)
boxplot(release_month)

plot(release_month,imdb_score)
lm.fit=lm(imdb_score~release_month)
lm.fit
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
# P-values < 0.1, so not linear 

# Variable relationship exploration
# Polynomials
rm_1 = lm(imdb_score~release_month)
rm_2 = lm(imdb_score~poly(release_month,2))
rm_3 = lm(imdb_score~poly(release_month,3))
rm_4 = lm(imdb_score~poly(release_month,4))
rm_5 = lm(imdb_score~poly(release_month,5))

names(rm_1$coefficients) = c('Intercept', 'release_month')
names(rm_2$coefficients) = c('Intercept', 'release_month', 'release_month_2')
names(rm_3$coefficients) = c('Intercept', 'release_month', 'release_month_2', 'release_month_3')
names(rm_4$coefficients) = c('Intercept', 'release_month', 'release_month_2', 'release_month_3', 'release_month_4')
names(rm_5$coefficients) = c('Intercept', 'release_month', 'release_month_2', 'release_month_3', 'release_month_4', 'release_month_5')

# Stargazer table
stargazer(rm_1, rm_2, rm_3, rm_4, rm_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "release month", "(release month)<sup>2</sup>", "(release month)<sup>3</sup>","(release month)<sup>4</sup>","(release month)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=release_month))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(rm_1, rm_2, rm_3, rm_4, rm_5)
# quadratic model d=2

# Splines
k1= quantile(release_month,.20)
k2= quantile(release_month,.40)
k3= quantile(release_month,.60)
k4= quantile(release_month,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(release_month,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(release_month,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### release_year ####
# Variable Exploration
summary(release_year)
hist(release_year)
boxplot(release_year)

plot(release_year,imdb_score)
lm.fit=lm(imdb_score~release_year)
lm.fit
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
# P-values < 0.1, so not linear 

# Variable relationship exploration
# Polynomials
ry_1 = lm(imdb_score~release_year)
ry_2 = lm(imdb_score~poly(release_year,2))
ry_3 = lm(imdb_score~poly(release_year,3))
ry_4 = lm(imdb_score~poly(release_year,4))
ry_5 = lm(imdb_score~poly(release_year,5))


names(ry_1$coefficients) = c('Intercept', 'release_year')
names(ry_2$coefficients) = c('Intercept', 'release_year', 'release_year_2')
names(ry_3$coefficients) = c('Intercept', 'release_year', 'release_year_2', 'release_year_3')
names(ry_4$coefficients) = c('Intercept', 'release_year', 'release_year_2', 'release_year_3', 'release_year_4')
names(ry_5$coefficients) = c('Intercept', 'release_year', 'release_year_2', 'release_year_3', 'release_year_4', 'release_year_5')

# Stargazer table
stargazer(ry_1, ry_2, ry_3, ry_4, ry_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "release year", "(release year)<sup>2</sup>", "(release year)<sup>3</sup>","(release year)<sup>4</sup>","(release year)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=release_year))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(ry_1, ry_2, ry_3, ry_4, ry_5)
# cubic model (d=3)

# Splines
k1= quantile(release_year,.20)
k2= quantile(release_year,.40)
k3= quantile(release_year,.60)
k4= quantile(release_year,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(release_year,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(release_year,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### duration ####
# Variable Exploration
summary(duration)
hist(duration)
boxplot(duration)

plot(duration,imdb_score)
lm.fit=lm(imdb_score~duration)
lm.fit
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
# P-values < 0.1, so not linear 

# Variable relationship exploration
dur1 = lm(imdb_score~duration)
dur2 = lm(imdb_score~poly(duration,2))
dur3 = lm(imdb_score~poly(duration,3))
dur4 = lm(imdb_score~poly(duration,4))
dur5 = lm(imdb_score~poly(duration,5))

names(dur1$coefficients) = c('Intercept', 'duration')
names(dur2$coefficients) = c('Intercept', 'duration', 'duration_2')
names(dur3$coefficients) = c('Intercept', 'duration', 'duration_2', 'duration_3')
names(dur4$coefficients) = c('Intercept', 'duration', 'duration_2', 'duration_3', 'duration_4')
names(dur5$coefficients) = c('Intercept', 'duration', 'duration_2', 'duration_3', 'duration_4', 'duration_5')

# Stargazer table
stargazer(dur1, dur2, dur3, dur4, dur5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "movie duration", "(movie duration)<sup>2</sup>", "(movie duration)<sup>3</sup>","(movie duration)<sup>4</sup>","(movie duration)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=duration))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(dur1, dur2, dur3, dur4, dur5) 
# quadratic model (d=2)

# Splines
k1= quantile(duration,.20)
k2= quantile(duration,.40)
k3= quantile(duration,.60)
k4= quantile(duration,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(duration,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(duration,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### language ####
# Variable exploration
language_table = table(imdb_data$language)
language_table = sort(language_table, decreasing = TRUE)
language_table_df = as.data.frame(language_table)
print(language_table_df)

# Extra preprocessing: Separating English VS non-English films
# Create a new column "is_english" with 1 for English and 0 for non-English
imdb_data$is_english = ifelse(imdb_data$language == "English", 1, 0)

# Dropping the language column
imdb_data = subset(imdb_data, select = -language)
attach(imdb_data)

#### country ####
# Variable exploration
country_table = table(imdb_data$country)
country_table = sort(country_table, decreasing = TRUE)
country_table_df = as.data.frame(country_table)
print(country_table_df)

# Extra preprocessing: Separating USA VS non-USA based films
# Create a new column "is_USA" with 1 for US-based movies and 0 for non-US-based movies
imdb_data$is_USA = ifelse(imdb_data$country == "USA", 1, 0) 

# Dropping the language column
imdb_data = subset(imdb_data, select = -country)
attach(imdb_data)

#### maturity_rating ####
# Variable exploration
mr_table = table(imdb_data$maturity_rating)
mr_table = sort(mr_table, decreasing = TRUE)
mr_table_df = as.data.frame(mr_table)
print(mr_table_df)

# Extra preprocessing: Separating adult VS non-adult films
# Create a new column "is_adult" with 1 for adult movies and 0 for non-adult movies
# Adult films: M, NC-17, Passed, PG-13, R, TV-14, X
# Non-adult: Approved, G, GP, PG, TV-G

# Creating an "adult_rating" variable
adult_ratings <- c("M", "NC-17", "Passed", "PG-13", "R", "TV-14", "X")


imdb_data$is_adult <- ifelse(imdb_data$maturity_rating %in% adult_ratings, 1, 0)

# Dropping the maturity_rating column
imdb_data = subset(imdb_data, select = -maturity_rating)
attach(imdb_data)

#### aspect_ratio ####
# Variable Exploration
summary(aspect_ratio)
hist(aspect_ratio)
boxplot(aspect_ratio)

plot(aspect_ratio,imdb_score)
lm.fit=lm(imdb_score~aspect_ratio)
lm.fit
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
# There is no P-value for the Tukey test

# Variable relationship exploration
# Polynomials
asp1 = lm(imdb_score~aspect_ratio)
asp2 = lm(imdb_score~poly(aspect_ratio,2))
asp3 = lm(imdb_score~poly(aspect_ratio,3))
asp4 = lm(imdb_score~poly(aspect_ratio,4))
asp5 = lm(imdb_score~poly(aspect_ratio,5))

names(asp1$coefficients) = c('Intercept', 'aspect_ratio')
names(asp2$coefficients) = c('Intercept', 'aspect_ratio', 'aspect_ratio_2')
names(asp3$coefficients) = c('Intercept', 'aspect_ratio', 'aspect_ratio_2', 'aspect_ratio_3')
names(asp4$coefficients) = c('Intercept', 'aspect_ratio', 'aspect_ratio_2', 'aspect_ratio_3', 'aspect_ratio_4')
names(asp5$coefficients) = c('Intercept', 'aspect_ratio', 'aspect_ratio_2', 'aspect_ratio_3', 'aspect_ratio_4', 'aspect_ratio_5')

# Stargazer table
stargazer(asp1, asp2, asp3, asp4, asp5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "Aspect ratio", "(Aspect ratio)<sup>2</sup>", "(Aspect ratio)<sup>3</sup>","(Aspect ratio)<sup>4</sup>","(Aspect ratio)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=aspect_ratio))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(asp1, asp2, asp3, asp4, asp5) 
# Quadratic model (d=2)

# Splines
k1= quantile(aspect_ratio,.20)
k2= quantile(aspect_ratio,.40)
k3= quantile(aspect_ratio,.60)
k4= quantile(aspect_ratio,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(aspect_ratio,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(aspect_ratio,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### distributor #### 
# Extra preprocessing: Combining all Walt Disney and Paramount for distributor 
# Walt Disney Explanation:
# - There are different variations of Disney in the dataset 
#   (i.e. Walt Disney Pictures, Walt Disney Studios, Walt Disney Studios Motion Pictures)
# - We have replaced Walt Disney Studios Motion Pictures and Walt Disney Pictures
#   with Walt Disney Studios.
imdb_data$distributor = gsub("Walt Disney Studios Motion Pictures|Walt Disney Pictures", 
                             "Walt Disney Studios", imdb_data$distributor)
attach(imdb_data)

# Paramount Explanation:
# - There are different variations of Disney in the dataset 
#   (i.e. Paramount Pictures, Paramount Vantage, Paramount Classics)
# - We have replaced Paramount Vantage and Paramount Classics
#   with Paramount Pictures.
imdb_data$distributor = gsub("Paramount Vantage|Paramount Classics", 
                             "Paramount Pictures", imdb_data$distributor)
attach(imdb_data)


# Variable exploration
distributor_table = table(imdb_data$distributor)
distributor_table = sort(distributor_table, decreasing = TRUE)
distributor_table_df = as.data.frame(distributor_table)
print(distributor_table_df)

# Extra preprocessing: Distributor to their count
imdb_data <- imdb_data %>%
  group_by(distributor) %>%
  mutate(distributor_count = n()) %>%
  ungroup() %>%
  mutate(distributor = distributor_count) %>%
  select(-distributor_count)

attach(imdb_data)

# Variable relationship exploration
plot(distributor, imdb_score)
distr1 = lm(imdb_score~distributor)
distr2 = lm(imdb_score~poly(distributor,2))
distr3 = lm(imdb_score~poly(distributor,3))
distr4 = lm(imdb_score~poly(distributor,4))
distr5 = lm(imdb_score~poly(distributor,5))

anova(distr1, distr2, distr3, distr4, distr5)
# Quartic model (d=4)

summary(distr4)

plot = ggplot(imdb_data, aes(y=imdb_score, x=distributor))
scatter=geom_point(color="grey")
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
ggarrange(plot+scatter+line2,plot+scatter+line4 + rremove("x.text"), 
          labels = c("Quadratic", "Quintic"),
          ncol = 2, nrow = 1)

# Splines
k1= quantile(distributor,.333)
k2= quantile(distributor,.666)
knots = c(k1,k2)
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(distributor,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(distributor,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### nb_news_articles ####
# Variable Exploration
summary(nb_news_articles)
hist(nb_news_articles)
boxplot(nb_news_articles)

plot(nb_news_articles,imdb_score)
lm.fit=lm(imdb_score~nb_news_articles)
lm.fit
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)

# Variable relationship exploration
news1 = lm(imdb_score~nb_news_articles)
news2 = lm(imdb_score~poly(nb_news_articles,2))
news3 = lm(imdb_score~poly(nb_news_articles,3))
news4 = lm(imdb_score~poly(nb_news_articles,4))
news5 = lm(imdb_score~poly(nb_news_articles,5))

names(news1$coefficients) = c('Intercept', 'nb_news_articles')
names(news2$coefficients) = c('Intercept', 'nb_news_articles', 'nb_news_articles_2')
names(news3$coefficients) = c('Intercept', 'nb_news_articles', 'nb_news_articles_2', 'nb_news_articles_3')
names(news4$coefficients) = c('Intercept', 'nb_news_articles', 'nb_news_articles_2', 'nb_news_articles_3', 'nb_news_articles_4')
names(news5$coefficients) = c('Intercept', 'nb_news_articles', 'nb_news_articles_2', 'nb_news_articles_3', 'nb_news_articles_4', 'nb_news_articles_5')

# Stargazer table
stargazer(asp1, asp2, asp3, asp4, asp5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "Nb of News Articles", "(Nb of News Articles)<sup>2</sup>", "(Nb of News Articles)<sup>3</sup>","(Nb of News Articles)<sup>4</sup>","(Nb of News Articles)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=nb_news_articles))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(news1, news2, news3, news4, news5) 
# Quartic model (d=4)

# Splines
k1= quantile(nb_news_articles,.20)
k2= quantile(nb_news_articles,.40)
k3= quantile(nb_news_articles,.60)
k4= quantile(nb_news_articles,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(nb_news_articles,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### director ####
# Variable exploration
director_table = table(imdb_data$director)
director_table = sort(director_table, decreasing = TRUE)
director_table_df = as.data.frame(director_table)
print(director_table_df)

# Extra preprocessing: Director to their count
imdb_data <- imdb_data %>%
  group_by(director) %>%
  mutate(director_count = n()) %>%
  ungroup() %>%
  mutate(director = director_count) %>%
  select(-director_count)

attach(imdb_data)

plot(director, imdb_score)
direc1 = lm(imdb_score~director)
direc2 = lm(imdb_score~poly(director,2))
direc3 = lm(imdb_score~poly(director,3))
direc4 = lm(imdb_score~poly(director,4))
direc5 = lm(imdb_score~poly(director,5))
anova(direc1, direc2, direc3, direc4, direc5)
anova(direc2, direc4) # 4th degree is better
summary(direc4)

plot = ggplot(imdb_data, aes(y=imdb_score, x=director))
scatter=geom_point(color="grey")
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
ggarrange(plot+scatter+line2,plot+scatter+line4 + rremove("x.text"), 
          labels = c("Quadratic", "Quintic"),
          ncol = 2, nrow = 1)

# Splines
k1= quantile(director,.333)
k2= quantile(director,.666)
knots = c(k1,k2)
mse=rep(NA,10)

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(director,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### actor1_star_meter ####
# Variable exploration
summary(actor1_star_meter)
hist(actor1_star_meter)
boxplot(actor1_star_meter)
plot(actor1_star_meter,imdb_score)
lm.fit=lm(imdb_score~actor1_star_meter)
abline(lm.fit)

summary(lm.fit)
#p-values > 0.1

library(car)
residualPlots(lm.fit)
# p-value > 0.05, no heteroskedasticity
ncvTest(lm.fit)

# Outlier 
outlierTest(lm.fit)
imdb_data=imdb_data[-c(192)]

#variable relationship exploration  #polynomials
mp_1 = lm(imdb_score~actor1_star_meter)
mp_2 = lm(imdb_score~poly(actor1_star_meter,2))
mp_3 = lm(imdb_score~poly(actor1_star_meter,3))
mp_4 = lm(imdb_score~poly(actor1_star_meter,4))
mp_5 = lm(imdb_score~poly(actor1_star_meter,5))

names(mp_1$coefficients) = c('Intercept', 'actor1_star_meter')
names(mp_2$coefficients) = c('Intercept', 'actor1_star_meter', 'actor1_star_meter_2')
names(mp_3$coefficients) = c('Intercept', 'actor1_star_meter', 'actor1_star_meter_2', 'actor1_star_meter_3')
names(mp_4$coefficients) = c('Intercept', 'actor1_star_meter', 'actor1_star_meter_2', 'actor1_star_meter_3', 'actor1_star_meter_4')
names(mp_5$coefficients) = c('Intercept', 'actor1_star_meter', 'actor1_star_meter_2', 'actor1_star_meter_3', 'actor1_star_meter_4', 'actor1_star_meter_5')

#stargazer table
stargazer(mp_1, mp_2, mp_3, mp_4, mp_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "actor 1 star meter", "(actor 1 star meter)<sup>2</sup>", "(actor 1 star meter)<sup>3</sup>","(actor 1 star meter)<sup>4</sup>","(actor 1 star meter)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=actor1_star_meter))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(mp_1, mp_2, mp_3, mp_4, mp_5)
# Quadratic model (d=2)

# Splines
k1= quantile(actor1_star_meter,.20)
k2= quantile(actor1_star_meter,.40)
k3= quantile(actor1_star_meter,.60)
k4= quantile(actor1_star_meter,.80)

knots = c(k1,k2,k3,k4)

library(splines)
library(boot)
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(actor1_star_meter,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(actor1_star_meter,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### actor2_star_meter ####
# variable exploration
summary(actor2_star_meter)
hist(actor2_star_meter)
boxplot(actor2_star_meter)

plot(actor2_star_meter,imdb_score)
lm.fit=lm(imdb_score~actor2_star_meter)
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
# P-values < 0.1

ncvTest(lm.fit)
# P-value > 0.05, no heteroskedasticity

#outlier 
outlierTest(lm.fit)
imdb_data=imdb_data[-c(1581)]

# Variable relationship exploration 
# Polynomials
mp_1 = lm(imdb_score~actor2_star_meter)
mp_2 = lm(imdb_score~poly(actor2_star_meter,2))
mp_3 = lm(imdb_score~poly(actor2_star_meter,3))
mp_4 = lm(imdb_score~poly(actor2_star_meter,4))
mp_5 = lm(imdb_score~poly(actor2_star_meter,5))

names(mp_1$coefficients) = c('Intercept', 'actor2_star_meter')
names(mp_2$coefficients) = c('Intercept', 'actor2_star_meter', 'actor2_star_meter_2')
names(mp_3$coefficients) = c('Intercept', 'actor2_star_meter', 'actor2_star_meter_2', 'actor2_star_meter_3')
names(mp_4$coefficients) = c('Intercept', 'actor2_star_meter', 'actor2_star_meter_2', 'actor2_star_meter_3', 'actor2_star_meter_4')
names(mp_5$coefficients) = c('Intercept', 'actor2_star_meter', 'actor2_star_meter_2', 'actor2_star_meter_3', 'actor2_star_meter_4', 'actor2_star_meter_5')

# Stargazer table
stargazer(mp_1, mp_2, mp_3, mp_4, mp_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "actor 2 star meter", "(actor 2 star meter)<sup>2</sup>", "(actor 2 star meter)<sup>3</sup>","(actor 2 star meter)<sup>4</sup>","(actor 2 star meter)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=actor2_star_meter))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(mp_1, mp_2, mp_3, mp_4, mp_5)
# Quadratic model (d=2)

# Splines
k1= quantile(actor2_star_meter,.20)
k2= quantile(actor2_star_meter,.40)
k3= quantile(actor2_star_meter,.60)
k4= quantile(actor2_star_meter,.80)

knots = c(k1,k2,k3,k4)

library(splines)
library(boot)
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(actor2_star_meter,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(actor2_star_meter,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### actor3_star_meter ####
# Variable exploration
summary(actor3_star_meter)
hist(actor3_star_meter)
boxplot(actor3_star_meter)

plot(actor3_star_meter,imdb_score)
lm.fit=lm(imdb_score~actor3_star_meter)

abline(lm.fit)

summary(lm.fit)

#p-values < 0.1
library(car)
residualPlots(lm.fit)

#p-value < 0.05, there is heteroskedasticity
ncvTest(lm.fit)

#outlier 
outlierTest(lm.fit)
imdb_data=imdb_data[-c(1581)]

# Variable relationship exploration  #polynomials
mp_1 = lm(imdb_score~actor3_star_meter)
mp_2 = lm(imdb_score~poly(actor3_star_meter,2))
mp_3 = lm(imdb_score~poly(actor3_star_meter,3))
mp_4 = lm(imdb_score~poly(actor3_star_meter,4))
mp_5 = lm(imdb_score~poly(actor3_star_meter,5))

names(mp_1$coefficients) = c('Intercept', 'actor3_star_meter')
names(mp_2$coefficients) = c('Intercept', 'actor3_star_meter', 'actor3_star_meter_2')
names(mp_3$coefficients) = c('Intercept', 'actor3_star_meter', 'actor3_star_meter_2', 'actor3_star_meter_3')
names(mp_4$coefficients) = c('Intercept', 'actor3_star_meter', 'actor3_star_meter_2', 'actor3_star_meter_3', 'actor3_star_meter_4')
names(mp_5$coefficients) = c('Intercept', 'actor3_star_meter', 'actor3_star_meter_2', 'actor3_star_meter_3', 'actor3_star_meter_4', 'actor3_star_meter_5')

# Stargazer table
stargazer(mp_1, mp_2, mp_3, mp_4, mp_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "actor 3 star meter", "(actor 3 star meter)<sup>2</sup>", "(actor 3 star meter)<sup>3</sup>","(actor 3 star meter)<sup>4</sup>","(actor 3 star meter)<sup>5</sup>"))

install.packages("ggplot2")
library(ggplot2)
require(methods)

plot = ggplot(imdb_data, aes(y=imdb_score, x=actor3_star_meter))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(mp_1, mp_2, mp_3, mp_4, mp_5)
# Quadratic model (d=2)

# Splines
k1= quantile(actor3_star_meter,.20)
k2= quantile(actor3_star_meter,.40)
k3= quantile(actor3_star_meter,.60)
k4= quantile(actor3_star_meter,.80)

knots = c(k1,k2,k3,k4)

library(splines)
library(boot)
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(actor3_star_meter,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(actor3_star_meter,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### colour_film ####
# Variable exploration
imdb_data$colour_film=as.factor(imdb_data$colour_film)
levels(colour_film)
table(colour_film)
mreg1=lm(imdb_score~colour_film)
summary(mreg1)

colour_table = table(imdb_data$colour_film)
colour_table = sort(colour_table, decreasing = TRUE)
colour_table_df = as.data.frame(colour_table)
print(colour_table_df)

# Extra preprocessing: Separating Colour VS Black & White films
# Create a new column "is_colour" with 1 for Colour films and 0 for Black & White films
imdb_data$is_colour = ifelse(imdb_data$colour_film == "Color", 1, 0)

# Dropping the colour_film column
imdb_data = subset(imdb_data, select = -colour_film)
attach(imdb_data)

#### genres (action, adventure, scifi, thriller, musical, romance, western, sport, horror, drama, war, animation, crime) ####
# Variable exploration
# Can drop adventure, scifi, sport and animation (not significant)
lm.fit=lm(imdb_score~action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime)

summary(lm.fit)

#### nb_faces  ####
# Variable exploration
summary(nb_faces)
hist(nb_faces)
boxplot(nb_faces)

plot(nb_faces,imdb_score)
lm.fit=lm(imdb_score~nb_faces)
abline(lm.fit)
summary(lm.fit)

residualPlots(lm.fit)
#P-values < 0.1, so not linear

#P-value > 0.05, so there is no heteroskedasticity
ncvTest(lm.fit) 

#There is outlier 
outlierTest(lm.fit)
imdb_data=imdb_data[-c(1581),]

# Variable relationship exploration
# Polynomials
nf_1 = lm(imdb_score~nb_faces)
nf_2 = lm(imdb_score~poly(nb_faces,2))
nf_3 = lm(imdb_score~poly(nb_faces,3))
nf_4 = lm(imdb_score~poly(nb_faces,4))
nf_5 = lm(imdb_score~poly(nb_faces,5))

names(nf_1$coefficients) = c('Intercept', 'nb_faces')
names(nf_2$coefficients) = c('Intercept', 'nb_faces', 'nb_faces_2')
names(nf_3$coefficients) = c('Intercept', 'nb_faces', 'nb_faces_2', 'nb_faces_3')
names(nf_4$coefficients) = c('Intercept', 'nb_faces', 'nb_faces_2', 'nb_faces_3', 'nb_faces_4')
names(nf_5$coefficients) = c('Intercept', 'nb_faces', 'nb_faces_2', 'nb_faces_3', 'nb_faces_4', 'nb_faces_5')

# Stargazer table
stargazer(nf_1, nf_2, nf_3, nf_4, nf_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "num faces", "(num faces)<sup>2</sup>", "(num faces)<sup>3</sup>","(num faces)<sup>4</sup>","(num faces)<sup>5</sup>"))


plot = ggplot(imdb_data, aes(y=imdb_score, x=nb_faces))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(nf_1, nf_2, nf_3, nf_4, nf_5)
# Quadratic model (d=2)

# Splines
k1= quantile(nb_faces,.20)
k2= quantile(nb_faces,.40)
k3= quantile(nb_faces,.60)
k4= quantile(nb_faces,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(nb_faces,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(nb_faces,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### movie_meter_IMDBpro ####
# Variable exploration
summary(movie_meter_IMDBpro)
hist(movie_meter_IMDBpro)
boxplot(movie_meter_IMDBpro)

plot(movie_meter_IMDBpro,imdb_score)
lm.fit=lm(imdb_score~movie_meter_IMDBpro)
abline(lm.fit)
summary(lm.fit)

#P-values < 0.1, so not linear
residualPlots(lm.fit)

#P-value < 0.05, so there is heteroskedasticity
ncvTest(lm.fit)

#There is outlier 
outlierTest(lm.fit)
imdb_data=imdb_data[-c(989,1581),]

# Variable relationship exploration
# Polynomials
mp_1 = lm(imdb_score~movie_meter_IMDBpro)
mp_2 = lm(imdb_score~poly(movie_meter_IMDBpro,2))
mp_3 = lm(imdb_score~poly(movie_meter_IMDBpro,3))
mp_4 = lm(imdb_score~poly(movie_meter_IMDBpro,4))
mp_5 = lm(imdb_score~poly(movie_meter_IMDBpro,5))

names(mp_1$coefficients) = c('Intercept', 'movie_meter_IMDBpro')
names(mp_2$coefficients) = c('Intercept', 'movie_meter_IMDBpro', 'movie_meter_IMDBpro_2')
names(mp_3$coefficients) = c('Intercept', 'movie_meter_IMDBpro', 'movie_meter_IMDBpro_2', 'movie_meter_IMDBpro_3')
names(mp_4$coefficients) = c('Intercept', 'movie_meter_IMDBpro', 'movie_meter_IMDBpro_2', 'movie_meter_IMDBpro_3', 'movie_meter_IMDBpro_4')
names(mp_5$coefficients) = c('Intercept', 'movie_meter_IMDBpro', 'movie_meter_IMDBpro_2', 'movie_meter_IMDBpro_3', 'movie_meter_IMDBpro_4', 'movie_meter_IMDBpro_5')

# Stargazer table
stargazer(mp_1, mp_2, mp_3, mp_4, mp_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "movie meter pro", "(movie meter pro)<sup>2</sup>", "(movie meter pro)<sup>3</sup>","(movie meter pro)<sup>4</sup>","(movie meter pro)<sup>5</sup>"))

plot = ggplot(imdb_data, aes(y=imdb_score, x=movie_meter_IMDBpro))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(mp_1, mp_2, mp_3, mp_4, mp_5)
# Quadratic model (d=2)

# Splines
k1= quantile(movie_meter_IMDBpro,.20)
k2= quantile(movie_meter_IMDBpro,.40)
k3= quantile(movie_meter_IMDBpro,.60)
k4= quantile(movie_meter_IMDBpro,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(movie_meter_IMDBpro,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(movie_meter_IMDBpro,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### cinematographer ####
# Variable exploration
cine_table = table(imdb_data$cinematographer)
cine_table = sort(cine_table, decreasing = TRUE)
cine_table_df = as.data.frame(cine_table)
print(cine_table_df)

# Extra preprocessing: Cinematographer to their count
imdb_data <- imdb_data %>%
  group_by(cinematographer) %>%
  mutate(cinematographer_count = n()) %>%
  ungroup() %>%
  mutate(cinematographer = cinematographer_count) %>%
  select(-cinematographer_count)

attach(imdb_data)

# Variable relationship exploration
plot(cinematographer,imdb_score)
cine1 = lm(imdb_score~cinematographer)
cine2 = lm(imdb_score~poly(cinematographer,2))
cine3 = lm(imdb_score~poly(cinematographer,3))
cine4 = lm(imdb_score~poly(cinematographer,4))
cine5 = lm(imdb_score~poly(cinematographer,5))

anova(cine1, cine2, cine3, cine4, cine5)
anova(cine2,cine5)
# Quadratic model (d=2)

summary(cine2)
summary(cine5)

plot = ggplot(imdb_data, aes(y=imdb_score, x=cinematographer))
scatter=geom_point(color="grey")
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
ggarrange(plot+scatter+line2,plot+scatter+line5 + rremove("x.text"), 
          labels = c("Quadratic", "Quintic"),
          ncol = 2, nrow = 1)

# Splines
k1= quantile(cinematographer,.333)
k2= quantile(cinematographer,.666)
knots = c(k1,k2)
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(cinematographer,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(cinematographer,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

#### production_company #### 
# Extra preprocessing: Combining all Walt Disney and Paramount for production company 
imdb_data$production_company = gsub("Walt Disney Studios Motion Pictures", 
                                    "Walt Disney Studios", imdb_data$production_company)

imdb_data$production_company = gsub("Paramount Vantage|Paramount Pictures|Paramount Classics", 
                                    "Paramount Pictures", imdb_data$production_company)

# Extra preprocessing: Production company to their count
imdb_data <- imdb_data %>%
  group_by(production_company) %>%
  mutate(prod_count = n()) %>%
  ungroup() %>%
  mutate(production_company = prod_count) %>%
  select(-production_company)

attach(imdb_data)

# switching the column to numeric data type
imdb_data$prod_count <- as.numeric(imdb_data$prod_count)
attach (imdb_data)

# Rename prod_count to production_company
imdb_data <- imdb_data %>%rename(production_company = prod_count)
attach (imdb_data)

# Variable exploration
summary(production_company)
hist(production_company)
boxplot(production_company)

plot(production_company,imdb_score)
lm.fit=lm(imdb_score~production_company)
abline(lm.fit)
summary(lm.fit)

#P-values > 0.1, so linear
residualPlots(lm.fit)

#P-value < 0.05, so there is heteroskedasticity
ncvTest(lm.fit) 

#There is outlier 
outlierTest(lm.fit)
imdb_data=imdb_data[-c(316),]

attach(imdb_data)

# Variable relationship exploration
# Polynomials
pc_1 = lm(imdb_score~production_company)
pc_2 = lm(imdb_score~poly(production_company,2))
pc_3 = lm(imdb_score~poly(production_company,3))
pc_4 = lm(imdb_score~poly(production_company,4))
pc_5 = lm(imdb_score~poly(production_company,5))

names(pc_1$coefficients) = c('Intercept', 'production_company')
names(pc_2$coefficients) = c('Intercept', 'production_company', 'production_company_2')
names(pc_3$coefficients) = c('Intercept', 'production_company', 'production_company_2', 'production_company_3')
names(pc_4$coefficients) = c('Intercept', 'production_company', 'production_company_2', 'production_company_3', 'production_company_4')
names(pc_5$coefficients) = c('Intercept', 'production_company', 'production_company_2', 'production_company_3', 'production_company_4', 'production_company_5')

# Stargazer table
stargazer(pc_1, pc_2, pc_3, pc_4, pc_5, title="Regression Results",
          align=TRUE, dep.var.labels=c("IMDB score"),
          no.space=TRUE, type="html",
          covariate.labels=c("Intercept", "production company", "(production company)<sup>2</sup>", "(production company)<sup>3</sup>","(production company)<sup>4</sup>","(production company)<sup>5</sup>"))


plot = ggplot(imdb_data, aes(y=imdb_score, x=production_company))
scatter=geom_point(color="grey")
line=geom_smooth(method="lm", formula=y~x, color="blue", se=FALSE)
plot+scatter+line
line2=geom_smooth(method="lm", formula=y~poly(x, 2), color="blue", se=FALSE)
plot+scatter+line2
line3=geom_smooth(method="lm", formula=y~poly(x, 3), color="blue", se=FALSE)
plot+scatter+line3
line4=geom_smooth(method="lm", formula=y~poly(x, 4), color="blue", se=FALSE)
plot+scatter+line4
line5=geom_smooth(method="lm", formula=y~poly(x, 5), color="blue", se=FALSE)
plot+scatter+line5

ggarrange(plot+scatter+line, plot+scatter+line2,
          plot+scatter+line3, plot+scatter+line4, plot+scatter+line5 + rremove("x.text"), 
          labels = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"),
          ncol = 2, nrow = 3)

anova(pc_1, pc_2, pc_3, pc_4, pc_5)
# Quadratic model (d=1)

# Splines
k1= quantile(production_company,.20)
k2= quantile(production_company,.40)
k3= quantile(production_company,.60)
k4= quantile(production_company,.80)

knots = c(k1,k2,k3,k4)

mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~bs(production_company,knots=knots, degree=i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse

# Polynomials without splines
mse=rep(NA,10)
for (i in 1:10){
  fit = glm(imdb_score~poly(production_company,i), data=imdb_data)
  mse[i] = cv.glm(imdb_data, fit, K=10)$delta[1]
}
mse


