################################################################################
# PRACTICAL 6: LOGISTIC REGRESSION
################################################################################

################################################################################
# PART 1. PREPARE THE WOMEN'S DATA
################################################################################

# The 20 women from Week 5. Hb is measured in g/dl.

women <- data.frame(
  Hb = c(
    11.1, 10.7, 12.4, 14.0, 13.1, 10.5, 9.6, 12.5, 13.5, 13.9,
    15.1, 13.9, 16.2, 16.3, 16.8, 17.1, 16.6, 16.9, 15.7, 16.5
  ),
  Age = c(
    20, 22, 25, 28, 28, 31, 32, 35, 38, 40,
    45, 49, 54, 55, 57, 60, 62, 63, 65, 67
  ),
  Menopause = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1
  )
)

head(women)

# as.integer() converts TRUE and FALSE to 1 and 0.
women$anaemia <- as.integer(women$Hb < 12)

table(women$anaemia)
mean(women$anaemia)  # The proportion with anaemia is 0.20.


# EXERCISE 1
#
# a. Create a column called under_30. Use 1 for women aged under 30
#    and 0 for women aged 30 or older.
women$under_30 <- as.integer(women$Age < 30)
p_under_30 <- mean(women$anaemia[women$under_30 == 1])
p_30_or_over <- mean(women$anaemia[women$under_30 == 0])
# b. Calculate the proportion with anaemia in each age group.
p_under_30
p_30_or_over


################################################################################
# PART 2. COMPARE THE TWO AGE GROUPS
################################################################################

# The rows give the age groups and the columns give the outcomes.
table(women$under_30, women$anaemia)

# family = binomial() fits a logistic model to this 0/1 outcome.

group_fit <- glm(
  anaemia ~ under_30,
  family = binomial(),
  data = women
)

group_result <- summary(group_fit)
group_result

# The reference group is age 30 or older (under_30 = 0).
# Read the Estimate, Std. Error and p-value on the under_30 row.

# Select an entry from the coefficient table by [row, column].
group_log_or <- group_result$coefficients["under_30", "Estimate"]
group_se <- group_result$coefficients["under_30", "Std. Error"]

group_or <- exp(group_log_or)
group_or

# Calculate the Wald interval on the log odds scale, then exponentiate.
group_log_ci <- group_log_or + c(-1, 1) * 1.96 * group_se
group_ci <- exp(group_log_ci)
group_ci

# OR about 4.33, with a 95% CI from 0.42 to 44.4.


# EXERCISE 2
#
# a. Use the proportions from Exercise 1 to calculate the observed
#    odds of anaemia in each age group.
# b. Divide the odds for women under 30 by those for women aged
#    30 or older. Compare this ratio with group_or.

# Discuss how much the confidence interval tells us about the
# direction and size of the association.
odds_under_30 <- p_under_30/(1-p_under_30)
odds_30_or_over <- p_30_or_over/(1-p_30_or_over)
observed_or <- odds_under_30 / odds_30_or_over
observed_or



################################################################################
# PART 3. MODEL AGE IN YEARS (35–55 MINUTES)
################################################################################

# Demonstrate predict() with the age-group model first.
# newdata gives the predictor values, using the name in the formula.
# type = "response" gives predicted probabilities.

age_groups <- data.frame(under_30 = c(0, 1))

age_groups$probability <- predict(
  group_fit,
  newdata = age_groups,
  type = "response"
)

age_groups

# The fitted probabilities match the observed proportions in the two groups.


# EXERCISE 3
#
# a. Fit a logistic model using Age in years. Display the summary and
#    calculate the odds ratio for a one-year increase in age.
# b. Predict the probabilities of anaemia at ages 30, 31, 45 and 46.
# c. Calculate the change in probability in percentage points from
#    age 30 to 31 and from age 45 to 46.

age_fit <- glm(
  anaemia ~ Age,
  family = binomial(),
  data = women
)

age_result <- summary(age_fit)
age_result
age_log_or <- age_result$coefficients["Age", "Estimate"]
age_or <- exp(age_log_or)
age_or
#b
ages <- data.frame(Age = c(30, 31, 45 , 46))
ages$probability <- predict(
  age_fit,
  newdata = ages,
  type = "response"
)

ages
#c

# The two one-year comparisons have the same odds ratio, about 0.81.
# Discuss why the changes in probability are different.


# Draw both fits over the observed age range.

age_grid <- data.frame(
  Age = seq(min(women$Age), max(women$Age), length.out = 200)
)

logistic_values <- predict(
  age_fit,
  newdata = age_grid,
  type = "response"
)

straight_fit <- lm(anaemia ~ Age, data = women)
straight_values <- predict(straight_fit, newdata = age_grid)

plot(
  women$Age, women$anaemia,
  xlab = "Age (years)",
  ylab = "Anaemia outcome or fitted value",
  ylim = c(-0.2, 1.05),
  pch = 16,
  col = "grey25"
)

abline(h = 0, col = "grey65", lty = 3)
lines(age_grid$Age, logistic_values, col = "#0072B2", lwd = 2)
lines(age_grid$Age, straight_values, col = "#D55E00", lwd = 2, lty = 2)

legend(
  "topright",
  legend = c("Observed outcome", "Logistic fit", "Straight-line fit"),
  col = c("grey25", "#0072B2", "#D55E00"),
  pch = c(16, NA, NA),
  lty = c(NA, 1, 2),
  lwd = c(NA, 2, 2),
  bty = "n"
)

# The straight-line fit gives a negative value at age 67.


################################################################################
# PART 4. CHECK THE DATA BEFORE ADJUSTING 
################################################################################

# Menopause is coded 0 before menopause and 1 after menopause.

# EXERCISE 4
#
# a. Make a two-way table of menopausal status and anaemia.
# b. Calculate the proportion with anaemia before and after menopause.



# Add menopausal status to the age model.

menopause_fit <- glm(
  anaemia ~ Age + Menopause,
  family = binomial(),
  data = women
)

summary(menopause_fit)

# Compare the Menopause estimate and standard error with the counts.
# All postmenopausal outcomes are 0. Making the menopause coefficient
# more negative keeps improving the fit, so its estimate is unstable.


################################################################################
# EXERCISE 5 (OPTIONAL)
################################################################################

# Recode the age groups so that women under 30 are the reference group.
# Refit the grouped model and compare its odds ratio with 1 / group_or.


################################################################################
# PART 5. ULCERATION IN THE MELANOMA DATA
################################################################################

# The boot package supplies the data. Ulceration was recorded at tumour removal.
# ulcer is 1 for present and 0 for absent. Thickness is measured in mm.

melanoma <- boot::melanoma
melanoma <- melanoma[, c("ulcer", "thickness", "age", "sex")]
head(melanoma)

# Count missing values in each column. All four counts should be zero.
colSums(is.na(melanoma))

# Label the recorded sex codes. Female is the reference group.

melanoma$sex_group <- factor(
  melanoma$sex,
  levels = c(0, 1),
  labels = c("Female", "Male")
)

table(melanoma$sex_group)


# EXERCISE 6
#
# a. Create a factor called ulcer_group with labels Absent and Present.
# b. Tabulate ulcer_group in the rows and sex_group in the columns.
# c. Calculate the percentage of tumours with ulceration within each sex.



# Draw the counts and percentages side by side.

plot_settings <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

barplot(
  ulcer_cases,
  main = "Number with ulceration",
  ylab = "Tumours",
  ylim = c(0, 55),
  col = "#0072B2",
  border = NA
)

barplot(
  ulcer_percent,
  main = "Percentage with ulceration",
  ylab = "Percentage within sex",
  ylim = c(0, 100),
  col = "#0072B2",
  border = NA
)

par(plot_settings)

# Discuss which group has more cases and which has the higher percentage.


################################################################################
# PART 6. CRUDE AND ADJUSTED MODELS
################################################################################

# Investigate the association between tumour thickness and ulceration.

ulcer_crude <- glm(
  ulcer ~ thickness,
  family = binomial(),
  data = melanoma
)

ulcer_adjusted <- glm(
  ulcer ~ thickness + age + sex_group,
  family = binomial(),
  data = melanoma
)

crude_result <- summary(ulcer_crude)
adjusted_result <- summary(ulcer_adjusted)

crude_result$coefficients
adjusted_result$coefficients

# Put the two thickness estimates and their SEs in the same order.

thickness_log_or <- c(
  crude_result$coefficients["thickness", "Estimate"],
  adjusted_result$coefficients["thickness", "Estimate"]
)

thickness_se <- c(
  crude_result$coefficients["thickness", "Std. Error"],
  adjusted_result$coefficients["thickness", "Std. Error"]
)

data.frame(
  Model = c("Crude", "Adjusted"),
  OR = exp(thickness_log_or),
  Lower_95 = exp(thickness_log_or - 1.96 * thickness_se),
  Upper_95 = exp(thickness_log_or + 1.96 * thickness_se)
)

# The adjusted OR compares tumours 1 mm apart in thickness for patients
# of the same age and sex.

# With the same outcome and observations, lower AIC indicates a better
# trade-off between fit and model complexity.
AIC(ulcer_crude, ulcer_adjusted)

# Use the adjusted model to compare two women aged 60.

thickness_profiles <- data.frame(
  thickness = c(2, 4),
  age = c(60, 60),
  sex_group = c("Female", "Female")
)

thickness_profiles$probability <- predict(
  ulcer_adjusted,
  newdata = thickness_profiles,
  type = "response"
)

thickness_profiles


################################################################################
# PART 7. INDEPENDENT WORK 
################################################################################

# EXERCISE 7
#
# Investigate the association between sex and tumour ulceration.
#
# a. Fit a logistic model with sex_group as the only predictor.
# b. Use your fit and ulcer_adjusted to compare the crude and adjusted
#    odds ratios for Male versus Female. Calculate both 95% Wald intervals.
# c. Predict from ulcer_adjusted for a woman and a man, both aged 60
#    and both with a 3 mm tumour. Calculate the probability difference
#    in percentage points (Male minus Female).



################################################################################
# DISCUSSION 
################################################################################

# Compare the change in the sex odds ratio with the width of its intervals.
# Which patient characteristics are held fixed in the predicted comparison?