################################################################################
# PRACTICAL 4: HYPOTHESIS TESTING AND STATISTICAL TESTS
################################################################################

# This practical follows Lecture 4.
#
# We will:
#   * calculate p-values from test statistics;
#   * calculate confidence intervals for differences;
#   * perform one-sample, paired and independent-sample t-tests;
#   * perform chi-squared tests for categorical outcomes;
#   * relate confidence intervals and hypothesis tests.
#
# This practical uses base R together with built-in datasets.
################################################################################


################################################################################
# PART 1. TEST STATISTICS, P-VALUES AND CONFIDENCE INTERVALS
################################################################################

# Physiotherapy example from the lecture.

difference <- 211 - 123

difference_se <- sqrt(
  118^2 / 93 +
    99^2 / 91
)

difference
difference_se

z <- difference / difference_se

z

# Two-sided p-value

2 * (1 - pnorm(abs(z)))

# 95% confidence interval

difference + c(-1, 1) * 1.96 * difference_se


# EXERCISE 1
#
# Another study reports:
#
# Treatment:
#   n = 64, mean = 82, SD = 20
#
# Control:
#   n = 60, mean = 75, SD = 18
#
# a. Calculate the difference between means.


# b. Calculate the SE of the difference.


# c. Calculate the z statistic.


# d. Calculate the two-sided p-value.
#2*(1-p)
# e. Calculate the 95% confidence interval.


################################################################################
# PART 2. ONE-SAMPLE t-TEST
################################################################################

# Example from the lecture.

heights <- c(
  178,165,181,166,
  170,168,182,183
)

mean(heights)
sd(heights)

n <- length(heights)

se <- sd(heights) / sqrt(n)

t <- (mean(heights) - 175) / se

t

t.test(heights,
       mu = 175,
       alternative = "two.sided")


# EXERCISE 2
#
# A random sample of students has heights:
#
# 171 166 173 179 170 168 175
#
# Test:
#
# H0: population mean = 175 cm
#
heights <- c(
  171,166,173,179,
  170,168,175
)

mean(heights)
sd(heights)

n <- length(heights)

se <- sd(heights) / sqrt(n)

t <- (mean(heights) - 175) / se

t

t.test(heights,
       mu = 175,
       alternative = "two.sided")


# a. Calculate the sample mean.
# b. Calculate the sample SD.
# c. Calculate the estimated SE.
# d. Calculate the t statistic manually.
# e. Verify using t.test().





################################################################################
# PART 3. INDEPENDENT TWO-SAMPLE t-TESTS
################################################################################

data(ToothGrowth)

head(ToothGrowth)

# Compare the two supplement types.

boxplot(
  len ~ dose,
  data = ToothGrowth,
  col = c("lightblue","lightpink"),
  ylab = "Tooth length",
  xlab = "Supplement"
)
#supp
OJ <- ToothGrowth$len[
  ToothGrowth$supp == "OJ"
]

VC <- ToothGrowth$len[
  ToothGrowth$supp == "VC"
]

mean(OJ)
mean(VC)

t.test(OJ, VC)


# EXERCISE 3
#
# Compare dose 0.5 mg with dose 1 mg.
#
# a. Create two vectors containing the observations.


# b. Calculate the two sample means.

# c. Perform a Welch two-sample t-test.
# d. Extract the p-value.
# e. Extract the confidence interval.



################################################################################
# PART 4. PAIRED t-TESTS
################################################################################

data(sleep)

head(sleep)

# Measurements come from the same subjects under
# two treatments.

group1 <- sleep$extra[
  sleep$group == 1
]

group2 <- sleep$extra[
  sleep$group == 2
]

mean(group1)
mean(group2)

paired_result <- t.test(
  group2,
  group1,
  paired = TRUE
)

paired_result


# EXERCISE 4
#
# a. Calculate the within-subject differences:
#      group2 - group1
#
# b. Calculate the mean difference.
#
# c. Calculate the SD of the differences.
#
# d. Verify the result using a paired t-test.



################################################################################
# PART 5. DIFFERENCES BETWEEN PROPORTIONS
################################################################################

# Incontinence study from the lecture.

intervention_success <- 112
intervention_total <- 279

control_success <- 76
control_total <- 245

p_int <- intervention_success /
  intervention_total

p_con <- control_success /
  control_total

difference <- p_int - p_con

difference

# Pooled proportion under H0.

pooled_p <-
  
  (intervention_success +
     control_success) /
  
  (intervention_total +
     control_total)

pooled_p

se_null <- sqrt(
  
  pooled_p *
    (1 - pooled_p) *
    
    (1 / intervention_total +
       1 / control_total)
  
)

se_null

z <- difference / se_null

z

2 * (1 - pnorm(abs(z)))


# EXERCISE 5
#
# Treatment:
#     48 successes from 120 patients
#
# Control:
#     34 successes from 110 patients
#
# a. Calculate the two sample proportions.

p_1 <- 48/120
p_2 <- 34/110

# b. Calculate the pooled proportion.

p_pooled <- (48+34)/(120+110)

# c. Calculate the standard error under H0.
se_null <- sqrt(
  p_pooled *
    (1-p_pooled)*
    
    (1/120+
       1/110)
)
se_null
# d. Calculate the z statistic.

z <- (p_1-p_2/se_null)
z
# e. Calculate the p-value.




################################################################################
# PART 6. CHI-SQUARED TESTS
################################################################################

# Leg ulcer study from the lecture.

ulcer <- matrix(
  c(
    22,98,
    17,96
  ),
  nrow = 2,
  byrow = TRUE
)

rownames(ulcer) <-
  c("Intervention","Control")

colnames(ulcer) <-
  c("Healed","Not_healed")

ulcer

chisq.test(
  ulcer,
  correct = FALSE
)

chisq.test(
  ulcer,
  correct = FALSE
)$expected


# EXERCISE 6
#
# Create the table:
#
#               Improved Not_improved
# Treatment          25           55
# Control            16           64
#
# a. Enter the table into R.

#Treatment_Improved <- 25
#Treatment_Not <- 55
#Control_Improved <- 16
#Control_not <- 64

trial <- matrix(c(25 , 16, 55, 64),
                nrow = 2,
                byrow = FALSE)
trial
# b. Perform a chi-squared test.
chisq.test(trial, correct = FALSE)$expected
# c. Display the expected counts.

# d. Repeat using Yates' correction.



################################################################################
# PART 7. CONFIDENCE INTERVALS AND SIGNIFICANCE
################################################################################

# A confidence interval can often be extracted directly.

tooth_result <- t.test(
  OJ,
  VC
)

tooth_result$conf.int

tooth_result$p.value


# EXERCISE 7
#
# Extract:
#
# a. the confidence interval;
# b. the p-value
#
# from:
#
#    paired_result
#
# generated in Part 4.



################################################################################
# EXERCISE 8: REVIEW
################################################################################

# Perform the following analyses.
#
# 1. One-sample t-test:
#      Compare
#      176 171 180 169 175 177
#      against a reference mean of 170.
#
# 2. Independent two-sample t-test:
#      Compare tooth length between OJ and VC.
#
# 3. Paired t-test:
#      Repeat the sleep analysis.
#
# 4. Chi-squared test:
#      Repeat the ulcer-healing analysis.
#
# For each analysis:
#
# a. Record the p-value.
# b. Record the confidence interval if available.
# c. State whether the result is statistically significant
#    at the 5% level.



################################################################################
# AFTER CLASS
################################################################################

# Make sure you can:
#
# * calculate a p-value from a z statistic;
# * perform a one-sample t-test;
# * perform a paired t-test;
# * perform an independent two-sample t-test;
# * perform a chi-squared test;
# * extract p-values and confidence intervals from R output;
# * explain the relationship between confidence intervals
#   and statistical significance.
#
# Help pages:
#
# ?pnorm
# ?t.test
# ?chisq.test
# ?ToothGrowth
# ?sleep
################################################################################
