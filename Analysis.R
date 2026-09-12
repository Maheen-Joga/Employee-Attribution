
# B105 Applied Statistical Modelling — Individual Project
# Employee Attrition: An Applied Statistical Analysis
# Dataset: IBM HR Analytics Employee Attrition & Performance
# Source: https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset

# 0. Setup 
d <- read.csv("hr_attrition.csv", fileEncoding = "UTF-8-BOM")
str(d)

# 1. Data cleaning 
# Check for missing values and duplicates
sum(is.na(d))
sum(duplicated(d))

# Drop constant / non-informative columns (no analytical value)
d <- d[, !(names(d) %in% c("EmployeeCount", "Over18", "StandardHours", "EmployeeNumber"))]

# Convert key variables to factors
d$Attrition <- factor(d$Attrition, levels = c("No", "Yes"))
d$OverTime  <- factor(d$OverTime,  levels = c("No", "Yes"))
d$Department <- factor(d$Department)

# 2. Descriptive statistics / EDA 
cat("Overall attrition rate:\n")
print(prop.table(table(d$Attrition)))

cat("\nAttrition by OverTime status:\n")
tab_ot <- table(d$OverTime, d$Attrition)
print(tab_ot)
print(round(prop.table(tab_ot, margin = 1) * 100, 1))  # row % (attrition rate within each OT group)

cat("\nMonthlyIncome summary by Attrition group:\n")
print(tapply(d$MonthlyIncome, d$Attrition, summary))
print(tapply(d$MonthlyIncome, d$Attrition, sd))

# Visualisations
png("plot_attrition_overtime.png", width = 900, height = 650, res = 130)
barplot(prop.table(tab_ot, margin = 1)[, "Yes"] * 100,
        col = c("#4C72B0", "#C44E52"),
        ylab = "Attrition rate (%)", xlab = "OverTime",
        main = "Attrition Rate by OverTime Status")
dev.off()

png("plot_income_boxplot.png", width = 900, height = 650, res = 130)
boxplot(MonthlyIncome ~ Attrition, data = d,
        col = c("#4C72B0", "#C44E52"),
        ylab = "Monthly Income (USD)", xlab = "Attrition",
        main = "Monthly Income Distribution by Attrition")
dev.off()

png("plot_income_hist.png", width = 1000, height = 500, res = 130)
par(mfrow = c(1, 2))
hist(d$MonthlyIncome[d$Attrition == "No"], main = "Income: Stayed", xlab = "Monthly Income", col = "#4C72B0")
hist(d$MonthlyIncome[d$Attrition == "Yes"], main = "Income: Left", xlab = "Monthly Income", col = "#C44E52")
dev.off()

# 3. Hypothesis 1: Attrition and OverTime (Chi-square test of independence) 
# H0: Attrition is independent of OverTime status
# H1: Attrition is associated with OverTime status
cat("\n\n--- H1: Chi-square test of independence (Attrition ~ OverTime) ---\n")
chi_test <- chisq.test(tab_ot)
print(chi_test)
print(chi_test$expected)  # check expected counts assumption (all >= 5)
cat("Cramer's V (effect size): ")
n <- sum(tab_ot)
k <- min(dim(tab_ot))
cramers_v <- sqrt(chi_test$statistic / (n * (k - 1)))
print(cramers_v)

# 4. Hypothesis 2: Monthly Income and Attrition (independent samples test) 
# H0: Mean MonthlyIncome is equal between employees who left and stayed
# H1: Mean MonthlyIncome differs between employees who left and stayed
cat("\n\n--- H2: Assumption checks for MonthlyIncome ~ Attrition ---\n")

# Normality: Shapiro-Wilk per group
cat("Shapiro-Wilk (Stayed):\n"); print(shapiro.test(d$MonthlyIncome[d$Attrition == "No"]))
cat("Shapiro-Wilk (Left):\n");   print(shapiro.test(d$MonthlyIncome[d$Attrition == "Yes"]))

# Homogeneity of variance: Levene's test (Brown-Forsythe variant, base R only,
# no internet access to CRAN required)
levene_test <- function(y, group) {
  group <- factor(group)
  medians <- tapply(y, group, median)
  abs_dev <- abs(y - medians[group])
  fit <- aov(abs_dev ~ group)
  summary(fit)[[1]]
}
cat("\nLevene's test (Brown-Forsythe, base R) for equal variances:\n")
print(levene_test(d$MonthlyIncome, d$Attrition))

# Because MonthlyIncome is right-skewed (confirmed by Shapiro-Wilk p < .05 and
# histograms above), the normality assumption for a standard t-test is violated.
# We therefore use the non-parametric Mann-Whitney U test (Wilcoxon rank-sum)
# as the primary inferential test, and report Welch's t-test as a robustness check.

cat("\n--- Primary test: Mann-Whitney U (Wilcoxon rank-sum) ---\n")
wilcox_test <- wilcox.test(MonthlyIncome ~ Attrition, data = d, conf.int = TRUE)
print(wilcox_test)

cat("\n--- Robustness check: Welch's t-test (unequal variances) ---\n")
t_test <- t.test(MonthlyIncome ~ Attrition, data = d)
print(t_test)

# Effect size for Wilcoxon (rank-biserial correlation, approx via z/sqrt(N))
z_val <- qnorm(wilcox_test$p.value / 2, lower.tail = FALSE)
effect_r <- z_val / sqrt(nrow(d))
cat("\nApprox effect size r =", round(effect_r, 3), "\n")

# 5. Session info (for reproducibility appendix) 
sessionInfo()
