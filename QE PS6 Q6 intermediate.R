fert <- read.csv('fertility.csv')
head(fert)
library(estimatr)
library(car)

# Extract values
morekids <- fert$morekids
boy1st <- fert$boy1st
boy2nd <- fert$boy2nd
samesex <- fert$samesex
agem1 <- fert$agem1
black <- fert$black
hispan <- fert$hispan
othrace <- fert$othrace
weeksm1 <- fert$weeksm1

# a)
# OLS regression: Y = B0 + B1Di + U
# morekids = 1 if mum has more than 2 kids
add_kids <- summary(lm_robust(formula = weeksm1 ~ morekids, data = fert))
add_kids
ad_coef <- add_kids$coefficient["morekids","Estimate"]
ad_err <- add_kids$coefficient["morekids","Std. Error"]
ad_err
# Using sprintf() for string interpolation
sprintf("On average, in the sample, the effect of having an additional child when a woman has already had two children on weeks spent working is %s weeks.", round(ad_coef, digits = 1))

# b)
"The regression in a) will not give an estimate of the causal effect of fertility on labour supply because the residual of the regression is not orthogonal to variables used to estimate the labour supply."
"For example, there is likely to be a correlation between number of children and wealth (not included as a variable in the regression) which also affects number of weeks worked."
"Or women who want to focus on their career may have fewer children (unobserved preferences)."
"Perhaps reverse causality."

# c)
# i) t test of mean morekids between boy1st==1 and boy1st==0
m_morekids <- fert$morekids[fert$boy1st==1]
f_morekids <- fert$morekids[fert$boy1st==0]
mean(m_morekids)
mean(f_morekids)

# 1. Hypotheses:
"H0: No diff in mean fertility between those whose first child was male vs female"
"ha: There is a diff in mean fertility between the groups"

# 2. Decision Rule:
"alpha = 0.05"
crit <- qt(0.975, nrow(fert))
sprintf("Reject H0 if |t| > %s", qt(0.975, nrow(fert)))

# 3. Calculate t value:
ttest <- t.test(f_morekids, m_morekids, mu=0, alternative="two.sided")
ttest

# 4. Compare with critical value:
sprintf("%s > %s", ttest$statistic, crit)
"We can therefore reject the hypothesis that there is no difference in fertility between those who have male first child and those who have a female first child."
'Those who have a male first child on average in the same go on to have more than two children less frequently than those who have a female first child.'
"This could be because of the desire to have a son that could mean that those who have a male first child are less likely to have more than two kids that those whose first two children are female."

# c)  i) alternatively different effects can be estimated using regressions and testing whether the coefficient is zero
lm_robust(morekids ~ boy1st, data = fert)
# gives same t value as mean comparison
"Estimate of coefficient on boy1st of -0.0088 is statistically significant but very small ('significant' does not mean large in statistics: significant means large relative to its imprecision)"

# ii) t test of the mean of morekids between samesex==1 and samesex==0
# samesex==1 when first two children are of the same sex
same_morekids <- fert$morekids[fert$samesex==1]
diff_morekids <- fert$morekids[fert$samesex==0]
mean(same_morekids)
mean(diff_morekids)

# 1. Hypotheses:
"H0: No difference in average fertility between women whose first two children are of the same sex and women whose first two children are of different sex"
"Ha: the average fertilities are not equal"

# 2. Decision Rule:
"alpha = 0.05"
"Reject if t value lies outside +/- 1.959973 (critical values with 254614 d.o.f.)"
# calculate critical t values
nrow(fert)

# inputting 0.975 to qt() gives the critical value at the 0.05% s.l. for a two sided test
# qt() is TDist quantile function
qt(.975, nrow(fert))
# WHY IS MY COUNT OF DOF DIFFERENT FROM R'S T TEST (off by 40)

# 3. Calculate:
ttest <- t.test(same_morekids, diff_morekids, mu=0, alternative="two.sided")
ttest

# 4. Compare critical values
" t value: 41.88 > 1.959973"
sprintf("We can therefore reject the null hypothesis that there is no difference between the average fertility of women whose first two children are of the same sex and women whose first two children are of different sex")
"This could also be due to the desire for a male heir that means that those with two female first children (samesex==1) are more likely to have further children."

# c) ii) using the regression method
summary(lm_robust(formula = morekids ~ samesex, data = fert))

# d)
'samesex might not be a valid instrument for morekids if it does not meet the exogeneity condition'
"Three conditions for a valid instrument: exclusion, relevance, exogeneity"
'samesex is exogenously assigned (assigned as if random) (theoretically) but may not only affect labour supply through morekids:'
"exogeneity could be tested with an F test of the other variables determining samesex"
"c): samesex is revelant to the determination of morekids"
'It could be that having children of different sexes means more work for the mother, e.g. ferrying them to different activities, resulting in correlation between samesex and weeksm1'
"This would be an effect of samesex on labour supply that does not go through morekids"

# Regression d
exog_reg <- lm_robust(formula = weeksm1 ~ samesex, data=fert)

# F test of significance
# hypothesis matrix requires single =

# 1. Hypotheses:
# H0: OLS coefficient on samesex = 0
# Ha: OLS coefficient on samesex != 0

# 2. Decision Rule:
# Reject H0 if F statistic > 10

# 3. Calculate F statistic
ftest <- linearHypothesis(exog_reg, c("samesex=0"), test="F")
ftest

# 4. Compare F statistic to critical value:
sprintf("%s > %s", ftest$F[2], 10)
"Therefore we can reject the null hypothesis"

# e)
# 2SLS manually
# Model: y = B0 + B1Xi + Ui
# weeksm1 ~ morekids
# Regress morekids on the instrument to calculate fitted values
fsls <- lm_robust(formula = morekids ~ samesex, data = fert)
summary(fsls)
# how are fitted values different from morekids?
morekids_hat = fitted.values(fsls)
tsls <- lm_robust(formula = weeksm1 ~ morekids_hat, data = fert)
summary(tsls)

# e) using packages
# iv_robust(formula = y_var ~ endog + ctrl1 + ctrl2 | instr1 + instr2 + ctrl1 + ctrl2, data = data, diagnostics = TRUE)
# endogenous variable to be instrumented for on the left of the vertical pipe only
# instruments on the right of the vertical pipe only
# control variables (neither instruments nor endogenous variables to be instrumented for) on both sides of the vertical pipe
ivreg <- iv_robust(formula = weeksm1 ~ morekids | samesex, diagnostics=TRUE)
iv_sum <- summary(ivreg)
iv_sum
sprintf('SE of 2SLS (%s) >> SE of OLS (%s): samesex is not a valid instrument as explained in d)', iv_sum$coefficient["morekids", "Std. Error"], ad_err)
'Estimate of -6.3 from 2SLS vs -5.3 from OLS:'

# f) manually
# othrace does not include white so no perfect multicollinearity with black, hispan, and othrace
# missed out age!!!
sum(fert$black)
sum(fert$othrace)
sum(fert$hispan)
fsls <- lm_robust(formula = morekids ~ samesex + black + hispan + othrace, data = fert)
summary(fsls)
# without controls, fitted.values(fsls) would also give morekids_hat; with controls it is shorter in length than the other variables
morekids_hat <- predict(fsls)
length(morekids_hat)
length(othrace)
tsls <- lm_robust(formula = weeksm1 ~ morekids_hat + black + hispan + othrace, data = fert)
summary(tsls)

# f)
iv_robust(weeksm1 ~ morekids + agem1 + black + hispan + othrace | samesex  + agem1 + black + hispan + othrace, data = fert, diagnostics = TRUE)

# g)
# perfect multicollinearity of samesex and two_male and two_female?
# Two_male: is the first child a girl? If girl, then 0. If boy, then if samesex, then 1. If boy but not samesex, then 0.
two_male <- ifelse((fert$boy1st==0), 0, ifelse((fert$samesex==1), 1, 0))
two_female <- ifelse((fert$boy1st==1), 0, ifelse((fert$samesex==1), 1, 0))
#two_male
#two_female

# adding/excluding samesex does not change anything
ivreg <- iv_robust(formula = weeksm1 ~ morekids | samesex + black + hispan + othrace + two_male + two_female, diagnostics=TRUE)
summary (ivreg)


ivreg <- iv_robust(formula = weeksm1 ~ morekids | two_male + two_female, diagnostics=TRUE)
summary (ivreg)
'The regression with just two_male and two_female as instruments, unsurprisingly, gives very similar estimates to the regression using samesex as an instrument.'

# Peter's dummies use logical operations on dummy values converting them to Booleans
two_boys <- as.numeric( fert$samesex & fert$boy1st )
two_girls <- as.numeric( fert$samesex & !fert$boy1st )

# Peter's iv_robust() with diff variables names
summary(iv_robust(weeksm1 ~ morekids + agem1 + black + hispan + othrace | two_male + two_female + agem1 + black + hispan + othrace, data = fert, diagnostics = TRUE))

# h)
'p value of 0.06 > 0.05 in Overidentifying restrictions means that there is not sufficient evidence at the 5% significance level to reject the null of exogeneity for two_male and two_female as instrumental variables'
'You do not want to reject exogeneity'


