library(magrittr)
library(leaps)
library(ggplot2)
library(RColorBrewer)
library(rms)
library(tree)
library(MLmetrics)
library(broom)

# loading the data
students <- read.table('students.csv', header=TRUE, sep=',')

# dropping middle grades
drops <- c('G1','G2')
students <- students[, !(names(students) %in% drops)]

students$subject <- ifelse(students$subject == 1, 
                           'Portuguese', 'Math')
students$address <- ifelse(students$address == 1, 
                           'Rural', 'Urban')
students$higher <- ifelse(students$higher == 1, 
                           'Yes', 'No')
students$Mjob_teacher <- ifelse(students$Mjob_teacher == 1, 
                          'Yes', 'No')
students$Fjob_teacher <- ifelse(students$Fjob_teacher == 1, 
                                'Yes', 'No')
students$schoolsup <- ifelse(students$schoolsup == 1, 
                                'Yes', 'No')
students$internet <- ifelse(students$internet == 1, 
                             'Yes', 'No')
students$romantic <- ifelse(students$romantic == 1, 
                            'Yes', 'No')

# converting all to factors
students[] <- lapply(students, factor)

# converting only numeric columns
nums <- c('age', 'overall_grade', 'failures', 'famrel',
          'freetime', 'goout', 'Dalc', 'Walc', 'health', 
          'absences', 'G3')
students[,nums] %<>% lapply(function(x) as.numeric(as.character(x)))

dim(students)
colnames(students)
str(students)

# EDA
ggplot(students, aes(G3)) +
  geom_histogram(alpha=.8, fill='slategray1', 
                 color = 'slategray2',bins=15) +
  labs(title = 'Histogram of Overall Grade', y = 'Count', 
       x = 'Overall Grade') +
  theme(plot.title = element_text(hjust = 0.5))

# slight skew but no transformations improved normality

# boxplot for factor variables
var = students$Mjob_teacher
ggplot(students, aes(x = var, y = overall_grade,
                     fill = var)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Boxplot of Weekly Alcohol Consumption",
       x = "Weekly Alcohol Consumption", 
       y = 'Overall Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5)) 

# table for factor variables
table(var)

# subject: 1 (portuguese), 0 (math)
# age: older tend to have lower grades
# address: rural -> lower grades
# Medu/Fedu: more mother/father edu -> higher grades
# studytime: studing levels 3 and 4 -> have higher grades
# schoolsup: lower grades
# failures: more failures -> lower grades (but most haven't failed)
# higher: want to take higher education -> higher grades
# internet: higher grades
# Dalc/Walc: very slightly lower grades for higher alc
# M/Fjob_teacher: higher grades
# binge/heavy_drinker: slighly lower grades

### LINEAR REGRESSION

# initial model based on EDA
lm.grade <- lm(overall_grade ~ studytime + failures +
                 higher + Fjob_teacher +
                 Dalc + Walc + address + famsize +
                 internet + romantic + goout + health + subject +
                 internet:address, 
               data = students)
summary(lm.grade)
plot(lm.grade)
vif(lm.grade)
RMSE(lm.grade$fitted.values, students$overall_grade)

# model used to test adding coefficients
lm.test <- lm(overall_grade ~ studytime + failures +
                higher + Fjob_teacher + 
                Dalc + Walc + address + famsize + 
                internet + romantic + goout + health + subject + 
                internet:address, data = students)
summary(lm.test)

plot(lm.test)
vif(lm.test)
RMSE(lm.test$fitted.values, students$overall_grade)

anova(lm.grade, lm.test)

### DECISION TREE

tree.grade <- tree(overall_grade ~ studytime + failures +
                higher +  Mjob_teacher + Fjob_teacher +
                Dalc + Walc + address + famsize + schoolsup +
                internet + romantic + goout + health, 
                data = students)
plot(tree.grade)
text(tree.grade, pretty = 0)

tree.preds <- predict(tree.grade, type = 'vector')
RMSE(tree.preds, students$overall_grade)

#############

## DELETED FROM REPORT

# used to generate eda plots
pmedu <- ggplot(students, aes(x = Medu, y = G3, fill = Medu)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Mother's Education",
       x = "Mother's Education", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

pfedu <- ggplot(students, aes(x = Fedu, y = G3, fill = Fedu)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Father's Education",
       x = "Father's Education", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

pstudytime <- ggplot(students, aes(x = studytime, y = G3, fill = studytime)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Study Time",
       x = "Study Time", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

pfailures <- ggplot(students, aes(x = failures, y = G3, fill = failures)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Failures",
       x = "Failures", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

phigher <- ggplot(students, aes(x = higher, y = G3, fill = higher)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Higher",
       x = "Higher", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

preason <- ggplot(students, aes(x = reason, y = G3, fill = reason)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Reason",
       x = "Reason for Attending School", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

pintadd <- ggplot(students, aes(x = address, y = G3, fill = var)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Address and Internet Access",
       x = "Address", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ internet)

pDalc <- ggplot(students, aes(x = Dalc, y = G3, fill = Dalc)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Daily Alcohol Consumption",
       x = "Daily Alcohol Consumption", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

pWalc <- ggplot(students, aes(x = Walc, y = G3, fill = Walc)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Weekly Alcohol Consumption",
       x = "Weekly Alcohol Consumption", 
       y = 'Final Grade') +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5))

#pdf('eda.pdf')
#grid.arrange(pmedu, pfedu, pstudytime, pfailures, phigher, preason, pintadd,
#             pDalc, pWalc, ncol=3)
#dev.off()

```{r edapng, echo=FALSE}
knitr::include_graphics("eda.png")
```

# initial linear regression model based on EDA
lm.grade <- lm(overall_grade ~ studytime + failures + higher + Mjob + Fjob +
                 Dalc + Walc + address + internet + goout + health + subject +
                 absences + schoolsup + paid + reason +
                 internet:address, 
               data = students)
summary(lm.grade)
plot(lm.grade)
# vif(lm.grade)
RMSE(lm.grade$fitted.values, students$G3)

"""
Summary

**linear regression** model was fit to predict overall grade. Based on the results of this linear regression model, we found that number of past class failures and planning on pursuing higher education are the strongest predictors of overall grade. Surprisingly, neither daily nor weekly student alcohol consumption impacted overall grade. Overall, this study aims to identify factors that influence student academic performance.

EDA from linear regression model 

Plotting various predictors against the response variable `G3` of final grade, we can see some important relationships, indicating that certain covariates may be strong predictors of a student's final grade. 

1. First, by plotting mother's and father's education against student's final grades, we can see that generally, students whose parents had higher levels of education had higher final grades. 
2. Second, higher levels of study time and lower numbers of past class failures were associated with higher final grades. While most students (837) did not have any past class failures, each additional failure was related to lower final grades. 
3. Third, planning on pursuing higher education and attending the school for reputation reasons also were associated with higher grades. 
4. Fourth, address (urban/rural) was plotted against final grade subsetted by internet access at home. In this plot (bottom left), it seems that those who live in urban areas and have internet access at home have higher final grades than the others. This indicates that an interaction effect between `internet` and `address` may be useful in predicting final grade. 
5. Lastly, plotting daily and weekly alcohol consumption against final grade shows no evident pattern. However, including these variables in the model will clarify if daily and/or weekly alcohol consumption significantly impacts final grade. 

Based on these plots, initial exploratory data analysis identifies at least ten variables that may possibly affect students' final grades.

"""

### PROPORTIONAL ODDS MODEL

chisq.test(table(students$grade, students$reason))

pom.grade <- polr(grade ~ failures + Medu + Dalc + higher + subject + 
                    studytime + address + freetime + internet + Fedu + age + 
                    Walc + Mjob + traveltime + famsize + schoolsup + goout,
                  data = students)

# model to test coeffs: schoolsup absences
test.pom.grade <- polr(grade ~ failures + Medu + Dalc + higher + subject + 
                         studytime + address + freetime + internet + Fedu + age + 
                         Walc + Mjob + traveltime + famsize + schoolsup + goout + address:internet,
                       data = students)

anova(pom.grade, test.pom.grade, test = "Chisq")
colnames(students)

pom.null <- polr(grade ~ 1, data = students)
pom.all <- polr(grade ~ failures + Dalc + Walc + famsize + nursery + subject + studytime +
                  guardian + age + higher + sex + age + address + internet + 
                  traveltime + freetime + Mjob + Fjob + Medu + Fedu, 
                data = students)
model_forward_aic <- step(pom.null, scope = formula(pom.all), direction = "both", trace=0)
summary(model_forward_aic)

# diagnostics
resid1 <- (as.numeric(students$grade) == 1) -  probs_pom[,1]
resid2 <- (as.numeric(students$grade) <= 2) -  rowSums(probs_pom[,1:2])
resid3 <- (as.numeric(students$grade) <= 3) -  rowSums(probs_pom[,1:3])
resid4 <- (as.numeric(students$grade) <= 4) -  rowSums(probs_pom[,1:4])

binnedplot(x=students$absences,y=resid4,col.pts="navy")

# studytime failures higher reason address internet Dalc Walc schoolsup absences Fjob
var = students$Fjob

tapply(resid1, var, mean)
tapply(resid2, var, mean)
tapply(resid3, var, mean)
tapply(resid4, var, mean)
tapply(resid5, var, mean)

pom.grade <- polr(grade ~ failures + Medu + Dalc + higher + subject + 
                    studytime + address + freetime + internet + Fedu + age + 
                    Walc + Mjob + traveltime + famsize + schoolsup + goout +
                    school + paid + health + address:internet,
                  data = students)

# model to test coeffs: schoolsup absences
test.pom.grade <- polr(grade ~ failures + Medu + Dalc + higher + subject + 
                         studytime + address + freetime + internet + Fedu + age + 
                         Walc + Mjob + traveltime + famsize + schoolsup + goout +
                         school + paid + health,
                       data = students)

anova(pom.grade, test.pom.grade, test = "Chisq")

pom.null <- polr(grade ~ 1, data = students)
pom.all <- polr(grade ~ failures + Medu + Dalc + higher + subject + 
                  studytime + address + freetime + internet + Fedu + age + 
                  Walc + Mjob + traveltime + famsize + schoolsup + goout +
                  school + paid + health + reason + nursery + Fjob + Fedu, 
                data = students)
model_forward_aic <- step(pom.null, scope = formula(pom.all), direction = "both", trace=0)
summary(model_forward_aic)

### DECISION TREE

tree.grade <- tree(grade ~ failures + Medu + Dalc + higher + subject + 
                     studytime + address + freetime + internet + Fedu + age + 
                     Walc + Mjob + traveltime + famsize + schoolsup + goout,
                   data = students)

plot(tree.grade)
text(tree.grade, pretty = 0)

probs_tree <- predict(tree.grade, type = 'vector')
preds_tree <- colnames(probs_tree)[max.col(probs_tree, ties.method="first")]
preds_tree <- as.factor(preds_tree)
table(preds_tree)