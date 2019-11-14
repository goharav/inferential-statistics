credit <- read.csv('Credit.csv', stringsAsFactors = F)
str(credit)
library(ggplot2)
#install.packages('gridExtra')
library(gridExtra)
#install.packages('FSA')
library(FSA)
#install.packages('multcomp')
library(multcomp)
library(car)
#install.packages('fistdistrplus')
library(fitdistrplus)
# testing wheter age of borrowers is 49 old
summary(credit$age)
ggplot(credit, aes(x=age)) + geom_histogram(bins=20)
# using t-test
t.test(x=credit$age, mu=49)
countries <- read.csv('Countries.csv', stringsAsFactors = F)
str(countries)
## two sample test
Summarize(data=countries, Unemployment ~ Region, digits = 2)
ggplot(countries, aes(x=Region, y=Unemployment)) + geom_boxplot()
t.test(data=countries, Unemployment ~ Region)

## proportion test
table(credit$gender, credit$homeown)

prop.test(x = c(360, 171), n = c(577, 331), correct=FALSE)
Summarize(data = countries, Business.Freedom ~ Region, digits = 2)
var.test(data = countries, Business.Freedom ~ Region)

## anova test
Summarize(data = credit, income ~ agecat, digits = 0)
ggplot(credit, aes(x=agecat, y=income)) + geom_boxplot()
# before testing whether the assumption of Homogeneity of Variance is satisfied
library(car)
leveneTest(income ~ agecat, data = credit)
anova <- aov(income ~ agecat, data = credit)
summary(anova)
oneway.test(income ~ agecat, data = credit, var.equal = FALSE)

library(multcomp)
TukeyHSD(anova)

## checking for normality
credit$ln_income = log(credit$income)
grid.arrange(ggplot(credit, aes(x=income)) + geom_histogram(bins=20),
             ggplot(credit, aes(x=ln_income)) + geom_histogram(bins=20),
             ncol=2)

shapiro.test(credit$income)
shapiro.test(credit$ln_income)

# fitting distributions in R
library(fitdistrplus)
dist1 <- fitdist(countries$Business.Freedom, 'norm')
dist1
denscomp(list(dist1, dist2, dist3, dist4, dist5),
         legendtext = distributions, xlim = c(30, 110))

## one sample Wilcoxon test
America <- countries[countries$Region == "America", ]
summary(America$Public.Debt.Perc.of.GDP)
ggplot(America, aes(x=Public.Debt.Perc.of.GDP)) +
  geom_histogram(bins=9) + 
  geom_vline(xintercept = 45, col='red')

wilcox.test(America$Public.Debt.Perc.of.GDP, mu = 45,
            alternative = "two.sided")

## signed rank test implementation
df <- America[, c("Country.Name", "Public.Debt.Perc.of.GDP")]
df$Diff <- df$Public.Debt.Perc.of.GDP - 45
df$Rank <- rank(abs(df$Diff))
df$SR <- sign(df$Diff)*df$Rank
df
df$SR_plus <- ifelse(df$SR > 0, df$SR, NA)
df$SR_minus <- ifelse(df$SR <= 0, -1*df$SR, NA)
head(df)
Wilc_test <- min(colSums(df[, c("SR_plus","SR_minus")], na.rm=T))
Wilc_test

## two sample Wilcox- Mann-Whitney test
Summarize(data = countries, Public.Debt.Perc.of.GDP ~ Region, digits = 1)
ggplot(countries, aes(x=Public.Debt.Perc.of.GDP)) + geom_histogram(bins=9) +
  facet_grid(Region~.)
wilcox.test(Public.Debt.Perc.of.GDP ~ Region, data = countries,
            alternative = "two.sided")

## wilcox test for two related populatiobns
summary(countries$Labor.Freedom_2015)
summary(countries$Labor.Freedom_2016)
grid.arrange(
  ggplot(countries, aes(x = factor(1:nrow(countries)), y = Labor.Freedom_2015)) +
    geom_point(size = 2) +
    geom_point(aes(y = Labor.Freedom_2016, col="red"), size = 2) +
    xlab("") + ylab("")+
    theme(legend.position = "none"),
  ggplot(countries, aes(x = Labor.Freedom_2016 - Labor.Freedom_2015)) +
    geom_histogram(bins = 9),
  ncol = 1)

wilcox.test(countries$Labor.Freedom_2016, countries$Labor.Freedom_2015,
            paired = TRUE)

# pvalue < 0.05 , we reject null hypothesis


## kruskal Wallis test- one way anova by ranks
kruskal.test(data = credit, income ~ edcat)
#???

## chi square test: Comparing 2 populations proportions
cross <- table(credit$gender, credit$default)
# getting the proportions
prop.table(cross, 2)
cross_df <- data.frame(prop.table(cross, 1))
cross_df
colnames(cross_df) <- c('gender', 'default', 'Proportion')
ggplot(cross_df, aes(x=gender, y=Proportion, fill=default))+
  geom_bar(stat='identity')
cross
chisq.test(cross)
str(credit)
credit$jobsat <- factor(credit$jobsat,
                        
                        levels = c ("Highly dissatisfied",
                                    "Somewhat dissatisfied",
                                    "Neutral",
                                    "Somewhat satisfied",
                                    "Highly satisfied"))
cross <- table(credit$jobsat, credit$marital)
cross
# getting proportions
round(prop.table(cross, 2), 3)
cross_df <- data.frame(prop.table(cross, 2))
colnames(cross_df) <- c('job_sat', 'marital', 'Proportion')
ggplot(cross_df, aes(x=marital, y=Proportion, fill=job_sat)) + geom_bar(stat='identity')
chisq.test(cross)
