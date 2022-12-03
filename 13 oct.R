#Housekeeping
rm(list=ls())
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)

#Importing Wave 11 Youth Dataset
k_youth <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/k_youth.dta")
View(k_youth)

j_youth <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/j_youth.dta")
View(j_youth)

j_youth$e_smoker <- NA #removing missing data (100)
j_youth$e_smoker[j_youth$j_ypevresmo == 1] <- "Never Used"
j_youth$e_smoker[j_youth$j_ypevresmo > 1]  <- "Used"
table(j_youth$e_smoker)

j_youth$cig_smoker <- NA #removing missing data (100)
j_youth$cig_smoker[j_youth$j_ypevrsmo == 1] <- "Smoker, n = 150 "
j_youth$cig_smoker[j_youth$j_ypevrsmo == 2]  <- "Non-Smoker, n = 2174"
table(j_youth$cig_smoker, j_youth$j_ypevrsmo)
table(j_youth$cig_smoker)

jdf = data.frame(j_youth$e_smoker,
                  j_youth$cig_smoker)

jdf <- jdf[!is.na(jdf$j_youth.e_smoker),]
jdf <- jdf[!is.na(jdf$j_youth.cig_smoker),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(jdf, aes(x = j_youth.cig_smoker, fill = j_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Smoker Status")+ ggtitle("Percentage of E-Cigarette users by Smoker status") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 10 Your Questionnaire
       n=2324")

#Questionnaire Module yprevsmo - Respondent smokes e-cigarettes
table(k_youth$k_ypevresmo)


#ungrouped e-cig frequencies
k_youth$u <- NA #removing missing data (100)
k_youth$u[k_youth$k_ypevresmo == 1] <- "I have never used e-cigarettes"
k_youth$u[k_youth$k_ypevresmo == 2]  <- "I have only tried using e-cigarettes once or twice"
k_youth$u[k_youth$k_ypevresmo == 3] <- "I used e-cigarettes in the past, but I never use them now"
k_youth$u[k_youth$k_ypevresmo == 4] <- "I sometimes use e-cigarettes but less than once a month"
k_youth$u[k_youth$k_ypevresmo == 5] <- "I use e-cigarettes at least once a month, but less than once a week"
k_youth$u[k_youth$k_ypevresmo == 6] <- "I use e-cigarettes at least once a week"
table(k_youth$u)
View(table(k_youth$u))


udf = data.frame(k_youth$u)
udf <- udf[!is.na(udf$k_youth.u),]



##################RENAMING VARIABLES###########################

#Creating variable e_smoker
k_youth$e_smoker <- NA #removing missing data (100)
k_youth$e_smoker[k_youth$k_ypevresmo == 1] <- "Never Used"
k_youth$e_smoker[k_youth$k_ypevresmo > 1]  <- "Used"
table(k_youth$e_smoker, k_youth$k_ypevresmo)
head(k_youth$e_smoker,12)
table(k_youth$e_smoker)

mytable <- table(k_youth$e_smoker)
prop.table(mytable)

#Creating variable Cigarette Smoker
k_youth$cig_smoker <- NA #removing missing data (100)
k_youth$cig_smoker[k_youth$k_ypevrsmo == 1] <- "Smoker, n = 84"
k_youth$cig_smoker[k_youth$k_ypevrsmo == 2]  <- "Non-Smoker, n = 1912"
table(k_youth$cig_smoker, k_youth$k_ypevrsmo)
head(k_youth$cig_smoker,12)
table(k_youth$cig_smoker)

#Renaming k_sex to Male and Female
k_youth$e_smoker_sex<- NA
k_youth$e_smoker_sex[k_youth$k_sex == 1] <- "Male, n = 1041"
k_youth$e_smoker_sex[k_youth$k_sex == 2]  <- "Female, n = 1059"
table(k_youth$e_smoker_sex)

mytable <- table(k_youth$e_smoker_sex)
prop.table(mytable)

#Renaming k_ypevralc to Alcohol and No Alcohol
k_youth$alcoholuser<- NA
k_youth$alcoholuser[k_youth$k_ypevralc == 1] <- "Alcohol, n = 459"
k_youth$alcoholuser[k_youth$k_ypevralc == 2]  <- "No Alcohol, n = 1530"
table(k_youth$alcoholuser)

#Renaming k_country to 4 countries
k_youth$country<- NA
k_youth$country[k_youth$k_country == 1] <- "England, n = 1718"
k_youth$country[k_youth$k_country == 2]  <- "Wales, n = 97"
k_youth$country[k_youth$k_country == 3] <- "Scotland, n = 158"
k_youth$country[k_youth$k_country == 4]  <- "Northern Ireland, n = 126"
table(k_youth$country)
mytable <- table(k_youth$country)
prop.table(mytable)

#Renaming k_ypsave
k_youth$spending<- NA
k_youth$spending[k_youth$k_ypsave == 1] <- "Save to buy, n = 892"
k_youth$spending[k_youth$k_ypsave == 2]  <- "Save, don't spend, n = 719"
k_youth$spending[k_youth$k_ypsave == 3] <- "Spend money as soon as I get it, n = 261"
k_youth$spending[k_youth$k_ypsave == 4] <- "No pocket money, n = 199"
table(k_youth$spending)


#Recoding Ethincity k_ethn_dv to 5 Categories
k_youth$ethnicity<- NA
k_youth$ethnicity[k_youth$k_ethn_dv >= 1] <- "White, n = 1530"
k_youth$ethnicity[k_youth$k_ethn_dv >= 5]  <- "Mixed, n = 183"
k_youth$ethnicity[k_youth$k_ethn_dv >= 9] <- "Asian/Asian British, n = 284"
k_youth$ethnicity[k_youth$k_ethn_dv >= 14]  <- "Black/African/Caribbean, n = 57"
k_youth$ethnicity[k_youth$k_ethn_dv >= 16]   <- "Other, n = 16"
table(k_youth$ethnicity)

#Recoding post16 yplvsc2do

k_youth$post16<- NA
k_youth$post16[k_youth$k_yplvsc2do == 1] <- "Get a full time job, n = 167"
k_youth$post16[k_youth$k_yplvsc2do == 2]  <- "Stay at school or college to do A levels / Highers, n = 1272"
k_youth$post16[k_youth$k_yplvsc2do == 3] <- "Get an apprenticeship, n = 143"
k_youth$post16[k_youth$k_yplvsc2do == 4]  <- "Do some other form of training, n = 64"
k_youth$post16[k_youth$k_yplvsc2do == 5]   <- "Do something else, n = 65"
k_youth$post16[k_youth$k_yplvsc2do == 6] <- "Get a full time job, n = 167"
table(k_youth$post16)

#remove 16+ year olds from sample

k_youth$age<- NA
k_youth$age[k_youth$k_dvage == 10] <- "10, n = 336"
k_youth$age[k_youth$k_dvage == 11] <- "11, n = 344"
k_youth$age[k_youth$k_dvage == 12] <- "12, n = 351"
k_youth$age[k_youth$k_dvage == 13] <- "13, n = 384"
k_youth$age[k_youth$k_dvage == 14] <- "14, n = 346"
k_youth$age[k_youth$k_dvage == 15] <- "15, n = 337"
table(k_youth$age)
mytable <- table(k_youth$age)
prop.table(mytable)


################ Coded Variables ###########

#table(k_youth$e_smoker,k_youth$k_
table(k_youth$e_smoker, k_youth$e_smoker_sex) #sex
table(k_youth$e_smoker,k_youth$age) #age
table(k_youth$e_smoker,k_youth$country) #country
table(k_youth$e_smoker,k_youth$ethnicity) #ethnicity
table(k_youth$e_smoker,k_youth$post16)#education
table(k_youth$e_smoker,k_youth$spending)#finance
table(k_youth$e_smoker,k_youth$cig_smoker)#smoker status
table(k_youth$e_smoker,k_youth$alcoholuser) #alcohol use
    
### Chi squared test for Association ###
sex <- table(k_youth$e_smoker, k_youth$e_smoker_sex)
chisq.test(sex,correct=FALSE) #p-value = 0.5958, ns

age <- table(k_youth$e_smoker,k_youth$age) 
chisq.test(age,correct=FALSE) #p-value < 2.2e-16, ****

country <- table(k_youth$e_smoker,k_youth$country)
chisq.test(country,correct=FALSE) #p-value = 0.5553, ns

ethnicity <- table(k_youth$e_smoker,k_youth$ethnicity)
chisq.test(ethnicity,correct=FALSE) #p-value = 0.02681, *

education <- table(k_youth$e_smoker,k_youth$post16)
chisq.test(education,correct=FALSE) #p-value = 0.0001318, ***

finance <- table(k_youth$e_smoker,k_youth$spending)
chisq.test(finance,correct=FALSE) #p-value = 1.826e-05, ****

cigsmoker <- table(k_youth$e_smoker,k_youth$cig_smoker)
chisq.test(cigsmoker,correct=FALSE) #p-value < 2.2e-16, ****

alcohol <- table(k_youth$e_smoker,k_youth$alcoholuser)
chisq.test(alcohol) #p-value < 2.2e-16, ****

#### Creating a dataframe of my coded variables ####
mydf = data.frame(k_youth$e_smoker,
                   k_youth$e_smoker_sex,
                   k_youth$age,
                   k_youth$country,
                   k_youth$ethnicity,
                   k_youth$post16,
                   k_youth$spending,
                   k_youth$cig_smoker,
                   k_youth$alcoholuser)


summary(mydf)
str(mydf)
mydf$k_youth.e_smoker
head(mydf)
row.names(mydf)

#Create a crosstab of frequencies and store it as an object
mytable <- table(k_youth$k_ypevresmo)
prop.table(mytable)

################ Odds Ratios ###########

#OR for Sex and E-Smoker Status
sex <- c('Male', 'Female')
esmoker_status <- c('Used', 'Never Used')
data <- matrix(c(80, 918, 73, 929), nrow=2, ncol=2, byrow=TRUE)
dimnames(data) <- list('Sex'=sex, 'Esmoker_status'=esmoker_status)
library(epitools)
#calculate odds ratio
oddsratio(data)

#OR for Cigarette and E-Smoker Status
cigsmoker <- c('Smoker', 'Non-Smoker')
esmoker_status <- c('Used', 'Never Used')
data2 <- matrix(c(55, 29, 98, 1814), nrow=2, ncol=2, byrow=TRUE)
dimnames(data) <- list('Cigarette Smoker'=cigsmoker, 'Esmoker_status'=esmoker_status)
library(epitools)
#calculate odds ratio
oddsratio(data2)

#OR for Alcohol and E-Smoker Status
alcohol <- c('Alcohol', 'No Alcohol')
esmoker_status <- c('Used', 'Never Used')
data3 <- matrix(c(123, 336, 29, 1501), nrow=2, ncol=2, byrow=TRUE)
dimnames(data3) <- list('Alcohol'=alcohol, 'Esmoker_status'=esmoker_status)
library(epitools)
#calculate odds ratio
oddsratio(data3)

############# Plots/Graphics ########

#Frequency of E-Cig use
mydf2 <- mydf[!is.na(mydf$k_youth.e_smoker),]
mydf2 <- mydf[!is.na(mydf$k_youth.cig_smoker),]
mydf2 <- mydf[!is.na(mydf$k_youth.post16),]

ggplot(mydf2, aes(x = k_youth.e_smoker)) + 
  geom_bar(fill = "grey", 
           color="black") +
  labs(x = "E-Cig status", 
       y = "Frequency", 
       title = "Frequency of E-Cig users")

library(dplyr)
library(scales)
plotdata <- mydf2 %>%
  count(k_youth.e_smoker) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

ggplot(plotdata, 
       aes(x = reorder(k_youth.e_smoker, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "grey", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "E-Cigarette status", 
       y = "Percentage",
       caption = "Source: Understanding Society Wave 11 Youth Questionnaire
       n=2000", 
       title  = "Proportion of E-Cigarette use among 10-15 year olds in the UK")

#Proportion of E-Cig use by Country

ukdf = data.frame(k_youth$e_smoker,k_youth$country)
ukdf <- ukdf[!is.na(ukdf$k_youth.e_smoker),]
ukdf <- ukdf[!is.na(ukdf$k_youth.country),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(ukdf, aes(x = k_youth.country, fill = k_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Country")+ ggtitle("Percentage of E-Cigarette users by Country") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 11 Your Questionnaire
       n=1999")

#Proportion of Cigarette users by E-Cig status

cigdf = data.frame(k_youth$e_smoker,k_youth$cig_smoker)
cigdf <- cigdf[!is.na(cigdf$k_youth.e_smoker),]
cigdf <- cigdf[!is.na(cigdf$k_youth.cig_smoker),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(cigdf, aes(x = k_youth.cig_smoker, fill = k_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Smoker Status")+ ggtitle("Percentage of E-Cigarette users by Smoker status") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 11 Your Questionnaire
       n=1996")

#Proportion of E-Cig use by Age

agedf = data.frame(k_youth$e_smoker,k_youth$age)
agedf <- agedf[!is.na(agedf$k_youth.e_smoker),]
agedf <- agedf[!is.na(agedf$k_youth.age),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(agedf, aes(x = k_youth.age, fill = k_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Age")+ ggtitle("Percentage of E-Cigarette users by Age") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 11 Your Questionnaire
       n=1998")

#Proportion of E-Cig use by Ethnicity

edf = data.frame(k_youth$e_smoker,k_youth$ethnicity)
edf <- edf[!is.na(edf$k_youth.e_smoker),]
edf <- edf[!is.na(edf$k_youth.ethnicity),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(edf, aes(x = k_youth.ethnicity, fill = k_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Ethnicity")+ ggtitle("Percentage of E-Cigarette users by Ethnicity") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 11 Your Questionnaire
       n=1971")

#Proportion of E-Cig use by Post 16 activity

adf = data.frame(k_youth$e_smoker,k_youth$post16)
adf <- adf[!is.na(adf$k_youth.e_smoker),]
adf <- adf[!is.na(adf$k_youth.post16),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(adf, aes(x = k_youth.post16, fill = k_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Post-16 Activity")+ ggtitle("Percentage of E-Cigarette users by Post-16 Activity") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 11 Your Questionnaire
       n=1625")

#Proportion of E-Cig use by Spending

sdf = data.frame(k_youth$e_smoker,k_youth$spending)
sdf <- sdf[!is.na(sdf$k_youth.e_smoker),]
sdf <- sdf[!is.na(sdf$k_youth.spending),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(sdf, aes(x = k_youth.spending, fill = k_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Spending")+ ggtitle("Percentage of E-Cigarette users by Spending") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 11 Your Questionnaire
       n=1976")

#Proportion of E-Cig use by Alcohol Status

alcdf = data.frame(k_youth$e_smoker,k_youth$alcoholuser)
alcdf <- alcdf[!is.na(alcdf$k_youth.e_smoker),]
alcdf <- alcdf[!is.na(alcdf$k_youth.alcoholuser),]

brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(alcdf, aes(x = k_youth.alcoholuser, fill = k_youth.e_smoker)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Alcohol status")+ ggtitle("Percentage of E-Cigarette users by Alcohol use") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "E-Cigarette user")) +
  coord_flip() +
  labs(caption="Source : Understanding Society Wave 11 Your Questionnaire
       n=1989")

#Distribution of E-Cig use by Age
ggplot(data = mydf2, aes(x = k_youth.k_dvage, y = k_youth.e_smoker)) +
  geom_boxplot()

# install.packages("gtsummary")
library(gtsummary)
tbl_summary(mydf)