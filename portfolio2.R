library(httr)
library(tidyr)
library(rscorecard)
library(pacman)
library("readxl")
pacman::p_load(pacman, dplyr, ggpubr, rscorecard, tidyr, ggplot2)
#Registered Key with Data.gov to access Data
sc_key('LRhOL6s72Ky6DuKgbYzr14VdFNyHCosBthbdEajL')
#Filter Selection for Graduation Debt and 10yr Earnings for Private colleges that predominately award Bachelors Degrees
df <- sc_init() %>% 
  sc_filter(control == 2, PREDDEG == 3) %>% 
  sc_select(grad_debt_mdn, md_earn_wne_p10) %>% 
  sc_year('latest') %>% 
  sc_get()
#Filter Selection for Graduation Debt and 10yr Earnings for Public colleges that predominately award Bachelors Degrees
df2 <- sc_init() %>% 
  sc_filter(control == 1, PREDDEG == 3) %>% 
  sc_select(grad_debt_mdn, md_earn_wne_p10) %>% 
  sc_year('latest') %>% 
  sc_get()

#Summary Statistics for Private College Earnings
group_by(df,year) %>%
  summarise(
    count=n(),
    Prvt10yrEarnMean = mean(md_earn_wne_p10, na.rm = TRUE),
    Prvt10yrEarnSd = sd(md_earn_wne_p10, na.rm = TRUE)
  )
#Summary Statistics for Private College Debt
group_by(df,year) %>%
  summarise(
    count=n(),
    PrvtGradDebtMean = mean(grad_debt_mdn, na.rm = TRUE),
    PrvtGradDebtSd = sd(grad_debt_mdn, na.rm = TRUE)
  )
#Summary Statistics for Public College Earnings
group_by(df2,year) %>%
  summarise(
    count=n(),
    Pub10yrEarnMean = mean(md_earn_wne_p10, na.rm = TRUE),
    Pub10yrEarnSd = sd(md_earn_wne_p10, na.rm = TRUE)
  )
#Summary Statistics for Public College Debt
group_by(df2,year) %>%
  summarise(
    count=n(),
    PubGradDebtMean = mean(grad_debt_mdn, na.rm = TRUE),
    PubGradDebtSd = sd(grad_debt_mdn, na.rm = TRUE)
  )
#Read excel file for HS only graduates earnings
NoCollegeSal <- read_excel(file.choose())

#Summary Statistics for HS only Earnings
group_by(NoCollegeSal) %>%
  summarise(
    count=n(),
    HSSalmean = mean(SalHSnoColl,na.rm = TRUE),
    HSSalSd = sd(SalHSnoColl, na.rm = TRUE)
  )
df %>%
  rename(
    Salary = md_earn_wne_p10
  )
df2 %>%
  rename(
    Salary = md_earn_wne_p10
  )
NoCollegeSal %>%
  rename(
    Salary = SalHSnoColl
  )

#Setting lists that contain only the specified columns from the above dataframes
PriCollegeGrad <- df[,2]
PubCollegeGrad <- df2[,2]
HSOnlyGrad <- NoCollegeSal[,3]

#Two Sample t-test for both Public and Private college graduate earnings vs HS only earnings
res1 <-t.test(PriCollegeGrad, HSOnlyGrad, var.equal = TRUE)
res1
res2 <-t.test(PubCollegeGrad, HSOnlyGrad, var.equal = TRUE)
res2

HSOnlyGrad <- lapply(HSOnlyGrad, 'length<-',max(lengths(PriCollegeGrad)))
PubCollegeGrad <- lapply(PubCollegeGrad, 'length<-',max(lengths(PriCollegeGrad)))

