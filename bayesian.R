library(netmeta)
library(readxl)
library(writexl)
library(dmetar)
library(dplyr)
library(summarytools)
library(gemtc)
library(rjags)
library(dmetar)


######## Bayesian NMA and meta-regression on year
### re-loading the response.xlsx as df1
dt1 = read_excel("response.xlsx",  na = "NA")
df1 = as.data.frame(dt1)
df1$ba = recode(df1$ba, '0'=0,'1'=1,'2'=0) ###0=high risk of bias

attach(df1)

###Creating arm-level data & study-level data
### Arm-level dataの定義
arm.data <- data.frame(id,arm2,e,n)
colnames(arm.data) <- c("study","treatment","responders","sampleSize")
# gemtcでは、データフレームの変数名を必ずこれにしないと動かないようです（使い勝手が悪い！）

### Study-level dataの定義（説明変数は、Study-levelで定義しなくてはいけないようです)
library(dplyr)
study.data = distinct(df1, id, .keep_all=T)
study.data = data.frame(study.data$id, study.data$year, study.data$meanage, study.data$propwomen, study.data$ba)
colnames(study.data) = c("study","year", "meanage", "propwomen", "ba")


### Run Bayesian NMA
### Define dataset
network1 <- mtc.network(data.ab=arm.data, studies=study.data)

### Run MCMC
model <- mtc.model(network1, type="consistency")
result <- mtc.run(model)
summary(result)
forest(result)
forest(relative.effect(result, t1="WL"))

###Running meta-regression for year
### define regressors
regressor1 <- list(coefficient='shared',
                  variable='year',
                  control='WL')
### use the same dataset and run MCMC
model1 <- mtc.model(network1,
                   type="regression",
                   regressor=regressor1)

result1 <- mtc.run(model1)
summary(result1)
forest(relative.effect(result1, t1="WL"))



######meta-regression by meanage and ba(blinding of assessor)
### re-loading the response.xlsx as df1, and dropping rows where meanage or propwomen is NA
df1 = subset(df1, !is.na(df1$meanage))
df1 = subset(df1, !is.na(df1$ba))

attach(df1)

###Creating arm-level data & study-level data
### Arm-level dataの定義
arm.data <- data.frame(id,arm2,e,n)
colnames(arm.data) <- c("study","treatment","responders","sampleSize")

### Study-level dataの定義（説明変数は、Study-levelで定義しなくてはいけないようです)
library(dplyr)
study.data = distinct(df1, id, .keep_all=T)
study.data = data.frame(study.data$id, study.data$year, study.data$meanage, study.data$propwomen, study.data$ba)
colnames(study.data) = c("study","year", "meanage", "propwomen", "ba")


### Define dataset
network2 <- mtc.network(data.ab=arm.data, studies=study.data)

### define regressors for meanage
regressor2 <- list(coefficient='shared',
                  variable='meanage',
                  control='WL')
model2 <- mtc.model(network2,
                   type="regression",
                   regressor=regressor2)

result2 <- mtc.run(model2)
summary(result2)
forest(relative.effect(result2, t1="WL"))


### define regressors for ba
regressor3 <- list(coefficient='shared',
                  variable='ba',
                  control='WL')
model3 <- mtc.model(network2,
                   type="regression",
                   regressor=regressor3)

result3 <- mtc.run(model3)
summary(result3)
forest(relative.effect(result3, t1="WL"))






#########################################################################################


###According to the older version of Doing Meta-analysis in R
###Running Bayesian NMA
###Creating a network dataset
network1 <- mtc.network(data.ab=arm.data, studies=study.data)
summary(network1)
plot(network1)

###modeling and running NMA
model = mtc.model(network1, linearModel="random", n.chain=4, type="consistency")
mcmc2 = mtc.run(model, n.adapt=5000, n.iter=100000, thin=10)
forest(mcmc2)
forest(relative.effect(mcmc2, t1="WL"))
summary(mcmc2)
summary(mcmc2)$DIC
gelman.plot(mcmc2)
gelman.diag(mcmc2)$mpsrf






#####################################################
### built-in example
data(certolizumab)
print(certolizumab)

# Define the explanatory variable
regressor <- list(coefficient='shared',
                  variable='diseaseDuration',
                  control='Placebo')

model <- mtc.model(certolizumab,
                   type="regression",
                   regressor=regressor)

result <- mtc.run(model)
summary(result)
####################################################