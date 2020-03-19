Sys.setenv(LANGUAGE="en_US.UTF-8")
Sys.setenv(LANG = "en")

install.packages("readxl")
install.packages("writexl")
install.packages("netmeta")
install.packages("gemtc")
install.packages("rjags")


if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")


### clearing the memory
rm(list=ls())   ###clears memory
cat("\014")   ###clears console

### GitHub
echo "# psychotherapytrialcontrols" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/tshkfrkw/psychotherapytrialcontrols.git
git push -u origin master



library(netmeta)
library(readxl)
library(writexl)
library(dmetar)
library(dplyr)
library(summarytools)


### merging Pim's all data file and Yannis's arm file, and dropping irrelevant studies
df1=read_excel("pim_full.xlsx",na="NA")
df1=as.data.frame(df1)
df2=read_excel("yannis_arm.xlsx",na="NA")
df2=as.data.frame(df2)
df=inner_join(df2,df1, by=c("id","t","study"))


### of the merged df, we will exclude some rows
df=subset(df, yannis==1) ### Yannis droped Pim's t=5=ipt, 6=dyn, 7=sup when intended as active intervention,
### 8=lrt, and called them arm=12
### Yannis also recoded Pim's 9=cau into arm=13=PsycholPlacebo or 14=NoTreatment, whwere appropriate
df=subset(df, arm!=12)   ### Just to be extra sure, drop studies where arm==12
df=subset(df, f2f==1)   ### we exclude non face-to-face studies because the nature of control conditions
### are different between f2f and non-f2f studies (e.g. no treatment)
df=subset(df, nsess>=4)   ###include arms only when there are four or more sessions
df$arm2=recode(df$arm, "1"="CBT","2"="BA","3"="PST","4"="3W","10"="WL","11"="PillPlacebo","13"="PsycholPlacebo","14"="NoTreatment")
### writing it as response.xlsx
write_xlsx(df, "all.xlsx", col_names=T)

### re-loading the all.xlsx as df
dt = read_excel("all.xlsx",  na = "NA")
df = as.data.frame(dt)

### describing the dataset
length(unique(df$study))   ###number of included studies
sum(df$N_total_paper)   ###total number of participants
df$meanage = as.numeric(as.character(df$meanage))
df$propwomen = as.numeric(as.character(df$propwomen))
dfSummary(df)   ###summary by arm
mean(df$propwomen, na.rm=T)
min(df$propwomen, na.rm=T)
max(df$propwomen, na.rm=T)
library(dplyr)
df11 = distinct(df, id, .keep_all=T)   ###keep only one record for the same "id"
dfSummary(df11)   ###summary by study
