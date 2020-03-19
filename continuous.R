library(netmeta)
library(readxl)
library(writexl)
library(dmetar)
library(dplyr)
library(summarytools)



### merging Pim's continuous file (and number of sessions) and Yannis's arm file, and dropping irrelevant studies
df1=read_excel("pim_continuous.xlsx",na="NA")
df1=as.data.frame(df1)
df2=read_excel("yannis_arm.xlsx",na="NA")
df2=as.data.frame(df2)
df=inner_join(df2,df1, by=c("id","t","study"))
df3=read_excel("pim_response.xlsx",na="NA")
df3=df3[c("id","t","study","nsess")]
df=inner_join(df,df3, by=c("id","t","study"))

### of the merged df, we will exclude some rows
df=subset(df, yannis==1) ### Yannis droped Pim's t=5=ipt, 6=dyn, 7=sup when intended as active intervention,
### 8=lrt, and called them arm=12
### Yannis also recoded Pim's 9=cau into arm=13=PsycholPlacebo or 14=NoTreatment
df=subset(df, f2f==1)   ### we exclude non face-to-face studies because the nature of control conditions
### are different between f2f and non-f2f studies (e.g. no treatment)
df=subset(df, nsess>=4)   ###include arms only when there are five or more sessions
df$arm2=recode(df$arm, "1"="CBT","2"="BA","3"="PST","4"="3W","10"="WL","11"="PillPlacebo","13"="PsycholPlacebo","14"="NoTreatment")
# then recode the arm2 classifications where there were errors in the original Pim's or Yannis's database
### writing it as continuous.xlsx
write_xlsx(df, "continuous.xlsx", col_names=T)


### re-loading the response.xlsx as df1
dt1 = read_excel("continuous.xlsx",  na = "NA")
df1 = as.data.frame(dt1)
str(df1)
table(df1$arm2)


###convert long format to wide format
dfpw1 = pairwise(treat = arm2, mean=m, n=n, sd=sd,
                 data = df1, studlab=study, sm = "SMD")

###perform NMA
nma1 = netmeta(dfpw1, comb.fixed=F, details.chkmultiarm=T, tol.multiarm=0.1)
summary(nma1, digits=2)
round(nma1$tau, 3)
paste(round(nma1$I2*100), "%", sep = "")

###draw network diagram
netgraph(nma1)
netgraph(nma1, plastic = T, multiarm = FALSE, thickness = "w.random",
         points = TRUE, cex.points = table(df1$arm2) / 2, col.points="blue", col = 1,
         seq=c("CBT","BA","PST","3W","WL","NoTreatment","PsycholPlacebo","PillPlacebo"))

###see direct and indirect evidence
directevidence1 = direct.evidence.plot(x=nma1)


###wrtie the league table
leaguetable1 = netleague(nma1, digits = 2,
                         seq=c("BA","PST","CBT","3W","PillPlacebo","NoTreatment","PsycholPlacebo","WL"))
leaguetable1
write_xlsx(leaguetable1$random, "leaguetable-continuous-random.xlsx")

t_leaguetable1 = t(leaguetable1$random)
t_leaguetable1
write_xlsx(as.data.frame(t_leaguetable1), "transposed leaguetable-continuous-random.xlsx")


###draw the forest plot
forest(nma1)
forest(nma1, ref = "WL", sortvar=TE)
forest(nma1, sortvar = TE, ref="PsycholPlacebo",
       xlim=c(-5,1),
       col.square="blue",
       leftcols = "studlab", leftlabs = "Intervention",
       xlab = "Response to treatment",
       smlab = "Psychotherapies vs 11 \n (random effects OR)")
forest(nma1, sortvar = TE, ref="WL",
       xlim=c(-3,1),
       col.square="blue",
       leftcols = "studlab", leftlabs = "Intervention",
       xlab = "Response to treatment",
       smlab = "Psychotherapies vs 11 \n (random effects OR)")



###SUCRA rankings
netrank(nma1, small = "bad")


###examine consistency
design1 = as.character(decomp.design(nma1)$Q.het.design$design)
design1

##design by treatment interaction
decomp.design(nma1)

##node splitting
netsplit1 = netsplit(nma1) 
netsplit1
print(netsplit1, show = "all", digits = 2)
forest(netsplit(nma1))

##net heat plot
netheat(nma1, nchar.trts=4, random=T)



###examination of publication bias
## examining small study effects between CBT and WL only
dfpw2 = subset(dfpw1, treat1=="CBT")
dfpw2 = subset(dfpw2, treat2=="WL")
meta2 = metagen(TE, seTE, data=dfpw2, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="SMD")
funnel(meta2)
metabias(meta2, method.bias="linreg")
## examining small study effects between CBT and NoTreatment only
dfpw3 = subset(dfpw1, treat1=="CBT")
dfpw3 = subset(dfpw3, treat2=="NoTreatment")
meta3 = metagen(TE, seTE, data=dfpw3, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="SMD")
funnel(meta3)
metabias(meta3, method.bias="linreg")
## examining small study effects between CBT and PsycholPlacebo only
dfpw4 = subset(dfpw1, treat1=="CBT")
dfpw4 = subset(dfpw4, treat2=="PsycholPlacebo")
meta4 = metagen(TE, seTE, data=dfpw4, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="SMD")
funnel(meta4)
metabias(meta4, method.bias="linreg")
## examining small study effects between CBT and PillPlacebo only
dfpw5 = subset(dfpw1, treat1=="CBT")
dfpw5 = subset(dfpw5, treat2=="PillPlacebo")
meta5 = metagen(TE, seTE, data=dfpw5, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="SMD")
funnel(meta5)
metabias(meta5, method.bias="linreg")

