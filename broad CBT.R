Sys.setenv(LANGUAGE="en_US.UTF-8")
rm(list=ls())
library(netmeta)
library(readxl)
library(writexl)
library(dmetar)
library(dplyr)
library(summarytools)



### re-loading the response.xlsx as df1
dt1 = read_excel("response.xlsx",  na = "NA")
df1 = as.data.frame(dt1)
table(df1$t)
df1$arm2=recode(df1$arm2, "BA"="CBT","PST"="CBT","3W"="CBT")


###CBT vs CBTは除外し
###deleting studies wehre broad CBT is compared against broad CBT
df1=subset(df1,df1$id!=179)
df1=subset(df1,df1$id!=297)
df1=subset(df1,df1$id!=304)
df1=subset(df1,df1$id!=307)
df1=subset(df1,df1$id!=315)
df1=subset(df1,df1$id!=316)
df1=subset(df1,df1$id!=329)
df1=subset(df1,df1$id!=330)
df1=subset(df1,df1$id!=331)
df1=subset(df1,df1$id!=342)
df1=subset(df1,df1$id!=348)
df1=subset(df1,df1$id!=351)

write_xlsx(df1, "response_broadCBT.xlsx", col_names=T)

###CBT vs CBT vs controlは2アームを統合する

#id==48
df1[df1$id==48, "no"]
df1[df1$id==48, "arm2"]
c=df1[df1$id==48, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id==52
df1[df1$id==52, "no"]
df1[df1$id==52, "arm2"]
c=df1[df1$id==52, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id==137
df1[df1$id==137, "no"]
df1[df1$id==137, "arm2"]
c=df1[df1$id==137, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id==141
df1[df1$id==141, "no"]
df1[df1$id==141, "arm2"]
c=df1[df1$id==141, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id=250
df1[df1$id==250, "no"]
df1[df1$id==250, "arm2"]
c=df1[df1$id==250, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id=257
df1[df1$id==257, "no"]
df1[df1$id==257, "arm2"]
c=df1[df1$id==257, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id=263
df1[df1$id==263, "no"]
df1[df1$id==263, "arm2"]
c=df1[df1$id==263, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id=283
df1[df1$id==283, "no"]
df1[df1$id==283, "arm2"]
c=df1[df1$id==283, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id=295
df1[df1$id==295, "no"]
df1[df1$id==295, "arm2"]
c=df1[df1$id==295, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id=327
df1[df1$id==327, "no"]
df1[df1$id==327, "arm2"]
c=df1[df1$id==327, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]
#id=337
df1[df1$id==337, "no"]
df1[df1$id==337, "arm2"]
c=df1[df1$id==337, "no"]
df1[df1$no==c[1], "e"]
df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "e"]=df1[df1$no==c[1], "e"]+df1[df1$no==c[2], "e"]
df1[df1$no==c[1], "n"]=df1[df1$no==c[1], "n"]+df1[df1$no==c[2], "n"]
df1=subset(df1,df1$no!=c[2])
df1[df1$no==c[1], "e"]


###convert long format to wide format
dfpw1 = pairwise(treat = arm2, event=e, n=n,
                 data = df1, studlab=id, sm = "OR")

###perform NMA
nma1 = netmeta(dfpw1, comb.fixed=FALSE)
summary(nma1, digits=2)
round(nma1$tau, 3)
paste(round(nma1$I2*100), "%", sep = "")

###draw network diagram
netgraph(nma1, plastic = F, multiarm = FALSE, thickness = "w.random",
         points = TRUE, cex.points = table(df1$arm2) / 2, col.points="blue", col = 1,
         number.of.studies = T, cex=1.5,
         seq=c("CBT","WL","NoTreatment","PsycholPlacebo","PillPlacebo"))

###see direct and indirect evidence
directevidence1 = direct.evidence.plot(x=nma1)


###wrtie the league table
leaguetable1 = netleague(nma1, digits = 2, seq=c("CBT","PillPlacebo","NoTreatment","PsycholPlacebo","WL"))
write_xlsx(leaguetable1$random, "leaguetable-broadCBT-response-random.xlsx")

t_leaguetable1 = t(leaguetable1$random)
write_xlsx(as.data.frame(t_leaguetable1), "transposed leaguetable-response-random.xlsx")


###draw the forest plot
forest(nma1, sortvar = -TE, ref="PsycholPlacebo",
       xlim=c(0.1, 10),
       col.square="blue",
       leftcols = "studlab", leftlabs = "Intervention",
       xlab = "Response to treatment",
       smlab = "Broadly conceived CBT vs Control conditiions \n (random effects OR)")
forest(nma1, sortvar = -TE, ref="WL",
       xlim=c(0.1, 10),
       col.square="blue",
       leftcols = "studlab", leftlabs = "Intervention",
       xlab = "Response to treatment",
       smlab = "Broadly conceived CBT vs Control conditiions \n (random effects OR)")


###SUCRA rankings
netrank(nma1, small = "bad")


###examine consistency
design1 = as.character(decomp.design(nma1)$Q.het.design$design)
design1

##design by treatment interaction
decomp.design(nma1)

##node splitting
netsplit1 = netsplit(nma1) 
print(netsplit1, show = "all", digits = 2)
forest(netsplit(nma1))

##net heat plot
netheat(nma1, nchar.trts=4, random=T)

## examining small study effects between CBT and WL only
dfpw2 = subset(dfpw1, treat1=="CBT")
dfpw2 = subset(dfpw2, treat2=="WL")
meta2 = metagen(TE, seTE, data=dfpw2, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="OR")
digits=1
funnel(meta2)
metabias(meta2, method.bias="linreg")
## examining small study effects between CBT and NoTreatment only
dfpw3 = subset(dfpw1, treat1=="CBT")
dfpw3 = subset(dfpw3, treat2=="NoTreatment")
meta3 = metagen(TE, seTE, data=dfpw3, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="OR")
funnel(meta3)
metabias(meta3, method.bias="linreg")
## examining small study effects between CBT and PsycholPlacebo only
dfpw4 = subset(dfpw1, treat1=="CBT")
dfpw4 = subset(dfpw4, treat2=="PsycholPlacebo")
meta4 = metagen(TE, seTE, data=dfpw4, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="OR")
funnel(meta4)
metabias(meta4, method.bias="linreg")
## examining small study effects between CBT and PillPlacebo only
dfpw5 = subset(dfpw1, treat1=="CBT")
dfpw5 = subset(dfpw5, treat2=="PillPlacebo")
meta5 = metagen(TE, seTE, data=dfpw5, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="OR")
funnel(meta5)
metabias(meta5, method.bias="linreg")
## examining small study effects between active arms togeter versus control arms together
dfpw6 = rbind.data.frame(dfpw2,dfpw3,dfpw4,dfpw5)
library(dplyr)
dfpw6 = distinct(dfpw6, id, .keep_all=T) 
meta6 = metagen(TE, seTE, data=dfpw6, studlab=studlab,comb.fixed=F,comb.random=T, hakn=F, sm="OR")
funnel(meta6)
metabias(meta6, method.bias="linreg")
