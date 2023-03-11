library(tableone)
library(naniar)
library(ggplot2)
library(tidyr)
library(caret)
library(randomForest)
library(mice)
library(vip)
library(corrplot)
library(pdp)
library(dplyr)
library(epiR)
library(pROC)

#Include all predictors

#Select only those who completed baseline survey
dat<- dat[dat$baseline_complete==2,]

dat$MDMA_K_psyc <- case_when (
  (dat$special_K == 1 | dat$MDMA == 1 | dat$psychedelics == 1) ~ 1,
  (dat$special_K == 0 & dat$MDMA == 0 & dat$psychedelics == 0) ~ 0
)

dat$MDMA_K_psyc_1 <- case_when (
  (dat$special_K_1 == 1 | dat$MDMA_1 == 1 | dat$psychedelics_1 == 1) ~ 1,
  (dat$special_K_1 == 0 & dat$MDMA_1 == 0 & dat$psychedelics_1 == 0) ~ 0
)

dat_vars <- dat %>% select(.,c(suicidal,age,province,curr_orient2,gender,trans,intersex,
poc,residence,education,house_income,ind_income,rural_city,where_live,
gen_health,fitness1,mental_health,stresslife,diagnosis,MDMA_K_psyc,
con_eating,con_anxiety,con_ADD,con_ADHD,con_depression,treat_comorbid,disability,
con_OCD,con_panic,con_PTSD,con_others,con_bipolar,receive_help,
cigs_smoked,curr_smoke,use_cigar,use_wp,use_smokeless,covid,risk1,risk2,risk3,risk4,
plan_quit,quit_attempts,tailored,quit_support,time_vape,curr_vape,alcohol,disability___1,disability___2,
disability___3,disability___4,disability___5,disability___6,disability___7,disability___9,disability,
alcohol_amount,cannabis,poppers,crystal_meth,crack,cocaine,heroin,pres_opioids,psychedelics,
treatment___1,treatment___2,treatment___3,treatment___4,treatment___5,
fentanyl,GHB,tranquilizers,drug_others,substances_covid,seek_help,central,not_sig,imprtant,
understand,mom,dad,sibs,partner,ext_fam,new_frnd,old_frnd,co_work,employr,relig_mem,stranger,
famdoc,oth_hlth,classmt,teach,part_q,pos_q,bond_q,proud_q,polit_q,solv_q,prob_q,norm_q,
pass_q,relat_q,hit_q,police_q,live_q,job_q,names_q,asslt_q,frnd_q,
hurt_q,fam_q,relig_q,comfrt,control,public,change,seen,sustain,anonym,promisc,
nervous,public_plc,bars,advnce,rates,accept,stress,stigma,pressure,mhealth,
culture,mentalill,drinker,streetdrug,jail,divorce,slap,feel1,feel2,feel3,feel4,feel5,feel6,feel7,
beat,swear,inapptouch,inapptouchyou,forced,employ,ethnicity))

table(dat$special_K)
table(dat$MDMA)
table(dat$psychedelics)
table(dat$MDMA_K_psyc)

summary(dat_vars)

dat_vars[sapply(dat_vars, is.nan)] <- NA

lapply(dat_vars,function(i) {
  table(i, useNA="ifany")})

sapply(dat_vars, function(x) sum(is.na(x)))

vis_miss(dat_vars,sort_miss=TRUE) 

q.hd <- dat_vars %>% summarise_all(~sum(is.na(.)))
q2.hd <- t(q.hd)
q3.hd <- data.frame('Number_of_missingness'= q2.hd[,1],
                    'percent_missingness'=round(q2.hd[,1]/nrow(dat_vars)*100, digit=2))
# sort
q3.hd <- q3.hd[order(q3.hd$percent_missingness,q3.hd$Number_of_missingness),]

m.hd <- q3.hd[q3.hd$percent_missingness>=5,]
dim(m.hd)

#change factors
names<-c("province","curr_orient2","gender","trans","intersex","ethnicity","treat_comorbid","disability",
         "disability___1","disability___2","disability___3","disability___4","disability___5","disability___6","disability___7",
         "poc","residence","education","house_income","ind_income","rural_city",
         "where_live","fitness1","diagnosis","alcohol","alcohol_amount","cannabis","poppers","crystal_meth","crack","cocaine","heroin","pres_opioids",
         "fentanyl","GHB","tranquilizers","drug_others","MDMA_K_psyc","psychedelics",
         "con_eating","con_anxiety","con_ADD","con_ADHD","con_depression","con_bipolar",
         "treatment___1","treatment___2","treatment___3","treatment___4","treatment___5",
         "con_OCD","con_panic","con_PTSD","con_others","cigs_smoked","curr_smoke","use_cigar","use_wp","use_smokeless","covid",
         "plan_quit","quit_attempts","tailored","quit_support","time_vape",
         "curr_vape","substances_covid","seek_help","employ")
dat_vars[,names]<-lapply(dat_vars[,names],factor)

names2<-c("mentalill","drinker","streetdrug","jail","divorce","slap","beat","swear",
          "inapptouch","inapptouchyou","forced")

dat_vars[,names2]<-sapply(dat_vars[,names2],as.numeric)

##Imputation for missing data. 
dat1.hd <- dat_vars
#retaining unimputed dataset as dat.hd

#using mice for data imputation
library(mice)
init <- mice(dat1.hd, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

predM[,"MDMA_K_psyc"] <- 0

set.seed(123)
imputed.hd <- mice(dat1.hd, method='pmm', predictorMatrix=predM, m=5)
summary(imputed.hd)

df1 <- complete(imputed.hd,1)
df2 <- complete(imputed.hd,2)
df3 <- complete(imputed.hd,3)
df4 <- complete(imputed.hd,4)
df5 <- complete(imputed.hd,5)

scalevars <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
  df <- df %>% mutate(cen_identity = select(.,central:understand) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(outness = select(.,mom:teach) %>% rowMeans(na.rm=TRUE)) 
  df <- df %>% mutate(connect_com = select(.,part_q: prob_q) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(ace = select(.,mentalill: forced) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(phobia = select(.,comfrt: advnce) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(per_stigma = select(.,c(norm_q:relat_q,hurt_q,fam_q)) %>% rowSums(na.rm=TRUE))
  df <- df %>% mutate(en_stigma = select(.,c(hit_q: frnd_q,relig_q)) %>% rowSums(na.rm=TRUE)) 
  df <- df %>% mutate(cesd_score = select(.,c(feel1: feel7)) %>% rowSums(na.rm=TRUE)) 
  # df <- df %>% mutate(per_sm_risk = select(.,c(risk1: risk4)) %>% rowSums(na.rm=TRUE)) 
  # df <- df %>% mutate(per_sm_lgbtq = select(.,c(accept: culture)) %>% rowSums(na.rm=TRUE)) 
  df[,c("cen_identity", "outness", "connect_com", "ace", "phobia", "per_stigma","en_stigma","cesd_score")] <- 
    df %>% select(cen_identity, outness, connect_com, ace, phobia, per_stigma, en_stigma, cesd_score) %>% scale()
  df <- df %>% select(-c(central:understand,mom:teach,part_q:prob_q,mentalill:forced,
                         comfrt: advnce,norm_q:relat_q,hurt_q,fam_q,hit_q: frnd_q,relig_q, feel1:feel7))
  return(df)
}

df1<-scalevars(df1)
df2<-scalevars(df2)
df3<-scalevars(df3)
df4<-scalevars(df4)
df5<-scalevars(df5)

lapply(df1,function(i) {
  table(i, useNA="ifany")})
#now got zero missing after MICE!

evalrf <- function(df) {
  temp <- df %>% select(-psychedelics)
  splitIndex <- createDataPartition(temp$MDMA_K_psyc,p=0.7,times=1,list=FALSE)
  train.df <- temp[splitIndex,]
  test.df <- temp[-splitIndex,]
  
  trControl <- trainControl(method = "repeatedcv", number = 10, repeats= 3, search = "grid")
  mtrygrid <- expand.grid(.mtry=c(1:25))
  
  rf.df <- train(MDMA_K_psyc ~ ., data = train.df,method = "rf",
                 trControl=trControl,metric="Accuracy")
  
  prediction.df <- predict(rf.df, test.df,type="raw")
  tb.dc <- table(as.numeric(as.character(prediction.df)), as.numeric(as.character(test.df$MDMA_K_psyc)))
  res1<-confusionMatrix(tb.dc)
  res2<-epi.tests(tb.dc)
  #AUC
  pROC_obj <- roc(response=test.df$MDMA_K_psyc,predict=as.numeric(as.character(prediction.df)),
                  smoothed = TRUE,ci=TRUE, ci.alpha=0.95, stratified=FALSE)  
  #Brier scores
  res3 <- colMeans((as.numeric(as.character(test.df$MDMA_K_psyc)) - predict(rf.df, test.df,type="prob")["1"])^2)
  return(list(rf.df,res1,res2,res3,pROC_obj,prediction.df))
}

evalrfp <- function(df) {
  temp <- df %>% select(-MDMA_K_psyc)
  splitIndex <- createDataPartition(temp$psychedelics,p=0.7,times=1,list=FALSE)
  train.df <- temp[splitIndex,]
  test.df <- temp[-splitIndex,]
  
  trControl <- trainControl(method = "repeatedcv", number = 10, repeats= 3, search = "grid")
  mtrygrid <- expand.grid(.mtry=c(1:25))
  
  rf.df <- train(psychedelics ~ ., data = train.df,method = "rf",
                 trControl=trControl,metric="Accuracy")
  
  prediction.df <- predict(rf.df, test.df,type="raw")
  tb.dc <- table(as.numeric(as.character(prediction.df)), as.numeric(as.character(test.df$psychedelics)))
  res1<-confusionMatrix(tb.dc)
  res2<-epi.tests(tb.dc)
  #AUC
  pROC_obj <- roc(response=test.df$psychedelics,predict=as.numeric(as.character(prediction.df)),
                  smoothed = TRUE,ci=TRUE, ci.alpha=0.95, stratified=FALSE)  
  #Brier scores
  res3 <- colMeans((as.numeric(as.character(test.df$psychedelics)) - predict(rf.df, test.df,type="prob")["1"])^2)
  return(list(rf.df,res1,res2,res3,pROC_obj,prediction.df))
}

importance <- function(rf) {
  #Show the importance of the variables from each rf
  vip1 <- vip(rf[[1]],scale=TRUE)
  imp <- data.frame(vip1$data)
  return(list(vip1,imp))
}

set.seed(1256)
rf1<-evalrf(df1)
rf1[[4]]
rf1[[5]]

rf1p<-evalrfp(df1)
rf1p[[4]]
rf1p[[5]]

set.seed(689)
rf2<-evalrf(df2)
rf2[[4]]
rf2[[5]]

rf2p<-evalrfp(df2)
rf2p[[4]]
rf2p[[5]]

set.seed(968)
rf3<-evalrf(df=df3)
rf3[[4]]
rf3[[5]]

rf3p<-evalrfp(df3)
rf3p[[4]]
rf3p[[5]]

set.seed(5830)
rf4<-evalrf(df4)
rf4[[4]]
rf4[[5]]

rf4p<-evalrfp(df4)
rf4p[[4]]
rf4p[[5]]

set.seed(350)
rf5<-evalrf(df5)
rf5[[4]]
rf5[[5]]

rf5p<-evalrfp(df5)
rf5p[[4]]
rf5p[[5]]

#Return the rf as an object and performance of the prediction (RMSE on test set)

importance(rf1)
importance(rf2)
importance(rf3)
importance(rf4)
importance(rf5)

importance(rf1p)
importance(rf2p)
importance(rf3p)
importance(rf4p)
importance(rf5p)

dfk<-df2
rf.shk<-rf2[[1]]

dfp<-df2
rf.shp<-rf2p[[1]]

##############correlation analysis
correlationplot <- function(df) {
cor<- (DescTools::PairApply(df, DescTools::CramerV))
corrplot(cor, method="circle")

#prepare to drop duplicates and correlations of 1     
cor[lower.tri(cor,diag=TRUE)] <- NA 
cor[cor == 1] <- NA 
cor <- as.data.frame(as.table(cor))
cor <- na.omit(cor) 

#subsetting corr more than 0.60
cor <- subset(cor, abs(Freq) >= 0.6)
mtx_cor <- reshape2::acast(cor, Var1~Var2, value.var="Freq")

corrplot(mtx_cor, is.corr=FALSE, method="circle", na.label=" ")
}

correlationplot(df=dfk)
correlationplot(df=dfp)
####partial dependence of top 10 correlates

pdp_continuous <- function(variable,xvar,label,rf) {
  pdp.partial <- partial(rf, xvar,which.class=2)
  pdp.plot <- plotPartial(pdp.partial,  xlab= label, ylab= 'PD')
  if (xvar == "cannabis") {
  pdp.partial$cannabis <- factor(pdp.partial$cannabis, levels = c(0,2,1), labels= c('None', 'Rarely', 'Frequent'))
  pdp.plot <- plotPartial(pdp.partial,  xlab= label, ylab= 'PD')
  }
  if (xvar == "cocaine") {
  pdp.partial$cocaine <- factor(pdp.partial$cocaine, levels = c(0,1), labels= c('No', 'Yes'))
  pdp.plot <- plotPartial(pdp.partial,  xlab= label, ylab= 'PD')
  }
return(print(pdp.plot))
}

pdp1<-pdp_continuous(variable=cocaine,xvar='cocaine',label = 'cocaine use',rf=rf.shk)
pdp2<-pdp_continuous(variable=cannabis,xvar='cannabis', label = 'cannabis use',rf=rf.shk)
pdp3<-pdp_continuous(variable=outness,xvar='outness', label = 'overall outness',rf=rf.shk)
pdp4<-pdp_continuous(variable=ace,xvar='ace',label = 'adverse childhood experience',rf=rf.shk)
pdp5<-pdp_continuous(variable=per_stigma,xvar='per_stigma', label = 'perceived stigma',rf=rf.shk)
pdp6<-pdp_continuous(variable=phobia,xvar='phobia',label = 'internalized homophobia',rf=rf.shk)
pdp7<-pdp_continuous(variable=connect_com,xvar='connect_com',label = 'community connectedness',rf=rf.shk)
pdp8<-pdp_continuous(variable=cen_identity,xvar='cen_identity', label = 'centralized identity',rf=rf.shk)
pdp9<-pdp_continuous(variable=age,xvar='age',label = 'age',rf=rf.shk)
pdp10<-pdp_continuous(variable=cesd_score,xvar='cesd_score', label = 'depressive symptoms score',rf=rf.shk)

library(ggpubr)

ggarrange(pdp1,pdp2,pdp3,pdp4,pdp5,pdp6,pdp7,pdp8,pdp9,pdp10,
          ncol = 5, nrow = 2)

pdp1p<-pdp_continuous(variable=cannabis,xvar='cannabis', label = 'cannabis use',rf=rf.shp)
pdp2p<-pdp_continuous(variable=cocaine,xvar='cocaine',label = 'cocaine use',rf=rf.shp)
pdp3p<-pdp_continuous(variable=phobia,xvar='phobia',label = 'internalized homophobia',rf=rf.shp)
pdp4p<-pdp_continuous(variable=ace,xvar='ace',label = 'adverse childhood experience',rf=rf.shp)
pdp5p<-pdp_continuous(variable=outness,xvar='outness', label = 'overall outness',rf=rf.shp)
pdp6p<-pdp_continuous(variable=connect_com,xvar='connect_com',label = 'community connectedness',rf=rf.shp)
pdp7p<-pdp_continuous(variable=cesd_score,xvar='cesd_score', label = 'depressive symptoms score',rf=rf.shp)
pdp8p<-pdp_continuous(variable=per_stigma,xvar='per_stigma', label = 'perceived stigma',rf=rf.shp)
pdp9p<-pdp_continuous(variable=cen_identity,xvar='cen_identity', label = 'centralized identity',rf=rf.shp)
pdp10p<-pdp_continuous(variable=age,xvar='age',label = 'age',rf=rf.shp)

ggarrange(pdp1p,pdp2p,pdp3p,pdp4p,pdp5p,pdp6p,pdp7p,pdp8p,pdp9p,pdp10p,
          ncol = 5, nrow = 2)

demovars<-c('age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','rural_city','residence')

##Calculate demographic interactions

dem_interaction <- function(rf) {
interact.sh <- vint(rf, feature_names = demovars)
interact.sh$Interaction 
interactiondem<-as.data.frame(interact.sh)
int.sh_plot <- ggplot(data=interact.sh,aes(reorder(Variables, Interaction), Interaction))+
  geom_bar(stat="identity",fill=NA,color="black")+
  geom_text(aes(label=Interaction),color="black",position=position_dodge(0.1),hjust=-0.1)+
  coord_flip()+
  labs(y="Interaction strength",
       x="Pairwise interaction")+
  theme_classic()
return(interact.sh)
}

interaction_top10 <- function(dem,rf) {
  int1<-vint(rf, feature_names = c(dem, 'cocaine'))
  int2<-vint(rf, feature_names = c(dem, 'cannabis'))
  int3<-vint(rf, feature_names = c(dem, 'outness'))
  int4<-vint(rf, feature_names = c(dem, 'per_stigma'))
  int5<-vint(rf, feature_names = c(dem, 'cesd_score'))
  int6<-vint(rf, feature_names = c(dem, 'cen_identity'))
  int7<-vint(rf, feature_names = c(dem, 'phobia'))
  int8<-vint(rf, feature_names = c(dem, 'ace'))
  int9<-vint(rf, feature_names = c(dem, 'connect_com'))
  int10<-vint(rf, feature_names = c(dem, 'en_stigma'))
  c1<-c(int1[1,2],int2[1,2],int3[1,2],int4[1,2],int5[1,2],int6[1,2],
        int7[1,2],int8[1,2],int9[1,2],int10[1,2])
  c2<-c("cocaine","cannabis","outness","per_stigma","cesd_score","cen_identity","phobia","ace","connect_com","en_stigma")
  c3<-c(dem)
  tab<-cbind(c1,c2,c3)
  if (dem != "age") {
    int11<-vint(rf, feature_names = c(dem, 'age'))
    c1<-c(c1,int11[1,2])
    c2<-c(c2,"age")
    tab<-cbind(c1,c2,c3)
  }
  return(tab)
}

##Combine all interaction tables

interactiontable <- function (rfenter) {
int1<-interaction_top10(dem="curr_orient2",rfenter)
int2<-interaction_top10(dem="gender",rfenter)
int3<-interaction_top10(dem="ethnicity",rfenter)
int4<-interaction_top10(dem="education",rfenter)
int5<-interaction_top10(dem="employ",rfenter)
int6<-interaction_top10(dem="house_income",rfenter)
int7<-interaction_top10(dem="where_live",rfenter)
int8<-interaction_top10(dem="age",rfenter)
intall<-as.data.frame(rbind(int1,int2,int3,int4,int5,int6,int7,int8))
rownames(intall)<-NULL

intall2<-intall %>% rename(Interaction = c1,Variables1 = c2,Variables2 = c3) %>%
  mutate(Variables=paste(Variables1,Variables2,sep="*")) %>%
  select(-c(Variables1,Variables2))
interactiondem<-dem_interaction(rfenter)
intall3<-rbind(intall2,interactiondem)

intall3$Interaction <- as.numeric(as.character(intall3$Interaction))
intall4 <- intall3 %>% arrange(desc(Interaction))
return(intall4)
}

interactionk<-interactiontable(rfenter=rf.shk)
interactionp<-interactiontable(rfenter=rf.shp)

library(xlsx)
write.xlsx(interactionk, file="intallk.xlsx", sheetName="sheet1a")
write.xlsx(interactionp, file="intallp.xlsx", sheetName="sheet1a")

demovars<-c('age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')
demovars2<-c('province','age','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')

factorVars<-c('province','curr_orient2','gender','ethnicity','education', 'employ','house_income', 'where_live','residence','rural_city')

tab<-CreateTableOne(vars=demovars2, factorVars = factorVars,data=dat_vars)
print(summary(tab),quote=TRUE,noSpaces=TRUE)
tab2<-as.data.frame(print(tab))
print(tab,quote=TRUE,noSpaces=TRUE)

library(xlsx)
write.xlsx(tab2, file="demp.xlsx", sheetName="sheet1a")

library(gmodels)
library(crosstable)

CrossTable(dat_vars$curr_orient2[which(dat_vars$cocaine==1)], 
           dat_vars$MDMA_K_psyc[which(dat_vars$cocaine==1)])

CrossTable(dat_vars$curr_orient2[which(dat_vars$cocaine==0)], 
           dat_vars$MDMA_K_psyc[which(dat_vars$cocaine==0)])

CrossTable(dat_vars$gender[which(dat_vars$cocaine==1)], 
           dat_vars$MDMA_K_psyc[which(dat_vars$cocaine==1)])

CrossTable(dat_vars$gender[which(dat_vars$cocaine==0)], 
           dat_vars$MDMA_K_psyc[which(dat_vars$cocaine==0)])

#Need to create the plots for interactions to illustrate the interaction
#maybe dichotomize some of them or show only the strongest categories
#####2-way PDP for top 8 inetarctions

#Ketamine ones
pd1.sh <- partial(rf.shk, c('cocaine', 'curr_orient2'), which.class=2)
pd1.sh$cocaine <- factor(pd1.sh$cocaine, levels = c(0,1), 
                            labels= c('No', 'Yes'))
pd1.sh$curr_orient2 <- factor(pd1.sh$curr_orient2, levels = c(0,1,2,3,4,5,6,7,8), 
                               labels= c('Asexual','Straight','Bisexual','Gay','Lesbian','Pansexual','Queer','Questioning','Two-spirit'))
pd1.sh.p <- plotPartial(pd1.sh,xlab= 'Cocaine use', ylab="PD")
print(pd1.sh.p)

pd2.sh <- partial(rf.shk, c('cocaine', 'gender'), which.class=2)
pd2.sh$cocaine <- factor(pd2.sh$cocaine, levels = c(0,1), 
                         labels= c('No', 'Yes'))
pd2.sh.p <- plotPartial(pd2.sh,xlab= 'Cocaine use', ylab="PD")
print(pd2.sh.p)

pd3.sh <- partial(rf.shk, c('cannabis', 'curr_orient2'), which.class=2)
pd3.sh$cannabis <- factor(pd3.sh$cannabis, levels = c(0,1), 
                         labels= c('No', 'Yes'))
pd3.sh$curr_orient2 <- factor(pd3.sh$curr_orient2, levels = c(0,1,2,3,4,5,6,7,8), 
                              labels= c('Asexual','Straight','Bisexual','Gay','Lesbian','Pansexual','Queer','Questioning','Two-spirit'))
pd3.sh.p <- plotPartial(pd3.sh,xlab= 'Cannabis use', ylab="PD")
print(pd3.sh.p)

pd4.sh <- partial(rf.shk, c('cannabis', 'gender'), which.class=2)
pd4.sh$cannabis <- factor(pd4.sh$cannabis, levels = c(0,1), 
                         labels= c('No', 'Yes'))
pd4.sh.p <- plotPartial(pd4.sh,xlab= 'Cannabis use', ylab="PD")
print(pd4.sh.p)

pd5.sh <- partial(rf.shk, c('cannabis', 'where_live'), which.class=2)
pd5.sh$cannabis <- factor(pd5.sh$cannabis, levels = c(0,1), 
                          labels= c('No', 'Yes'))
pd5.sh$where_live <- factor(pd5.sh$where_live, levels = c(0,1,2), 
                              labels= c('Other','Alone','Parents'))
pd5.sh.p <- plotPartial(pd5.sh,xlab= 'Cannabis use', ylab="PD")
print(pd5.sh.p)

pd6.sh <- partial(rf.shk, c('cocaine', 'where_live'), which.class=2)
pd6.sh$cocaine <- factor(pd6.sh$cocaine, levels = c(0,1), 
                          labels= c('No', 'Yes'))
pd6.sh$where_live <- factor(pd6.sh$where_live, levels = c(0,1,2), 
                            labels= c('Other','Alone','Parents'))
pd6.sh.p <- plotPartial(pd6.sh,xlab= 'Cocaine use', ylab="PD")
print(pd6.sh.p)

###Psychedelics
pd1.shp <- partial(rf.shp, c('cocaine', 'curr_orient2'), which.class=2)
pd1.shp$cocaine <- factor(pd1.shp$cocaine, levels = c(0,1), 
                         labels= c('No', 'Yes'))
pd1.shp$curr_orient2 <- factor(pd1.shp$curr_orient2, levels = c(0,1,2,3,4,5,6,7,8), 
                              labels= c('Asexual','Straight','Bisexual','Gay','Lesbian','Pansexual','Queer','Questioning','Two-spirit'))
pd1.shp.p <- plotPartial(pd1.shp,xlab= 'Cocaine use', ylab="PD")
print(pd1.shp.p)

pd2.shp <- partial(rf.shp, c('cocaine', 'gender'), which.class=2)
pd2.shp$cocaine <- factor(pd2.shp$cocaine, levels = c(0,1), 
                         labels= c('No', 'Yes'))
pd2.shp$gender <- factor(pd2.shp$gender, levels = c(0,1,2,3,4), 
                               labels= c('cismen','ciswomen','transmen','transwomen','NB'))
pd2.shp.p <- plotPartial(pd2.shp,xlab= 'Cocaine use', ylab="PD")
print(pd2.shp.p)

pd3.shp <- partial(rf.shp, c('cannabis', 'curr_orient2'), which.class=2)
pd3.shp$cannabis <- factor(pd3.shp$cannabis, levels = c(0,1), 
                          labels= c('No', 'Yes'))
pd3.shp$curr_orient2 <- factor(pd3.shp$curr_orient2, levels = c(0,1,2,3,4,5,6,7,8), 
                              labels= c('Asexual','Straight','Bisexual','Gay','Lesbian','Pansexual','Queer','Questioning','Two-spirit'))
pd3.shp.p <- plotPartial(pd3.shp,xlab= 'Cannabis use', ylab="PD")
print(pd3.shp.p)

pd4.shp <- partial(rf.shp, c('cannabis', 'gender'), which.class=2)
pd4.shp$cannabis <- factor(pd4.shp$cannabis, levels = c(0,1), 
                          labels= c('No', 'Yes'))
pd4.shp$gender <- factor(pd4.shp$gender, levels = c(0,1,2,3,4), 
                         labels= c('cismen','ciswomen','transmen','transwomen','NB'))
pd4.shp.p <- plotPartial(pd4.shp,xlab= 'Cannabis use', ylab="PD")
print(pd4.shp.p)

pd5.shp <- partial(rf.shp, c('cannabis', 'where_live'), which.class=2)
pd5.shp$cannabis <- factor(pd5.shp$cannabis, levels = c(0,1), 
                          labels= c('No', 'Yes'))
pd5.shp$where_live <- factor(pd5.shp$where_live, levels = c(0,1,2), 
                            labels= c('Other','Alone','Parents'))
pd5.shp.p <- plotPartial(pd5.shp,xlab= 'Cannabis use', ylab="PD")
print(pd5.shp.p)

pd6.shp <- partial(rf.shp, c('cocaine', 'where_live'), which.class=2)
pd6.shp$cocaine <- factor(pd6.shp$cocaine, levels = c(0,1), 
                         labels= c('No', 'Yes'))
pd6.shp$where_live <- factor(pd6.shp$where_live, levels = c(0,1,2), 
                            labels= c('Other','Alone','Parents'))
pd6.shp.p <- plotPartial(pd6.shp,xlab= 'Cocaine use', ylab="PD")
print(pd6.shp.p)


