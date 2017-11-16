library(XML)
setwd("C:\\Users\\Hubert\\Documents\\NBA")
Sys.setlocale("LC_TIME", "C");

L<-list(100)
k=1
for (year in (2014:2017)) {
  for (mth in c("oct","nov","dec","jan","feb","mar","apr")) {
link<-paste0('http://www.landofbasketball.com/results/',year,'_',year+1,'_',mth,'_scores.htm')

doc.html<-htmlTreeParse(link,
                        useInternal = TRUE)

doc.text = unlist(xpathApply(doc.html, '//td', xmlValue))

doc.text<-gsub('[\n | ]','',doc.text)
doc.text<-gsub('Philadelphia76ers','PhiladelphiaErs',doc.text)
doc.text<-gsub('2OT2OT','OTdwaOT',doc.text)
doc.text<-gsub('3OT3OT','OTtrzyOT',doc.text)
doc.text<-gsub('4OT4OT','OTczteryOT',doc.text)

doc.text<-doc.text[which(!grepl("Playoffs",doc.text))]
doc.text<-doc.text[which(nchar(doc.text)>0)]
doc.text<-doc.text[which(!grepl("H2H",doc.text))]
#doc.text<-gsub('[A-Z]','',doc.text)
#doc.text<-gsub("([A-Z])", " \\1",doc.text)



#######
shape<-function(x) {
  teams<-unlist(strsplit(x,split='[0-9]|Originally.*'))
  teams
  teams<-teams[which(nchar(teams)>0)]
  teams[3]<-sub('at','',teams[3])
  dogrywka<-grepl('OT',teams[3])+grepl('dwa',teams[3])+grepl('trzy',teams[3])+grepl('cztery',teams[3])
  teams[3]<-sub('OTOT','',teams[3])
  teams[3]<-sub('OTdwaOT','',teams[3])
  teams[3]<-sub('OTtrzyOT','',teams[3])
  teams[3]<-sub('OTczteryOT','',teams[3])
  teams[2]<-gsub('-','',teams[2])
  
  
  score<-unlist(strsplit(x,split='[A-Z |a-z]'))
  score<-score[which(nchar(score)>0)]
  score[1]<-gsub('-','',score[1])
  host<-grepl(teams[3],teams[1:2]) # uwaga!!!!! moze byc niejednoznaczne
  

  
  if(sum(is.na(host))) {
    
  } else {
  if (sum(host)==2) { host[2]=FALSE}
  data.frame(gospodarz=teams[which(host)],gosc=teams[which(!host)]
             ,gospodarz_res=score[which(host)], gosc_res=score[which(!host)],dogrywka)}
}

df<-Reduce(rbind,lapply(doc.text,FUN=shape))

#lapply(doc.text[200],FUN=shape)


# data #
tmp<-grepl(toupper(mth),toupper(doc.text))*(grepl(paste0(",",year),doc.text)+grepl(paste0(",",year+1),doc.text))
tmp[which(tmp==1 & nchar(doc.text)>10)]<-0
daty<-doc.text[which(as.logical(tmp))]
daty<-daty[which(nchar(daty)<11)]
tab<-table(cumsum(tmp))[-1]
daty<-rep(daty,times=tab-1)
daty <- as.Date(daty, "%b%d,%Y")
L[[k]]<-cbind(df,daty)
k=k+1
}
}
df<-Reduce(rbind,L)
df$gospodarz_res<-as.integer(as.character(df$gospodarz_res))
df$gosc_res<-as.integer(as.character(df$gosc_res))


####################################################################################
#write.csv(df,file="wyniki.csv")

#df<-read.csv("C:\\Users\\Hubert\\Documents\\NBA\\wyniki.csv")


df$daty<-as.Date(as.character(df$daty))
df$gospodarz_res<-as.numeric(df$gospodarz_res)
df$gosc_res<-as.numeric(df$gosc_res)
df$total<-df$gosc_res+df$gospodarz_res

add_vars<-function(i) {
  gsp<-df[i,]$gospodarz
  gsc<-df[i,]$gosc
  ind<-which(df$gospodarz==gsp| df$gosc==gsp)
  kiedy<-max(df$daty[ind][which(df$daty[ind]<df$daty[i])] )
  tmp<-df[ind,][which(df$daty[ind]==kiedy),]
 # print(tmp)
  win_gosp=ifelse ( (tmp$gosc==gsp & tmp$gosc_res>tmp$gospodarz_res) |
                      (tmp$gospodarz==gsp & tmp$gosc_res<tmp$gospodarz_res),1,0 )
  gosp_gosp=ifelse ( tmp$gospodarz==gsp ,1,0 )
  gosp_past_points<-ifelse ( tmp$gospodarz==gsp ,tmp$gospodarz_res,tmp$gosc_res )
  gosp_past_total<-tmp$total
  gosp_past_add<-tmp$dogrywka
  ##to samno dla obecnego goscia
  
  ind<-which(df$gospodarz==gsc| df$gosc==gsc)
  kiedy<-max(df$daty[ind][which(df$daty[ind]<df$daty[i])] )
  tmp<-df[ind,][which(df$daty[ind]==kiedy),]
  
  win_gosc=ifelse ( (tmp$gosc==gsc & tmp$gosc_res>tmp$gospodarz_res) |
                      (tmp$gospodarz==gsc & tmp$gosc_res<tmp$gospodarz_res),1,0 )
  gosc_gosp=ifelse ( tmp$gospodarz==gsc ,1,0 )
  gosc_past_points<-ifelse ( tmp$gospodarz==gsc ,tmp$gospodarz_res,tmp$gosc_res )
  gosc_past_total<-tmp$total
  gosc_past_add<-tmp$dogrywka
  #print(tmp)
  data.frame(win_gosp,gosp_gosp,gosp_past_points,win_gosc,gosc_gosp,gosc_past_points,gosp_past_add,gosc_past_add
             ,gosp_past_total,gosc_past_total,gosp_past_add)
}
LL<-lapply(100:nrow(df),add_vars)
LL<-Reduce(rbind,LL)
df_new<-cbind(df[100:nrow(df),],LL)

#write.csv(df_new,file="wyniki2.csv")

df_new$mth<-as.factor(format(df_new$daty,'%b'))

df_new<-df_new[which(df_new$daty>"2015-09-30"),]
df_new$daty_r<-rank(df_new$daty)
#TESTwhich(df$daty<as.Date("2017-11-01"))
#df<-which(df$daty<as.Date("2017-11-01"))

##################################################################################################
#model
TRAIN<-df_new[which(df_new$daty<"2017-11-11"),]
TEST<-df_new[which(df_new$daty>="2017-11-11"),]
TRAIN$daty_r<-rank(TRAIN$daty)
TEST$daty_r<-rank(TEST$daty)

#write.csv(TRAIN,"TRAIN.csv")
#write.csv(TEST,"TEST.csv")
## ANOVA
#gospodarz<- lm(gospodarz_res ~ gospodarz*gosc, data = df)
#gosc<-lm(gosc_res ~ gospodarz*gosc+daty, data = df)


#total<-lm(total ~.+ gospodarz:gosc-gospodarz_res-gosc_res-dogrywka-win_gosc-daty_r-
#            gosc_past_add-gosc_gosp-win_gosp-gosp_past_add-gosc_past_total-eps
#          -gosp_past_add.1-pred_lm_train, data = TRAIN)

total<-lm(total ~ gospodarz*gosc , data = TRAIN)


anova(total)

step(total)


#predykcja

median(abs(predict(total,newdata=TEST)-TEST$total))
mean(abs(predict(total,newdata=TEST)-TEST$total))

median(abs(predict(total,newdata=TRAIN)-TRAIN$total))
mean(abs(predict(total,newdata=TRAIN)-TRAIN$total))


pred_lm_test<-predict(total,newdata=TEST)

TRAIN$pred_lm_train<-predict(total,newdata=TRAIN)
TRAIN$eps<-TRAIN$total-TRAIN$pred_lm_train
#ndf<-expand.grid(unique(df$gosc),unique(df$gosc))
#ndf$daty<-as.numeric(Sys.Date()-16735)
#ndf$daty_fac<-max(df$daty_fac)+1
#names(ndf)<-c("gospodarz","gosc","daty","daty_fac")
#predykcja<-predict(total,newdata=ndf)
#pred_dta<-cbind(ndf,predykcja)

#write.csv(pred_dta,file="ANOVA2.csv")


library(randomForest)

TRAIN$daty=as.numeric(TRAIN$daty)
TEST$daty=as.numeric(TEST$daty)

RF<-randomForest(total ~        
                 #+gosp_past_points
                 #+gosc_past_points 
                 #+gosp_past_total
                 #+gosc_past_total
                 #+daty 
                 #+daty_r
                 #+win_gosp
                 #+win_gosc
                 #+gosp_gosp
                 #+gosc_gosp
                 #+mth
                 +gospodarz+gosc , data = TRAIN,ntree=500)

cor(TRAIN[,"daty_r"],as.numeric(TRAIN$daty))
pred_RF_train<-predict(RF,newdata=TRAIN)

median(abs(predict(RF,newdata=TRAIN)-TRAIN$total))
mean(abs(predict(RF,newdata=TRAIN)-TRAIN$total))

median(abs(predict(RF,newdata=TEST)-TEST$total))
mean(abs(predict(RF,newdata=TEST)-TEST$total))

importance(RF)
median(abs((predict(RF,newdata=TRAIN)+predict(total,newdata=TRAIN))/2-TRAIN$total))
mean(abs(predict(RF,newdata=TRAIN)+predict(total,newdata=TRAIN)-TRAIN$total))

median(abs((predict(RF,newdata=TEST)+predict(total,newdata=TEST))/2-TEST$total))
mean(abs(predict(RF,newdata=TEST)+predict(total,newdata=TEST)-TEST$total))

hist(predict(RF,newdata=TEST)-TEST$total)

## œrednia

PRED<-(pred_lm+pred_RF)/2

median(abs(PRED-TEST$total))
mean(abs(PRED-TEST$total))

################################### predykcja n a fortuna
gsp="BrooklynNets";gsc="BostonCeltics";df=df_new;model=RF

prd<-function(gsp,gsc,df,model1,model2,daty,mth) {

    ind<-which(df$gospodarz==gsp| df$gosc==gsp)
    kiedy<-max(df$daty[ind][which(df$daty[ind]<daty)] )
    tmp<-df[ind,][which(df$daty[ind]==kiedy),]
    # print(tmp)
    win_gosp=ifelse ( (tmp$gosc==gsp & tmp$gosc_res>tmp$gospodarz_res) |
                        (tmp$gospodarz==gsp & tmp$gosc_res<tmp$gospodarz_res),1,0 )
    gosp_gosp=ifelse ( tmp$gospodarz==gsp ,1,0 )
    gosp_past_points<-ifelse ( tmp$gospodarz==gsp ,tmp$gospodarz_res,tmp$gosc_res )
    gosp_past_total<-tmp$total
    gosp_past_add<-tmp$dogrywka
    ##to samno dla obecnego goscia
    
    ind<-which(df$gospodarz==gsc| df$gosc==gsc)
    kiedy<-max(df$daty[ind][which(df$daty[ind]<daty)] )
    tmp<-df[ind,][which(df$daty[ind]==kiedy),]
    
    win_gosc=ifelse ( (tmp$gosc==gsc & tmp$gosc_res>tmp$gospodarz_res) |
                        (tmp$gospodarz==gsc & tmp$gosc_res<tmp$gospodarz_res),1,0 )
    gosc_gosp=ifelse ( tmp$gospodarz==gsc ,1,0 )
    gosc_past_points<-ifelse ( tmp$gospodarz==gsc ,tmp$gospodarz_res,tmp$gosc_res )
    gosc_past_total<-tmp$total
    gosc_past_add<-tmp$dogrywka
    #print(tmp)
  new_df<-data.frame(gospodarz=gsp,gosc=gsc,gosp_past_points=as.numeric(gosp_past_points),gosc_past_total
                     ,gosc_past_points=as.numeric(gosc_past_points) ,win_gosp,win_gosc,gosp_gosp,gosc_gosp,
                     gosp_past_total=as.numeric(gosp_past_total),daty=daty,mth=mth)
  #eps=predict(model2,newdata=new_df)
  
 for (i in c("gospodarz","gosc","mth" ))
   
   levels(new_df[,i]) <- levels(TRAIN[,i])
  #new_df$eps=eps
  predict(model1,newdata=new_df)#+predict(model2,newdata=new_df)
}

#TEST  

prd(TEST$gospodarz[i],TEST$gosc[i]
    ,df=df_new,model1=RF,model2=total,daty=TEST$daty[i],mth=TEST$mth[i])



sort(unique(as.character(TRAIN$gosc)))

p_RF=function(gospodarz,gosc) {
newdata=data.frame(gospodarz=gospodarz,gosc=gosc,daty=as.numeric(as.Date('2017-11-15')))
levels(newdata$gospodarz) <- levels(TRAIN$gospodarz)
levels(newdata$gosc) <- levels(TRAIN$gosc)
predict(RF,newdata=newdata)
}

p_RF(gospodarz=as.factor("MiamiHeat"),gosc=as.factor("WashingtonWizards"))
p_RF("NewYorkKnicks","UtahJazz")

predict(RF,newdata=data.frame(gospodarz="MiamiHeat",gosc="WashingtonWizards",daty=as.numeric(as.Date('2017-11-15'))))

prd("NewYorkKnicks","UtahJazz",df=df_new,model1=RF,model2=total
    ,daty=as.Date("2017-11-15"),mth=as.factor("Nov"))

ndf<-expand.grid(unique(TRAIN$gosc),unique(TRAIN$gosc))
ndf$daty<-as.numeric(Sys.Date())
names(ndf)<-c("gospodarz","gosc","daty")

predykcja<-predict(RF,newdata=ndf)
pred_dta<-cbind(ndf,predykcja)

write.csv(pred_dta,file="RF2.csv")




prd("HoustonRockets","TorontoRaptors",df=df_new,model=RF,daty=as.Date("2017-11-14"),mth=as.factor("Nov"))
#######
cbind(dimnames(importance(RF))[[1]][order(importance(RF))],sort(importance(RF))
plot(importance(RF))
      
pred_RF<-predict(RF,pred_dta)
write.csv(cbind(pred_dta,pred_RF),file="PRED_RF.csv")

mean(abs(predict(RF)-df$total))
mean(abs(predict(total)-df$total))



#################################
### porównanie
TRAIN<-df[which(df$daty<730),]
TEST<-df[which(df$daty>=730),]

total<-lm(total ~ gospodarz*gosc+daty+daty_fac, data = TRAIN)
pred_lm<-predict(total,newdata = TEST)
mean(abs(pred_lm-TEST$total))
median(abs(pred_lm-TEST$total))

RF<-randomForest(total~gosc+gospodarz+daty+daty_fac,data=TRAIN)
pred_RF<-predict(RF,TEST)
mean(abs(pred_RF-TEST$total))
median(abs(pred_RF-TEST$total))
