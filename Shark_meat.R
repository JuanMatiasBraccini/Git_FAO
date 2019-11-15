# Script for analysing FAO non-fin product project
#MISSING: set up groups of questions for 3 interviews, with a & b being now and before...
library(readxl)
library(tidyverse)
library(lubridate)

# Section 1. Import Data --------------------------------------------------
path='C:\\Matias\\FAO\\Shark_meat\\data\\Mexico\\'

  #Questionnaire data
Fisher <- read_excel(paste(path,"DATA BASE SURVEY.xlsx",sep=""), sheet = "Fisher survey")
Middle <- read_excel(paste(path,"DATA BASE SURVEY.xlsx",sep=""), sheet = "Middleman-Aggregator survey")
Seller <- read_excel(paste(path,"DATA BASE SURVEY.xlsx",sep=""), sheet = "Seller survey")

  #Reference guide
Fisher.ref <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "G.reference (Fisher Surv)")
Middle.ref <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "G.reference (middle)")
Seller.ref <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "G.reference (seller)")
Species <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "Guide sp")


# Section 2. Manipulate Data --------------------------------------------------
  #tidy up data
Fisher <- Fisher%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))
          
Middle <- Middle%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))%>%
          filter(!is.na(surveys))

Seller <- Seller%>%as.data.frame%>%
          rename_all(tolower)%>%
            mutate(year=year(date),
                   month=month(date))

Fisher.ref <- Fisher.ref%>%as.data.frame%>%
              mutate(Question=tolower(Question))

Middle.ref <- Middle.ref%>%as.data.frame%>%
  mutate(Question=tolower(Question))

Seller.ref <- Seller.ref%>%as.data.frame%>%
  mutate(Question=tolower(Question))
  #define how to group and summarise each question

Q.list.Fisher=list(
    General=c('location','estate (districit)',  #define how to group vars for display
              'q1','q2','q3','q4'),  
    Catch=c('q5 & q6',                          #the & groups the same question 'now' and 'before'
           'q7_a & q8_a',
           'q7_b & q8_b',
           'q7_c & q8_c',
           'q9_a & q10_a',
           'q9_b & q10_b',
           'q9_c & q10_c',
           'q15 & q16'),
   Effort=c('q11 & q12',
            'q13 & q14'),
   Boat=c(''),
   Management=c(''),
   Species_compo=c(''))

Q.list.Middle=list(General="Market_name","Location","Estate")

Q.list.Seller=list(General="Village","Location","Estate","Type_of_location")


# Section 3. Analyses --------------------------------------------------
colfunc <- colorRampPalette(c("lightcyan", "dodgerblue4"))
fn.brplt=function(MAT,XLAB)
{
  if(!is.character(dummy) &!is.data.frame(dummy))
  {
    Fill.col="steelblue"
    Ls=1
    if(max(nchar(names(MAT)))>1)Ls=2
    barplot(MAT,main="",col=Fill.col,xlab=XLAB,las=Ls)
  }
  if(is.character(dummy)&!is.data.frame(dummy))
  {
    Fill.col= colfunc(length(MAT))
    MAT=as.matrix(MAT) 
    barplot(MAT,main="",xlab=XLAB,cex.names = 1.5,col=Fill.col)
    lgn=rownames(MAT)
    if(length(lgn)<=2)legend("top", fill = Fill.col, 
                             legend = lgn, horiz = F,cex=1.3,
                             inset = c(0,-0.25),bty='n', xpd = TRUE)
    if(length(lgn)>2)
    {
      nn=1:(length(lgn)/2)
      legend("topleft", fill = Fill.col[nn], 
             legend = lgn[nn], horiz = F,cex=1.25,
             inset = c(0,-0.35),bty='n', xpd = TRUE)
      nn=(1+(length(lgn)/2)):length(lgn)
      legend("topright", fill = Fill.col[nn], 
             legend = lgn[nn], horiz = F,cex=1.25,
             inset = c(0,-0.35),bty='n', xpd = TRUE)
    }
  }
  
  if(is.data.frame(dummy))
  {
    Fill.col= colfunc(nrow(MAT))
    barplot(MAT,main="",xlab=XLAB,cex.names = 1.25,col=Fill.col)
    lgn=rownames(MAT)
    if(length(lgn)<=2)legend("top", fill = Fill.col, 
                             legend = lgn, horiz = F,cex=1.3,
                             inset = c(0,-0.25),bty='n', xpd = TRUE)
    if(length(lgn)>2)
    {
      nn=1:(length(lgn)/2)
      legend("topleft", fill = Fill.col[nn], 
             legend = lgn[nn], horiz = F,cex=1.25,
             inset = c(0,-0.35),bty='n', xpd = TRUE)
      nn=(1+(length(lgn)/2)):length(lgn)
      legend("topright", fill = Fill.col[nn], 
             legend = lgn[nn], horiz = F,cex=1.25,
             inset = c(0,-0.35),bty='n', xpd = TRUE)
    }
  }
  box()
}

# Section 4. Outputs --------------------------------------------------
fn.fig=function(NAME,Width,Height) jpeg(file=paste(NAME,".jpeg",sep=""),width=Width,height=Height,units="px",res=300)
smart.par=function(n.plots,MAR,OMA,MGP) return(par(mfrow=n2mfrow(n.plots),mar=MAR,oma=OMA,las=1,mgp=MGP,xpd=T))
path='C:\\Matias\\FAO\\Shark_meat\\Outputs\\Mexico\\'

#Output date of interviews
Tab.dates=with(data.frame(group=c(rep("Fisher",length(Fisher$date)),
                   rep("Middle",length(Middle$date)),
                   rep("Seller",length(Seller$date))),
           date=as.character(c(Fisher$date,Middle$date,Seller$date))),
     table(group,date))
write.csv(Tab.dates,paste(path,"Interview.dates.csv",sep="\\"),row.names = T)

  #Fisher
for(i in 1:length(Q.list.Fisher))
{
  fn.fig(paste(path,"Fisher_",names(Q.list.Fisher)[i],sep=""),2000,2400)
  smart.par(length(Q.list.Fisher[[i]]),MAR=c(2.5,2,3.9,.5),OMA=c(.1,1.25,.2,.1),
            MGP=c(1.65,.2,0))
  par(tck=.025,cex.axis=1.25,cex.lab=1.5)
  for(n in 1:length(Q.list.Fisher[[i]]))
  {
    this=Q.list.Fisher[[i]][n]
    if(grepl("&",this))
    {
      this=unlist(strsplit(this," & "))
      dummy=Fisher%>%select(this)
      dummy$Currently=rep('Currently',nrow(dummy))
      dummy$Before=rep('Before',nrow(dummy))
      dummy=data.frame(var=c(dummy[,1],dummy[,2]),
                      period=c(dummy$Currently,dummy$Before))
      MAT=table(dummy$var,dummy$period)
    }else
    {
      dummy=Fisher%>%select(this)%>%pull
      MAT=table(dummy,useNA = 'ifany')
    }
      
    
    fn.brplt(MAT=MAT,XLAB=Q.list.Fisher[[i]][n])
  }
  mtext("Frequency",2,line=-.25,outer=T,las=3,cex=1.35)
  dev.off()
}

  #Middle
for(i in 1:length(Q.list.Middle))
{
  
}


  #Seller
for(i in 1:length(Q.list.Seller))
{
  
}
