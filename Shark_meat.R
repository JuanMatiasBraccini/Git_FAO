# Script for analysing FAO non-fin product project

library(readxl)
library(tidyverse)

# Section 1. Import Data --------------------------------------------------
path='C:\\Matias\\FAO\\Shark_meat\\data\\Mexico\\'
Fisher <- read_excel(paste(path,"DATA BASE SURVEY.xlsx",sep=""), sheet = "Fisher survey")
#Middle <- read_excel(paste(path,"DATA BASE SURVEY.xlsx",sep=""), sheet = "Middleman-Aggregator survey")
#Seller <- read_excel(paste(path,"DATA BASE SURVEY.xlsx",sep=""), sheet = "Seller survey")


# Section 2. Manipulate Data --------------------------------------------------
  #tidy up data
Fisher <- Fisher%>%as.data.frame
#Middle <- Middle%>%as.data.frame
#Seller <- Seller%>%as.data.frame


  #define how to summarise each question

Q.list.Fisher=list(
              Boxplot=c('Q4'),        #concatenate all continuous vars
              Barplot=c('Q2','Q3'),   #concatenate all character vars
              Grouping=list(General=c('Q2','Q3','Q4'),  #define how to group vars for display
                            Boat_fishery=c('Q7'),
                            Management=c('Q6'),
                            Species_compo=c('Q5')))

Q.list.Middle=list(
              Boxplot=c(),
              Barplot=c(),
              Grouping=list())

Q.list.Seller=list(
              Boxplot=c(),
              Barplot=c(),
              Grouping=list())


# Section 3. Analyses --------------------------------------------------
fn.bxplt=function(MAT)
{
  boxplot(MAT)
}

fn.brplt=function(MAT,where,XLAB)
{
  Fill.col=gray(seq(0,.9,len = length(MAT)))
  barplot(as.matrix(MAT),main="",legend= names(MAT),names.arg=XLAB,
          col=Fill.col,args.legend=list(cex=1.2,x=where,bty="n",title=""))
}

# Section 4. Outputs --------------------------------------------------
fn.fig=function(NAME,Width,Height) jpeg(file=paste(NAME,".jpeg",sep=""),width=Width,height=Height,units="px",res=300)
smart.par=function(n.plots,MAR,OMA,MGP) return(par(mfrow=n2mfrow(n.plots),mar=MAR,oma=OMA,las=1,mgp=MGP))
path='C:\\Matias\\FAO\\Shark_meat\\Outputs\\Mexico\\'



  #Fisher
attach(Q.list.Fisher)
for(i in 1:length(Grouping))
{
  fn.fig(paste(path,"Fisher_",names(Grouping)[i],sep=""),2400,2400)
  smart.par(length(Grouping[[i]]),MAR=c(2,2,1,1),OMA=c(2,2,2,2),MGP=c(1,.7,0))
  for(n in 1:length(Grouping[[i]]))
  {
    dummy=Fisher%>%select(Grouping[[i]][n])%>%pull
    if(Grouping[[i]][n]%in%Boxplot)
    {
      fn.bxplt(dummy)
    }
    if(Grouping[[i]][n]%in%Barplot)
    {
      fn.brplt(MAT=table(dummy),where="top",XLAB=Grouping[[i]][n])
    }
  }
  dev.off()
}
detach(Q.list.Fisher)


  #Middle
attach(Q.list.Middle)
for(i in 1:length(Grouping))
{
  fn.fig(paste(path,"Middle_",names(Grouping)[i],sep=""),2400,2400)
}
detach(Q.list.Middle)



  #Seller
attach(Q.list.Seller)
for(i in 1:length(Grouping))
{
  fn.fig(paste(path,"Middle_",names(Grouping)[i],sep=""),2400,2400)
}
detach(Q.list.Seller)