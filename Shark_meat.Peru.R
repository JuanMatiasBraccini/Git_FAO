# Script for analysing FAO non-fin product project --Peru


library(readxl)
library(tidyverse)
library(lubridate)
library("ggplot2")
library("sf")
library("rnaturalearthdata")
library(rnaturalearth)
library("ggspatial")  #for scale bar
library(gsubfn)
library(stargazer)
library(MASS)
library(nnet)

do.exploratory=FALSE

# Section 1. Import Data --------------------------------------------------
path='C:\\Matias\\FAO\\Shark_meat\\data\\Peru\\'

  #Questionnaire data
Fisher <- read_excel(paste(path,"Peru NFC Surveys Database Matias.xlsx",sep=""), sheet = "Fisher")
Middle <- read_excel(paste(path,"Peru NFC Surveys Database Matias.xlsx",sep=""), sheet = "Aggregator")
Seller <- read_excel(paste(path,"Peru NFC Surveys Database Matias.xlsx",sep=""), sheet = "Seller")

  #Reference guide
Fisher.ref <- read_excel(paste(path,"Peru NFC Surveys Database Matias.xlsx",sep=""), sheet = "Ref Fisher")
Middle.ref <- read_excel(paste(path,"Peru NFC Surveys Database Matias.xlsx",sep=""), sheet = "Ref Aggregator")
Seller.ref <- read_excel(paste(path,"Peru NFC Surveys Database Matias.xlsx",sep=""), sheet = "Ref Seller")
#Species <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "Guide sp")


# Section 2. Manipulate Data --------------------------------------------------
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

  #2.1. tidy up data
Fisher <- Fisher%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))%>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns

Fisher$q1=ifelse(Fisher$q1=="san_josé","san josé",Fisher$q1)
Fisher$q1=firstup(Fisher$q1)

Middle <- Middle%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))%>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns
Middle$q1=firstup(Middle$q1)

Seller <- Seller%>%as.data.frame%>%
          rename_all(tolower)%>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns
Seller$q2=ifelse(Seller$q2=="san jose","san josé",Seller$q2)
Seller$q2=firstup(Seller$q2)

#convert all character to lower to avoid lower and upper case   
for(n in 1:ncol(Fisher)) if(is.character(Fisher[,n])) Fisher[,n]=tolower(Fisher[,n])
for(n in 1:ncol(Middle)) if(is.character(Middle[,n])) Middle[,n]=tolower(Middle[,n])
for(n in 1:ncol(Seller)) if(is.character(Seller[,n])) Seller[,n]=tolower(Seller[,n])



# Section 3. Grouping of columns --------------------------------------------------
fn.pst1=function(Q,rng,D)
{
  a=paste(paste(Q,tolower(LETTERS[rng]),sep='_'),sep=",")
  return(a=a[which(a%in%colnames(D))])
}
#fn.pst1('q5',1:21,Fisher)

fn.grp=function(d1,d2)
{
  Grouping=unique(sapply(strsplit(d1, "_"), "[", 1))
  Grouping=tolower(subset(Grouping,!Grouping=="Date"))
  names.d2=sapply(strsplit(names(d2), "_"), "[", 1)
  dummy=vector('list',length(Grouping))
  names(dummy)=Grouping
  for(q in 1:length(dummy)) dummy[[q]]=which(names.d2==names(dummy)[q])
  return(dummy)
}
Tabl.Fisher=fn.grp(d1=Fisher.ref$Survey,d2=Fisher)
Tabl.Middle=fn.grp(d1=Middle.ref$Survey,d2=Middle)
Tabl.Seller=fn.grp(d1=Seller.ref$Surveys,d2=Seller)


# Section 4. Outputs --------------------------------------------------
fn.fig=function(NAME,Width,Height) tiff(file=paste(NAME,".tiff",sep=""),
              width=Width,height=Height,units="px",res=300,compression="lzw")
smart.par=function(n.plots,MAR,OMA,MGP) return(par(mfrow=n2mfrow(n.plots),mar=MAR,oma=OMA,las=1,mgp=MGP,xpd=T))
path='C:\\Matias\\FAO\\Shark_meat\\Outputs\\Peru\\'

#Map of Study Area
do.Map=FALSE
if(do.Map)
{
  theme_set(theme_bw()) 
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  sites.fisher=data.frame(Port=c('Máncora','Salaverry','San José',
                                 'Pucusa','Paita','Talara','Tumbes'),
                          longitude=c(,-79,-79.99,
                                      ,,,),
                          latitude=c(,-8.214,-6.76,
                                     ,,,))
  sites.middle=data.frame(MArket=c("San José","Santa Rosa","Salaverry",
                                   "tumbes","paita","sulla","piura",
                                   "sechuria","sechura","bayovar","chiclayo",
                                   "chimbote","zarumilla"),
                          longitude=c(-79.6,-77.161,-78.55,
                                      ,,,,
                                      ,,,,
                                      ,,),
                          latitude=c(-6.76,-11.806,-8.214,
                                     ,,,,
                                     ,,,,
                                     ,,))
  
  sites.seller=data.frame(Village=c('Lima',"San José","Salaverry",
                                    "tumbes","talara","paita",
                                    "piura","chiclayo","jaen"),
                          longitude=c(-77.04,-79.3,-78.2,
                                      ,,,
                                      ,,),
                          latitude=c(-12.04,-6.76,-8.214,
                                     ,,,
                                     ,,))
  
  
  ggplot(data = world) +
    geom_sf(color = "grey20", fill = "grey85") +
    coord_sf(xlim = c(-82, -65), 
             ylim = c(-18, 0), expand = T)+
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl",
                           which_north = "true", pad_x = unit(0.75, "in"), 
                           pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    xlab("Longitude") + ylab("Latitude") +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12,face="bold"))+
    annotate(geom = "text", x = -75, y = -10, label = "Peru", 
             fontface = "italic", color = "grey40", size = 9,srt=-60)  +
    geom_point(data = sites.fisher, aes(x = longitude, y = latitude),
               size = 3, shape = 21, fill = "darkred")   +
    geom_point(data = sites.middle, aes(x = longitude, y = latitude),
               size = 3, shape = 22, fill = "darkgreen") +
    geom_point(data = sites.seller, aes(x = longitude, y = latitude),
               size = 3, shape = 23, fill = "blue") +
    geom_text(aes(x=-81,y=18),label="fisher",size=5) +
    geom_text(aes(x=-81,y=17),label="middle",size=5) +
    geom_text(aes(x=-81,y=16),label="seller",size=5) +
    geom_point(aes(x=-82.75,y=18),size=3,shape = 21, fill = "darkred") +
    geom_point(aes(x=-82.75,y=17),size=3, shape = 22, fill = "darkgreen") +
    geom_point(aes(x=-82.75,y=16),size=3, shape = 23, fill = "blue")
  
  ggsave(paste(path,"Figure 1.map.tiff",sep=''),width = 10, height = 6, dpi = 300)
}

#Date of interviews for all groups
Tab.dates=with(data.frame(group=c(rep("Fisher",length(Fisher$date)),
                                  rep("Middle",length(Middle$date)),
                                  rep("Seller",length(Seller$date))),
                          date=as.character(c(Fisher$date,Middle$date,Seller$date))),
               table(group,date))
write.csv(Tab.dates,paste(path,"Table.1.Interview.dates.csv",sep="\\"),row.names = T)


  #...Tables and Figures
fn.tbl=function(d,where,DATA)    
{
  for(i in 1:length(d))
  {
    if(length(d[[i]])>0)
    {
      for(nn in 1:length(d[[i]]))
      {
        this=d[[i]][nn]
        if(grepl("&",this))
        {
          this=unlist(strsplit(this," & "))
          dummy=DATA%>%dplyr::select(this)
          Dat=data.frame(period=rep(this,each=nrow(dummy)),
                         var=unlist(dummy))
          MAT=with(Dat,table(var,period,useNA = 'ifany'))
          write.csv(MAT,paste(path,where,names(d)[i],"_",d[[i]][nn],".csv",sep=""))
          
        }else
        {
          Value=DATA%>%dplyr::select(this)
          MAT=table(Value,useNA = 'ifany')
          write.csv(MAT,paste(path,where,names(DATA)[this],".csv",sep=""),row.names=F)
          
          
          fn.fig(paste(path,where,names(DATA)[this],sep=""),2400,2400)
          if(length(MAT)>3) par(las=3)
          barplot(MAT)
          dev.off()
        }
      }
    }
  }
}

fn.tbl(d=Tabl.Fisher,where="Fisher\\",DATA=Fisher) 
fn.tbl(d=Tabl.Middle,where="Middle\\",DATA=Middle) 
fn.tbl(d=Tabl.Seller,where="Seller\\",DATA=Seller) 

  
