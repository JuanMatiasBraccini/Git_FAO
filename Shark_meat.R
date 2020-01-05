# Script for analysing FAO non-fin product project
#MISSING: plots and tables. 
#        Go thru each figure and decide what is main figures, 
#           what is appendix figures, what is text (output as table)
#           and what is main tables and appendix table
#        Use latest version of database
#        Add a flowchart of shark product, harvesting, processing and trade 
#         (see Figures 1-4 Monjurul et al and Figure 2 Jabado et al)

#missing: q13 & q14 must be analysed if they are shark fishers or not!
#problem: Q17 and Q19 are 'catch now at average capacity' but 
#         Q18 and Q20 are 'catch before at maximum capacity'
#         Q36, 37 & 38: are these 'now' or 'before' would have to assume 'now'


#deje ACA: fisher. Fisher_4_Effort.tiff; but first group ALL questions 'now' 'before'

#1. Just mention in Text (export as table):
#   'location','estate (districit)' 
#   'q1','q3','q4'
#ktch: 'q5 & q6','q25 & q26','q27 & q28',

#2. Nice table in body text:
#Species catch composition now and before with IUCN status

#3. Nice table in appendix:

#4. Figure in body text:
#Figure fisher catch now and then:
# 'q7_a & q7_b & q7_c', 'q8_a & q8_b & q8_c,
# 'q9_a & q9_b & q9_c', 'q10_a & q10_b & q10_c,


#   Fig. Climate change 'q86' & '86_a' 


#5. Figure in Apendices:
#   Figure general: q2 (use histograms, not barplot; cut in 5 year intervals),
#   Figure fisher catch: 'q15 & q16', 'q17 & q18','q19 & q20',
#                         'q56_1 & q56_2 & q56_3','q65 & q66' (use horizontal histograms, not barplot)


library(readxl)
library(tidyverse)
library(lubridate)
library("ggplot2")
library("sf")
library("rnaturalearthdata")
library("rnaturalearth")
library("ggspatial")  #for scale bar
library(gsubfn)

do.exploratory=FALSE

# Section 1. Import Data --------------------------------------------------
path='C:\\Matias\\FAO\\Shark_meat\\data\\Mexico\\'

  #Questionnaire data
Fisher <- read_excel(paste(path,"DATA BASE SURVEY_2019_12.xlsx",sep=""), sheet = "Fisher survey")
Middle <- read_excel(paste(path,"DATA BASE SURVEY_2019_12.xlsx",sep=""), sheet = "Middleman-Aggregator survey")
Seller <- read_excel(paste(path,"DATA BASE SURVEY_2019_12.xlsx",sep=""), sheet = "Seller survey")

  #Reference guide
Fisher.ref <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "G.reference (Fisher Surv)")
Middle.ref <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "G.reference (middle)")
Seller.ref <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "G.reference (seller)")
Species <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "Guide sp")


# Section 2. Manipulate Data --------------------------------------------------
  #2.1. tidy up data
Fisher <- Fisher%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))%>% 
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns
       
Middle <- Middle%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))%>%
          filter(!is.na(surveys)) %>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns
if("q1_d" %in% colnames(Middle)) colnames(Middle)[match("q1_d",colnames(Middle))] <- "q14_d"

Seller <- Seller%>%as.data.frame%>%
          rename_all(tolower)%>%
            mutate(year=year(date),
                   month=month(date))%>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns

#convert all character to lower to avoid lower and upper case
for(n in 1:ncol(Fisher)) if(is.character(Fisher[,n])) Fisher[,n]=tolower(Fisher[,n])
for(n in 1:ncol(Middle)) if(is.character(Middle[,n])) Middle[,n]=tolower(Middle[,n])
for(n in 1:ncol(Seller)) if(is.character(Seller[,n])) Seller[,n]=tolower(Seller[,n])

Fisher.ref <- Fisher.ref%>%as.data.frame%>%
              mutate(Question=tolower(Question))%>%
              rename('Question number'=Question,
                      Question=Question_long)%>%
              select(-c(Section,Combo))

Middle.ref <- Middle.ref%>%as.data.frame%>%
                  mutate(Question=tolower(Question))%>%
                  rename('Question number'=Question,
                         Question='Question long')%>%
                  select(-c(Section,Combo))

Seller.ref <- Seller.ref%>%as.data.frame%>%
                  mutate(Question=tolower(Question))%>%
                  rename('Question number'=Question,
                         Question='Question long')%>%
                  select(-c(Section,Combo))

  #2.2. define how to group and summarise each question

#define how to group vars for display
#note: the '&' groups the same question 'now' and 'before'

fn.pst=function(Q,rng) paste(paste(Q,tolower(LETTERS[rng]),sep='_'),collapse=" & ")
Q.list.Fisher=list(
    General=c('location','estate (districit)'),
    'Fisher profile'=c('q1','q2','q3','q4','q86 & q86_a'),
    Catch=c('q5 & q6',                          
           'q7_a & q8_a',
           'q7_b & q8_b',
           'q7_c & q8_c',
           'q9_a & q10_a',
           'q9_b & q10_b',
           'q9_c & q10_c',
           'q15 & q16',
           'q17 & q18',
           'q19 & q20',
           'q25 & q26',
           'q27 & q28',
           'q56_1 & q56_2 & q56_3',
           'q65 & q66'
           ),
   Effort=c('q11 & q12',
            'q13 & q14',
            'q46 & q47',
            'q48 & q49',
            'q50 & q51',
            'q52 & q53',
            'q54 & q55',
            'q57 & q58',
            'q59 & q60',
            'q61 & q62',
            'q63 & q64',
            fn.pst('q81',1:3)),
   Socio.economics=c('q21 & q22',
                     'q23 & q24',
                     'q29 & q30',
                     'q31 & q32',
                     'q33',
                     'q34 & q35',
                     'q36','q37',
                     'q38',
                     'q39','q39_b',
                     'q40 & q41',
                     'q42','q42_b',
                     'q43','q43_b',
                     'q44','q44_b',
                     'q45',
                     'q82 & q83',
                     'q84 & q85',
                     fn.pst('q104',1:11),
                     fn.pst('q105',1:10),
                     'q106 & q107'),
   Management=c(paste('q',67:77,sep=''),
                      'q79 & q80'),
   Seasonal.patrn=c(fn.pst('q89',1:10),
                    fn.pst('q90',1:10),
                    'q91 & q92',
                    'q95',
                    'q97',
                    'q99',
                    'q100 & q102',
                    'q101 & q103'
                    ),
   Species_compo=c(fn.pst('q87',1:10),
                   fn.pst('q88',1:10),
                   fn.pst('q93',1:11),
                   fn.pst('q94',1:11),
                   fn.pst('q96',1:11),
                   fn.pst('q98',1:11)))

Q.list.Middle=list(General=c("market_name","location","estate",
                             'q1',
                             'q2 & q3 & q4_a & q4_b & q4_c',
                             'q5 & q6_a & q6_b',
                             'q7',
                             'q8',
                             fn.pst('q9',c(1:4,6:7)),
                             paste('q',10:13,sep=''),
                             fn.pst('q14',1:4),
                             'q15',
                             paste('q',26:31,sep='')
                             ),
                   Socio.economics=c(paste(c(fn.pst('q18_meat',1:4),
                                             fn.pst('q18_cartilage',1:2),
                                             fn.pst('q18_skin',1:2),
                                           'q18_gill_a',
                                           'q18_oil_a'),collapse=" & "),
                                      paste(c(fn.pst('q19_meat',1:4),
                                                 fn.pst('q19_cartilage',1:2),
                                                 fn.pst('q19_skin',1:2),
                                                 'q19_gill_a',
                                                 'q19_oil_a'),collapse=" & "),
                                      c('q21_meat & q21_cartilage & q21_skin & q21_gill & q21_oil'),
                                     fn.pst('q22',1:5),
                                     fn.pst('q23',1:5),
                                     fn.pst('q24',1:5),
                                     'q25'),
                   Management=c(paste('q',32:34,sep=''),
                                'q35 & q35_a',
                                'q36'),
                   Species_compo=c(fn.pst('q16',1:15),
                                   fn.pst('q17',1:15),
                                   c('q20_meat & q20_cartilage & q20_skin & q20_gill & q20_oil'))
                   )

#note: all these questions have only NAs so not included
# Q5_b	Q5_c	Q5_d	Q5_e
# Q6_cartilage_b	Q6_skin_c	Q6_gill_d	Q6_oil_e
# Q8
Q.list.Seller=list(General=c("village","location","estate","type_of_location",
                             'q1',
                             'q2','q3 & q3_a'),
                   Commercial=c('q4',fn.pst('q5',1),
                                'q6_meat_a',
                                paste('q',c(7,9:11),sep=''),fn.pst('q12',1:6),
                                paste('q',13:17,sep='')),
                   Consumption=c(paste('q',18:29,sep='')),
                   Management=c('q30','q31','q32',
                                'q33 & q33_b','q34 & q34_b'))


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
fn.fig=function(NAME,Width,Height) tiff(file=paste(NAME,".tiff",sep=""),width=Width,height=Height,units="px",res=300)
smart.par=function(n.plots,MAR,OMA,MGP) return(par(mfrow=n2mfrow(n.plots),mar=MAR,oma=OMA,las=1,mgp=MGP,xpd=T))
path='C:\\Matias\\FAO\\Shark_meat\\Outputs\\Mexico\\'


#Map of Study Area
theme_set(theme_bw()) # check other themes in ?ggtheme
world <- ne_countries(scale = "medium", returnclass = "sf")


sites.fisher=data.frame(Port=c('La Cruz de Huanacaxtle','Novilleros','Palmar de Cuautla',
                               'San Blas','Tamiahua','Tuxpan',
                               'Ensenada','Mazatlán'),
                        longitude=c(-105.39,-105.68,-105.66,
                                    -105.30,-97.45,-97.4,
                                    -116.8,-106.4),
                        latitude=c(20.74,22.38,22.22,
                                   21.54,21.28,20.96,
                                   31.86,23.23))
sites.middle=data.frame(MArket=c('Cooperativa','La nueva viga',
                                 ' La U','Mercado del mar',
                                 'Ensenada','Mazatlán',
                                 'Guadalajara'),
                        longitude=c(-105.2,-99.05,
                                    -104.85,-104.94,
                                    -116.4,-106.6,
                                    -103.35),
                        latitude=c(22.22,19.34,
                                   21.54,20.75,
                                   31.90,23.25,
                                   20.68))

sites.seller=data.frame(Village=c('Puerto Vallarta','Tamiahua',
                                  'Tuxpan',
                                  'Ensenada','Mazatlán',
                                  'León','Guadalajara',
                                  'La nueva viga'),
                        longitude=c(-105.22,-97.84,
                                    -97.80,
                                    -116.2,-106.2,
                                    -101.68,-103.15,
                                    -98.85),
                        latitude=c(20.65,21.27,
                                   20.96,
                                   31.80,23.21,
                                   21.12,20.64,
                                   19.32))

ggplot(data = world) +
  geom_sf(color = "grey20", fill = "grey85") +
  coord_sf(xlim = c(-118, -80), 
           ylim = c(15, 33), expand = T)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl",
                         which_north = "true", pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitude") + ylab("Latitude") +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"))+
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey40", size = 5) +
  annotate(geom = "text", x = -102.5, y = 24, label = "Mexico", 
           fontface = "italic", color = "grey40", size = 9,srt=-60) +
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

ggsave(paste(path,"map.png",sep=''),width = 10, height = 6, dpi = 300)


#Output date of interviews
Tab.dates=with(data.frame(group=c(rep("Fisher",length(Fisher$date)),
                   rep("Middle",length(Middle$date)),
                   rep("Seller",length(Seller$date))),
           date=as.character(c(Fisher$date,Middle$date,Seller$date))),
     table(group,date))
write.csv(Tab.dates,paste(path,"Interview.dates.csv",sep="\\"),row.names = T)

#Output full questions
write.csv(Fisher.ref,paste(path,"ref.Fisher.csv",sep="\\"),row.names = T)
write.csv(Middle.ref,paste(path,"ref.Middle.csv",sep="\\"),row.names = T)
write.csv(Seller.ref,paste(path,"ref.Seller.csv",sep="\\"),row.names = T)


# Difference between now and before Stacked likert-type histogram
Seq.brk=seq(-125,125,25)
colfunc.pos <- colorRampPalette(c("cadetblue2", "darkblue"))
colfunc.neg <- colorRampPalette(c("brown4","lightpink"))
fn.diff.before.now=function(before,now)
{
  delta=100*(now-before)/before
  delta=ifelse(delta<(-100),-125,ifelse(delta>100,125,delta))
  return(table(cut(delta,breaks=Seq.brk,include.lowest=T,right = F)))
}

Before.now.questions=list(q11_12=c('q12','q11'),
                          q13_14=c('q14','q13'),
                          q15_16=c('q16','q15'))
Mat=Before.now.questions
for(b in 1:length(Before.now.questions))
{
  before=Fisher%>%pull(Before.now.questions[[b]][1])
  now=Fisher%>%pull(Before.now.questions[[b]][2])
  Tab=fn.diff.before.now(before,now)
  Mat[[b]]=Tab
}
Mat=do.call(cbind,Mat)

Lgn=read.pattern(text = names(Tab), pattern = ".(.+),(.+).", 
                 col.names = c("lower", "upper"))%>%
  mutate(rango=paste('[',paste(paste(lower,'%',sep=''),
                               paste(upper,'%',sep=''),sep=','),
                     ']',sep=''))%>%
  pull(rango)
Lgn[c(1,length(Lgn))]=c('> -100%','> 100%')  

NN=(length(Seq.brk)-1)/2
Col.pos=colfunc.pos(NN)
Col.neg=colfunc.neg(NN)

fn.fig(paste(path,"Figure 2. Percent difference now before",sep=""),2400,1600)
par(mar=c(1,2.6,2,.2),oma=c(.1,3,2,.4),cex.axis=1.25,xpd=T,las=1,mgp=c(1,.5,0))
barplot(Mat,horiz = T,
        axes = F,col = c(Col.neg,Col.pos))
legend('topleft',Lgn[1:NN],horiz=T,inset = c(-.1,-.095),
       fill=Col.neg,cex=1.14,bty='n',x.intersp=.5)
 legend('topleft',Lgn[(NN+1):length(Lgn)],horiz=T,inset = c(-.1,-.05),
        fill=Col.pos,cex=1.14,bty='n',x.intersp=0.5)
dev.off()

#Exploratory
if(do.exploratory)
{
  #Fisher
  for(i in 1:length(Q.list.Fisher))
  {
    fn.fig(paste(path,"Exploratory\\Fisher_",i,"_",names(Q.list.Fisher)[i],sep=""),2000,2400)
    smart.par(length(Q.list.Fisher[[i]]),MAR=c(3,2,3.9,.5),OMA=c(.1,1.25,.2,.1),
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
    fn.fig(paste(path,"Exploratory\\Middle_",i,"_",names(Q.list.Middle)[i],sep=""),2000,2400)
    smart.par(length(Q.list.Middle[[i]]),MAR=c(3,2,3.9,.5),OMA=c(.1,1.25,.2,.1),
              MGP=c(1.65,.2,0))
    par(tck=.025,cex.axis=1.25,cex.lab=1.5)
    for(n in 1:length(Q.list.Middle[[i]]))
    {
      this=Q.list.Middle[[i]][n]
      if(grepl("&",this))
      {
        this=unlist(strsplit(this," & "))
        dummy=Middle%>%select(this)
        dummy$Currently=rep('Currently',nrow(dummy))
        dummy$Before=rep('Before',nrow(dummy))
        dummy=data.frame(var=c(dummy[,1],dummy[,2]),
                         period=c(dummy$Currently,dummy$Before))
        MAT=table(dummy$var,dummy$period)
      }else
      {
        dummy=Middle%>%select(this)%>%pull
        MAT=table(dummy)
      }
      
      
      fn.brplt(MAT=MAT,XLAB=Q.list.Middle[[i]][n])
    }
    mtext("Frequency",2,line=-.25,outer=T,las=3,cex=1.35)
    dev.off()
  }
  
  #Seller
  for(i in 1:length(Q.list.Seller))
  {
    fn.fig(paste(path,"Exploratory\\Seller_",i,"_",names(Q.list.Seller)[i],sep=""),2000,2400)
    smart.par(length(Q.list.Seller[[i]]),MAR=c(3,2,3.9,.5),OMA=c(.1,1.25,.2,.1),
              MGP=c(1.65,.2,0))
    par(tck=.025,cex.axis=1.25,cex.lab=1.5)
    for(n in 1:length(Q.list.Seller[[i]]))
    {
      this=Q.list.Seller[[i]][n]
      if(grepl("&",this))
      {
        this=unlist(strsplit(this," & "))
        dummy=Seller%>%select(this)
        dummy$Currently=rep('Currently',nrow(dummy))
        dummy$Before=rep('Before',nrow(dummy))
        dummy=data.frame(var=c(dummy[,1],dummy[,2]),
                         period=c(dummy$Currently,dummy$Before))
        MAT=table(dummy$var,dummy$period)
      }else
      {
        dummy=Seller%>%select(this)%>%pull
        MAT=table(dummy)
      }
      fn.brplt(MAT=MAT,XLAB=Q.list.Seller[[i]][n])
    }
    mtext("Frequency",2,line=-.25,outer=T,las=3,cex=1.35)
    dev.off()
  }
  
}
