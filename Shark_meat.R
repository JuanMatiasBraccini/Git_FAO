# Script for analysing FAO non-fin product project
#MISSING: 1. For Fisher, can compare 'Industrial' and 'Artisanal' with contingency tables 


library(readxl)
library(tidyverse)
library(lubridate)
library("ggplot2")
library("sf")
library("rnaturalearthdata")
library("rnaturalearth")
library("ggspatial")  #for scale bar
library(gsubfn)
library(stargazer)
library(MASS)
library(nnet)

do.exploratory=FALSE

# Section 1. Import Data --------------------------------------------------
path='C:\\Matias\\FAO\\Shark_meat\\data\\Mexico\\'

  #Questionnaire data
Fisher <- read_excel(paste(path,"Base final corregida 24enero19.xlsx",sep=""), sheet = "Fisher survey ")
Middle <- read_excel(paste(path,"Base final corregida 24enero19.xlsx",sep=""), sheet = "Middle survey")
Seller <- read_excel(paste(path,"Base final corregida 24enero19.xlsx",sep=""), sheet = "Seller survey")

  #Reference guide
Fisher.ref <- read_excel(paste(path,"NFC database questions guide 17 enero.xlsx",sep=""), sheet = "G.reference (Fisher)")
Middle.ref <- read_excel(paste(path,"NFC database questions guide 17 enero.xlsx",sep=""), sheet = "G.reference (middle)")
Seller.ref <- read_excel(paste(path,"NFC database questions guide 17 enero.xlsx",sep=""), sheet = "G.reference (seller)")
Species <- read_excel(paste(path,"guide base.xlsx",sep=""), sheet = "Guide sp")


# Section 2. Manipulate Data --------------------------------------------------
  #2.1. tidy up data
Fisher <- Fisher%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))%>%
          rename(surveys=!!"survey number")%>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns
       
Middle <- Middle%>%as.data.frame%>%
          rename_all(tolower)%>%
          mutate(year=year(date),
                 month=month(date))%>%
          rename(surveys=!!"survey number")%>%
          filter(!is.na(surveys)) %>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns
if("q1_d" %in% colnames(Middle)) colnames(Middle)[match("q1_d",colnames(Middle))] <- "q14_d"

Seller <- Seller%>%as.data.frame%>%
          rename_all(tolower)%>%
            mutate(year=year(date),
                   month=month(date))%>%
          rename(surveys=!!"survey number")%>%
          select_if(~sum(!is.na(.)) > 0)   #remove all NA columns

#convert all character to lower to avoid lower and upper case   
for(n in 1:ncol(Fisher)) if(is.character(Fisher[,n])) Fisher[,n]=tolower(Fisher[,n])
for(n in 1:ncol(Middle)) if(is.character(Middle[,n])) Middle[,n]=tolower(Middle[,n])
for(n in 1:ncol(Seller)) if(is.character(Seller[,n])) Seller[,n]=tolower(Seller[,n])

Fisher.ref <- Fisher.ref%>%as.data.frame%>%
              mutate(Question=tolower(Question))%>%
              rename('Question number'=Question,
                      Question=Question_long)%>%
              dplyr::select(-c(Section,Combo))

Middle.ref <- Middle.ref%>%as.data.frame%>%
                  mutate(Question=tolower(Question))%>%
                  rename('Question number'=Question,
                         Question='Question long')%>%
              dplyr::select(-c(Section,Combo))

Seller.ref <- Seller.ref%>%as.data.frame%>%
                  mutate(Question=tolower(Question))%>%
                  rename('Question number'=Question,
                         Question='Question long')%>%
              dplyr::select(-c(Section,Combo))

fn.pst1=function(Q,rng) paste(paste(Q,tolower(LETTERS[rng]),sep='_'),sep=",")

#Fix columns that should be numeric
Fisher.to.numeric=c(paste('q',c(15:28,31:32,46:49,52:55,57:58,61:64,70,83:84),sep=''),
    fn.pst1('q92',1:11),fn.pst1('q93',1:11),
    fn.pst1('q95',1:11),fn.pst1('q97',1:11),
    fn.pst1('q103',1:11),fn.pst1('q104',1:11)) 
for(f in 1:length(Fisher.to.numeric))
{
  id=match(Fisher.to.numeric[f],names(Fisher))
  if(Fisher.to.numeric[f]%in%c(fn.pst1('q103',1:11),fn.pst1('q104',1:11)))
  {
    Fisher[,id]=ifelse(Fisher[,id]=="45-50","47.5",
                ifelse(Fisher[,id]=="50-60","55",
                Fisher[,id]))
  }
  suppressWarnings({Fisher[,id]=as.numeric(Fisher[,id])})
}

Middle.to.numeric=c(paste('q',c(28),sep=''),
        fn.pst1('q6',2),fn.pst1('q9',c(1:4,6:7)),
        fn.pst1('q18_meat',1:4),fn.pst1('q18_cartilage',1:2),
        fn.pst1('q18_skin',1:2),fn.pst1('q18_gill',1),
        fn.pst1('q18_oil',1),
        fn.pst1('q19_meat',1:4),fn.pst1('q19_cartilage',1:2),
        fn.pst1('q19_skin',1:2),fn.pst1('q19_gill',1),
        fn.pst1('q19_oil',1),
        fn.pst1('q22',1:5),fn.pst1('q23',1:5),
        fn.pst1('q24',1:5)) 
for(f in 1:length(Middle.to.numeric))
{
  id=match(Middle.to.numeric[f],names(Middle))
  suppressWarnings({Middle[,id]=as.numeric(Middle[,id])})
}
Seller.to.numeric=c(paste('q',c(11,14,17),sep='')) 
for(f in 1:length(Seller.to.numeric))
{
  id=match(Seller.to.numeric[f],names(Seller))
  suppressWarnings({Seller[,id]=as.numeric(Seller[,id])})
}

#Fix some dodgy stuff
Fisher=Fisher%>%
  mutate(q5=ifelse(q5=='crustecean','crustacean',q5),
         q66=ifelse(q66=='nana','na',q66),
         q59=ifelse(q59=='longlines_bottom_nets','longline_bottom_nets',q59),
         q60=ifelse(q60=='longlines_bottom_nets','longline_bottom_nets',q60),
         q33=ifelse(q33=='a_little_more','little_more_expensive',q33),
         q42_a=ifelse(q42_a=='price_flavor_','price_flavor',q42_a),
         q44_a=ifelse(q44_a%in%c('devalauted','devaluated'),'devalued',q44_a))

Middle=Middle%>%
  mutate(q12=ifelse(q12=='cutting_fins_','cutting_fins',
             ifelse(q12=='filleted,framed_frozen','filleted_framed_frozen',q12))) 
 

# Section 3. Grouping of columns --------------------------------------------------

#3.1__FISHER:
  #3.1.1. Just mention in Text (export as appendix table):
Tabl.appn=list(TableFisher.2_general=c('locality & state',
                         'q1','q3','q4'),
               TableFisher.3_month.ktch=c('q94 & q96','q98','q99 & q101','q100 & q102'),
               TableFisher.4_size.ktch=c('q95_a & q97_a','q95_c & q97_c','q95_e & q97_e'), #other letters have all NAs in q97
               TableFisher.5_pups=c('q90 & q91'),
               TableFisher.6_management=c('q67','q68','q69','q70','q71',
                            'q72','q73','q74','q75','q76',
                            'q77','q78','q79'),
               TableFisher.7_economics=c('q80_a & q80_b & q80_c',
                           'q81 & q82','q83 & q84','q105 & q106'),
               TableFisher.8_climate.change=c('q85','q85_a'))


  #3.1.2. Table in body text:
Tabl.body=list(TableFisher.1_Target.ktch.now.before='q5 & q6',
               TableFisher.2_Sekndry.ktch.now.before=
                 c('q7','q8','q9','q10'),
               TableFisher.3_Target.ktch.non_shark.now.before=
                 c('q56','q65 & q66'),
               TableFisher.4_Fishing_methods= 'q59 & q60',
               TableFisher.5_Effort='q50 & q51',
               TableFisher.6_Economics=
                 c('q33', 'q34 & q35','q29 & q30',
                   'q36','q37','q38','q39 & q39_a',
                   'q40 & q41', 'q42 & q42_a','q43 & q43_a',
                   'q44 & q44_a','q45'))


Table.Before.now.species.landings=list(before=fn.pst1('q92',1:11),  
                                       now=fn.pst1('q93',1:11))

Table.Before.now.species.economics=list(before=fn.pst1('q104',c(1:5,7:9)),  
                                       now=fn.pst1('q103',c(1:5,7:9)))   #_k,_j,_f all NAs

  #3.1.3. Figure in body text:
Before.now.questions.fisher=list(
          #catch
          q15_16=c(before='q16',now='q15'),
          q17_18=c(before='q18',now='q17'),
          q19_20=c(before='q20',now='q19'),
          q25_26=c(before='q26',now='q25'),
          q57_58=c(before='q58',now='q57'),
          
          #effort
          q11_12=c(before='q12',now='q11'),
          q13_14=c(before='q14',now='q13'),
          q46_47=c(before='q47',now='q46'),
          q48_49=c(before='q49',now='q48'),
          q52_53=c(before='q53',now='q52'),
          q54_55=c(before='q55',now='q54'),
          q61_62=c(before='q62',now='q61'),
          q63_64=c(before='q64',now='q63'),
          
          #economics
          q21_22=c(before='q22',now='q21'),
          q23_24=c(before='q24',now='q23'),
          q31_32=c(before='q32',now='q31'))

Before.now.species.comp=list(abundance=fn.pst1('q86',1:10), 
                             size=fn.pst1('q87',1:10),
                             month.now=fn.pst1('q88',1:10),
                             month.before=fn.pst1('q89',1:10))


  #3.1.4. Figure in Apendices:
Fig.appn=list(yrs.in.fishery='q2') 


#3.2__MIDDLE:
#3.2.1. Just mention in Text (export as appendix table):
Tabl.appn.middle=list(TableMiddle.1_general=
                        c('market_name & locality & state'),
                      TableMiddle.2_collection=
                        c('q2 & q3',
                          'q4_a & q4_b & q4_c',
                          'q5 & q6_a',
                          'q15','q27'),
                      TableMiddle.2_sale=c('q14',
                                           'q25'))


#3.2.2. Table in body text:
Tabl.body.middle=list(TableMiddle.1_Product.location=
                        c('q7'),
                      TableMiddle.2_Product.parts.bought=
                        c('q8'),
                      TableMiddle.3_Product.condition=
                        c('q10'),
                      TableMiddle.4_Product.average.quality=
                        c('q11'),
                      TableMiddle.5_Product.service=
                        c('q12'),
                      TableMiddle.6_Product.process=
                        c('q13'),
                      TableMiddle.7_Buyer.at.beginning=c('q29 & q31'),
                      TableMiddle.8_Management=c('q32 & q33 & q34 & q35 & q35_a & q36'))

Table.Avrg.bought=list(c('q9_b','q9_c','q9_g','q9_d'))
Table.Price.sold=list(colnames(Middle)[grep('18',colnames(Middle))])
Table.Arg.sale.per.month=list(colnames(Middle)[grep('19',colnames(Middle))])
Table.Percent.increase.in.price=list(colnames(Middle)[grep('22',colnames(Middle))])
Table.Percent.decrease.in.price=list(colnames(Middle)[grep('23',colnames(Middle))])
Table.Percent.stable.in.price=list(colnames(Middle)[grep('24',colnames(Middle))])


#3.2.3. Figure in body text:
Before.now.species.comp.middle=list(
      abundance=colnames(Middle)[grep('16',colnames(Middle))],
      size=colnames(Middle)[grep('17',colnames(Middle))],
      product.availability=colnames(Middle)[grep('20',colnames(Middle))],
      price.change=colnames(Middle)[grep('21',colnames(Middle))])


#3.2.4. Figure in Apendices:
Fig.appn.middle=list(yrs.as.collector='q1',
                     yrs.parents.collector='q6_b',
                     first.time.collecting='q26',
                     Number.collectors.first.time='q28',
                     Number.collectors.now.in.village='q30') 


#3.3__SELLER:

#3.3.1. Just mention in Text (export as appendix table):
Tabl.appn.seller=list(TableSeller.1_general=
                        c('q2 & q3 & q3_a'),
                      TableSeller.2_percent.prod.increase=
                        c('q8'),
                      TableSeller.3_volume.sold.week=
                        c('q11'),
                      TableSeller.4_species.trends=
                        c('q13 & q14 & q15 & q16 & q17'),
                      TableSeller.5_why.sell.shark=
                        c('q20'),
                      TableSeller.6_business.interference=
                        c('q21'),
                      TableSeller.7_what.if.less.profit=
                        c('q22'),
                      TableSeller.8_history.shark.use=
                        c('q23'),
                      TableSeller.9_first.time.shark.consumption.and.what=
                        c('q24 & q25'),
                      TableSeller.10_first.market.sold=
                        c('q26'))
#3.3.2. Table in body text:
Tabl.body.seller=list(TableSeller.1_Product.source.from=
                        c('q4'),
                      TableSeller.2_Product.monthly.volume=
                        c('q5_a'),
                      TableSeller.3_Product.condition=
                        c('q6_meat_a'),
                      TableSeller.4_Product.service=
                        c('q7'),
                      TableSeller.5_Product.sold=
                        c('q9'),
                      TableSeller.6_Product.destination=
                        c('q10'),
                      TableSeller.7_Product.buy.from.why=
                        c('q18 & q19'),
                      TableSeller.8_Consumption.trend=
                        c('q28'),
                      TableSeller.9_Management=
                        c('q30 & q31 & q32 & q33 & q33_b & q34 & q34_b'))

#3.3.3. Figure in body text:
Before.now.species.comp.seller=list(
  abundance=colnames(Seller)[grep('12',colnames(Seller))],
  consumption=c('q27', 'q29'))

#3.3.4. Figure in Apendices:
Fig.appn.seller=list(yrs.as.seller='q1')


# Section 4. Outputs --------------------------------------------------
fn.fig=function(NAME,Width,Height) tiff(file=paste(NAME,".tiff",sep=""),
              width=Width,height=Height,units="px",res=300,compression="lzw")
smart.par=function(n.plots,MAR,OMA,MGP) return(par(mfrow=n2mfrow(n.plots),mar=MAR,oma=OMA,las=1,mgp=MGP,xpd=T))
path='C:\\Matias\\FAO\\Shark_meat\\Outputs\\Mexico\\'

#4.1 Map of Study Area
do.Map=FALSE
if(do.Map)
{
  theme_set(theme_bw()) 
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
  
  ggsave(paste(path,"Figure 1.map.tiff",sep=''),width = 10, height = 6, dpi = 300)
}

#4.2 Output full questions
write.csv(Fisher.ref,paste(path,"Full.questionnaire\\ref.Fisher.csv",sep="\\"),row.names = T)
write.csv(Middle.ref,paste(path,"Full.questionnaire\\ref.Middle.csv",sep="\\"),row.names = T)
write.csv(Seller.ref,paste(path,"Full.questionnaire\\ref.Seller.csv",sep="\\"),row.names = T)


#date of interviews for all groups
Tab.dates=with(data.frame(group=c(rep("Fisher",length(Fisher$date)),
                                  rep("Middle",length(Middle$date)),
                                  rep("Seller",length(Seller$date))),
                          date=as.character(c(Fisher$date,Middle$date,Seller$date))),
               table(group,date))
write.csv(Tab.dates,paste(path,"Appendix\\TableFisher.1.Interview.dates.csv",sep="\\"),row.names = T)


#4.3. Fisher 
  #...Figures
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
Mat=Before.now.questions.fisher
for(b in 1:length(Before.now.questions.fisher))
{
  before=Fisher%>%pull(Before.now.questions.fisher[[b]][1])
  now=Fisher%>%pull(Before.now.questions.fisher[[b]][2])
  if(!is.numeric(before)) before=as.numeric(before)
  if(!is.numeric(now)) now=as.numeric(now)
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
fn.fig(paste(path,"Figure Fisher 2. Percent difference now before",sep=""),2400,1600)
par(mar=c(1,2.6,2,.2),oma=c(.1,3,2,.4),cex.axis=1.25,xpd=T,las=1,mgp=c(1,.5,0))
barplot(Mat,horiz = T,
        axes = F,col = c(Col.neg,Col.pos))
legend('topleft',Lgn[1:NN],horiz=T,inset = c(-.1,-.095),
       fill=Col.neg,cex=1.14,bty='n',x.intersp=.5)
 legend('topleft',Lgn[(NN+1):length(Lgn)],horiz=T,inset = c(-.1,-.05),
        fill=Col.pos,cex=1.14,bty='n',x.intersp=0.5)
dev.off()

    # Changes in species thru time
fn.fig(paste(path,"Figure Fisher 3. Species changes",sep=""),2400,2000)
par(mfrow=c(1,length(Before.now.species.comp)),mar=c(1,2.6,.1,2.5),
    oma=c(.1,3,.1,.4),cex.axis=.95,xpd=T,las=1,mgp=c(1,.1,0))
for(b in 1:length(Before.now.species.comp))
{
  Tab=Fisher%>%dplyr::select(Before.now.species.comp[[b]])
  Levls=unique(unlist(Tab))
  Levls=Levls[!is.na(Levls)]
  Tab=mutate_all(Tab,factor,Levls)
  Tab1=vector('list',ncol(Tab))
  names(Tab1)=names(Tab)
  for(tt in 1:length(Tab1)) Tab1[[tt]]=table(Tab[,tt])
  Mat=do.call(cbind,Tab1)
  CL=1:nrow(Mat)
  colnames(Mat)=paste(colnames(Mat),paste('(n=',colSums(Mat),')',sep=''))
  barplot(Mat,horiz = T,axes = F,col=CL)
  legend('top',rownames(Mat),box.col="white",
         cex=.85,fill=CL, inset=c(0,0.05))
}
dev.off()

    # Appendix figure
fn.fig(paste(path,"Appendix\\Fisher q2. Years in the fishery",sep=""),2400,2000)
hist(Fisher%>%pull(Fig.appn$yrs.in.fishery),breaks=seq(0,60,5),
     xlab="Years in the fishery",main='',col=2,cex.axis=1.25,cex.lab=1.5)
dev.off()


  #...Tables
fn.tbl=function(d,where)    
{
  for(i in 1:length(d))
  {
    for(nn in 1:length(d[[i]]))
    {
      this=d[[i]][nn]
      if(grepl("&",this))
      {
        this=unlist(strsplit(this," & "))
        dummy=Fisher%>%dplyr::select(this)
        Dat=data.frame(period=rep(this,each=nrow(dummy)),
                       var=unlist(dummy))
        MAT=with(Dat,table(var,period,useNA = 'ifany'))
        write.csv(MAT,paste(path,where,names(d)[i],"_",d[[i]][nn],".csv",sep=""))
        
      }else
      {
        dummy=Fisher%>%dplyr::select(this)
        MAT=table(dummy,useNA = 'ifany')
        write.csv(MAT,paste(path,where,names(d)[i],"_",this,".csv",sep=""),row.names=F)
      }
    }
   }
}
    #appendix tables
fn.tbl(d=Tabl.appn,where="Appendix\\") 

    #main body tables
fn.tbl(d=Tabl.body,where="")     

    #Summary number and prices landed now and before 
# note: too small a sample for significance test
fn.tabl.n.land.now.before=function(d)
{
  Tab=Fisher%>%dplyr::select(d)
  Tab1=vector('list',ncol(Tab))
  names(Tab1)=names(Tab)
  for(tt in 1:length(Tab1))
  {
    v=Tab[,tt]
    if(is.character(v))
    {
      v.char=v[grep("_",v)]
      v.char=as.numeric(unlist(str_split(v.char, "_")))
      v=as.numeric(v[-grep("_",v)])
      v=c(v,v.char)
    }
    Tab1[[tt]]=summary(v) 
    
  }
    
  return(do.call(rbind,Tab1))
}
bef=fn.tabl.n.land.now.before(d=Table.Before.now.species.landings$before)
nw=fn.tabl.n.land.now.before(d=Table.Before.now.species.landings$now)
out=cbind(t(bef),t(nw))
out.id=sapply(strsplit(colnames(out), split='_', fixed=TRUE), function(x) (x[2]))
colnames(out)=paste(out.id,colnames(out),sep=".")
out=out%>%data.frame%>%dplyr::select(sort(names(.)))
colnames(out)=sapply(strsplit(colnames(out), split='.', fixed=TRUE), function(x) (x[2]))
write.csv(out,paste(path,"Appendix\\Fisher.Summary_Number.landed.now.before.csv",sep=""),row.names = T)


bef=fn.tabl.n.land.now.before(d=Table.Before.now.species.economics$before)
nw=fn.tabl.n.land.now.before(d=Table.Before.now.species.economics$now)
out=cbind(t(bef),t(nw))
out.id=sapply(strsplit(colnames(out), split='_', fixed=TRUE), function(x) (x[2]))
colnames(out)=paste(out.id,colnames(out),sep=".")
out=out%>%data.frame%>%dplyr::select(sort(names(.)))
colnames(out)=sapply(strsplit(colnames(out), split='.', fixed=TRUE), function(x) (x[2]))
write.csv(out,paste(path,"Appendix\\Fisher.Summary_Price.now.before.csv",sep=""),row.names = T)



#4.4. Middle 

  #...Figures 
    # Changes in species thru time
fn.fig(paste(path,"Figure Middle 1. Species changes",sep=""),2400,2000)
par(mfrow=c(1,length(Before.now.species.comp.middle)),mar=c(1,2.6,.1,2.5),
    oma=c(.1,3,.1,.4),cex.axis=.95,xpd=T,las=1,mgp=c(1,.1,0))
for(b in 1:length(Before.now.species.comp.middle))
{
  Tab=Middle%>%dplyr::select(Before.now.species.comp.middle[[b]])
  Levls=unique(unlist(Tab))
  Levls=Levls[!is.na(Levls)]
  Tab=mutate_all(Tab,factor,Levls)
  Tab1=vector('list',ncol(Tab))
  names(Tab1)=names(Tab)
  for(tt in 1:length(Tab1)) Tab1[[tt]]=table(Tab[,tt])
  Mat=do.call(cbind,Tab1)
  CL=1:nrow(Mat)
  colnames(Mat)=paste(colnames(Mat),paste('(n=',colSums(Mat),')',sep=''))
  barplot(Mat,horiz = T,axes = F,col=CL)
  legend('bottomright',rownames(Mat),
         cex=1,fill=CL,bty='n')
}
dev.off()

    # Appendix figure
fn.fig(paste(path,"Appendix\\Middle q1. Years as a collector",sep=""),2400,2000)
hist(Middle%>%pull(Fig.appn.middle$yrs.as.collector),breaks=seq(0,60,5),
     xlab="Years as a collector",main='',col=2,cex.axis=1.25,cex.lab=1.5)
dev.off()

fn.fig(paste(path,"Appendix\\Middle q6_b. Years parents as collectors",sep=""),2400,2000)
hist(Middle%>%pull(Fig.appn.middle$yrs.parents.collector),breaks=seq(0,60,5),
     xlab="Years parents as a collector",main='',col=2,cex.axis=1.25,cex.lab=1.5)
dev.off()

fn.fig(paste(path,"Appendix\\Middle q26. First time collecting in village",sep=""),2400,2000)
hist(Middle%>%pull(Fig.appn.middle$first.time.collecting),breaks=seq(0,60,5),
     xlab="Years first collection",main='',col=2,cex.axis=1.25,cex.lab=1.5)
dev.off()

fn.fig(paste(path,"Appendix\\Middle q28. Number.collectors.first.time",sep=""),2400,2000)
hist(Middle%>%pull(Fig.appn.middle$Number.collectors.first.time),breaks=seq(0,200,5),
     xlab="Initial number of collectors",main='',col=2,cex.axis=1.25,cex.lab=1.5)
dev.off()

fn.fig(paste(path,"Appendix\\Middle q30. Number.collectors.now.in.village",sep=""),2400,2000)
hist(Middle%>%pull(Fig.appn.middle$Number.collectors.now.in.village),breaks=seq(0,100,5),
     xlab="Number of collectors now",main='',col=2,cex.axis=1.25,cex.lab=1.5)
dev.off()


  #...Tables  
fn.tbl.middle=function(d,where) 
{
  for(i in 1:length(d))
  {
    for(nn in 1:length(d[[i]]))
    {
      this=d[[i]][nn]
      if(grepl("&",this))
      {
        this=unlist(strsplit(this," & "))
        dummy=Middle%>%dplyr::select(this)
        Dat=data.frame(period=rep(this,each=nrow(dummy)),
                       var=unlist(dummy))
        MAT=with(Dat,table(var,period,useNA = 'ifany'))
        write.csv(MAT,paste(path,where,names(d)[i],"_",d[[i]][nn],".csv",sep=""))
        
      }else
      {
        dummy=Middle%>%dplyr::select(this)
        MAT=table(dummy,useNA = 'ifany')
        write.csv(MAT,paste(path,where,names(d)[i],"_",this,".csv",sep=""),row.names=F)
      }
    }
  }
}
    #appendix tables
fn.tbl.middle(d=Tabl.appn.middle,where="Appendix\\") 

    #main body tables
fn.tbl.middle(d=Tabl.body.middle,where="")  

    #Summary 
fn.tabl.n.summary.middle=function(d)
{
  Tab=Middle%>%dplyr::select(d)
  Tab1=vector('list',ncol(Tab))
  names(Tab1)=names(Tab)
  my_summary <- function(v){
    if(!any(is.na(v))){
      res <- c(summary(v),"NA's"=0)
    } else{
      if(is.character(v))
      {
        v.char=v[grep("_",v)]
        v.char=as.numeric(unlist(str_split(v.char, "_")))
        v=as.numeric(v[-grep("_",v)])
        v=c(v,v.char)
      }
      
      res <- summary(v)
    }
    return(res)
  }
  for(tt in 1:length(Tab1))
  {
    dum.smry=my_summary(Tab[,tt])
    Tab1[[tt]]=dum.smry
  }
    
  return(do.call(rbind,Tab1))
}
out=t(fn.tabl.n.summary.middle(d=Table.Avrg.bought[[1]]))
write.csv(out,paste(path,"Appendix\\Middle.Summary_Average.shark.ray.bought.csv",sep=""),row.names = T)

out=t(fn.tabl.n.summary.middle(d=Table.Price.sold[[1]]))
write.csv(out,paste(path,"Appendix\\Middle.Summary_Price.sold.csv",sep=""),row.names = T)

out=t(fn.tabl.n.summary.middle(d=Table.Arg.sale.per.month[[1]]))
write.csv(out,paste(path,"Appendix\\Middle.Summary_Average.sale.per.month.csv",sep=""),row.names = T)

out=t(fn.tabl.n.summary.middle(d=Table.Percent.increase.in.price[[1]]))
write.csv(out,paste(path,"Appendix\\Middle.Summary_Percent.increase.in.price.csv",sep=""),row.names = T)

out=t(fn.tabl.n.summary.middle(d=Table.Percent.decrease.in.price[[1]]))
write.csv(out,paste(path,"Appendix\\Middle.Summary_Percent.decrease.in.price.csv",sep=""),row.names = T)

out=t(fn.tabl.n.summary.middle(d=Table.Percent.stable.in.price[[1]]))
write.csv(out,paste(path,"Appendix\\Middle.Summary_Percent.stable.in.price.csv",sep=""),row.names = T)


#4.5. Seller 

  #...Figures 
    # Changes in species thru time
# Changes in species thru time
fn.fig(paste(path,"Figure Seller 1. Species changes",sep=""),2400,2000)
par(mfrow=c(1,length(Before.now.species.comp.seller)),mar=c(1,2.6,.1,2.5),
    oma=c(.1,3,.1,.4),cex.axis=.95,xpd=T,las=1,mgp=c(1,.1,0))
for(b in 1:length(Before.now.species.comp.seller))
{
  Tab=Seller%>%dplyr::select(Before.now.species.comp.seller[[b]])
  Levls=unique(unlist(Tab))
  Levls=Levls[!is.na(Levls)]
  Tab=mutate_all(Tab,factor,Levls)
  Tab1=vector('list',ncol(Tab))
  names(Tab1)=names(Tab)
  for(tt in 1:length(Tab1)) Tab1[[tt]]=table(Tab[,tt])
  Mat=do.call(cbind,Tab1)
  CL=1:nrow(Mat)
  colnames(Mat)=paste(colnames(Mat),paste('(n=',colSums(Mat),')',sep=''))
  barplot(Mat,horiz = T,axes = F,col=CL)
  legend('bottomright',rownames(Mat),
         cex=1,fill=CL,bty='n')
}
dev.off()

    # Appendix figure
fn.fig(paste(path,"Appendix\\Seller q1. Years as a seller",sep=""),2400,2000)
hist(Seller%>%pull(Fig.appn.seller$yrs.as.seller),breaks=seq(0,35,5),
     xlab="Years as a seller",main='',col=2,cex.axis=1.25,cex.lab=1.5)
dev.off()


  #...Tables  
fn.tbl.seller=function(d,where)    
{
  for(i in 1:length(d))
  {
    for(nn in 1:length(d[[i]]))
    {
      this=d[[i]][nn]
      if(grepl("&",this))
      {
        this=unlist(strsplit(this," & "))
        dummy=Seller%>%dplyr::select(this)
        Dat=data.frame(period=rep(this,each=nrow(dummy)),
                       var=unlist(dummy))
        MAT=with(Dat,table(var,period,useNA = 'ifany'))
        write.csv(MAT,paste(path,where,names(d)[i],"_",d[[i]][nn],".csv",sep=""))
        
      }else
      {
        dummy=Seller%>%dplyr::select(this)
        MAT=table(dummy,useNA = 'ifany')
        write.csv(MAT,paste(path,where,names(d)[i],"_",this,".csv",sep=""),row.names=F)
      }
    }
  }
}
    #appendix tables
fn.tbl.seller(d=Tabl.appn.seller,where="Appendix\\") 

    #main body tables
fn.tbl.seller(d=Tabl.body.seller,where="")  


#ACA Test for differences between artisanal and industrial
#yes/no responses
fnlogit=function(Response,Dat)
{
  if(length(Response)==1)
  {
    Dat[,Response]=factor(Dat[,Response])
    Dat[,'q3']=factor(Dat[,'q3'])
    Formula=as.formula(paste(Response,'q3',sep='~'))
  }else
  {
    D=Dat%>%dplyr::select(c(!!Response,q3))
    Dat=data.frame(Response=c(D[,1],D[,2]),
                   Time=c(rep('now',length(D[,1])),rep('then',length(D[,2]))),
                   q3=c(D[,3],D[,3]))
    Formula=as.formula(paste('Response',paste('q3','Time',sep='+'),sep='~'))
  }
  mod <- glm(Formula, family=binomial(link="logit"),data=Dat)
  return(mod)
}

#ordered categorical responses
fn.ordinal.logit=function(Response,Dat)
{
  if(length(Response)==1)
  {
    Dat[,Response]=factor(Dat[,Response])
    Dat[,'q3']=factor(Dat[,'q3'])
    Formula=as.formula(paste(Response,'q3',sep='~'))
  }else
  {
    D=Dat%>%dplyr::select(c(!!Response,q3))
    Dat=data.frame(Response=c(D[,1],D[,2]),
                   Time=c(rep('now',length(D[,1])),rep('then',length(D[,2]))),
                   q3=c(D[,3],D[,3]))
    Formula=as.formula(paste('Response',paste('q3','Time',sep='+'),sep='~'))
  }
  mod <- polr(Formula, data=Dat, Hess=F)
  return(mod)
}

#un-ordered categorical responses
fn.un_ordinal.logit=function(Response,Dat)
{
  if(length(Response)==1)
  {
    Dat[,Response]=factor(Dat[,Response])
    Dat[,'q3']=factor(Dat[,'q3'])
    Formula=as.formula(paste(Response,'q3',sep='~'))
  }else
  {
    D=Dat%>%dplyr::select(c(!!Response,q3))
    Dat=data.frame(Response=c(D[,1],D[,2]),
                   Time=c(rep('now',length(D[,1])),rep('then',length(D[,2]))),
                   q3=c(D[,3],D[,3]))
    Formula=as.formula(paste('Response',paste('q3','Time',sep='+'),sep='~'))
  }
  mod <- multinom(Formula, data=Dat)
  return(mod)
}

#Aca select what columns to analyse and of what type they are: yes/no,ordered, un-ordered,

fnlogit(Response='qx',Dat=Fisher)
fn.ordinal.logit(Response=c('q5','q6'),Dat=Fisher)

#stargazer(mod, type="html", out="logit.htm")
#stargazer(mod,type = "text")

#####
M=ftable(Fisher$q3,Fisher$q6)
barplot(M)
M


# Section 5. Exploratory --------------------------------------------------
if(do.exploratory)
{
  fn.pst=function(Q,rng) paste(paste(Q,tolower(LETTERS[rng]),sep='_'),collapse=" & ")
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
  
  # Define how to group and summarise each question
  
  #note: the '&' groups the same question 'now' and 'before'
  
  #Fisher
  Q.list.Fisher=list(
    General=c('locality','state'),
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
                      'q39','q39_a',
                      'q40 & q41',
                      'q42','q42_a',
                      'q43','q43_a',
                      'q44','q44_a',
                      'q45',
                      'q82 & q83',
                      'q84 & q85',
                      fn.pst('q104',1:11),
                      fn.pst('q105',1:10),
                      'q106 & q107'),
    Management=c(paste('q',67:77,sep=''),
                 'q79'),
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
        dummy=Fisher%>%dplyr::select(this)
        dummy$Currently=rep('Currently',nrow(dummy))
        dummy$Before=rep('Before',nrow(dummy))
        dummy=data.frame(var=c(dummy[,1],dummy[,2]),
                         period=c(dummy$Currently,dummy$Before))
        MAT=table(dummy$var,dummy$period)
      }else
      {
        dummy=Fisher%>%dplyr::select(this)%>%pull
        MAT=table(dummy,useNA = 'ifany')
      }
      
      
      fn.brplt(MAT=MAT,XLAB=Q.list.Fisher[[i]][n])
    }
    mtext("Frequency",2,line=-.25,outer=T,las=3,cex=1.35)
    dev.off()
  }
  
  #Middle
  Q.list.Middle=list(
    General=c("market_name","locality","state",
              'q1',
              'q2 & q3 & q4_a & q4_b & q4_c',
              'q5 & q6_a & q6_b',
              'q7',
              'q8',
              fn.pst('q9',c(1:4,6:7)),
              paste('q',10:13,sep=''),
              fn.pst('q14',1:4),
              'q15',
              paste('q',26:31,sep='')),
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
  Q.list.Seller=list(
    General=c("locality","location","state","type_of_location",
              'q1','q2','q3 & q3_a'),
    Commercial=c('q4',fn.pst('q5',1),
                 'q6_meat_a',
                 paste('q',c(7,9:11),sep=''),fn.pst('q12',1:6),
                 paste('q',13:17,sep='')),
    Consumption=c(paste('q',18:29,sep='')),
    Management=c('q30','q31','q32',
                 'q33 & q33_b','q34 & q34_b'))
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