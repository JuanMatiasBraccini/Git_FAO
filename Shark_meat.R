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
  #2.1. tidy up data
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

  #2.2. define how to group and summarise each question

#define how to group vars for display
#note: the '&' groups the same question 'now' and 'before'

#missing: q13 & q14 must be analysed if they are shark fishers or not!
#problem: Q17 and Q19 are 'catch now at average capacity' but 
#         Q18 and Q20 are 'catch before at maximum capacity'
#         Q36, 37 & 38: are these 'now' or 'before' would have to assume 'now'
Q.list.Fisher=list(
    General=c('location','estate (districit)',  
              'q1','q2','q3','q4','q86 & q86_a'),  
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
            'q81'),
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
                     paste(paste('q104',tolower(LETTERS[1:11]),sep='_'),collapse=" & "),
                     paste(paste('q105',tolower(LETTERS[1:11]),sep='_'),collapse=" & "),
                     'q106 & q107'),
   Management=c(paste('q',67:77,sep=''),
                      'q78 & q79',
                      'q80'),
   Seasonal.patrn=c(paste(paste('q89',tolower(LETTERS[1:10]),sep='_'),collapse=" & "),
                    paste(paste('q90',tolower(LETTERS[1:10]),sep='_'),collapse=" & "),
                    'q91 & q92',
                    'q95',
                    'q97',
                    'q99',
                    'q100 & q102',
                    'q101 & q103'
                    ),
   Species_compo=c(paste(paste('q87',tolower(LETTERS[1:10]),sep='_'),collapse=" & "),
                   paste(paste('q88',tolower(LETTERS[1:10]),sep='_'),collapse=" & "),
                   paste(paste('q93',tolower(LETTERS[1:11]),sep='_'),collapse=" & "),
                   paste(paste('q94',tolower(LETTERS[1:11]),sep='_'),collapse=" & "),
                   paste(paste('q96',tolower(LETTERS[1:11]),sep='_'),collapse=" & "),
                   paste(paste('q98',tolower(LETTERS[1:11]),sep='_'),collapse=" & ")
                   ))

Q.list.Middle=list(General=c("Market_name","Location","Estate",
                             'q1',
                             'q2 & q3 & q4_a & q4_b & q4_c',
                             'q5 & q6_a & q6_b',
                             'q7',
                             'q8',
                             paste(paste('q9',tolower(LETTERS[1:6]),sep='_'),collapse=" & "),
                             paste('q',10:13,sep=''),
                             paste(paste('q14',tolower(LETTERS[1:4]),sep='_'),collapse=" & "),
                             'q15',
                             paste('q',26:31,sep='')
                             ),
                   Socio.economics=c(paste(c(paste(paste('q18_meat',tolower(LETTERS[1:4]),sep='_'),collapse=" & "),
                                           paste(paste('q18_cartilage',tolower(LETTERS[1:2]),sep='_'),collapse=" & "),
                                           paste(paste('q18_skin',tolower(LETTERS[1:2]),sep='_'),collapse=" & "),
                                           'Q18_gill_a',
                                           'Q18_oil_a'
                   ),collapse=" & "),
                   paste(c(paste(paste('q19_meat',tolower(LETTERS[1:4]),sep='_'),collapse=" & "),
                           paste(paste('q19_cartilage',tolower(LETTERS[1:2]),sep='_'),collapse=" & "),
                           paste(paste('q19_skin',tolower(LETTERS[1:2]),sep='_'),collapse=" & "),
                           'Q19_gill_a',
                           'Q19_oil_a'
                   ),collapse=" & "),
                   ),
                   Management=c(paste('q',32:34,sep=''),
                                'q35 & q35_a',
                                'q36'),
                   Species_compo=c(paste(paste('q16',tolower(LETTERS[1:15]),sep='_'),collapse=" & "),
                                   paste(paste('q17',tolower(LETTERS[1:15]),sep='_'),collapse=" & "),
                                   c('Q20_meat & Q20_cartilage & Q20_skin & Q20_gill & Q20_oil'))
                   )

Q.list.Seller=list(General=c("Village","Location","Estate","Type_of_location"))


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
