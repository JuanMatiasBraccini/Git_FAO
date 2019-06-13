##--SCRIPT FOR SUMMARISING FAO-CITES QUESTIONNAIRES--
#note:   make sure all files are .docx
#        modify Reviewr.exprtis is new reviewers available based on Kim spreadsheet
#        Denham P == Parker Denham

rm(list=ls(all=TRUE))
library(docxtractr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(ggpubr)
library(gridExtra)
library(grid)
theme_set(theme_pubr())


#Control 
do.mean=T


# 1. Data ---------------------------------------------------------

#Check who hasn't returned 2nd round
check.2nd.round="NO"
if(check.2nd.round=="YES")
{
  #original data set
  #1. Import questionnaires
  setwd('C:\\Matias\\FAO\\data_First')
  filenames =list.files()
  doc=lapply(filenames,read_docx)
  
  #2. Extract data by species
  names(doc)=filenames
  Species= sapply(strsplit(filenames, "_"), "[", 2)
  id=grep(".docx",Species)
  Species[id]=read.table(text = Species[id], sep = ".", as.is = TRUE)$V1
  SP=unique(Species)
  Split.sp=lapply(SP,function(x) which(Species==x))
  doc.sp=vector('list',length(Split.sp))
  names(doc.sp)=SP
  for(i in 1:length(doc.sp))
  {
    doc.sp[[i]]=doc[unlist(Split.sp[i])]
    names(doc.sp[[i]])= sub("_.*", "", names(doc.sp[[i]]))
  }
  First_draft=vector('list',length(doc.sp))
  for(i in 1:length(doc.sp))First_draft[[i]]=paste(names(doc.sp)[i],names(doc.sp[[i]]),sep="_")
  
  #Final draft
  #1. Import questionnaires
  setwd('C:\\Matias\\FAO\\data')
  filenames =list.files()
  doc=lapply(filenames,read_docx)
  
  #2. Extract data by species
  names(doc)=filenames
  Species= sapply(strsplit(filenames, "_"), "[", 2)
  id=grep(".docx",Species)
  Species[id]=read.table(text = Species[id], sep = ".", as.is = TRUE)$V1
  SP=unique(Species)
  Split.sp=lapply(SP,function(x) which(Species==x))
  doc.sp=vector('list',length(Split.sp))
  names(doc.sp)=SP
  for(i in 1:length(doc.sp))
  {
    doc.sp[[i]]=doc[unlist(Split.sp[i])]
    names(doc.sp[[i]])= sub("_.*", "", names(doc.sp[[i]]))
  }
  Final_draft=vector('list',length(doc.sp))
  for(i in 1:length(doc.sp))Final_draft[[i]]=paste(names(doc.sp)[i],names(doc.sp[[i]]),sep="_")
  
  #compare
  naughty=Final_draft
  for(i in 1:length(doc.sp))
  {
    id=which(!First_draft[[i]]%in%Final_draft[[i]])
    naughty[[i]]=First_draft[[i]][id]
  }
  print(naughty)
}else
{
  #1. Import questionnaires first
  setwd('C:\\Users\\myb\\Desktop\\FAO\\data_First')
  filenames =list.files()
  doc=lapply(filenames,read_docx)
  
  #2. Extract data by species
  names(doc)=filenames
  Species= sapply(strsplit(filenames, "_"), "[", 2)
  id=grep(".docx",Species)
  Species[id]=read.table(text = Species[id], sep = ".", as.is = TRUE)$V1
  SP=unique(Species)
  Split.sp=lapply(SP,function(x) which(Species==x))
  doc.sp=vector('list',length(Split.sp))
  names(doc.sp)=SP
  for(i in 1:length(doc.sp))
  {
    doc.sp[[i]]=doc[unlist(Split.sp[i])]
    names(doc.sp[[i]])= sub("_.*", "", names(doc.sp[[i]]))
  }
  doc.sp.first=doc.sp
  
  #1. Import questionnaires final
  setwd('C:\\Users\\myb\\Desktop\\FAO\\data_Third')
  filenames =list.files()
  doc=lapply(filenames,read_docx)
  
  #2. Extract data by species
  names(doc)=filenames
  Species= sapply(strsplit(filenames, "_"), "[", 2)
  id=grep(".docx",Species)
  Species[id]=read.table(text = Species[id], sep = ".", as.is = TRUE)$V1
  SP=unique(Species)
  Split.sp=lapply(SP,function(x) which(Species==x))
  doc.sp=vector('list',length(Split.sp))
  names(doc.sp)=SP
  for(i in 1:length(doc.sp))
  {
    doc.sp[[i]]=doc[unlist(Split.sp[i])]
    names(doc.sp[[i]])= sub("_.*", "", names(doc.sp[[i]]))
  }
  
}


#Extract all relevant tables
for(i in 1:length(doc.sp))
{
  tbls=vector('list',length(doc.sp[[i]]))
  names(tbls)=names(doc.sp[[i]])
  for(s in 1:length(doc.sp[[i]]))
  {
    tbls[[s]] <- docx_extract_all_tbls(doc.sp[[i]][[s]])
    ##remove intro table if included in questionnaire
    if(length(tbls[[s]])==34) tbls[[s]] =tbls[[s]][-1]   
  }
  doc.sp[[i]]=tbls
}

  #original data set
for(i in 1:length(doc.sp.first))
{
  tbls.first=vector('list',length(doc.sp.first[[i]]))
  names(tbls.first)=names(doc.sp.first[[i]])
  for(s in 1:length(doc.sp.first[[i]]))
  {
    tbls.first[[s]] <- docx_extract_all_tbls(doc.sp.first[[i]][[s]])
    ##remove intro table if included in questionnaire
    if(length(tbls.first[[s]])==34) tbls.first[[s]] =tbls.first[[s]][-1]   
  }
  doc.sp.first[[i]]=tbls.first
}

# 2. Combine data from each questionnaire by species-question number ---------------------------------------------------------
dat=vector('list',length(doc.sp))
names(dat)=names(doc.sp)
for(i in 1:length(dat))
{
  a=doc.sp[[i]]
  dummy=vector('list',length(a[[1]]))
  names(dummy)=paste("Q",1:length(dummy),sep=".")
  for(r in 1:length(a))
  {
    b=a[[r]]
    for(q in 1:length(b))
    {
      d=cbind(data.frame(Reviewer=names(a)[r]),b[[q]])
      if(r>1)names(d)=names(dummy[[q]])
      dummy[[q]]=rbind(dummy[[q]],d)
    }
  }
  dat[[i]]=dummy
}

  #first
dat.first=vector('list',length(doc.sp.first))
names(dat.first)=names(doc.sp.first)
for(i in 1:length(dat.first))
{
  a=doc.sp.first[[i]]
  dummy=vector('list',length(a[[1]]))
  names(dummy)=paste("Q",1:length(dummy),sep=".")
  for(r in 1:length(a))
  {
    b=a[[r]]
    for(q in 1:length(b))
    {
      d=cbind(data.frame(Reviewer=names(a)[r]),b[[q]])
      if(r>1)names(d)=names(dummy[[q]])
      dummy[[q]]=rbind(dummy[[q]],d)
    }
  }
  dat.first[[i]]=dummy
}

# 3. Analyse each question by species ---------------------------------------------------------
setwd('C:\\Users\\myb\\Desktop\\FAO')

#define questions and location in list
Questions=c("Reviewer_name","Reviewed_species","Reviewer_qualifications","Reviewer_field","Reviewer_CITES,cons,ass",
            "Q.Process_1","Q.Process_1_comments","Q.Process_2","Q.Process_2_comments",
            "Q.Species_1a","Q.Species_1a_comments","Q.Species_1b","Q.Species_1b_comments","Q.Species_1c",
            "Q.Species_2a","Q.Species_2a_comments","Q.Species_2b","Q.Species_2b_comments","Q.Species_2c",
            "Q.Species_3a","Q.Species_3a_comments","Q.Species_3b","Q.Species_3b_comments","Q.Species_3c",
            "Q.Species_4a","Q.Species_4a_comments","Q.Species_4b",
            "Q.Species_5","Q.Species_5_comments",
            "Q.Communication_1a","Q.Communication_1a_comments","Q.Communication_1b","Q.Communication_1b_comments"
)
Analsd.q=Questions[-match(c("Reviewer_name","Reviewed_species",
                            "Reviewer_CITES,cons,ass"),Questions)]
Analsd.q.comments=Analsd.q[which(!is.na(sapply(strsplit(Analsd.q, "_"), "[", 3)))]
id.questions=match(Analsd.q,Questions)
id.comments=match(Analsd.q.comments,Questions)

Score.q=c("Q.Process_1","Q.Species_1a","Q.Species_1b","Q.Species_2a","Q.Species_2b",
          "Q.Species_3a","Q.Species_3b","Q.Species_4a","Q.Species_5","Q.Communication_1a",
          "Q.Communication_1b")

Question_summary=c("Reviewer_name","Reviewed_species","Reviewer_qualifications","Reviewer_field","Reviewer_CITES,cons,ass",
                   "Process_1_Understanding of level of scientific training",
                   "Process_1_Understanding of level of scientific training_comments",
                   "Process_2_Contributor's range of fishery, trade and conservation expertise",
                   "Process_2_Contributor's range of fishery, trade and conservation expertise_comments",
                   "Productivity_1a_References best available information",
                   "Productivity_1a_References best available information_comments",
                   "Productivity_1b_Productivity interpretation against CITES criteria",
                   "Productivity_1b_Productivity interpretation against CITES criteria_comments",
                   "Productivity_1c_Your assessment",
                   "Productivity_2a_Historical decline_references best available information",
                   "Productivity_2a_Historical decline_references best available information_comments",
                   "Productivity_2b_Historical decline_interpretation against CITES criteria",
                   "Productivity_2b_Historical decline_interpretation against CITES criteria_comments",
                   "Productivity_2c_Historical decline_your assessment",
                   "Productivity_3a_Recent decline rate_references best available information",
                   "Productivity_3a_Recent decline rate_references best available information_comments",
                   "Productivity_3b_Recent decline rate_interpretation against CITES criteria",
                   "Productivity_3b_Recent decline rate_interpretation against CITES criteria_comments",
                   "Productivity_3c_Recent decline rate_your assessment",
                   "Productivity_4a_Combined historical decline and recent decline rate_interpretation",
                   "Productivity_4a_Combined historical decline and recent decline rate_interpretation_comments",
                   "Productivity_4b_Combined historical decline and recent decline rate_interpretation_your assessment",
                   "Productivity_5_Other factors",
                   "Productivity_5_Other factors_comments",
                   "Communication_1a_Assessment effectively communicated",
                   "Communication_1a_Assessment effectively communicated_comments",
                   "Communication_1b_Likely effectiveness for conservation of CITES listing",
                   "Communication_1b_Likely effectiveness for conservation of CITES listing_comments")

CXmn=.9
CX.tbl=.9
Tbl.expnsn=120
Paddn=c(5, 3)
add.kmnts.to.pdf="NO"


Q.groups=c("Scientific_training","Species_char_and_status","Species_char_and_status_pers_opn","Communication")
names(Q.groups)=c("Scientific training","Species characteristics  and status",
                  "Personal opinion on species characteristics  and status","Communication")
Grouped.q=ifelse(Analsd.q%in% c("Reviewer_qualifications","Reviewer_field",
                                "Q.Process_1","Q.Process_1_comments","Q.Process_2","Q.Process_2_comments"), Q.groups[1],
                 ifelse(Analsd.q%in% c("Q.Species_1a","Q.Species_1a_comments","Q.Species_1b",              
                                       "Q.Species_1b_comments","Q.Species_2a",               
                                       "Q.Species_2a_comments","Q.Species_2b","Q.Species_2b_comments",      
                                       "Q.Species_3a","Q.Species_3a_comments",      
                                       "Q.Species_3b","Q.Species_3b_comments",               
                                       "Q.Species_4a","Q.Species_4a_comments",              
                                       "Q.Species_5","Q.Species_5_comments"), Q.groups[2],
                        ifelse(Analsd.q%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"),Q.groups[3],
                               ifelse(Analsd.q%in% c("Q.Communication_1a","Q.Communication_1a_comments",
                                                     "Q.Communication_1b","Q.Communication_1b_comments"), Q.groups[4],NA))))
names(Analsd.q)=Grouped.q
Q.grouping=unique(names(Analsd.q))

Grouped.q_comments=ifelse(Analsd.q.comments%in% c("Q.Process_1_comments","Q.Process_2_comments"), Q.groups[1],
                          ifelse(Analsd.q.comments%in% c("Q.Species_1a_comments",            
                                                         "Q.Species_1b_comments","Q.Species_2a_comments","Q.Species_2b_comments",      
                                                         "Q.Species_3a_comments","Q.Species_3b_comments","Q.Species_4a_comments",
                                                         "Q.Species_5_comments"), Q.groups[2],
                                 ifelse(Analsd.q.comments%in% c("Q.Communication_1a_comments",
                                                                "Q.Communication_1b_comments"), Q.groups[4],NA)))
names(Analsd.q.comments)=Grouped.q_comments



#Check for blank scores
for(s in 1:length(dat))
{
  D=dat[[s]]
  Store.Miss.score=vector('list',length(id.questions))
  for(q in 1:length(id.questions))
  {
    indx=id.questions[q]
    dd=D[[indx]]
    if(Questions[id.questions[q]]%in%c(Score.q))
    {
      names(dd)=c("Reviewer","Group","Score")
      Miss.score=dd[which(dd$Score%in%c(""," ")),]
      if(nrow(Miss.score)>0)
      {
        Miss.score$Question=Questions[id.questions[q]]
        Store.Miss.score[[q]]=Miss.score
      }
      
      
    }
    
    if(Questions[id.questions[q]]%in%c("Q.Process_2"))
    {
      names(dd)=c("Reviewer","Group","Score")
      
      SplT=unique(dd$Group)
      SplT=SplT[match(c("2a. Range of fishery biology and population dynamics expertise of information contributors",
                        "2b. Range of fishery management expertise of information contributors",
                        "2c. Range of conservation biology expertise of information contributors",
                        "2d. Range of trade expertise of information contributors"),SplT)]
      dummy=vector('list',length(SplT))
      for(tt in 1:length(SplT))
      {
        Id=which(dd$Group==SplT[tt])
        Id_plus4=Id+4
        IId=vector('list',length(Id))
        for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
        a=dd[unlist(IId),]
        
        Miss.score=a[which(a$Score%in%c(""," ")),]
        if(nrow(Miss.score)>0)
        {
          Miss.score$Question=SplT[tt]
          dummy[[tt]]=Miss.score
        }
      }
      Store.Miss.score[[q]]=do.call(rbind,dummy)
    }
  }
  write.csv(do.call(rbind,Store.Miss.score),paste("outputs/Data_issues/blank.scores_",names(dat)[s],".csv",sep=""),row.names=F)
}

#Check for NA scores
NA.group=c("N/A","NA","na","Na","unknown","unknown, but 3 implied","unknown, but 4 implied","not described",
           "NA (or 1 if NA is not an option)")
for(s in 1:length(dat))
{
  D=dat[[s]]
  Store.Miss.score=vector('list',length(id.questions))
  for(q in 1:length(id.questions))
  {
    indx=id.questions[q]
    dd=D[[indx]]
    if(Questions[id.questions[q]]%in%c(Score.q))
    {
      names(dd)=c("Reviewer","Group","Score")
      Miss.score=dd[which(dd$Score%in%NA.group),]  
      if(nrow(Miss.score)>0)
      {
        Miss.score$Question=Questions[id.questions[q]]
        Store.Miss.score[[q]]=Miss.score
      }
    }
    
    if(Questions[id.questions[q]]%in%c("Q.Process_2"))
    {
      names(dd)=c("Reviewer","Group","Score")
      
      SplT=unique(dd$Group)
      SplT=SplT[match(c("2a. Range of fishery biology and population dynamics expertise of information contributors",
                        "2b. Range of fishery management expertise of information contributors",
                        "2c. Range of conservation biology expertise of information contributors",
                        "2d. Range of trade expertise of information contributors"),SplT)]
      dummy=vector('list',length(SplT))
      for(tt in 1:length(SplT))
      {
        Id=which(dd$Group==SplT[tt])
        Id_plus4=Id+4
        IId=vector('list',length(Id))
        for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
        a=dd[unlist(IId),]
        
        Miss.score=a[which(a$Score%in%NA.group),]
        if(nrow(Miss.score)>0)
        {
          Miss.score$Question=SplT[tt]
          dummy[[tt]]=Miss.score
        }
      }
      Store.Miss.score[[q]]=do.call(rbind,dummy)
    }
  }
  write.csv(do.call(rbind,Store.Miss.score),paste("outputs/Data_issues/NA.scores_",names(dat)[s],".csv",sep=""),row.names=F)
}


CEX.x=.95
#note: for "Reviewer_qualifications", Walsh has a Masters in Fisheries and also in Enviro, 
#       hence N=19 instead of 18
show.Reviewer_field="NO" #not shown, redundant with Reviewer qualifications

Q.g2title=c("Productivity: best av. info.",
            "Productivity: criteria interp.",
            "Historical decline: best av. info.",
            "Historical decline: criteria interp.",
            "Recent decline: best av. info.",
            "Recent decline: criteria interp.",
            "Combined historical & recent decline",
            "Other factors")
Q.g4title=c("Assessment effectively communicated","Likely effectiveness for conservation of CITES listing")
fn.fig=function(NAME,Width,Height) jpeg(file=paste(NAME,".jpeg",sep=""),width=Width,height=Height,units="px",res=300)

Fill.col=c("white","grey85","grey65","grey45","black") 


# 4. Mean approach option -------------------------------------
smart.par=function(n.plots,MAR,OMA,MGP) return(par(mfrow=n2mfrow(n.plots),mar=MAR,oma=OMA,las=1,mgp=MGP))
if("plyr" %in% (.packages())) detach(package:plyr)
if(do.mean)
{
  fn.plt=function(dd,dd1)
  {
    with(dd,{
      plot(1:4,mean,pch=19,cex=1.5,xlim=c(0.5,4.5),ylim=c(0.5,5.5),xaxt="n",ylab="",xlab="")
      suppressWarnings(arrows(1:4,mean-se,1:4,mean+se,lwd=1.5,angle=90, length=0.05,code=3))
      axis(1,1:4,c("Proposal","FAO","IUCN","CITES"),cex.axis=cex.LB)
      if(is.null(dd1))text(1:4,rep(Txt.ln,4),paste("n=",dd$N),pos=3)
    })
    if(!is.null(dd1))
    {
      with(dd1,{
        points((1:4)+.2,mean,pch=19,cex=1.5,col="grey60")
        suppressWarnings(arrows((1:4)+.2,mean-se,(1:4)+.2,mean+se,lwd=1.5,angle=90, length=0.05,code=3,col="grey60"))
        axis(1,1:4,c("Proposal","FAO","IUCN","CITES"),cex.axis=cex.LB)
      })
      text(1:4,rep(Txt.ln,4),paste("n=",dd$N+dd1$N),pos=3)
    }
  }
  fn.brplt=function(MAT,LGND,where,XLAB)
  {
    barplot(t(MAT),beside=T,main="",legend=c(LGND),names.arg=XLAB,
            col=Fill.col,args.legend=list(cex=1.2,x=where,bty="n",title="Score"))
    box()
  } 
  fn.plt.all.rev=function(dd,dd1)
  {
    with(dd,{
      plot(1:4,mean,pch=19,cex=1.5,xlim=c(0.75,5),ylim=c(0,6),xaxt="n",ylab="",xlab="",col="red")
      suppressWarnings(arrows(1:4,mean-se,1:4,mean+se,lwd=1.5,angle=90,col="red", length=0.05,code=3))
      axis(1,1:4,c("Proposal","FAO","IUCN","CITES"),cex.axis=cex.LB)
      if(is.null(dd1))text(1:4,rep(-.25,4),paste("n=",dd$N),pos=3)
    })
    if(!is.null(dd1))
    {
      with(dd1,{
        points((1:4)+.2,mean,pch=19,cex=1.5,col="red")
        suppressWarnings(arrows((1:4)+.2,mean-se,(1:4)+.2,mean+se,lwd=1.5,angle=90, length=0.05,code=3,col="red"))
        axis(1,1:4,c("Proposal","FAO","IUCN","CITES"),cex.axis=cex.LB)
      })
      text(1:4,rep(-.25,4),paste("n=",dd$N+dd1$N),pos=3)
    }
  }
  fun.add.rev=function(Add.rev,CEX)
  {
    Add.rev$x=as.numeric(substr(Add.rev$Group,1,1))
    Add.rev=Add.rev[with(Add.rev, order(Group,Score)),]
    
    #Add.rev$x=Add.rev$x+seq(-.35,.35,length.out=17)
    #Add.rev$x=jitter(Add.rev$x,1)
    
    #Add.rev$Score=Add.rev$Score+seq(-.35,.35,length.out=17)
    Add.rev$Score=jitter(Add.rev$Score,1.5)
    
    with(Add.rev,text(x,Score,Reviewer,srt=0,cex=CEX,pos=4,col="black"))
  }
  
  
  #First. Store data in right format
  dat.cleansed=dat 
  Q.c=vector('list',length(dat))
  names(Q.c)=names(dat)
  for(s in 1:length(dat))
  {
    #Extract info
    D=dat[[s]]
    dummy.Q.g=vector('list',length(Q.grouping))
    names(dummy.Q.g)=Q.grouping
    dummy.q.c=vector('list',length=4)
    names(dummy.q.c)=c("Q.Species_1c","Q.Species_2c","Q.Species_3c", "Q.Species_4b")
    
    for(Q.g in 1:length(Q.grouping))
    {
      this.q=Analsd.q[which(names(Analsd.q)==Q.grouping[Q.g])]
      this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
      this.q=this.q[which(!this.q%in%this.q.comments)]
      id.questions=match(this.q,Questions)
      id.comments=match(this.q.comments,Questions)
      dummy.q=vector('list',length(id.questions))
      names(dummy.q)=this.q
      for(q in 1:length(id.questions))  
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
        #this just outputs table of reviewer's field and qualification
        if(Questions[id.questions[q]]=="Reviewer_qualifications")
        {
          dd=subset(dd,!Qualification%in%c("Qualification"))  #only use max qualification
          dd$Quali=with(dd,ifelse(Qualification=="PhD",3,ifelse(Qualification=="Masters",2,
                                                                ifelse(Qualification=='BSc',1,ifelse(Qualification=="Other",0,NA)))))
          id1=match("Fisheries - Resource Use focus",names(dd))
          if(is.na(id1))id1=match("Fisheries - Resource Use Focus",names(dd))
          dd$Top.qual.fish=ifelse(dd[,id1]%in%c("X","x"),1,NA)
          id1=match("Environment - Biodiversityfocus",names(dd))
          if(is.na(id1))id1=match("Environment - BiodiversityFocus",names(dd))
          dd$Top.qual.env=ifelse(dd[,id1]%in%c("X","x"),1,NA)
          dd$Top.qual.fish=dd$Top.qual.fish*dd$Quali
          dd$Top.qual.env=dd$Top.qual.env*dd$Quali
          dd1=aggregate(Top.qual.fish~Reviewer,dd,max,na.rm=T)
          dd2=aggregate(Top.qual.env~Reviewer,dd,max,na.rm=T)
          dd1$Reviewer=as.character(dd1$Reviewer)
          dd2$Reviewer=as.character(dd2$Reviewer)
          names(dd1)[2]=names(dd2)[2]="Top_qual"
          dd1$Field="Fisheries"
          dd2$Field="Environment"
          dd3=rbind(dd1,dd2)
          dd3=dd3 %>% group_by(Reviewer) %>% filter(Top_qual==max(Top_qual)) #only keep max for Both
          dd3$Top_qual=with(dd3,ifelse(Top_qual==3,"Ph.D",ifelse(Top_qual==2,"MSc","BSc")))
          dd3$N=1
          dd3=aggregate(N~Top_qual+Field,dd3,sum)
          dd3=reshape(dd3, v.names="N",idvar="Field",timevar="Top_qual",direction="wide")
          colnames(dd3)[2:ncol(dd3)]=substr(colnames(dd3)[2:ncol(dd3)],3,20)
          write.csv(dd3,paste("outputs/Paper/Reviewers.qualifications.",names(dat)[s],".csv",sep=""),row.names=F)
        }
        
        if(Questions[id.questions[q]]=="Reviewer_field")
        {
          dd$Tick=with(dd,ifelse(Tick%in%c("X","x"),1,NA))
          
          #Experts are classed based on field of higher degree 
          Reviewr.exprtis=aggregate(Tick~Reviewer+Statement,dd,sum,na.rm=T)
          Reviewr.exprtis$Field=with(Reviewr.exprtis,
                                     ifelse(Statement=="Conservation of the natural environment",'Environment',
                                            ifelse(Statement=="Sustainable use and management of natural resources","Fisheries",NA)))
          
          #reclass experts based on Kim's request
          Reviewr.exprtis$Field=with(Reviewr.exprtis,ifelse(Reviewer%in%c('Frisk','Fisk','Kingma','Walsh'),'Environment',Field))
          
          dd=aggregate(Tick~Field,Reviewr.exprtis,sum)
          fn.fig(paste(getwd(),"/outputs/Paper/Appendix.Field.barplot",names(dat)[s],sep=""),2400,2400)
          par(las=1)  
          barplot(dd$Tick,names.arg=dd$Field,main='Area of expertise',cex.main=1.5,cex.axis=1.25,cex.names=1.25)
          box()
          dev.off()
          
          Reviewr.exprtis=Reviewr.exprtis[,c('Reviewer','Field')]
          dummy.q[[q]]=Reviewr.exprtis
        }
        
        if(Questions[id.questions[q]]%in%Score.q)   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$dummy=substr(dd$Group,1,2)
          dd=subset(dd,dummy%in%paste(as.character(1:4),".",sep=""))
          dd=dd[,-match('dummy',names(dd))]
          dd$N=1
          dd$Score=with(dd,ifelse(Score%in%c("3 - Assuming that M.S. Adam is lead author.",
                                             "3 - Assuming that Daniel Fernando is lead author.",
                                             "unknown, but coverage of literature and insight suggest high level of training (i.e., 3)"),3,
                                  ifelse(Score=="4-5",4,
                                         ifelse(Score=="3-4",3,
                                                ifelse(Score=="4 (inferred)",4,
                                                       ifelse(Score=="1 (NA unavailable)",1,
                                                              Score))))))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          #add reviewer expertise
          dd=merge(dd,Reviewr.exprtis,by="Reviewer",all.x=T)
          dummy.q[[q]]=dd
        }
        
        #export the opinion of reviewers on their own interpretation
        if(Questions[id.questions[q]]%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"))
        {
          names(dd)=c("Reviewer","Criteria","Assessment")
          dd=subset(dd,!Assessment%in%c("PROCTIVITY: Low - Medium - High","PROCTIVITY: Low - Medium - High",
                                        "Meets or Does Not Meet the CITES Criteria"))
          dummy.q.c[[q]]=dd
          # g = sapply(lapply(as.character(dd$Assessment), strwrap, width=Tbl.expnsn), paste, collapse="\n")
          #  g=unique(g)
          # g=subset(g,!g=="")
          write.csv(dd,paste("outputs/Paper/Comments/",names(dat)[s],"_",Questions[id.questions[q]],".csv",sep=""),row.names = F)
        }
        
        if(Questions[id.questions[q]]=="Q.Process_2")   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd=subset(dd,!Group=="Range of fishery biology and population dynamics expertise of information contributors")
          #dd$dummy=substr(dd$Group,1,2)
          #dd=subset(dd,dummy%in%as.character(paste(1:4,".",sep="")))
          #dd=dd[,-match('dummy',names(dd))]
          
          
          dd$Score=with(dd,ifelse(Score%in%c("2-3 - Assuming that M.S. Adam is lead author.",
                                             "2-3 - Assuming that Daniel Fernando is lead author."),2,
                                  ifelse(Score%in%c("4-5","unknown, but 4 implied"),4,
                                         ifelse(Score=="unknown, but 4 implied",4,                   
                                                ifelse(Score%in%c("5 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                  "5 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)",
                                                                  "5 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                  "5 (I know about two of the four reviewers so my score also reflects personal knowledge in addition to what is contained in the review)"),5, 
                                                       ifelse(Score%in%c("4 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)",
                                                                         "4 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                         "4 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                         "4 (I know about two of the four reviewers so my score also reflects personal knowledge in addition to what is contained in the review)"),4,
                                                              ifelse(Score%in%c("3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)","unknown, but 3 implied",
                                                                                "3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)",
                                                                                "3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)"),3,                   
                                                                     Score)))))))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          SplT=unique(dd$Group)
          SplT=SplT[match(c("2a. Range of fishery biology and population dynamics expertise of information contributors",
                            "2b. Range of fishery management expertise of information contributors",
                            "2c. Range of conservation biology expertise of information contributors",
                            "2d. Range of trade expertise of information contributors"),SplT)]
          
          TITL=c("Fishery biology and pop. dyn. expertise",
                 "Fishery management expertise",
                 "Conservation biology expertise",
                 "Trade expertise")
          Store.SplT=vector('list',length(SplT))
          names(Store.SplT)=TITL
          for(tt in 1:length(SplT))
          {
            Id=which(dd$Group==SplT[tt])
            Id_plus4=Id+4
            IId=vector('list',length(Id))
            for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
            a=dd[unlist(IId),]
            
            #add reviewer expertise
            a=merge(a,Reviewr.exprtis,by="Reviewer",all.x=T)
            # dd1=aggregate(N~Group+Score,a,sum)
            # #add missing scores
            # sCOREs=1:5 
            # id=sCOREs[which(!sCOREs%in%unique(dd1$Score))]
            # if(length(id)>0)
            # {
            #   Dumy=dd1[1:length(id),]
            #   Dumy$Score=id
            #   Dumy$N=NA
            #   dd1=rbind(Dumy,dd1)
            #   dd1=dd1[order(dd1$Score),]
            # }
            # dd1=reshape(dd1, v.names="N",idvar="Group",timevar="Score",direction="wide")
            # dd1=dd1[order(dd1$Group),]
            # colnames(dd1)[-1]=substr(colnames(dd1)[-1],3,10)
            #Store.SplT[[tt]]=cbind(Question=TITL[tt],dd1)
            Store.SplT[[tt]]=a
          }
          dummy.q[[q]]=Store.SplT
        }
      }
      dummy.Q.g[[Q.g]]=dummy.q
      Q.c[[s]]=dummy.q.c
    }
    dat.cleansed[[s]]=dummy.Q.g
  }
  
    #original data set
  dat.cleansed.first=dat.first 
  Q.c.first=vector('list',length(dat.first))
  names(Q.c.first)=names(dat.first)
  for(s in 1:length(dat.first))
  {
    #Extract info
    D=dat.first[[s]]
    dummy.Q.g=vector('list',length(Q.grouping))
    names(dummy.Q.g)=Q.grouping
    dummy.q.c=vector('list',length=4)
    names(dummy.q.c)=c("Q.Species_1c","Q.Species_2c","Q.Species_3c", "Q.Species_4b")
    
    for(Q.g in 1:length(Q.grouping))
    {
      this.q=Analsd.q[which(names(Analsd.q)==Q.grouping[Q.g])]
      this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
      this.q=this.q[which(!this.q%in%this.q.comments)]
      id.questions=match(this.q,Questions)
      id.comments=match(this.q.comments,Questions)
      dummy.q=vector('list',length(id.questions))
      names(dummy.q)=this.q
      for(q in 1:length(id.questions))  
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
        #this just outputs table of reviewer's field and qualification
        if(Questions[id.questions[q]]=="Reviewer_qualifications")
        {
          dd=subset(dd,!Qualification%in%c("Qualification"))  #only use max qualification
          dd$Quali=with(dd,ifelse(Qualification=="PhD",3,ifelse(Qualification=="Masters",2,
                                                                ifelse(Qualification=='BSc',1,ifelse(Qualification=="Other",0,NA)))))
          id1=match("Fisheries - Resource Use focus",names(dd))
          if(is.na(id1))id1=match("Fisheries - Resource Use Focus",names(dd))
          dd$Top.qual.fish=ifelse(dd[,id1]%in%c("X","x"),1,NA)
          id1=match("Environment - Biodiversityfocus",names(dd))
          if(is.na(id1))id1=match("Environment - BiodiversityFocus",names(dd))
          dd$Top.qual.env=ifelse(dd[,id1]%in%c("X","x"),1,NA)
          dd$Top.qual.fish=dd$Top.qual.fish*dd$Quali
          dd$Top.qual.env=dd$Top.qual.env*dd$Quali
          dd1=aggregate(Top.qual.fish~Reviewer,dd,max,na.rm=T)
          dd2=aggregate(Top.qual.env~Reviewer,dd,max,na.rm=T)
          dd1$Reviewer=as.character(dd1$Reviewer)
          dd2$Reviewer=as.character(dd2$Reviewer)
          names(dd1)[2]=names(dd2)[2]="Top_qual"
          dd1$Field="Fisheries"
          dd2$Field="Environment"
          dd3=rbind(dd1,dd2)
          dd3=dd3 %>% group_by(Reviewer) %>% filter(Top_qual==max(Top_qual)) #only keep max for Both
          dd3$Top_qual=with(dd3,ifelse(Top_qual==3,"Ph.D",ifelse(Top_qual==2,"MSc","BSc")))
          dd3$N=1
          dd3=aggregate(N~Top_qual+Field,dd3,sum)
          dd3=reshape(dd3, v.names="N",idvar="Field",timevar="Top_qual",direction="wide")
          colnames(dd3)[2:ncol(dd3)]=substr(colnames(dd3)[2:ncol(dd3)],3,20)
          write.csv(dd3,paste("outputs/Paper/Reviewers.qualifications.",names(dat)[s],".csv",sep=""),row.names=F)
        }
        
        if(Questions[id.questions[q]]=="Reviewer_field")
        {
          dd$Tick=with(dd,ifelse(Tick%in%c("X","x"),1,NA))
          
          #Experts are classed based on field of higher degree 
          Reviewr.exprtis=aggregate(Tick~Reviewer+Statement,dd,sum,na.rm=T)
          Reviewr.exprtis$Field=with(Reviewr.exprtis,
                                     ifelse(Statement=="Conservation of the natural environment",'Environment',
                                            ifelse(Statement=="Sustainable use and management of natural resources","Fisheries",NA)))
          
          #reclass experts based on Kim's request
          Reviewr.exprtis$Field=with(Reviewr.exprtis,ifelse(Reviewer%in%c('Frisk','Fisk','Kingma','Walsh'),'Environment',Field))
          
          dd=aggregate(Tick~Field,Reviewr.exprtis,sum)
          fn.fig(paste(getwd(),"/outputs/Paper/Appendix.Field.barplot",names(dat)[s],sep=""),2400,2400)
          par(las=1)  
          barplot(dd$Tick,names.arg=dd$Field,main='Area of expertise',cex.main=1.5,cex.axis=1.25,cex.names=1.25)
          box()
          dev.off()
          
          Reviewr.exprtis=Reviewr.exprtis[,c('Reviewer','Field')]
          dummy.q[[q]]=Reviewr.exprtis
        }
        
        if(Questions[id.questions[q]]%in%Score.q)   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$dummy=substr(dd$Group,1,2)
          dd=subset(dd,dummy%in%paste(as.character(1:4),".",sep=""))
          dd=dd[,-match('dummy',names(dd))]
          dd$N=1
          dd$Score=with(dd,ifelse(Score%in%c("3 - Assuming that M.S. Adam is lead author.",
                                             "3 - Assuming that Daniel Fernando is lead author.",
                                             "unknown, but coverage of literature and insight suggest high level of training (i.e., 3)"),3,
                                  ifelse(Score=="4-5",4,
                                         ifelse(Score=="3-4",3,
                                                ifelse(Score=="4 (inferred)",4,
                                                       ifelse(Score=="1 (NA unavailable)",1,
                                                              Score))))))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          #add reviewer expertise
          dd=merge(dd,Reviewr.exprtis,by="Reviewer",all.x=T)
          dummy.q[[q]]=dd
        }
        
        #export the opinion of reviewers on their own interpretation
        if(Questions[id.questions[q]]%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"))
        {
          names(dd)=c("Reviewer","Criteria","Assessment")
          dd=subset(dd,!Assessment%in%c("PROCTIVITY: Low - Medium - High","PROCTIVITY: Low - Medium - High",
                                        "Meets or Does Not Meet the CITES Criteria"))
          dummy.q.c[[q]]=dd
          # g = sapply(lapply(as.character(dd$Assessment), strwrap, width=Tbl.expnsn), paste, collapse="\n")
          #  g=unique(g)
          # g=subset(g,!g=="")
          write.csv(dd,paste("outputs/Paper/Comments/",names(dat)[s],"_",Questions[id.questions[q]],".csv",sep=""),row.names = F)
        }
        
        if(Questions[id.questions[q]]=="Q.Process_2")   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd=subset(dd,!Group=="Range of fishery biology and population dynamics expertise of information contributors")
          #dd$dummy=substr(dd$Group,1,2)
          #dd=subset(dd,dummy%in%as.character(paste(1:4,".",sep="")))
          #dd=dd[,-match('dummy',names(dd))]
          
          
          dd$Score=with(dd,ifelse(Score%in%c("2-3 - Assuming that M.S. Adam is lead author.",
                                             "2-3 - Assuming that Daniel Fernando is lead author."),2,
                                  ifelse(Score%in%c("4-5","unknown, but 4 implied"),4,
                                         ifelse(Score=="unknown, but 4 implied",4,                   
                                                ifelse(Score%in%c("5 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                  "5 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)",
                                                                  "5 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                  "5 (I know about two of the four reviewers so my score also reflects personal knowledge in addition to what is contained in the review)"),5, 
                                                       ifelse(Score%in%c("4 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)",
                                                                         "4 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                         "4 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                         "4 (I know about two of the four reviewers so my score also reflects personal knowledge in addition to what is contained in the review)"),4,
                                                              ifelse(Score%in%c("3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)","unknown, but 3 implied",
                                                                                "3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)",
                                                                                "3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)"),3,                   
                                                                     Score)))))))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          SplT=unique(dd$Group)
          SplT=SplT[match(c("2a. Range of fishery biology and population dynamics expertise of information contributors",
                            "2b. Range of fishery management expertise of information contributors",
                            "2c. Range of conservation biology expertise of information contributors",
                            "2d. Range of trade expertise of information contributors"),SplT)]
          
          TITL=c("Fishery biology and pop. dyn. expertise",
                 "Fishery management expertise",
                 "Conservation biology expertise",
                 "Trade expertise")
          Store.SplT=vector('list',length(SplT))
          names(Store.SplT)=TITL
          for(tt in 1:length(SplT))
          {
            Id=which(dd$Group==SplT[tt])
            Id_plus4=Id+4
            IId=vector('list',length(Id))
            for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
            a=dd[unlist(IId),]
            
            #add reviewer expertise
            a=merge(a,Reviewr.exprtis,by="Reviewer",all.x=T)
            # dd1=aggregate(N~Group+Score,a,sum)
            # #add missing scores
            # sCOREs=1:5 
            # id=sCOREs[which(!sCOREs%in%unique(dd1$Score))]
            # if(length(id)>0)
            # {
            #   Dumy=dd1[1:length(id),]
            #   Dumy$Score=id
            #   Dumy$N=NA
            #   dd1=rbind(Dumy,dd1)
            #   dd1=dd1[order(dd1$Score),]
            # }
            # dd1=reshape(dd1, v.names="N",idvar="Group",timevar="Score",direction="wide")
            # dd1=dd1[order(dd1$Group),]
            # colnames(dd1)[-1]=substr(colnames(dd1)[-1],3,10)
            #Store.SplT[[tt]]=cbind(Question=TITL[tt],dd1)
            Store.SplT[[tt]]=a
          }
          dummy.q[[q]]=Store.SplT
        }
      }
      dummy.Q.g[[Q.g]]=dummy.q
      Q.c.first[[s]]=dummy.q.c
    }
    dat.cleansed.first[[s]]=dummy.Q.g
  }
  
  #Second. Plot and analyse data
  Lista.show=names(dat.cleansed$FAL)
  
  #A. Mean scores
  if("dplyr" %in% (.packages())) detach(package:dplyr)
  library(plyr)
  Flat.tab.fal=Flat.tab.thr=vector('list',2) 
  for(l in 1:length(Lista.show))
  {
    if(Lista.show[l]%in%c("Species_char_and_status","Communication"))
    {
      FAL=dat.cleansed$FAL[[l]]
      THR=dat.cleansed$THR[[l]]
      if(l==2)
      {
        Fig1.FAL=FAL[match(c("Q.Species_1a","Q.Species_1b"),names(FAL))]
        Fig1.THR=THR[match(c("Q.Species_1a","Q.Species_1b"),names(THR))]
        Fig2.FAL=FAL[-match(c("Q.Species_1a","Q.Species_1b"),names(FAL))]
        Fig2.THR=THR[-match(c("Q.Species_1a","Q.Species_1b"),names(THR))]
        
        
        dum.FAL=list(Fig1.FAL,Fig2.FAL)
        dum.THR=list(Fig1.THR,Fig2.THR)
        
        dummy.flat.tab.fal=dummy.flat.tab.thr=vector('list',length(dum.FAL))
        
        for(du in 1:length(dum.FAL))
        {
          if(du==1)
          {
            wid=2000;Hei=2400
            cex.LB=1.25
            cXQ=1.25
            Txt.ln=0.25
          }
          
          if(du==2)
          {
            cex.LB=0.9
            cXQ=0.8
            Txt.ln=0.1
            wid=1200;Hei=2400
          }
          
          #plot mean scores
          fn.fig(paste(getwd(),"/outputs/Paper/Figure.",du,".",Lista.show[l],sep=""),wid,Hei)
          if(du==1)par(mfrow=c(2,2),mar=c(1,1,.5,.25),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
          if(du==2)par(mfrow=c(6,2),mar=c(1,1,.5,.25),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
          for(f in 1:length(dum.FAL[[du]]))
          {
            fal=dum.FAL[[du]][[f]]
            thr=dum.THR[[du]][[f]]
            fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
            thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
            
            fal.mean <- ddply(subset(fal,!Score=="N/A"), c("Group"), summarise,   
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            thr.mean <- ddply(subset(thr,!Score=="N/A"), c("Group"), summarise,   
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            
            #plot fal
            fn.plt(dd=fal.mean,dd1=NULL)
            if(f==1) mtext("Silky shark",3,cex=1.25)
            
            #plot thr
            fn.plt(dd=thr.mean,dd1=NULL)
            Tit=names(dum.FAL[[du]])[f]
            #sapply(strsplit(filenames, "_"), "[", 2)
            mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
            if(f==1) mtext("Thresher shark",3,cex=1.25)
          } 
          mtext("Score (mean ±SE)",2,outer=T,cex=1.25,las=3,line=.25)
          dev.off()
          
          
          #plot mean scores and each reviewer
          dummy.flat.tab.f.fal=dummy.flat.tab.f.thr=vector('list',length(dum.FAL[[du]]))   #NEW
          for(f in 1:length(dum.FAL[[du]]))
          {
            fn.fig(paste(getwd(),"/outputs/Paper/MeanScore_and_reviewer/Figure.",du,".",Lista.show[l],"_with_reviewers","_Q.",f,sep=""),3000,1500)
            
            par(mfrow=c(1,2),mar=c(1,1,.5,.25),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
            
            fal=dum.FAL[[du]][[f]]
            thr=dum.THR[[du]][[f]]
            fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
            thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
            
            fal.mean <- ddply(subset(fal,!Score=="N/A"), c("Group"), summarise,   
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            thr.mean <- ddply(subset(thr,!Score=="N/A"), c("Group"), summarise,   
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            
            #plot fal
            fn.plt.all.rev(dd=fal.mean,dd1=NULL)
            mtext("Silky shark",3,cex=1.25)
            fun.add.rev(Add.rev=fal,CEX=.75)
            
            #plot thr
            fn.plt.all.rev(dd=thr.mean,dd1=NULL)
            fun.add.rev(Add.rev=thr,CEX=.75)
            
            Tit=names(dum.FAL[[du]])[f]
            mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
            mtext("Thresher shark",3,cex=1.25)
            mtext("Score (mean ±SE)",2,outer=T,cex=1.25,las=3,line=.25)
            dev.off()
            
            dummy.flat.tab.f.fal[[f]]=cbind(Question=Tit,subset(fal,select=c(Reviewer,Group,Score)))
            dummy.flat.tab.f.thr[[f]]=cbind(Question=Tit,subset(thr,select=c(Reviewer,Group,Score)))
          } 
          
          #NEW
          dummy.flat.tab.f.fal=do.call(rbind,dummy.flat.tab.f.fal)
          dummy.flat.tab.f.thr=do.call(rbind,dummy.flat.tab.f.thr)
          
          dummy.flat.tab.f.fal$Group=with(dummy.flat.tab.f.fal,
                                          ifelse(Group=="1.    Proposal Authors","Proposal",
                                                 ifelse(Group=="2.    FAO Expert Panel","FAO",
                                                        ifelse(Group%in%c("3.    IUCN - TRAFFIC Panel",
                                                                          "3.    IUCN - TRAFFIC Panel"),"IUCN",
                                                               ifelse(Group=="4.    CITES Secretariat","CITES",NA
                                                               )))))
          dummy.flat.tab.f.fal$Group=with(dummy.flat.tab.f.fal,paste(Question,Group))
          
          dummy.flat.tab.f.thr$Group=with(dummy.flat.tab.f.thr,
                                          ifelse(Group=="1.    Proposal Authors","Proposal",
                                                 ifelse(Group=="2.    FAO Expert Panel","FAO",
                                                        ifelse(Group%in%c("3.    IUCN - TRAFFIC Panel",
                                                                          "3.    IUCN - TRAFFIC Panel"),"IUCN",
                                                               ifelse(Group=="4.    CITES Secretariat","CITES",NA
                                                               )))))
          dummy.flat.tab.f.thr$Group=with(dummy.flat.tab.f.thr,paste(Question,Group))
          
          dummy.flat.tab.fal[[du]]=subset(dummy.flat.tab.f.fal,select=-Question)
          dummy.flat.tab.thr[[du]]=subset(dummy.flat.tab.f.thr,select=-Question)
          #NEW
          
          
          
          
          #by area of expertise
          fn.fig(paste(getwd(),"/outputs/Paper/Figure.",du,".",Lista.show[l],"_by.expertise",sep=""),wid,Hei)
          if(du==1)par(mfrow=c(2,2),mar=c(1,1,.5,.25),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
          if(du==2)par(mfrow=c(6,2),mar=c(1,1,.5,.25),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
          for(f in 1:length(dum.FAL[[du]]))
          {
            fal=dum.FAL[[du]][[f]]
            thr=dum.THR[[du]][[f]]
            
            fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
            thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
            
            fal.mean <- ddply(fal, c("Group", "Field"), summarise,   
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            thr.mean <- ddply(thr, c("Group", "Field"), summarise,   
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            
            #fal
            fn.plt(dd=subset(fal.mean,Field=="Fisheries"),dd1=subset(fal.mean,Field=="Environment"))
            if(f==1) mtext("Silky shark",3,cex=1.25)
            if(f==1)
            {
              if(du==1) legend("topleft", c("Fisheries","Environment"),bty='n',pch=19,col=c("black","grey60"))  
              if(du==2) legend("bottomleft", c("Fisheries","Environment",""),bty='n',pch=19,col=c("black","grey60","transparent"))  
            }
            
            #thr
            fn.plt(dd=subset(thr.mean,Field=="Fisheries"),dd1=subset(thr.mean,Field=="Environment"))
            Tit=names(dum.FAL[[du]])[f]
            mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
            
            if(f==1) mtext("Thresher shark",3,cex=1.25)
          } 
          mtext("Score (mean ±SE)",2,outer=T,cex=1.25,las=3,line=.25)
          dev.off()
        }
        
        dummy.flat.tab.fal=do.call(rbind,dummy.flat.tab.fal)
        dummy.flat.tab.thr=do.call(rbind,dummy.flat.tab.thr)
        
        Flat.tab.fal[[1]]=reshape(dummy.flat.tab.fal,
                                  v.names = "Score", idvar = "Reviewer",
                                  timevar = "Group", direction = "wide")
        
        Flat.tab.thr[[1]]=reshape(dummy.flat.tab.thr,
                                  v.names = "Score", idvar = "Reviewer",
                                  timevar = "Group", direction = "wide")
        
      }
      
      if(l==4)
      {
        
        #combined
        fn.fig(paste(getwd(),"/outputs/Paper/Figure.3.",Lista.show[l],sep=""),2000,2400)
        par(mfrow=c(2,2),mar=c(1,1,.5,.5),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
        cex.LB=1.25
        cXQ=1
        Txt.ln=0.25
        for(f in 1:length(FAL))
        {
          fal=FAL[[f]]
          thr=THR[[f]]
          fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
          thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
          
          fal.mean <- ddply(subset(fal,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          thr.mean <- ddply(subset(thr,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          
          #plot fal
          fn.plt(dd=fal.mean,dd1=NULL)
          if(f==1) mtext("Silky shark",3,cex=1.25)
          
          #plot thr
          fn.plt(dd=thr.mean,dd1=NULL)
          Tit=names(FAL)[f]
          mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
          if(f==1) mtext("Thresher shark",3,cex=1.25)
        } 
        mtext("Score (mean ±SE)",2,outer=T,cex=1.25,las=3,line=.25)
        dev.off()
        
        
        #plot mean scores and each reviewer
        dummy.flat.tab.fal=dummy.flat.tab.thr=vector('list',length(FAL))
        for(f in 1:length(FAL))
        {
          fn.fig(paste(getwd(),"/outputs/Paper/MeanScore_and_reviewer/Figure.3.",Lista.show[l],"_with_reviewers","_Q.",f,sep=""),3000,1500)
          
          par(mfrow=c(1,2),mar=c(1,1,.5,.25),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
          
          fal=FAL[[f]]
          thr=THR[[f]]
          fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
          thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
          
          fal.mean <- ddply(subset(fal,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          thr.mean <- ddply(subset(thr,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          
          #plot fal
          fn.plt.all.rev(dd=fal.mean,dd1=NULL)
          mtext("Silky shark",3,cex=1.25)
          fun.add.rev(Add.rev=fal,CEX=.75)
          
          #plot thr
          fn.plt.all.rev(dd=thr.mean,dd1=NULL)
          fun.add.rev(Add.rev=thr,CEX=.75)
          
          Tit=names(FAL)[f]
          mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
          mtext("Thresher shark",3,cex=1.25)
          mtext("Score (mean ±SE)",2,outer=T,cex=1.25,las=3,line=.25)
          dev.off()
          
          dummy.flat.tab.fal[[f]]=cbind(Question=Tit,subset(fal,select=c(Reviewer,Group,Score)))
          dummy.flat.tab.thr[[f]]=cbind(Question=Tit,subset(thr,select=c(Reviewer,Group,Score)))
          
        } 
        
        dummy.flat.tab.fal=do.call(rbind,dummy.flat.tab.fal)
        dummy.flat.tab.thr=do.call(rbind,dummy.flat.tab.thr)
        
        dummy.flat.tab.fal$Group=with(dummy.flat.tab.fal,
                                      ifelse(Group=="1.    Proposal Authors","Proposal",
                                             ifelse(Group=="2.    FAO Expert Panel","FAO",
                                                    ifelse(Group%in%c("3.    IUCN - TRAFFIC Panel",
                                                                      "3.    IUCN - TRAFFIC Panel"),"IUCN",
                                                           ifelse(Group=="4.    CITES Secretariat","CITES",NA
                                                           )))))
        dummy.flat.tab.fal$Group=with(dummy.flat.tab.fal,paste(Question,Group))
        
        dummy.flat.tab.thr$Group=with(dummy.flat.tab.thr,
                                      ifelse(Group=="1.    Proposal Authors","Proposal",
                                             ifelse(Group=="2.    FAO Expert Panel","FAO",
                                                    ifelse(Group%in%c("3.    IUCN - TRAFFIC Panel",
                                                                      "3.    IUCN - TRAFFIC Panel"),"IUCN",
                                                           ifelse(Group=="4.    CITES Secretariat","CITES",NA
                                                           )))))
        dummy.flat.tab.thr$Group=with(dummy.flat.tab.thr,paste(Question,Group))
        
        dummy.flat.tab.fal=subset(dummy.flat.tab.fal,select=-Question)
        dummy.flat.tab.thr=subset(dummy.flat.tab.thr,select=-Question)
        
        Flat.tab.fal[[2]]=reshape(dummy.flat.tab.fal,
                                  v.names = "Score", idvar = "Reviewer",
                                  timevar = "Group", direction = "wide")
        
        Flat.tab.thr[[2]]=reshape(dummy.flat.tab.thr,
                                  v.names = "Score", idvar = "Reviewer",
                                  timevar = "Group", direction = "wide")
        
        
        #by area of expertise
        fn.fig(paste(getwd(),"/outputs/Paper/Figure.3.",Lista.show[l],"_by.expertise",sep=""),2000,2400)
        par(mfrow=c(2,2),mar=c(1,1,.5,.5),oma=c(.5,2,1,1.5),mgp=c(1,.5,0),par(las=1))
        cex.LB=1.25
        cXQ=1
        Txt.ln=0.25
        for(f in 1:length(FAL))
        {
          fal=FAL[[f]]
          thr=THR[[f]]
          fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
          thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
          fal.mean <- ddply(fal, c("Group", "Field"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          thr.mean <- ddply(thr, c("Group", "Field"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          
          #plot fal
          fn.plt(dd=subset(fal.mean,Field=="Fisheries"),dd1=subset(fal.mean,Field=="Environment"))
          if(f==1) mtext("Silky shark",3,cex=1.25)
          if(f==1) legend("topleft", c("Fisheries","Environment"),bty='n',pch=19,col=c("black","grey60"))
          
          #plot thr
          fn.plt(dd=subset(thr.mean,Field=="Fisheries"),dd1=subset(thr.mean,Field=="Environment"))
          Tit=names(FAL)[f]
          mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
          if(f==1) mtext("Thresher shark",3,cex=1.25)
        }
        mtext("Score (mean ±SE)",2,outer=T,cex=1.25,las=3,line=.25)
        dev.off()
      }
    } 
  }
  Flat.tab.fal=merge(Flat.tab.fal[[1]],Flat.tab.fal[[2]],by="Reviewer")
  Flat.tab.thr=merge(Flat.tab.thr[[1]],Flat.tab.thr[[2]],by="Reviewer")
  write.csv(Flat.tab.fal,paste(getwd(),
      "/outputs/Paper/MeanScore_and_reviewer/Scores_flat_table_fal.csv",sep=''),row.names=F)
  write.csv(Flat.tab.thr,paste(getwd(),
      "/outputs/Paper/MeanScore_and_reviewer/Scores_flat_table_thr.csv",sep=''),row.names=F)
  
  
  detach("package:plyr") #remove plyr as it stuffs up dplyr
  library(dplyr)
  
  #B. Ordinal data
  library(rms)
  library(stargazer)
  library(erer)
  library(MASS)
  
  #function for plotting data as should be
  fn.bar.plts.ok=function(D,PlusY)
  {
    out1 = D %>% mutate(Score=as.factor(ifelse(Score%in%c('N/A'),NA,Score)))%>%
      filter(!is.na(Score)) %>%
      group_by(Group,Field,Score) %>%
      summarise(n = length(N)) %>%
      mutate(prop=n/sum(n))%>%
      mutate(Group1=paste(substr(Group,1,1),Field,sep="."))%>%
      as.data.frame
    
    Add=out1[1:3,]
    Add[,]=NA
    Add$Group1=paste(1:3,'X',sep='.')
    Add$Score=0
    out1$Score=as.numeric(as.character(out1$Score))
    out1=rbind(out1,Add)
    out2=out1[,-match(c("n","Group","Field"),names(out1))]%>%
      spread('Group1','prop')%>% filter(!Score==0) %>%mutate_all(coalesce, 0)
    out2$`1.X`=out2$`2.X`=out2$`3.X`=NA
    
    DD=as.matrix(out2[,2:ncol(out2)])
    WID=c(1,1,.25)
    
    B=barplot(100*DD,names.arg=rep("",ncol(DD)),col=Fill.col,width=c(rep(WID,3),WID[1:2]))
    axis(1,c(mean(B[1:2]),mean(B[4:5]),mean(B[7:8]),mean(B[10:11])),TTI,
         line=1,col='white')
    axis(1,c(B[1:2],B[4:5],B[7:8],B[10:11]),rep(c("Env.","Fish."),4),cex.axis=.85)
    box()
    
    out3=out1[,-match(c("Group","Field","prop","Score"),names(out1))]%>%
      group_by(Group1) %>%summarise(n = sum(n))%>%spread('Group1','n')%>%
      as.data.frame
    out3=out3[,which(!is.na(out3))]
    text(c(B[1:2],B[4:5],B[7:8],B[10:11]),rep(100*PlusY,8),paste("N=",out3,sep=""),cex=.85)
  }
  
  #functions for plotting Kim's barplots
  fn.bar.plts.ok.kim=function(D,PlusY)
  {
    out1 = D %>% mutate(Score=as.factor(ifelse(Score%in%c('N/A'),NA,Score)))%>%
      filter(!is.na(Score)) %>%
      group_by(Group,Q,Score) %>%
      summarise(n = length(N)) %>%
      mutate(prop=n/sum(n))%>%
      mutate(Group1=paste(substr(Group,1,1),Q,sep="."))%>%
      as.data.frame
    
    Add=out1[1:3,]
    Add[,]=NA
    Add$Group1=paste(1:3,'X',sep='.')
    Add$Score=0
    out1$Score=as.numeric(as.character(out1$Score))
    out1=rbind(out1,Add)
    out2=out1[,-match(c("n","Group","Q"),names(out1))]%>%
      spread('Group1','prop')%>% filter(!Score==0) %>%mutate_all(coalesce, 0)
    out2$`1.X`=out2$`2.X`=out2$`3.X`=NA
    
    DD=as.matrix(out2[,2:ncol(out2)])
    WID=c(1,1,.25)
    
    B=barplot(100*DD,names.arg=rep("",ncol(DD)),col=Fill.col,width=c(rep(WID,3),WID[1:2]))
    axis(1,c(mean(B[1:2]),mean(B[4:5]),mean(B[7:8]),mean(B[10:11])),TTI,
         line=1.25,col='white',cex.axis=1.2)
    axis(1,c(B[1:2],B[4:5],B[7:8],B[10:11]),rep(c("a","b"),4),cex.axis=1.1)
    box()
    
    out3=out1[,-match(c("Group","Q","prop","Score"),names(out1))]%>%
      group_by(Group1) %>%summarise(n = sum(n))%>%spread('Group1','n')%>%
      as.data.frame
    out3=out3[,which(!is.na(out3))]
  }
  fn.bar.plts.ok.kim_single=function(D,PlusY)
  {
    out1 = D %>% mutate(Score=as.factor(ifelse(Score%in%c('N/A'),NA,Score)))%>%
      filter(!is.na(Score)) %>%
      group_by(Group,Score) %>%
      summarise(n = length(N)) %>%
      mutate(prop=n/sum(n))%>%
      mutate(Group1=paste(substr(Group,1,1),sep="."))%>%
      as.data.frame
    
    out1$Score=as.numeric(as.character(out1$Score))
    
    out2=out1[,-match(c("n","Group"),names(out1))]%>%
      spread('Group1','prop')%>% filter(!Score==0) %>%mutate_all(coalesce, 0)
    
    
    DD=as.matrix(out2[,2:ncol(out2)])
    
    B=barplot(100*DD,names.arg=rep("",ncol(DD)),col=Fill.col)
    axis(1,B,TTI,line=0.5,col='white',cex.axis=1.2)
    box()
  }
  
  #Function for contigency table
  fn.cont.tab=function(d,Formula) ftable(xtabs(paste(Formula), data = d))
  
  #function for fitting proportional odds model
  fn.prop.odd.mdl=function(daT,EXPRT.path,Use.Field)
  {
    daT$Score <- as.ordered(daT$Score)
    daT$Field <- factor(daT$Field, levels=c("Environment","Fisheries"))
    daT$Group <- factor(daT$Group, levels=sort(unique(daT$Group)))
    
    # fit proportional odds model
    pom <- polr(Score ~ Group, data=daT, Hess=T)
    
    #stargazer(pom, type = "html", out=paste(EXPRT.path,".htm",sep=""))  #put model in nice table format
    
    # predict probabilities
    if(Use.Field=="YES")
    {
      Enviro.prob=predict(pom,newdata = data.frame(Field="Environment",Group=levels(daT$Group)),type="p")
      Fisheries.prob=predict(pom,newdata = data.frame(Field="Fisheries",Group=levels(daT$Group)),type="p")
      
      return(list(Enviro.prob=Enviro.prob,Fisheries.prob=Fisheries.prob,
                  pom=pom,daT=daT))
    }else
    {
      return(list(pom=pom,daT=daT))
    }
    
  }
  
  
  #prelim
  fn.prop.odd.mdl.preliminary=function(daT,EXPRT.path,Use.Field)
  {
    daT$Score <- as.ordered(daT$Score)
    daT$Field <- factor(daT$Field, levels=c("Environment","Fisheries"))
    daT$Group <- factor(daT$Group, levels=sort(unique(daT$Group)))
    daT$DataSet <- factor(daT$DataSet, levels=sort(unique(daT$DataSet)))
    
    # fit proportional odds model
    pom <- polr(Score ~ Group+DataSet, data=daT, Hess=T)
    
    #stargazer(pom, type = "html", out=paste(EXPRT.path,".htm",sep=""))  #put model in nice table format
    
    # predict probabilities
    if(Use.Field=="YES")
    {
      Enviro.prob=predict(pom,newdata = data.frame(Field="Environment",Group=levels(daT$Group)),type="p")
      Fisheries.prob=predict(pom,newdata = data.frame(Field="Fisheries",Group=levels(daT$Group)),type="p")
      
      return(list(Enviro.prob=Enviro.prob,Fisheries.prob=Fisheries.prob,
                  pom=pom,daT=daT))
    }else
    {
      return(list(pom=pom,daT=daT))
    }
    
  }
  # get coefficients and p values
  ANOVA.pom=function(pom)
  {
    m1.coef <- data.frame(coef(summary(pom)))
    m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2),2)
    return(m1.coef)
  }
  
  #function for plotting predictions
  fn.bar.plts.preds=function(Env,Fish)
  {
    Env=as.data.frame(Env)
    Fish=as.data.frame(Fish)
    colnames(Env)=paste(colnames(Env),"Environment",sep=".")
    colnames(Fish)=paste(colnames(Fish),"Fisheries",sep=".")
    ADD=Env
    colnames(ADD)=paste(1:4,"X",sep=".")
    ADD[,]=NA
    DD=cbind(Env,Fish,ADD)
    DD=DD[,sort(names(DD))]
    WID=c(1,1,.25)
    B=barplot(as.matrix(DD),names.arg=rep("",ncol(DD)),col=Fill.col,width=c(rep(WID,3),WID[1:2]))
    axis(1,c(mean(B[1:2]),mean(B[4:5]),mean(B[7:8]),mean(B[10:11])),TTI,line=1,col='white')
    axis(1,c(B[1:2],B[4:5],B[7:8],B[10:11]),rep(c("Env.","Fish."),4),cex.axis=.85)
    box()
  }
  
  #Preliminary comparison original and final question scores
  ANOV.fal=vector('list',length(Lista.show))
  names(ANOV.fal)=Lista.show
  ANOV.thr=ANOV.fal
  
  for(l in 1:length(Lista.show))
  {
    if(Lista.show[l]%in%c("Species_char_and_status","Communication"))
    {
      FAL=dat.cleansed$FAL[[l]]
      THR=dat.cleansed$THR[[l]]
      
      FAL.first=dat.cleansed.first$FAL[[l]]
      THR.first=dat.cleansed.first$THR[[l]]
      
      #combine final and first data sets
      for(qq in 1:length(FAL))
      {
        FAL[[qq]]$DataSet='final'
        THR[[qq]]$DataSet='final'
        
        FAL.first[[qq]]$DataSet='first'
        THR.first[[qq]]$DataSet='first'
        
        FAL[[qq]]=rbind(FAL[[qq]],FAL.first[[qq]])
        THR[[qq]]=rbind(THR[[qq]],THR.first[[qq]])
      }
      
      if(l==2)
      {
        Fig1.FAL=FAL[match(c("Q.Species_1a","Q.Species_1b"),names(FAL))]
        Fig1.THR=THR[match(c("Q.Species_1a","Q.Species_1b"),names(THR))]
        Fig2.FAL=FAL[-match(c("Q.Species_1a","Q.Species_1b"),names(FAL))]
        Fig2.THR=THR[-match(c("Q.Species_1a","Q.Species_1b"),names(THR))]
        
        dum.FAL=list(Fig1.FAL,Fig2.FAL)
        dum.THR=list(Fig1.THR,Fig2.THR)
        
        fal.anova=thr.anova=dum.FAL
        for(du in 1:length(dum.FAL))
        {
          #2. Analyse data    
          # fn.fig(paste(getwd(),"/outputs/Paper/Figure.",du,".",Lista.show[l],"_mod.pred_by.expertise",sep=""),WII,HEI)
          if(du==1)par(mfrow=c(2,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
          if(du==2)par(mfrow=c(6,2),mar=c(1.25,1,2.5,1),oma=c(2,2,.35,1.5),mgp=c(1,.5,0),las=1,xpd=T)
          for(f in 1:length(dum.FAL[[du]]))
          {
            fal=dum.FAL[[du]][[f]]
            thr=dum.THR[[du]][[f]]
            
            #2.1. fit ordered regression model    
            #notes: justify odered log regression :https://en.wikipedia.org/wiki/Ordered_logit
            #source: https://www.princeton.edu/~otorres/LogitR101.pdf
            
            fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
            thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
            
            
            #fit model
            Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.",du,".",Lista.show[l],"_fal_preliminary_",names(dum.FAL[[du]])[f],sep="")
            Mod.fal=fn.prop.odd.mdl.preliminary(daT=subset(fal,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")
            daT=Mod.fal$daT
            fal.anova[[du]][[f]]=cbind(ANOVA.pom(Mod.fal$pom)[1:4,],Q=names(dum.FAL[[du]])[f])
            
            Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.",du,".",Lista.show[l],"_thr_preliminary_",names(dum.THR[[du]])[f],sep="")
            Modl.thr=fn.prop.odd.mdl.preliminary(daT=subset(thr,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")  
            daT=Modl.thr$daT
            thr.anova[[du]][[f]]=cbind(ANOVA.pom(Modl.thr$pom)[1:4,],Q=names(dum.THR[[du]])[f])
            
          }
        }
        
        a=c(fal.anova[[1]],fal.anova[[2]])
        names(a)=NULL
        fal.anova=do.call(rbind,a)
        a=c(thr.anova[[1]],thr.anova[[2]])
        names(a)=NULL
        thr.anova=do.call(rbind,a)
      }
      if(l==4)
      {
        #2. Analyse data  
        fal.anova=thr.anova=FAL
        for(f in 1:length(FAL))
        {
          fal=FAL[[f]]
          thr=THR[[f]]
          
          #2.1. fit ordered regression model    
          fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
          thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
          
          Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.3.",Lista.show[l],"_fal_preliminary_",names(FAL)[f],sep="")
          Mod.fal=fn.prop.odd.mdl.preliminary(daT=subset(fal,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")
          daT=Mod.fal$daT
          fal.anova[[f]]=cbind(ANOVA.pom(Mod.fal$pom)[1:4,],Q=names(fal.anova)[f])
          
          Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.3.",Lista.show[l],"_thr_preliminary_",names(THR)[f],sep="") 
          Modl.thr=fn.prop.odd.mdl.preliminary(daT=subset(thr,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")
          daT=Modl.thr$daT
          thr.anova[[f]]=cbind(ANOVA.pom(Modl.thr$pom)[1:4,],Q=names(thr.anova)[f])
          
        }
        a=fal.anova
        names(a)=NULL
        fal.anova=do.call(rbind,a)
        a=thr.anova
        names(a)=NULL
        thr.anova=do.call(rbind,a)
      }
      
      ANOV.fal[[l]]=fal.anova
      ANOV.thr[[l]]=thr.anova
    }
  }
  write.csv(do.call(rbind,ANOV.fal),paste(getwd(),"/outputs/Paper/ANOVA/ANOVA.fal_preliminary.csv",sep=""))
  write.csv(do.call(rbind,ANOV.thr),paste(getwd(),"/outputs/Paper/ANOVA/ANOVA.thr_preliminary.csv",sep=""))
  
  
  #final analysis
  ANOV.fal=vector('list',length(Lista.show))
  names(ANOV.fal)=Lista.show
  ANOV.thr=ANOV.fal
  
  for(l in 1:length(Lista.show))
  {
    if(Lista.show[l]%in%c("Species_char_and_status","Communication"))
    {
      FAL=dat.cleansed$FAL[[l]]
      THR=dat.cleansed$THR[[l]]
      if(l==2)
      {
        Fig1.FAL=FAL[match(c("Q.Species_1a","Q.Species_1b"),names(FAL))]
        Fig1.THR=THR[match(c("Q.Species_1a","Q.Species_1b"),names(THR))]
        Fig2.FAL=FAL[-match(c("Q.Species_1a","Q.Species_1b"),names(FAL))]
        Fig2.THR=THR[-match(c("Q.Species_1a","Q.Species_1b"),names(THR))]
        
        dum.FAL=list(Fig1.FAL,Fig2.FAL)
        dum.THR=list(Fig1.THR,Fig2.THR)
        
        fal.anova=thr.anova=dum.FAL
        for(du in 1:length(dum.FAL))
        {
          if(du==1)
          {
            wid=2000;Hei=2400
            cex.LB=1.25
            cXQ=1.25
            Txt.ln=0.25
          }
          if(du==2)
          {
            cex.LB=0.9
            cXQ=0.8
            Txt.ln=0.1
            wid=1200;Hei=2400
          }
          
          #1. plot data as should be
          TTI=c("Proposal","FAO","IUCN","CITES")
          if(du==1)
          {
            WII=2400
            HEI=2400
            PlusY=1.025 
          }
          if(du==2)
          {
            WII=2000
            HEI=2400
            PlusY=1.075
          } 
          fn.fig(paste(getwd(),"/outputs/Paper/Figure.",du,".",Lista.show[l],"_by.expertise_barplot",sep=""),WII,HEI)
          if(du==1)par(mfrow=c(2,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
          if(du==2)par(mfrow=c(6,2),mar=c(1.25,1,2.5,1),oma=c(2,2,.35,1.5),mgp=c(1,.5,0),las=1,xpd=T)
          for(f in 1:length(dum.FAL[[du]]))
          {
            fn.bar.plts.ok(D=dum.FAL[[du]][[f]],PlusY)
            if(f==1)
            {
              mtext("Silky shark                                         ",3,1,cex=1.25)
              if(du==1)legend("topright",paste(1:5),cex=1.15,horiz=T,fill=Fill.col,bty='n',inset=c(-0.05,-.15))
              if(du==2)legend("topright",paste(1:5),cex=1.15,horiz=T,fill=Fill.col,bty='n',inset=c(-0.035,-.425))
            }
            fn.bar.plts.ok(D=dum.THR[[du]][[f]],PlusY)
            if(f==1) mtext("Thresher shark                            ",3,1,cex=1.25)
            Tit=names(dum.FAL[[du]])[f]
            mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
          }
          mtext("Frequency",2,outer=T,cex=1.25,las=3,line=.7)
          dev.off()
          
          #Kim figures  
          if(du==1)   
          {
            dummy=dum.FAL[[du]]
            dummy$Q.Species_1a$Q="a"
            dummy$Q.Species_1b$Q="b"
            Agg.dum.FAL=list(Q.Species_1=rbind(dummy$Q.Species_1a,dummy$Q.Species_1b))       
            
            dummy=dum.THR[[du]]
            dummy$Q.Species_1a$Q="a"
            dummy$Q.Species_1b$Q="b"
            Agg.dum.THR=list(Q.Species_1=rbind(dummy$Q.Species_1a,dummy$Q.Species_1b)) 
            Plot.type=c("double")
          }
          if(du==2)   
          {
            dummy=dum.FAL[[du]]
            dummy$Q.Species_2a$Q=dummy$Q.Species_3a$Q="a"
            dummy$Q.Species_2b$Q=dummy$Q.Species_3b$Q="b"
            Agg.dum.FAL=list(Q.Species_2=rbind(dummy$Q.Species_2a,dummy$Q.Species_2b),
                             Q.Species_3=rbind(dummy$Q.Species_3a,dummy$Q.Species_3b),
                             Q.Species_4a=dummy$Q.Species_4a)       
            
            dummy=dum.THR[[du]]
            dummy$Q.Species_2a$Q=dummy$Q.Species_3a$Q="a"
            dummy$Q.Species_2b$Q=dummy$Q.Species_3b$Q="b"
            Agg.dum.THR=list(Q.Species_2=rbind(dummy$Q.Species_2a,dummy$Q.Species_2b),
                             Q.Species_3=rbind(dummy$Q.Species_3a,dummy$Q.Species_3b),
                             Q.Species_4a=dummy$Q.Species_4a) 
            Plot.type=c("double","double","single")
            
            #Q.5
            dumY.FAL=dum.FAL[[du]]$Q.Species_5
            dumY.THR=dum.THR[[du]]$Q.Species_5
          }
          
          if(du==1)HEI=2000
          if(du==2)HEI=2000
          fn.fig(paste(getwd(),"/outputs/Paper/Kim_Figure.",du,".",Lista.show[l],"_barplot",sep=""),WII,HEI)  
          if(du==1)par(mfrow=c(1,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
          if(du==2)par(mfrow=c(3,2),mar=c(1.25,1,2.5,1),oma=c(2,2.5,.35,1.5),mgp=c(1,.5,0),las=1,xpd=T)
          for(f in 1:length(Agg.dum.FAL))
          {
            if(Plot.type[f]=="double") fn.bar.plts.ok.kim(D=Agg.dum.FAL[[f]],PlusY)   
            if(Plot.type[f]=="single") fn.bar.plts.ok.kim_single(D=Agg.dum.FAL[[f]],PlusY)   
            if(f==1)
            {
              mtext("Silky shark                                         ",3,1,cex=1.25)
              if(du==1)legend("topright",paste(1:5),cex=1,horiz=T,fill=Fill.col,bty='n',inset=c(-0.065,-.1))
              if(du==2)legend("topright",paste(1:5),cex=1.3,horiz=T,fill=Fill.col,bty='n',inset=c(-0.035,-.23))
            }
            
            if(Plot.type[f]=="double")fn.bar.plts.ok.kim(D=Agg.dum.THR[[f]],PlusY)  
            if(Plot.type[f]=="single") fn.bar.plts.ok.kim_single(D=Agg.dum.THR[[f]],PlusY)  
            
            if(f==1) mtext("Thresher shark                            ",3,1,cex=1.25)
            Tit=names(Agg.dum.FAL)[f]   
            mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.5,cex=1.25,las=3)
          }
          mtext("Frequency",2,outer=T,cex=1.5,las=3,line=.675)
          dev.off()
          
          if(du==2)   
          {
            fn.fig(paste(getwd(),"/outputs/Paper/Kim_Figure.3.Other_barplot",sep=""),2400,2000)  
            par(mfrow=c(1,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2.5,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
            for(f in 1:1)
            {
              fn.bar.plts.ok.kim_single(D=dumY.FAL,PlusY)   
              if(f==1)
              {
                mtext("Silky shark                                         ",3,1,cex=1.25)
                legend("topright",paste(1:5),cex=1,horiz=T,fill=Fill.col,bty='n',inset=c(-0.065,-.1))
              }
              fn.bar.plts.ok.kim_single(D=dumY.THR,PlusY) 
              if(f==1) mtext("Thresher shark                            ",3,1,cex=1.25)
              mtext("Question 5",4,line=0.25,cex=1.25,las=3)
            }
            mtext("Frequency",2,outer=T,cex=1.5,las=3,line=.7)
            dev.off()
          }
          
          
          #2. Analyse data    
          # fn.fig(paste(getwd(),"/outputs/Paper/Figure.",du,".",Lista.show[l],"_mod.pred_by.expertise",sep=""),WII,HEI)
          if(du==1)par(mfrow=c(2,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
          if(du==2)par(mfrow=c(6,2),mar=c(1.25,1,2.5,1),oma=c(2,2,.35,1.5),mgp=c(1,.5,0),las=1,xpd=T)
          for(f in 1:length(dum.FAL[[du]]))
          {
            fal=dum.FAL[[du]][[f]]
            thr=dum.THR[[du]][[f]]
            
            #2.1. fit ordered regression model    
            #notes: justify odered log regression :https://en.wikipedia.org/wiki/Ordered_logit
            #source: https://www.princeton.edu/~otorres/LogitR101.pdf
            
            fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
            thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
            
            #cros tab
            Xtab.fal=fn.cont.tab(d=subset(fal,!is.na(Score)),Formula="~ Field+Group+Score")
            Xtab.thr=fn.cont.tab(d=subset(thr,!is.na(Score)),Formula="~ Field+Group+Score")
            
            #fit model
            Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.",du,".",Lista.show[l],"_fal_",names(dum.FAL[[du]])[f],sep="")
            Mod.fal=fn.prop.odd.mdl(daT=subset(fal,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")
            daT=Mod.fal$daT
            fal.anova[[du]][[f]]=cbind(ANOVA.pom(Mod.fal$pom)[1:4,],Q=names(dum.FAL[[du]])[f])
            
            Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.",du,".",Lista.show[l],"_thr_",names(dum.THR[[du]])[f],sep="")
            Modl.thr=fn.prop.odd.mdl(daT=subset(thr,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")  
            daT=Modl.thr$daT
            thr.anova[[du]][[f]]=cbind(ANOVA.pom(Modl.thr$pom)[1:4,],Q=names(dum.THR[[du]])[f])
            
            #2.2 plot model predictions
            #silky
            #fn.bar.plts.preds(Env=t(Mod.fal$Enviro.prob),Fish=t(Mod.fal$Fisheries.prob))
            # if(f==1)
            # {
            #   mtext("Silky shark                                         ",3,1,cex=1.25)
            #   if(du==1)legend("topright",paste(1:5),cex=1.15,horiz=T,fill=Fill.col,bty='n',inset=c(-0.05,-.15))
            #   if(du==2)legend("topright",paste(1:5),cex=1.15,horiz=T,fill=Fill.col,bty='n',inset=c(-0.035,-.425))
            # }
            # 
            #thresher
            #   fn.bar.plts.preds(Env=t(Modl.thr$Enviro.prob),Fish=t(Modl.thr$Fisheries.prob))
            #   if(f==1) mtext("Thresher shark                            ",3,1,cex=1.25)
            #   Tit=names(dum.FAL[[du]])[f]
            #   mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
          }
          #  mtext("Probability",2,outer=T,cex=1.25,las=3,line=.7)
          #  dev.off()
          
        }
        
        a=c(fal.anova[[1]],fal.anova[[2]])
        names(a)=NULL
        fal.anova=do.call(rbind,a)
        a=c(thr.anova[[1]],thr.anova[[2]])
        names(a)=NULL
        thr.anova=do.call(rbind,a)
      }
      if(l==4)
      {
        #1. plot data as should be
        TTI=c("Proposal","FAO","IUCN","CITES")
        PlusY=1.025
        fn.fig(paste(getwd(),"/outputs/Paper/Figure.3.",Lista.show[l],"_by.expertise_barplot",sep=""),2400,2400)
        par(mfrow=c(2,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
        for(f in 1:length(FAL))
        {
          fn.bar.plts.ok(D=FAL[[f]],PlusY)
          if(f==1)
          {
            mtext("Silky shark                                         ",3,1,cex=1.25)
            legend("topright",paste(1:5),cex=1.15,horiz=T,fill=Fill.col,bty='n',inset=c(-0.05,-.15))
          }
          fn.bar.plts.ok(D=THR[[f]],PlusY)
          if(f==1) mtext("Thresher shark                            ",3,1,cex=1.25)
          Tit=names(FAL)[f]
          mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=1,las=3)
        }
        mtext("Frequency",2,outer=T,cex=1.25,las=3,line=.7)
        dev.off()
        
        #Plot Kim's figures  
        dummy=FAL
        dummy$Q.Communication_1a$Q="a"
        dummy$Q.Communication_1b$Q="b"
        Agg.dum.FAL=list(Q.Communication_1=rbind(dummy$Q.Communication_1a,dummy$Q.Communication_1b))       
        
        dummy=THR
        dummy$Q.Communication_1a$Q="a"
        dummy$Q.Communication_1b$Q="b"
        Agg.dum.THR=list(Q.Communication_1=rbind(dummy$Q.Communication_1a,dummy$Q.Communication_1b)) 
        Plot.type=c("double")
        
        
        fn.fig(paste(getwd(),"/outputs/Paper/Kim_Figure.4.",Lista.show[l],"_barplot",sep=""),2400,2000)
        par(mfrow=c(1,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
        for(f in 1:length(Agg.dum.FAL))
        {
          fn.bar.plts.ok.kim(D=Agg.dum.FAL[[f]],PlusY)
          if(f==1)
          {
            mtext("Silky shark                                         ",3,1,cex=1.25)
            legend("topright",paste(1:5),cex=1,horiz=T,fill=Fill.col,bty='n',inset=c(-0.065,-.1))
          }
          
          fn.bar.plts.ok.kim(D=Agg.dum.THR[[f]],PlusY)
          if(f==1) mtext("Thresher shark                            ",3,1,cex=1.25)
          Tit=names(Agg.dum.FAL)[f]   #NEW
          mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=1.25,las=3)
        }
        mtext("Frequency",2,outer=T,cex=1.5,las=3,line=.675)
        dev.off()
        
        
        #2. Analyse data  
        fal.anova=thr.anova=FAL
        # fn.fig(paste(getwd(),"/outputs/Paper/Figure.3.",Lista.show[l],"_mod.pred_by.expertise",sep=""),2400,2400)
        #  par(mfrow=c(2,2),mar=c(1.25,1.25,2.5,.65),oma=c(2,2,.5,1),mgp=c(1,.5,0),las=1,xpd=T)
        for(f in 1:length(FAL))
        {
          fal=FAL[[f]]
          thr=THR[[f]]
          
          #2.1. fit ordered regression model    
          fal$Score=as.numeric(ifelse(fal$Score%in%c('N/A'),NA,fal$Score))
          thr$Score=as.numeric(ifelse(thr$Score%in%c('N/A'),NA,thr$Score))
          
          Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.3.",Lista.show[l],"_fal_",names(FAL)[f],sep="")
          Mod.fal=fn.prop.odd.mdl(daT=subset(fal,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")
          daT=Mod.fal$daT
          fal.anova[[f]]=cbind(ANOVA.pom(Mod.fal$pom)[1:4,],Q=names(fal.anova)[f])
          
          Path=paste(getwd(),"/outputs/Paper/ANOVA/Figure.3.",Lista.show[l],"_thr_",names(THR)[f],sep="") 
          Modl.thr=fn.prop.odd.mdl(daT=subset(thr,!is.na(Score)),EXPRT.path=Path,Use.Field="NO")
          daT=Modl.thr$daT
          thr.anova[[f]]=cbind(ANOVA.pom(Modl.thr$pom)[1:4,],Q=names(thr.anova)[f])
          
          #2.2 plot model predictions
          #silky
          # fn.bar.plts.preds(Env=t(Mod.fal$Enviro.prob),Fish=t(Mod.fal$Fisheries.prob))
          # if(f==1)
          # {
          #   mtext("Silky shark                                         ",3,1,cex=1.25)
          #   if(du==1)legend("topright",paste(1:5),cex=1.15,horiz=T,fill=Fill.col,bty='n',inset=c(-0.05,-.15))
          #   if(du==2)legend("topright",paste(1:5),cex=1.15,horiz=T,fill=Fill.col,bty='n',inset=c(-0.035,-.425))
          # }
          # 
          #thresher
          # fn.bar.plts.preds(Env=t(Modl.thr$Enviro.prob),Fish=t(Modl.thr$Fisheries.prob))
          # if(f==1) mtext("Thresher shark                            ",3,1,cex=1.25)
          # Tit=names(FAL)[f]
          # mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.25,cex=cXQ,las=3)
        }
        #   mtext("Probability",2,outer=T,cex=1.25,las=3,line=.7)
        #  dev.off()
        a=fal.anova
        names(a)=NULL
        fal.anova=do.call(rbind,a)
        a=thr.anova
        names(a)=NULL
        thr.anova=do.call(rbind,a)
      }
      
      ANOV.fal[[l]]=fal.anova
      ANOV.thr[[l]]=thr.anova
    }
    
    if(Lista.show[l]%in%c("Scientific_training"))   
    {
      nameS=c("Silky","Thresher")
      fn.fig(paste(getwd(),"/outputs/Paper/Appendix.",Lista.show[l],sep=""),2000,2400)
      par(mfcol=c(4,2),mar=c(1,1,.3,.5),oma=c(2,3,2,.1),mgp=c(1,.5,0),las=1)   
      for(s in 1:length(dat))
      {
        #Extract info
        D=dat[[s]]
        
        Store.Q.grouping=vector('list',1)
        for(Q.g in 1:length(Store.Q.grouping))
        {
          this.q=Analsd.q[which(names(Analsd.q)==Q.grouping[Q.g])]
          this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
          this.q=this.q[which(!this.q%in%this.q.comments)]
          id.questions=match(this.q,Questions)
          id.comments=match(this.q.comments,Questions)
          
          #loop over these ids  
          Store.q=vector('list',length(id.questions))
          names(Store.q)=Questions[id.questions]
          for(q in 1:length(id.questions))  
          {
            indx=id.questions[q]
            dd=D[[indx]]
            
            #Questions[id.questions[q]]=="Reviewer_qualifications" #already exported as csv
            
            if(Questions[id.questions[q]]=="Reviewer_field")
            {
              dd$Tick=with(dd,ifelse(Tick%in%c("X","x"),1,NA))
              
              #Experts are classed based on field of higher degree 
              Reviewr.exprtis=aggregate(Tick~Reviewer+Statement,dd,sum,na.rm=T)
              Reviewr.exprtis$Field=with(Reviewr.exprtis,
                                         ifelse(Statement=="Conservation of the natural environment",'Environment',
                                                ifelse(Statement=="Sustainable use and management of natural resources","Fisheries",NA)))
              
              #reclass experts based on Kim's request
              Reviewr.exprtis$Field=with(Reviewr.exprtis,ifelse(Reviewer%in%c('Frisk','Fisk','Kingma','Walsh'),'Environment',Field))
            }
            if(Questions[id.questions[q]]%in%Score.q)   
            {
              names(dd)=c("Reviewer","Group","Score")
              dd$dummy=substr(dd$Group,1,2)
              dd=subset(dd,dummy%in%paste(as.character(1:4),".",sep=""))
              dd=dd[,-match('dummy',names(dd))]
              dd$N=1
              dd$Score=with(dd,ifelse(Score=="3 - Assuming that M.S. Adam is lead author.",3,
                                      ifelse(Score=="4-5",4,Score)))
              dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
              dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
              
              #add reviewer expertise
              dd=merge(dd,Reviewr.exprtis,by="Reviewer",all.x=T)
              
              dd1=aggregate(N~Group+Score,dd,sum)
              sCOREs=1:5 #add missing scores
              id=sCOREs[which(!sCOREs%in%unique(dd1$Score))]
              if(length(id)>0)
              {
                Dumy=dd1[1:length(id),]
                Dumy$Score=id
                Dumy$N=NA
                dd1=rbind(Dumy,dd1)
                dd1=dd1[order(dd1$Score),]
              }
              dd1=reshape(dd1, v.names="N",idvar="Group",timevar="Score",direction="wide")
              dd1=dd1[order(dd1$Group),]
              colnames(dd1)[-1]=substr(colnames(dd1)[-1],3,10)
              Store.q[[q]]=cbind(Question=Questions[id.questions[q]],dd1)  
            }  
            if(Questions[id.questions[q]]=="Q.Process_2")   
            {
              names(dd)=c("Reviewer","Group","Score")
              dd$N=1
              dd=subset(dd,!Group=="Range of fishery biology and population dynamics expertise of information contributors")
              dd$Score=with(dd,ifelse(Score=="2-3 - Assuming that M.S. Adam is lead author.",2,
                                      ifelse(Score%in%c("4-5","unknown, but 4 implied"),4,
                                             ifelse(Score=="unknown, but 4 implied",4,                   
                                                    ifelse(Score%in%c("5 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                      "5 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)"),5, 
                                                           ifelse(Score%in%c("4 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)",
                                                                             "4 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)"),4,
                                                                  ifelse(Score%in%c("3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)","unknown, but 3 implied",
                                                                                    "3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)"),3,                   
                                                                         Score)))))))
              dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
              dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
              
              SplT=unique(dd$Group)
              SplT=SplT[match(c("2a. Range of fishery biology and population dynamics expertise of information contributors",
                                "2b. Range of fishery management expertise of information contributors",
                                "2c. Range of conservation biology expertise of information contributors",
                                "2d. Range of trade expertise of information contributors"),SplT)]
              
              TITL=c("Fishery biology and pop. dyn. expertise",
                     "Fishery management expertise",
                     "Conservation biology expertise",
                     "Trade expertise")
              Store.SplT=vector('list',length(SplT))
              names(Store.SplT)=TITL
              for(tt in 1:length(SplT))
              {
                Id=which(dd$Group==SplT[tt])
                Id_plus4=Id+4
                IId=vector('list',length(Id))
                for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
                a=dd[unlist(IId),]
                
                #add reviewer expertise
                a=merge(a,Reviewr.exprtis,by="Reviewer",all.x=T)
                dd1=aggregate(N~Group+Score,a,sum)
                #add missing scores
                sCOREs=1:5 
                id=sCOREs[which(!sCOREs%in%unique(dd1$Score))]
                if(length(id)>0)
                {
                  Dumy=dd1[1:length(id),]
                  Dumy$Score=id
                  Dumy$N=NA
                  dd1=rbind(Dumy,dd1)
                  dd1=dd1[order(dd1$Score),]
                }
                dd1=reshape(dd1, v.names="N",idvar="Group",timevar="Score",direction="wide")
                dd1=dd1[order(dd1$Group),]
                colnames(dd1)[-1]=substr(colnames(dd1)[-1],3,10)
                Store.SplT[[tt]]=cbind(Question=TITL[tt],dd1)
              }
              Store.q[[q]]=do.call(rbind,Store.SplT)
            }
          }
          Store.Q.grouping[[Q.g]]=do.call(rbind,Store.q)
          row.names(Store.Q.grouping[[Q.g]])=NULL
          
        }
        names(Store.Q.grouping)=Q.grouping[1]
        Store.Q.grouping.names=nameS[s]
        this=c(1)
        Grup.names=c("Proposal","FAO","IUCN","CITES")
        COLS=c("white","grey90","grey70","grey40","black")
        for(t in this)
        {
          a=Store.Q.grouping[[t]]
          a$Nm=with(a,ifelse(Question=="Q.Process_1","Q. 1",
                             ifelse(Question=="Fishery biology and pop. dyn. expertise","Q. 2a",
                                    ifelse(Question=="Fishery management expertise","Q. 2b",
                                           ifelse(Question=="Conservation biology expertise","Q. 2c",
                                                  "Q. 2d")))))
          a$Group=with(a,ifelse(Group=="2.    FAO Expert Advisory Panel",
                                "2.    FAO Expert Panel",ifelse(Group=="4.    CITES Secretariat Advice",
                                                                "4.    CITES Secretariat",Group)))
          Grups=unique(a$Group)
          
          for(g in 1:length(Grups))
          {
            dd=subset(a,Group==Grups[g])
            datt=t(dd[,-match(c("Question","Group","N/A","Nm"),names(dd))])
            colnames(datt)=dd$Nm
            #colnames(datt)=paste("Q",1:ncol(datt),sep=".")
            m=barplot(datt,beside=T,xlab="",ylab="",axisnames=F,col=COLS,cex.axis=1.15)
            mtext(colnames(datt),1,at=m[3,],line=0.1,cex=0.7)
            if(t==1 & s==1) mtext(Grup.names[g],2,las=3,line=1.25,cex=1.25)
            box()
            if(g==1) mtext(Store.Q.grouping.names[t],3,line=0.25,cex=1.25)
            if(s==2 & g==1) legend("topleft",paste(1:5),xpd=TRUE,horiz=T,inset=c(0,0),cex=1.2, bty = "n", fill=COLS)
          }
        }
      }
      mtext("Processes",1,line=0.65,cex=1.5,outer=T)
      mtext("Frequency",2,line=1.25,cex=1.5,outer=T,las=3)
      dev.off()
    }
  }
  write.csv(do.call(rbind,ANOV.fal),paste(getwd(),"/outputs/Paper/ANOVA/ANOVA.fal.csv",sep=""))
  write.csv(do.call(rbind,ANOV.thr),paste(getwd(),"/outputs/Paper/ANOVA/ANOVA.thr.csv",sep=""))
  
  
  #Plot reviewer's interpretation of Author proposal's assessment
  fn.fig(paste(getwd(),"/outputs/Paper/Kim_Figure.5",sep=""),2000,2400)
  par(mfrow=c(4,2),mar=c(1.25,1.75,1.5,1),oma=c(1,3,.5,1),mgp=c(1,.5,0),las=1,xpd=T,cex.axis=1.25)
  for(q in 1:length(Q.c[[1]]))
  {
    fal=Q.c[[1]][[q]]
    thr=Q.c[[2]][[q]]
    
    if(names(Q.c[[1]])[q]=="Q.Species_1c") key=c("Low","LOW","low")
    if(names(Q.c[[1]])[q]%in%c("Q.Species_2c","Q.Species_3c")) key=c('not',
                                                                     'Not','cannot','insufficient',"No","not enough support",
                                                                     "no reason to consider")
    if(names(Q.c[[1]])[q]=="Q.Species_1c")KEY="TRUE" else
      KEY="FALSE"
    
    if(names(Q.c[[1]])[q]=="Q.Species_4b") fal$Assessment=with(fal,ifelse(Reviewer=="Campbell",
                                                                          subset(Q.c[[1]]$Q.Species_3c,Reviewer=="Campbell",select=Assessment),
                                                                          Assessment)) 
    fal$find.key=grepl(paste(key,collapse="|"), fal$Assessment)
    thr$find.key=grepl(paste(key,collapse="|"), thr$Assessment)
    
    fal$Agreement=with(fal,ifelse(find.key==KEY,"YES","NO"))
    thr$Agreement=with(thr,ifelse(find.key==KEY,"YES","NO"))
    
    fal$Agreement=factor(with(fal,ifelse(Assessment=="","N/A",Agreement)),levels=c("YES","NO","N/A"))
    thr$Agreement=factor(with(thr,ifelse(Assessment=="","N/A",Agreement)),levels=c("YES","NO","N/A"))
    
    TAB=table(fal$Agreement)
    TAB=100*TAB/sum(TAB)
    barplot(TAB,ylim=c(0,100))
    box()
    if(q==1) mtext("Silky shark",3,0.5,cex=1.25)
    
    
    TAB=table(thr$Agreement)
    TAB=100*TAB/sum(TAB)
    barplot(TAB,ylim=c(0,100))
    box()
    if(q==1) mtext("Thresher shark",3,0.5,cex=1.25)
    Tit=names(Q.c[[1]])[q]
    mtext(paste("Question ",sub(".*_","",Tit),sep=''),4,line=0.5,cex=1.25,las=3)  
  }
  mtext("Percentage agreement",2,outer=T,cex=1.5,las=3,line=1)
  dev.off()
  
}

#use this to change question names
# idx=match(c("Q.Species_1a", "Q.Species_1b", "Q.Species_2a", "Q.Species_2b", "Q.Species_3a", "Q.Species_3b",
#             "Q.Species_4a", "Q.Species_5"),names(dummy.q))
# if(!is.na(idx)[1])names(dummy.q)[idx]=c("Q.1a Productivity","Q.1b Productivity","Q.2a Decline","Q.2b Decline","Q.3a Decline",
#                                         "Q.3b Decline","Q.4a Decline","Q.5 Other factors")
# idx=match(c("Q.Communication_1a", "Q.Communication_1b"),names(dummy.q))
# if(!is.na(idx)[1])names(dummy.q)[idx]=c("Q.1a Communication","Q.1b Communication")



# 5. Output comments----------------------------------------------------------------------
for(s in 1:length(dat))
{
  D=dat[[s]]
  for(Q.g in 1:length(Q.grouping))
  {
    this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
    id.comments=match(this.q.comments,Questions)
    if(length(id.comments)>0)
    {
      for(q in 1:length(id.comments))
      {
        indx=id.comments[q]
        dd=D[[indx]]
        
        #Include comments if applicable
        if(Questions[id.comments[q]]%in%Analsd.q.comments)   
        { 
          nub="Group"
          names(dd)=c("Reviewer",nub,"Strengths","Room for Improvement")
          Show=c("Strengths","Room for Improvement")
          dd=dd[order(dd[,2]),]
          SH=subset(dd,select=c(nub,"Strengths","Room for Improvement"))
          SH=SH[-which(SH[,1]=="#"),]
          g=SH[!SH[,match(Show,names(SH))]%in%c("NA","","None","No further comment"),]
          g[,2]=sapply(lapply(g[,2], strwrap, width=90), paste, collapse="\n")
          
          write.csv(g,paste(getwd(),"/outputs/Paper/Comments/",names(dat)[s],"_",Questions[id.comments[q]],".csv",sep=""),row.names = F)
          
        } 
      }
    }
  }
}

output.comments.pdf="NO"
if(output.comments.pdf=='YES')
{
  for(s in 1:length(dat))
  {
    D=dat[[s]]
    pdf(paste(getwd(),"/outputs/Paper/Comments_",names(dat)[s],".pdf",sep=""))
    par(las=1)
    for(Q.g in 1:length(Q.grouping))
    {
      this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
      id.comments=match(this.q.comments,Questions)
      if(length(id.comments)>0)
      {
        for(q in 1:length(id.comments))
        {
          indx=id.comments[q]
          dd=D[[indx]]
          
          #Include comments if applicable
          if(Questions[id.comments[q]]%in%Analsd.q.comments)   
          { 
            par(mfcol=c(1,1))
            nub=paste(Questions[id.comments[q]],"Group",sep="_")
            names(dd)=c("Reviewer",nub,"Strengths","Room for Improvement")
            Show=c("Strengths","Room for Improvement")
            dd=dd[order(dd[,2]),]
            for(sh in 1:length(Show))
            {
              SH=subset(dd,select=c(nub,Show[sh]))
              SH=SH[-which(SH[,1]=="#"),]
              g=SH[!SH[,match(Show[sh],names(SH))]%in%c("NA",""),]
              g[,2]=sapply(lapply(g[,2], strwrap, width=90), paste, collapse="\n")
              rownames(g)=1:nrow(g)
              mytheme=ttheme_default(base_size = 9, base_colour = "black", base_family = "",
                                     parse = T, padding = unit(c(4,3), "mm"),
                                     core=list(fg_params=list(cex =.8,hjust=0, x=0)))
              if(Show[sh]=="Strengths") L.out=4 else L.out=8
              if(Show[sh]=="Strengths") Adjst=1.9 else Adjst=3.25
              CHUNKS=round(seq(1,nrow(g),length.out=L.out))
              for(ch in 1:(length(CHUNKS)-1))
              {
                droP=1
                if(ch==(length(CHUNKS)-1))droP=0
                chunk=CHUNKS[ch]:(CHUNKS[ch+1]-droP)
                table <- tableGrob(g[chunk,],theme = mytheme)
                h <- grobHeight(table)
                gt=table
                if(ch==1)
                {
                  title <- textGrob(Question_summary[id.questions[q]],
                                    y=unit(0.5,"npc") + Adjst*h,vjust=0, gp=gpar(fontsize=10))
                  gt <- gTree(children=gList(table, title))
                }
                grid.newpage()
                grid.draw(gt)
              }
            }
          } 
        }
      }
      
    }
    dev.off()
  }
}

