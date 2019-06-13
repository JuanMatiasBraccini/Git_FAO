##################################### NOT USED #####################################
#Columns Barplot option
do.column.barplot=FALSE
if(do.column.barplot)
{
  Store.Rev.Qual=vector('list',length(dat))
  names(Store.Rev.Qual)=names(dat)
  for(s in 1:length(dat))
  {
    #Extract infoctiona
    D=dat[[s]]
    Store.Q.grouping=vector('list',length(Q.grouping))
    
    for(Q.g in 1:length(Q.grouping))
    {
      this.q=Analsd.q[which(names(Analsd.q)==Q.grouping[Q.g])]
      this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
      this.q=this.q[which(!this.q%in%this.q.comments)]
      id.questions=match(this.q,Questions)
      id.comments=match(this.q.comments,Questions)
      
      #loop over these ids and plot together as stacked. Missing: do stats by question: fishers vs enviro (? other factors)
      Store.q=vector('list',length(id.questions))
      names(Store.q)=Questions[id.questions]
      for(q in 1:length(id.questions))  
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
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
          Store.Rev.Qual[[s]]=dd3
          
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
          if(show.Reviewer_field=="YES")
          {
            barplot(dd$Tick,names.arg=dd$Field,main=TITL,cex.main=CXmn)
            box()
          }
          
          Reviewr.exprtis=Reviewr.exprtis[,c('Reviewer','Field')]
        }
        if(Questions[id.questions[q]]%in%Score.q)   #ACA: make sure all dd$Group options included!! dd shoudl be 72
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
          
          Mean.all <- ddply(subset(dd,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          Mean.expertise <- ddply(subset(dd,!Score=="N/A"), c("Group", "Field"), summarise,
                                  N    = length(as.numeric(Score)),
                                  mean = round(mean(as.numeric(Score),na.rm=T),2),
                                  sd   = round(sd(as.numeric(Score),na.rm=T),2),
                                  se   = round(sd / sqrt(N),2))
          Mean.expertise=subset(Mean.expertise,!is.na(Field)) 
          
          #by area of expertise
          #Environment
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
        if(Questions[id.questions[q]]%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"))
        {
          names(dd)=c("Reviewer","Criteria","Assessment")
          g = sapply(lapply(as.character(dd$Assessment), strwrap, width=Tbl.expnsn), paste, collapse="\n")
          g=unique(g)
          g=subset(g,!g=="")
          write.csv(g,paste("outputs/Paper/",names(dat)[s],"_",Questions[id.questions[q]],".csv",sep=""))
        }
        if(Questions[id.questions[q]]=="Q.Process_2")   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd=subset(dd,!Group=="Range of fishery biology and population dynamics expertise of information contributors")
          #dd$dummy=substr(dd$Group,1,2)
          #dd=subset(dd,dummy%in%as.character(paste(1:4,".",sep="")))
          #dd=dd[,-match('dummy',names(dd))]
          
          
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
    names(Store.Q.grouping)=Q.grouping
    Store.Q.grouping.names=c("Scient. training","Species char. & status",      
                             "Species_char_and_status_pers_opn","Communication")
    
    #Plot figure
    fn.fig(paste("outputs/Paper/",names(dat)[s],sep=""),2400,2400)
    par(mfcol=c(4,3),mar=c(1,1,.3,.5),oma=c(2,2,2,.1),mgp=c(1,.5,0),las=1)   
    this=c(1,2,4)
    Grup.names=c("Authors","FAO","IUCN-TRAFFIC","CITES")
    COLS=c("white","grey85","black","grey65","grey30")
    for(t in this)
    {
      a=Store.Q.grouping[[t]]
      a$Group=with(a,ifelse(Group=="2.    FAO Expert Advisory Panel",
                            "2.    FAO Expert Panel",ifelse(Group=="4.    CITES Secretariat Advice",
                                                            "4.    CITES Secretariat",Group)))
      Grups=unique(a$Group)
      
      for(g in 1:length(Grups))
      {
        dd=subset(a,Group==Grups[g])
        datt=t(dd[,-match(c("Question","Group","N/A"),names(dd))])
        colnames(datt)=paste("Q",1:ncol(datt),sep=".")
        m=barplot(datt,beside=T,xlab="",ylab="",axisnames=F,col=COLS,cex.axis=1.15)
        mtext(colnames(datt),1,at=m[3,],line=0.1,cex=0.8)
        if(t==1) mtext(Grup.names[g],2,las=3,line=1.25,cex=1.25)
        box()
      }
      mtext(Store.Q.grouping.names[t],1,line=1.5,cex=1.25)
      
    }
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("top",paste(1:5),xpd=TRUE,horiz=T,inset=c(0,0),cex=1.5, bty = "n", fill=COLS)
    dev.off()
    
    
  }
  
}

#Inverted Barplot option
do.invert.barplot=FALSE
if(do.invert.barplot)
{
  fn.brplt=function(MAT.f,MAT.e,LGND,where,add.Fish,add.Env)
  {
    MIN=-max(MAT.e,na.rm=T)
    MAX=max(MAT.f,na.rm=T)
    
    #fisher
    barplot(MAT.f,beside=T,main="",cex.main=CXmn,ylim=c(MIN,MAX),yaxt='n',
            legend=c(LGND),args.legend=list(cex=1.2,x=where,bty="n"))
    if(add.Fish=="YES")text(1,min(MAT.f,na.rm=T),"Fisheries",cex=1.5,pos=4)
    
    #enviro
    barplot(-MAT.e,beside=T,main="",add=T,yaxt='n')
    if(add.Env=="YES")text(1,-min(MAT.e,na.rm=T),"Environment",cex=1.5,pos=4)
    abline(h=0)
    box()
    
    axis(2,seq(MIN,MAX,2),abs(seq(MIN,MAX,2)),las=1)
  }
  for(s in 1:length(dat))
  {
    D=dat[[s]]
    par(las=1)
    
    for(Q.g in 1:length(Q.grouping))
    {
      this.q=Analsd.q[which(names(Analsd.q)==Q.grouping[Q.g])]
      this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
      this.q=this.q[which(!this.q%in%this.q.comments)]
      id.questions=match(this.q,Questions)
      id.comments=match(this.q.comments,Questions)
      
      n.figs=length(id.questions)
      if(Q.grouping[Q.g]=="Scientific_training") n.figs=6
      if(Q.grouping[Q.g]=="Communication") n.figs=length(id.questions)+1
      
      if(!Q.g==3)
      {
        WIDTH=2400
        if(n.figs==3) WIDTH=1800
        fn.fig(paste("outputs/Paper/",names(dat)[s],"_",Q.g,"_",Q.grouping[Q.g],sep=""),WIDTH,2400)
        par(mfrow=n2mfrow(n.figs),mar=c(1,1,2,1),
            oma=c(3,3,.1,.1),mgp=c(1,.6,0),las=1,cex.axis=1.2)
      }
      
      #loop over these ids and plot together as stacked. Missing: do stats by question: fishers vs enviro (? other factors)
      for(q in 1:length(id.questions))  
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
        TITL=gsub("_"," ",Question_summary[id.questions[q]])
        #if(Q.g==2) TITL=unlist(strsplit(Question_summary[id.questions[q]], "_"))[3]
        if(Q.g==2) TITL=Q.g2title[q]
        if(Q.g==4) TITL=Q.g4title[q]
        
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
          barplot(as.matrix(dd3[,-1]),beside=T,main="",cex.main=CXmn,
                  legend=dd3$Field,args.legend=list(x="topleft",bty="n",cex=1.25))
          box()
          mtext(TITL,3,0.25,cex=CEX.x) 
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
          if(show.Reviewer_field=="YES")
          {
            barplot(dd$Tick,names.arg=dd$Field,main=TITL,cex.main=CXmn)
            box()
          }
          
          Reviewr.exprtis=Reviewr.exprtis[,c('Reviewer','Field')]
        }
        
        if(Questions[id.questions[q]]%in%Score.q)   
        {
          if(Q.g==1)TITL="Understanding of level of scientific training"
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
          
          Mean.all <- ddply(subset(dd,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          Mean.expertise <- ddply(subset(dd,!Score=="N/A"), c("Group", "Field"), summarise,
                                  N    = length(as.numeric(Score)),
                                  mean = round(mean(as.numeric(Score),na.rm=T),2),
                                  sd   = round(sd(as.numeric(Score),na.rm=T),2),
                                  se   = round(sd / sqrt(N),2))
          Mean.expertise=subset(Mean.expertise,!is.na(Field)) 
          
          #by area of expertise
          #Environment
          dd1=aggregate(N~Group+Score,subset(dd,Field=="Environment"),sum)
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
          dd1.env=dd1
          MAT.env=as.matrix(dd1.env[,-1])
          
          #Fisheries
          dd1=aggregate(N~Group+Score,subset(dd,Field=="Fisheries"),sum)
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
          dd1.fish=dd1
          dd1.fish=dd1.fish[1:4,]
          MAT.fish=as.matrix(dd1.fish[,-1])
          
          #combine barplots
          LGND=NULL
          Add.f=add.e="NO"
          if(Q.g%in%c(1))
          {
            LGND=gsub("[^[:alpha:][:space:]]","",dd1$Group[1:4])
            Add.f=add.e="YES"
          }
          
          fn.brplt(MAT.f=MAT.fish,MAT.e=MAT.env,LGND,where="topleft",add.Fish=Add.f,add.Env=add.e)
          mtext(TITL,3,0.25,cex=CEX.x)
          if(Q.g==2&q==1)
          {
            legend('topleft','Fisheries',bty='n',cex=1.25 )
            legend('bottomleft','Environment',bty='n',cex=1.25 )
          }
          if(Q.g%in%c(2,4)&q==length(id.questions))
          {
            plot.new()
            legend('center',gsub("[^[:alpha:][:space:]]","",dd1$Group[1:4]),bty='n',
                   fill=c("grey30","grey55","grey75","grey90"),cex=1.5)
          }
        }  
        
        if(Questions[id.questions[q]]%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"))
        {
          names(dd)=c("Reviewer","Criteria","Assessment")
          g = sapply(lapply(as.character(dd$Assessment), strwrap, width=Tbl.expnsn), paste, collapse="\n")
          g=unique(g)
          g=subset(g,!g=="")
          write.csv(g,paste("outputs/Paper/",names(dat)[s],"_",Questions[id.questions[q]],".csv",sep=""))
        }
        
        if(Questions[id.questions[q]]=="Q.Process_2")   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd=subset(dd,!Group=="Range of fishery biology and population dynamics expertise of information contributors")
          #dd$dummy=substr(dd$Group,1,2)
          #dd=subset(dd,dummy%in%as.character(paste(1:4,".",sep="")))
          #dd=dd[,-match('dummy',names(dd))]
          
          
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
          for(tt in 1:length(SplT))
          {
            Id=which(dd$Group==SplT[tt])
            Id_plus4=Id+4
            IId=vector('list',length(Id))
            for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
            a=dd[unlist(IId),]
            
            #add reviewer expertise
            a=merge(a,Reviewr.exprtis,by="Reviewer",all.x=T)
            
            
            #by area of expertise
            
            #Environment
            dd1=aggregate(N~Group+Score,subset(a,Field=="Environment"),sum)
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
            dd1.env=dd1
            MAT.env=as.matrix(dd1.env[,-1])
            
            #Fisheries
            dd1=aggregate(N~Group+Score,subset(a,Field=="Fisheries"),sum)
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
            
            dd1.fish=dd1
            MAT.fish=as.matrix(dd1.fish[,-1])
            
            fn.brplt(MAT.f=MAT.fish,MAT.e=MAT.env,LGND=NULL,where="topleft",add.Fish="NO",add.Env="NO")
            mtext(TITL[tt],3,0.25,cex=CEX.x)
          }
        }
        
      }
      if(!Q.g==3)
      {
        mtext("Frequency",2,1,las=3,cex=2,outer=T)
        mtext("Score",1,1.7,cex=2,outer=T) 
        dev.off()
      }
      
    }
  }
  
}


#horizontal Barplot option
do.horiz.barplot=FALSE
if(do.horiz.barplot)
{
  for(s in 1:length(dat))
  {
    D=dat[[s]]
    par(las=1)
    
    for(Q.g in 1:length(Q.grouping))
    {
      this.q=Analsd.q[which(names(Analsd.q)==Q.grouping[Q.g])]
      this.q.comments=Analsd.q.comments[which(names(Analsd.q.comments)==Q.grouping[Q.g])]
      this.q=this.q[which(!this.q%in%this.q.comments)]
      id.questions=match(this.q,Questions)
      id.comments=match(this.q.comments,Questions)
      
      n.figs=length(id.questions)
      if(Q.grouping[Q.g]=="Scientific_training") n.figs=6
      if(Q.grouping[Q.g]=="Communication") n.figs=length(id.questions)+1
      
      if(!Q.g==3)
      {
        WIDTH=2400
        if(n.figs==3) WIDTH=1800
        fn.fig(paste("outputs/Paper/",names(dat)[s],"_",Q.g,"_",Q.grouping[Q.g],sep=""),WIDTH,2400)
        par(mfrow=n2mfrow(n.figs),mar=c(1,1,2,1),
            oma=c(3,3,.1,.1),mgp=c(1,.6,0),las=1,cex.axis=1.2)
      }
      
      #loop over these ids and plot together as stacked. Missing: do stats by question: fishers vs enviro (? other factors)
      for(q in 1:length(id.questions))  
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
        TITL=gsub("_"," ",Question_summary[id.questions[q]])
        #if(Q.g==2) TITL=unlist(strsplit(Question_summary[id.questions[q]], "_"))[3]
        if(Q.g==2) TITL=Q.g2title[q]
        if(Q.g==4) TITL=Q.g4title[q]
        
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
          barplot(as.matrix(dd3[,-1]),beside=T,main="",cex.main=CXmn,
                  legend=dd3$Field,args.legend=list(x="topleft",bty="n",cex=1.25))
          box()
          mtext(TITL,3,0.25,cex=CEX.x) 
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
          if(show.Reviewer_field=="YES")
          {
            barplot(dd$Tick,names.arg=dd$Field,main=TITL,cex.main=CXmn)
            box()
          }
          
          Reviewr.exprtis=Reviewr.exprtis[,c('Reviewer','Field')]
        }
        
        if(Questions[id.questions[q]]%in%Score.q)  
        {
          if(Q.g==1)TITL="Understanding of level of scientific training"
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
          
          Mean.all <- ddply(subset(dd,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          Mean.expertise <- ddply(subset(dd,!Score=="N/A"), c("Group", "Field"), summarise,
                                  N    = length(as.numeric(Score)),
                                  mean = round(mean(as.numeric(Score),na.rm=T),2),
                                  sd   = round(sd(as.numeric(Score),na.rm=T),2),
                                  se   = round(sd / sqrt(N),2))
          Mean.expertise=subset(Mean.expertise,!is.na(Field)) 
          
          
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
          MAT=as.matrix(dd1[,-match(c("Group","N/A"),names(dd1))]) 
          NAs=sum(dd1$`N/A`,na.rm=T)
          
          LGND=NULL
          Add.f=add.e="NO"
          if(Q.g%in%c(1))
          {
            LGND=c(1:5)
            Add.f=add.e="YES"
          }
          fn.brplt(MAT=MAT,LGND=LGND,where="topleft",
                   XLAB=c("Prop. auth.","FAO","IUCN","CITES"))
          legend("topright",paste("N/A =",NAs),bty='n',cex=1.2)
          
          
          if(Q.g%in%c(2,4)&q==length(id.questions))
          {
            plot.new()
            legend('center',gsub("[^[:alpha:][:space:]]","",dd1$Group[1:4]),bty='n',
                   fill=c("grey30","grey55","grey75","grey90"),cex=1.5)
          }
        }  
        
        if(Questions[id.questions[q]]%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"))
        {
          names(dd)=c("Reviewer","Criteria","Assessment")
          g = sapply(lapply(as.character(dd$Assessment), strwrap, width=Tbl.expnsn), paste, collapse="\n")
          g=unique(g)
          g=subset(g,!g=="")
          write.csv(g,paste("outputs/Paper/",names(dat)[s],"_",Questions[id.questions[q]],".csv",sep=""))
        }
        
        if(Questions[id.questions[q]]=="Q.Process_2")   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd=subset(dd,!Group=="Range of fishery biology and population dynamics expertise of information contributors")
          #dd$dummy=substr(dd$Group,1,2)
          #dd=subset(dd,dummy%in%as.character(paste(1:4,".",sep="")))
          #dd=dd[,-match('dummy',names(dd))]
          
          
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
          for(tt in 1:length(SplT))
          {
            Id=which(dd$Group==SplT[tt])
            Id_plus4=Id+4
            IId=vector('list',length(Id))
            for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
            a=dd[unlist(IId),]
            
            #add reviewer expertise
            a=merge(a,Reviewr.exprtis,by="Reviewer",all.x=T)
            
            
            #by area of expertise
            
            #Environment
            dd1=aggregate(N~Group+Score,subset(a,Field=="Environment"),sum)
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
            dd1.env=dd1
            MAT.env=as.matrix(dd1.env[,-1])
            
            #Fisheries
            dd1=aggregate(N~Group+Score,subset(a,Field=="Fisheries"),sum)
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
            
            dd1.fish=dd1
            MAT.fish=as.matrix(dd1.fish[,-1])
            
            fn.brplt(MAT.f=MAT.fish,MAT.e=MAT.env,LGND=NULL,where="topleft",add.Fish="NO",add.Env="NO")
            mtext(TITL[tt],3,0.25,cex=CEX.x)
          }
        }
        
      }
      if(!Q.g==3)
      {
        mtext("Frequency",2,1,las=3,cex=2,outer=T)
        mtext("Score",1,1.7,cex=2,outer=T) 
        dev.off()
      }
      
    }
  }
}

#Preliminary
#pdf Plots for reviewers
do.pdf="NO"
if(do.pdf=='YES')
{
  for(s in 1:length(dat))
  {
    D=dat[[s]]
    if(add.kmnts.to.pdf=="YES")
    {
      pdf(paste("outputs/Questionnaire.summary_",names(dat)[s],".pdf",sep=""))
      par(las=1)
      for(q in 1:length(id.questions))
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
        if(Questions[id.questions[q]]=="Reviewer_qualifications")
        {
          par(mfcol=c(1,1))
          dd=subset(dd,!Qualification=="Other")  #only use max qualification
          dd$Quali=with(dd,ifelse(Qualification=="PhD",3,ifelse(Qualification=="Masters",2,1)))
          dd$Top.qual.fish=ifelse(dd$`Fisheries - Resource Use focus`%in%c("X","x"),1,NA)
          dd$Top.qual.env=ifelse(dd$`Environment - Biodiversityfocus`%in%c("X","x"),1,NA)
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
          barplot(as.matrix(dd3[,-1]),beside=T,main=Question_summary[id.questions[q]],cex.main=CXmn,
                  legend=dd3$Field,args.legend=list(x="topleft",bty="n"))
          box()
          mtext("Frequency",2,2,las=3,cex=1.5)
          mtext("Maximum qualification",1,2,cex=1.5) 
        }
        
        if(Questions[id.questions[q]]=="Reviewer_field")
        {
          par(mfcol=c(1,1))
          dd$Tick=with(dd,ifelse(Tick%in%c("X","x"),1,NA))
          
          #Experts are classed based on field of higher degree 
          Reviewr.exprtis=aggregate(Tick~Reviewer+Statement,dd,sum)
          Reviewr.exprtis$Field=with(Reviewr.exprtis,ifelse(Statement=="Conservation of the natural environment",'Conservation',"Fisheries"))
          
          #reclass experts based on Kim's request
          Reviewr.exprtis$Field=with(Reviewr.exprtis,ifelse(Reviewer%in%c('Fisk','Kingma','Walsh'),'Conservation',Field))
          
          dd=aggregate(Tick~Field,Reviewr.exprtis,sum)
          barplot(dd$Tick,names.arg=dd$Field,main=Question_summary[id.questions[q]],cex.main=CXmn)
          box()
          mtext("Frequency",2,2,las=3,cex=1.5)
          
          Reviewr.exprtis=Reviewr.exprtis[,c('Reviewer','Field')]
        }
        
        if(Questions[id.questions[q]]%in%Score.q)
        {
          nf <- layout(matrix(c(1,1,2,3),2,2,byrow = TRUE), widths=c(1,1), heights=c(1,1), respect=TRUE)
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd$Score=with(dd,ifelse(Score=="3 - Assuming that M.S. Adam is lead author.",3,
                                  ifelse(Score=="4-5",4,Score)))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          #add reviewer expertise
          dd=merge(dd,Reviewr.exprtis,by="Reviewer",all.x=T)
          
          Mean.all <- ddply(subset(dd,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          Mean.expertise <- ddply(subset(dd,!Score=="N/A"), c("Group", "Field"), summarise,
                                  N    = length(as.numeric(Score)),
                                  mean = round(mean(as.numeric(Score),na.rm=T),2),
                                  sd   = round(sd(as.numeric(Score),na.rm=T),2),
                                  se   = round(sd / sqrt(N),2))
          Mean.expertise=subset(Mean.expertise,!is.na(Field)) 
          
          
          
          #all
          dd1=aggregate(N~Group+Score,dd,sum)
          
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
          
          LGND=dd1$Group    
          LGND=subset(Mean.all,Group%in%LGND,select=c(Group,mean,se))
          LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
          barplot(as.matrix(dd1[,-1]),beside=T,main=Question_summary[id.questions[q]],cex.main=CXmn,ylim=c(0,12),
                  legend=LGND,args.legend=list(x="topleft",bty="n"))
          box()
          
          #by area of expertise
          
          #Conservation
          dd1=aggregate(N~Group+Score,subset(dd,Field=="Conservation"),sum)
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
          
          
          LGND=dd1$Group    
          LGND=subset(Mean.expertise,Group%in%LGND&Field=="Conservation",select=c(Group,mean,se))
          LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
          barplot(as.matrix(dd1[,-1]),beside=T,main="Conservation only",cex.main=CXmn,ylim=c(0,8),
                  legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
          box()
          
          #Fisheries
          dd1=aggregate(N~Group+Score,subset(dd,Field=="Fisheries"),sum)
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
          
          LGND=dd1$Group    
          LGND=subset(Mean.expertise,Group%in%LGND&Field=="Fisheries",select=c(Group,mean,se))
          LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
          
          
          barplot(as.matrix(dd1[,-1]),beside=T,main="Fisheries only",cex.main=CXmn,ylim=c(0,8),
                  legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
          box()
          
          mtext("Frequency",2,-2,las=3,cex=2,outer=T)
          mtext("Score",1,-1.5,cex=2,outer=T) 
        }  
        
        if(Questions[id.questions[q]]=="Q.Process_2")   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd$Score=with(dd,ifelse(Score=="2-3 - Assuming that M.S. Adam is lead author.",2,
                                  ifelse(Score%in%c("4-5","unknown, but 4 implied"),4,
                                         ifelse(Score=="unknown, but 4 implied",4,                   
                                                ifelse(Score%in%c("5 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                  "5 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)"),5, 
                                                       ifelse(Score%in%c("4 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)",
                                                                         "4 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)"),4,
                                                              ifelse(Score%in%c("3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)","unknown, but 3 implied"),3,                   
                                                                     Score)))))))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          SplT=unique(dd$Group)
          SplT=SplT[match(c("2a. Range of fishery biology and population dynamics expertise of information contributors",
                            "2b. Range of fishery management expertise of information contributors",
                            "2c. Range of conservation biology expertise of information contributors",
                            "2d. Range of trade expertise of information contributors"),SplT)]
          
          for(tt in 1:length(SplT))
          {
            nf <- layout(matrix(c(1,1,2,3),2,2,byrow = TRUE), widths=c(1,1), heights=c(1,1), respect=TRUE)
            Id=which(dd$Group==SplT[tt])
            Id_plus4=Id+4
            IId=vector('list',length(Id))
            for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
            a=dd[unlist(IId),]
            
            #add reviewer expertise
            a=merge(a,Reviewr.exprtis,by="Reviewer",all.x=T)
            
            
            Mean.all <- ddply(subset(a,!Score=="N/A"), c("Group"), summarise,   #NEW
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            Mean.expertise <- ddply(subset(a,!Score=="N/A"), c("Group", "Field"), summarise,
                                    N    = length(as.numeric(Score)),
                                    mean = round(mean(as.numeric(Score),na.rm=T),2),
                                    sd   = round(sd(as.numeric(Score),na.rm=T),2),
                                    se   = round(sd / sqrt(N),2))
            Mean.expertise=subset(Mean.expertise,!is.na(Field)) 
            
            
            #all
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
            
            LGND=dd1$Group    #NEW
            LGND=subset(Mean.all,Group%in%LGND,select=c(Group,mean,se))
            LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
            
            barplot(as.matrix(dd1[,-1]),beside=T,main=paste("Process_",SplT[tt],sep=""),cex.main=CXmn,ylim=c(0,12),
                    legend=LGND,args.legend=list(x="topleft",bty="n"))
            box()
            
            #by area of expertise
            
            #Conservation
            dd1=aggregate(N~Group+Score,subset(a,Field=="Conservation"),sum)
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
            
            LGND=dd1$Group    #NEW
            LGND=subset(Mean.expertise,Group%in%LGND&Field=="Conservation",select=c(Group,mean,se))
            LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
            
            barplot(as.matrix(dd1[,-1]),beside=T,main="Conservation only",cex.main=CXmn,ylim=c(0,8),
                    legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
            box()
            
            #Fisheries
            dd1=aggregate(N~Group+Score,subset(a,Field=="Fisheries"),sum)
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
            LGND=dd1$Group    #NEW
            LGND=subset(Mean.expertise,Group%in%LGND&Field=="Fisheries",select=c(Group,mean,se))
            LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
            barplot(as.matrix(dd1[,-1]),beside=T,main="Fisheries only",cex.main=CXmn,ylim=c(0,8),
                    legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
            box()
            
            mtext("Frequency",2,-2,las=3,cex=2,outer=T)
            mtext("Score",1,-1.5,cex=2,outer=T)  
          }
        }
        
        if(Questions[id.questions[q]]%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"))
        {
          par(mfcol=c(1,1))
          Cxs=CX.tbl
          if(Questions[id.questions[q]]=="Q.Species_4b") Cxs=.8
          names(dd)=c("Reviewer","Criteria","Assessment")
          g = sapply(lapply(as.character(dd$Assessment), strwrap, width=Tbl.expnsn), paste, collapse="\n")
          g=unique(g)
          g=subset(g,!g=="")
          mytheme=ttheme_default(base_size = 10, base_colour = "black", base_family = "",
                                 parse = T, padding = unit(Paddn, "mm"),
                                 core=list(fg_params=list(cex =Cxs,hjust=0, x=0)))
          table <- tableGrob(g,theme = mytheme)
          h <- grobHeight(table)
          title <- textGrob(Question_summary[id.questions[q]],
                            y=unit(0.5,"npc") + 1.9*h,vjust=0, gp=gpar(fontsize=10))
          gt <- gTree(children=gList(table, title))
          grid.newpage()
          grid.draw(gt)
        }
        
        #Include comments if applicable
        if(Questions[id.questions[q]]%in%Analsd.q.comments)   
        { 
          par(mfcol=c(1,1))
          names(dd)=c("Reviewer","Group","Strengths","Room for Improvement")
          Show=c("Strengths","Room for Improvement")
          for(sh in 1:length(Show))
          {
            SH=subset(dd,select=c("Group",Show[sh]))
            g=SH[!SH[,match(Show[sh],names(SH))]%in%c("NA",""),]
            g[,2]=sapply(lapply(g[,2], strwrap, width=90), paste, collapse="\n")
            rownames(g)=1:nrow(g)
            mytheme=ttheme_default(base_size = 9, base_colour = "black", base_family = "",
                                   parse = T, padding = unit(c(4,3), "mm"),
                                   core=list(fg_params=list(cex =.8,hjust=0, x=0)))
            if(Show[sh]=="Strengths") L.out=4 else L.out=8
            if(Show[sh]=="Strengths") Adjst=1.9 else Adjst=3.25
            if(Questions[id.questions[q]]=="Q.Species_2b_comments" & !Show[sh]=="Strengths") L.out=10
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
      dev.off()
    }
    
    if(add.kmnts.to.pdf=="NO")
    {
      #Export figures only as pdf
      pdf(paste("outputs/Questionnaire.summary_",names(dat)[s],".pdf",sep=""))
      par(las=1)
      for(q in 1:length(id.questions))
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
        if(Questions[id.questions[q]]=="Reviewer_qualifications")
        {
          par(mfcol=c(1,1))
          dd=subset(dd,!Qualification=="Other")  #only use max qualification
          dd$Quali=with(dd,ifelse(Qualification=="PhD",3,ifelse(Qualification=="Masters",2,1)))
          dd$Top.qual.fish=ifelse(dd$`Fisheries - Resource Use focus`%in%c("X","x"),1,NA)
          dd$Top.qual.env=ifelse(dd$`Environment - Biodiversityfocus`%in%c("X","x"),1,NA)
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
          barplot(as.matrix(dd3[,-1]),beside=T,main=Question_summary[id.questions[q]],cex.main=CXmn,
                  legend=dd3$Field,args.legend=list(x="topleft",bty="n"))
          box()
          mtext("Frequency",2,2,las=3,cex=1.5)
          mtext("Maximum qualification",1,2,cex=1.5) 
        }
        
        if(Questions[id.questions[q]]=="Reviewer_field")
        {
          par(mfcol=c(1,1))
          dd$Tick=with(dd,ifelse(Tick%in%c("X","x"),1,NA))
          
          #Experts are classed based on field of higher degree 
          Reviewr.exprtis=aggregate(Tick~Reviewer+Statement,dd,sum)
          Reviewr.exprtis$Field=with(Reviewr.exprtis,ifelse(Statement=="Conservation of the natural environment",'Conservation',"Fisheries"))
          
          #reclass experts based on Kim's request
          Reviewr.exprtis$Field=with(Reviewr.exprtis,ifelse(Reviewer%in%c('Fisk','Kingma','Walsh'),'Conservation',Field))
          
          dd=aggregate(Tick~Field,Reviewr.exprtis,sum)
          barplot(dd$Tick,names.arg=dd$Field,main=Question_summary[id.questions[q]],cex.main=CXmn)
          box()
          mtext("Frequency",2,2,las=3,cex=1.5)
          
          Reviewr.exprtis=Reviewr.exprtis[,c('Reviewer','Field')]
        }
        
        if(Questions[id.questions[q]]%in%Score.q)
        {
          nf <- layout(matrix(c(1,1,2,3),2,2,byrow = TRUE), widths=c(1,1), heights=c(1,1), respect=TRUE)
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd$Score=with(dd,ifelse(Score=="3 - Assuming that M.S. Adam is lead author.",3,
                                  ifelse(Score=="4-5",4,Score)))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          #add reviewer expertise
          dd=merge(dd,Reviewr.exprtis,by="Reviewer",all.x=T)
          
          Mean.all <- ddply(subset(dd,!Score=="N/A"), c("Group"), summarise,   
                            N    = length(as.numeric(Score)),
                            mean = round(mean(as.numeric(Score),na.rm=T),2),
                            sd   = round(sd(as.numeric(Score),na.rm=T),2),
                            se   = round(sd / sqrt(N),2))
          Mean.expertise <- ddply(subset(dd,!Score=="N/A"), c("Group", "Field"), summarise,
                                  N    = length(as.numeric(Score)),
                                  mean = round(mean(as.numeric(Score),na.rm=T),2),
                                  sd   = round(sd(as.numeric(Score),na.rm=T),2),
                                  se   = round(sd / sqrt(N),2))
          Mean.expertise=subset(Mean.expertise,!is.na(Field)) 
          
          
          
          #all
          dd1=aggregate(N~Group+Score,dd,sum)
          
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
          
          LGND=dd1$Group    
          LGND=subset(Mean.all,Group%in%LGND,select=c(Group,mean,se))
          LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
          barplot(as.matrix(dd1[,-1]),beside=T,main=Question_summary[id.questions[q]],cex.main=CXmn,ylim=c(0,12),
                  legend=LGND,args.legend=list(x="topleft",bty="n"))
          box()
          
          #by area of expertise
          
          #Conservation
          dd1=aggregate(N~Group+Score,subset(dd,Field=="Conservation"),sum)
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
          
          
          LGND=dd1$Group    
          LGND=subset(Mean.expertise,Group%in%LGND&Field=="Conservation",select=c(Group,mean,se))
          LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
          barplot(as.matrix(dd1[,-1]),beside=T,main="Conservation only",cex.main=CXmn,ylim=c(0,8),
                  legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
          box()
          
          #Fisheries
          dd1=aggregate(N~Group+Score,subset(dd,Field=="Fisheries"),sum)
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
          
          LGND=dd1$Group    
          LGND=subset(Mean.expertise,Group%in%LGND&Field=="Fisheries",select=c(Group,mean,se))
          LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
          
          
          barplot(as.matrix(dd1[,-1]),beside=T,main="Fisheries only",cex.main=CXmn,ylim=c(0,8),
                  legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
          box()
          
          mtext("Frequency",2,-2,las=3,cex=2,outer=T)
          mtext("Score",1,-1.5,cex=2,outer=T) 
        }  
        
        if(Questions[id.questions[q]]=="Q.Process_2")   
        {
          names(dd)=c("Reviewer","Group","Score")
          dd$N=1
          dd$Score=with(dd,ifelse(Score=="2-3 - Assuming that M.S. Adam is lead author.",2,
                                  ifelse(Score%in%c("4-5","unknown, but 4 implied"),4,
                                         ifelse(Score=="unknown, but 4 implied",4,                   
                                                ifelse(Score%in%c("5 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)",
                                                                  "5 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)"),5, 
                                                       ifelse(Score%in%c("4 (I know about three of the five reviewers so my score also reflects personal knowledge in addition to what is contained in the review)",
                                                                         "4 (I know about half of the panel members so my score also reflects personal knowledge in addition to what is contained in the expert panel report)"),4,
                                                              ifelse(Score%in%c("3 (since I don't know all the proponents, this is a perception based on what is contained in the text of the proposal)","unknown, but 3 implied"),3,                   
                                                                     Score)))))))
          dd$Score=with(dd,ifelse(!Score%in%c(1:5,NA),NA,Score))
          dd$Score=ifelse(is.na(dd$Score),"N/A",dd$Score)
          
          SplT=unique(dd$Group)
          SplT=SplT[match(c("2a. Range of fishery biology and population dynamics expertise of information contributors",
                            "2b. Range of fishery management expertise of information contributors",
                            "2c. Range of conservation biology expertise of information contributors",
                            "2d. Range of trade expertise of information contributors"),SplT)]
          
          for(tt in 1:length(SplT))
          {
            nf <- layout(matrix(c(1,1,2,3),2,2,byrow = TRUE), widths=c(1,1), heights=c(1,1), respect=TRUE)
            Id=which(dd$Group==SplT[tt])
            Id_plus4=Id+4
            IId=vector('list',length(Id))
            for(kk in 1:length(IId)) IId[[kk]]=seq((Id[kk]+1),Id_plus4[kk])
            a=dd[unlist(IId),]
            
            #add reviewer expertise
            a=merge(a,Reviewr.exprtis,by="Reviewer",all.x=T)
            
            
            Mean.all <- ddply(subset(a,!Score=="N/A"), c("Group"), summarise,   #NEW
                              N    = length(as.numeric(Score)),
                              mean = round(mean(as.numeric(Score),na.rm=T),2),
                              sd   = round(sd(as.numeric(Score),na.rm=T),2),
                              se   = round(sd / sqrt(N),2))
            Mean.expertise <- ddply(subset(a,!Score=="N/A"), c("Group", "Field"), summarise,
                                    N    = length(as.numeric(Score)),
                                    mean = round(mean(as.numeric(Score),na.rm=T),2),
                                    sd   = round(sd(as.numeric(Score),na.rm=T),2),
                                    se   = round(sd / sqrt(N),2))
            Mean.expertise=subset(Mean.expertise,!is.na(Field)) 
            
            
            #all
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
            
            LGND=dd1$Group    #NEW
            LGND=subset(Mean.all,Group%in%LGND,select=c(Group,mean,se))
            LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
            
            barplot(as.matrix(dd1[,-1]),beside=T,main=paste("Process_",SplT[tt],sep=""),cex.main=CXmn,ylim=c(0,12),
                    legend=LGND,args.legend=list(x="topleft",bty="n"))
            box()
            
            #by area of expertise
            
            #Conservation
            dd1=aggregate(N~Group+Score,subset(a,Field=="Conservation"),sum)
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
            
            LGND=dd1$Group    #NEW
            LGND=subset(Mean.expertise,Group%in%LGND&Field=="Conservation",select=c(Group,mean,se))
            LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
            
            barplot(as.matrix(dd1[,-1]),beside=T,main="Conservation only",cex.main=CXmn,ylim=c(0,8),
                    legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
            box()
            
            #Fisheries
            dd1=aggregate(N~Group+Score,subset(a,Field=="Fisheries"),sum)
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
            LGND=dd1$Group    #NEW
            LGND=subset(Mean.expertise,Group%in%LGND&Field=="Fisheries",select=c(Group,mean,se))
            LGND=paste(LGND$Group," (",LGND$mean," ± ",LGND$se,")",sep="")
            barplot(as.matrix(dd1[,-1]),beside=T,main="Fisheries only",cex.main=CXmn,ylim=c(0,8),
                    legend=LGND,args.legend=list(cex=.7,x="topleft",bty="n"))
            box()
            
            mtext("Frequency",2,-2,las=3,cex=2,outer=T)
            mtext("Score",1,-1.5,cex=2,outer=T)  
          }
        }
        
      }
      dev.off()
      
      #Export comments as tables
      for(q in 1:length(id.questions))
      {
        indx=id.questions[q]
        dd=D[[indx]]
        
        if(Questions[id.questions[q]]%in%c("Q.Species_1c","Q.Species_2c","Q.Species_3c","Q.Species_4b"))
        {
          names(dd)=c("Reviewer","Criteria","Assessment")  
          g = sapply(lapply(as.character(dd$Assessment), strwrap, width=Tbl.expnsn), paste, collapse="\n")
          g=unique(g)
          g=subset(g,!g=="")
          write.csv(data.frame(Comment=g),paste("outputs/",names(dat)[s],"_",Questions[id.questions[q]],"_comments.csv",sep=""),row.names=F)
        }
        
        #Include comments if applicable
        if(Questions[id.questions[q]]%in%Analsd.q.comments)   
        { 
          names(dd)=c("Reviewer","Group","Strengths","Room for Improvement")
          dd=dd[,-1]
          dd=dd[order(dd$Group),]
          write.csv(dd,paste("outputs/",names(dat)[s],"_",Questions[id.questions[q]],".csv",sep=""),row.names=F)
        } 
      }
    }
  }
}
