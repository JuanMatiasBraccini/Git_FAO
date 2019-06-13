#----SCRIPT FOR CREATING PERCENT DECLINE FIGURE ADAPTED FROM Nicholas K. Dulvy's CODE----

#load relevant library
library("dplyr")


# DATA SECTION ------------------------------------------------------------

setwd("C:\\Matias\\FAO\\cop18\\2019_FAOPanelGraph")  #define working directory where data is stored
files = list.files(pattern="*.csv")  #list all .csv files
myfiles = lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)) #read them into a single list
names(myfiles)=gsub(".csv", "", files)


# PARAMETERS SECTION ------------------------------------------------------

ref.point=c(70,80)  #define reference points
ref.point.col=list(grey.scale=c("grey80","grey60"),
                   color=c("yellow","orange")) #define ref points color



# PROCEDURE SECTION -------------------------------------------------------

# create relevant variables
nsp=length(myfiles)
for( i in 1:nsp)  myfiles[[i]]=myfiles[[i]]%>% mutate(ID = paste(Location,timespan,sep=": "))%>%
                                      filter(!Location=="")


#create plotting function 
fn=function(dat,cl,addline)
{
  nn=nrow(dat)
  plot(NA,NA,xlim = c(1,nn),ylim=c(102,-2),xlab="",ylab="",xaxt='n',tcl=-.25, yaxs="i",cex.axis=1.25) #plot dummy
  for(r in 1:length(ref.point)) polygon(x=c(0,nn+1,nn+1,0),y=c(102,102,ref.point[r],ref.point[r]),
                  col=cl[r],border=1)  #plot polygon
  points(1:length(dat$ID),dat$PercentDecline, pch=15,cex=2)
  text(1:length(dat$ID),ref.point[1]*.99,dat$Text_if_none,srt=90,adj=c(0,.1),cex=.95)
  if(NCHAR<Max.lower.margin)
    {axis(1,at= 1:nn, dat$ID, cex.axis=0.8, col=1, las=2,tcl=-.25)}else
  {
    AXs.lab1=paste(sapply(strsplit(dat$ID, ":"), "[[", 1),":",sep="")
    AXs.lab2=sapply(strsplit(dat$ID, ":"), "[[", 2)
    axis(1,at= 1:nn, AXs.lab1, cex.axis=0.8, col=1, las=2,tcl=-.25,padj=-.4)
    axis(1,at= 1:nn, AXs.lab2, cex.axis=0.8, col="transparent", las=2,padj=1)
  }
  mtext("Percent decline", side = 3, line =0, adj =0,cex=1.5)
  box()
  if(addline=="YES")
  {
    Areas=unique(dat$Area)
    for(ar in 1:length(Areas))
    {
      txt=Areas[ar]
      x.loc=which(dat$Area%in%txt)
      if(length(x.loc)<2) txt=paste(substr(txt,1,3),".",sep="")
      text(mean(x.loc),1,txt,cex=1.25)
      abline(v=mean(c(x.loc[length(x.loc)],x.loc[length(x.loc)]+1)))
    }
  }
}

#execute function and export figure
Add.line=rep("NO",length(myfiles))  #add geographic area if required
for(a in 1:length(Add.line)) if("Area"%in%names(myfiles[[a]])) Add.line[a]="YES"
  
Max.lower.margin=35  #max number of character for lower margin before splitting legend
for( i in 1:nsp)
{
  NCHAR=max(nchar(myfiles[[i]]$ID))
  if(NCHAR<Max.lower.margin) MAR=c(8,1,1,.5) else MAR=c(6,1,1,.5)
  for(k in 1:length(ref.point.col))
  {
    jpeg(file=paste("fig_",names(myfiles)[i],"_",names(ref.point.col)[k],".jpeg",sep=""),width=2400,height=2000,units="px",res=300)
    par(mfrow=c(1,1),mar=MAR,oma=c(2,1,.5,.25),mgp=c(3,.35,0),tcl=.25,las=1) #set figure margins
    fn(dat=myfiles[[i]],cl=ref.point.col[[k]],addline=Add.line[i])
    dev.off()
  }
}