for(x in as.character(alldays$ITEM_NAME)) nlist[length(nlist)+1]<-paste(unlist(strsplit(x,",")),collapse="+")
ax<-aggregate(cbind(ITEM_LIST=ITEM_ID,ITEM_COUNT=ITEM_ID,ITEM_NAME=ITEM_NAME)~CHK_NUM,alldays,FUN=function(x) { return(paste(sort(union(NULL,x)),collapse="__")) })
ax$ITEM_COUNT<-unlist(lapply(strsplit(ax$ITEM_LIST,"__"),FUN=length))
agg<-aggregate(CHK_NUM~ITEM_COUNT,ax,FUN=length)
agg<-agg[order(agg$CHK_NUM,decreasing=TRUE),]
agg<-aggregate(CHK_NUM~ITEM_COUNT,ax,FUN=length)
agg<-agg[order(agg$CHK_NUM,decreasing=TRUE),]
agg<-aggregate(CHK_NUM~ITEM_LIST+ITEM_COUNT+ITEM_NAME,ax,FUN=length)
agg<-aggregate(CHK_NUM~ITEM_LIST,ax,FUN=length)
agg<-aggregate(CHK_NUM~ITEM_LIST+ITEM_COUNT+ITEM_NAME,ax,FUN=length)
agg<-agg[order(agg$CHK_NUM,decreasing=TRUE),]

ax<-aggregate(cbind(ITEM_LIST=ITEM_ID,ITEM_COUNT=ITEM_ID,ITEM_NAME=ITEM_NAME)~CHK_NUM,alldays,FUN=function(x) { return(paste(sort(union(NULL,x)),collapse="__")) })

a1<-aggregate(CHECK_COUNT~CHK_NUM+ITEM_ID,alldays,FUN=function(x) { return(c(mx=max(x),n=length(x))) })
a1<-cbind(a1,a1$CHECK_COUNT)
a1<-subset(a1,n==1 & mx==1)
a1$CHECK_COUNT<-NULL
a1<-aggregate(ITEM_ID~CHK_NUM, a1, FUN=function(x) { return(c(ITEM_ID=max(x), n=length(x))) })
a1<-cbind(a1,a1$ITEM_ID)
a1[[2]]<-NULL
a1<-subset(a1,n==1)

x1<-subset(axx[,c(1:2,5)],ITEM_COUNT>3)

x1<-melt(data.frame(t(apply(subset(axx[,c(1:2,5)],ITEM_COUNT==3), 1, 
                            FUN=function(x) { return(unlist(x)) }))),id=c("CHK_NUM","ITEM_COUNT"))
x2<-melt(data.frame(t(apply(subset(axx[,c(1:2,6)],ITEM_COUNT==3), 1, 
                            FUN=function(x) { return(unlist(x)) }))),id=c("CHK_NUM","ITEM_COUNT"))
x1$variable<-NULL
x2$variable<-NULL
names(x1)[3]<-"ITEM_PAIR"
names(x2)[3]<-"ITEM_WTS"
xx<-merge(x1,x2)

