  library(sva)
  library(preprocessCore)
  Args<-commandArgs()
  load("m6a.RData")
  rawDatafile<-Args[6]
  species<-Args[7]
  resultFile<-Args[8]
  rawData<-read.table(Args[6])
  if(species=='mouse')
  {
    batchlist=mousebatchlist
    baseData<-read.table("baseData\\base_mouse.txt")
  }else
  {
    batchlist=humanbatchlist
    baseData<-read.table("baseData\\base_human.txt")
  }

  #rawData<-read.table(rawDatafile)
  batchlist=humanbatchlist
  rawcolnames<-colnames(rawData)
  batchlist1<-batchlist
  batchlist2<-batchlist
  for(i in 1:length(rawcolnames))
  {
    batchlist1<-append(batchlist1,strsplit(rawcolnames[i],split = '_')[[1]][1])
    batchlist2<-append(batchlist2,strsplit(rawcolnames[i],split = '_')[[1]][2])
  }

  csif<-cbind(batchlist1,batchlist2)
  csif<-as.data.frame(csif)
  tsif<-t(csif)
  modcombat = model.matrix(~1,data = csif)
  id=rownames(baseData)
  ink1=data.frame(id,baseData)
  id=rownames(rawData)
  ink2=data.frame(id,rawData)
  mergeData<-merge(ink1,ink2,by="id",all=F)
  colname = (colnames(mergeData))
  name<-mergeData[,1]
  mergeData<-mergeData[,-1]
  mergeData<-apply(mergeData,2,as.numeric)
  row.names(mergeData)<-name
  mergeData<-mergeData[which(rowSums(mergeData)>0),]
  name<-row.names(mergeData)
  mergeData<-as.matrix(mergeData)
  mergeData<-normalize.quantiles(mergeData)
  modcombat = model.matrix(~1,data = csif)
  combat_edata = ComBat(dat=mergeData,batch=batchlist2,mod=modcombat,par.prior=TRUE,prior.plots=FALSE)
  combat_edata<-as.data.frame(combat_edata)
  row.names(combat_edata)<-name
  #combat_edata<-cbind(name,combat_edata)
  combat_edata<-combat_edata[,-1:-36]
  colname<-as.vector(colname)
  tsif<-colname[-1:-37]
  write.table(t(c("name",t(tsif))),resultFile,sep='\t',row.names=FALSE,col.names=FALSE,quote=F,append=TRUE)
  combat_edata<-as.data.frame(combat_edata)
  combat_edata<-cbind(name,combat_edata)
  write.table(combat_edata,resultFile,sep='\t',row.names=FALSE,col.names=FALSE,quote=F,append=TRUE)

