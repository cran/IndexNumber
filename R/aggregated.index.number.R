aggregated.index.number<-function(x,base,type,name,opt.plot=FALSE,opt.summary=FALSE){


if (is.matrix(x)==FALSE){
message("Please, introduce a matrix with the prices of different products","\n")
}
if (is.character(base)==FALSE){
    message("Please, introduce the type of index number","\n")
}
if (is.character(type)==FALSE){
  message("Please, introduce the type of aggregate index number","\n")
}
if (is.character(name)==FALSE){
message("Please, introduce a name of the variable as a character","\n")
}
n.prod<-ncol(x)
ind.index<-matrix(0,ncol=ncol(x),nrow=nrow(x))
if (base=="serie"){
for (i in 1:ncol(x)){
ind.index[,i]<-index.number.serie(x[,i],name,opt.plot=FALSE,opt.summary=FALSE)[,3]
}
}
if (base=="chain"){
for (i in 1:ncol(x)){
ind.index[,i]<-index.number.chain(x[,i],name,opt.plot=FALSE,opt.summary=FALSE)[,3]
}
}

if (type=="arithmetic"){index.n.aggregate<-apply(ind.index,1,mean)}
if (type=="geometric"){index.n.aggregate<-apply(ind.index,1,prod)^(1/n.prod)}
if (type=="harmonic"){index.n.aggregate<-n.prod/apply(1/ind.index,1,sum)}
if (type=="BDutot"){
  ind.index<-apply(x,1,sum)
  index.n.aggregate<-index.number.serie(ind.index,name,opt.plot = FALSE, opt.summary = FALSE)[,3]
}


if (opt.plot==TRUE){
if (is.character(name)){
min.in<-min(min(index.n.aggregate)*0.9,100)
max.in<-max(max(index.n.aggregate)*1.1,100)
plot(index.n.aggregate,type="l",xaxt='n',
     ylim=c(min.in,max.in),xlab="Stages",ylab=name,col="red")
                  axis(side = 1, at =seq(1,length(x),by=1),labels=0:(length(x)-1))
abline(h=100,col="gray",lty=2)
}else{
message("Please, introduce a name for the representation of the variable", "\n")
}
}
name.vector<-c()
for (i in 1:n.prod){name.vector[i]<-paste(name,i)}

Stages<-0:(nrow(x)-1)
      table.index<-data.frame(cbind(Stages,x,index.n.aggregate))
colnames(table.index)<-c("Stages",name.vector,"Agg. index number")
row.names(table.index)<-c()

results<-table.index
message(" ", "\n")

message("Aggregate index number", "\n")
if (type=="arithmetic"){message("Arithmetic","\n")}
if (type=="geometric"){message("Geometric","\n")}
if (type=="harmonic"){message("Harmonic","\n")}
if (type=="BDutot"){message("Bradstreet-Dutot","\n")}


message(" ","\n")
if (opt.summary){
message("Summary","\n")
message("Min.=",min(index.n.aggregate[-1]),"\n")
message("Stage=",which.min(index.n.aggregate[-1]),"\n")
message("Max.=",max(index.n.aggregate[-1]),"\n")
message("Stage=",which.max(index.n.aggregate[-1]),"\n")
results<-list(summary(index.n.aggregate[-1]),results)
names(results)<-c("Summary","Agg. index number")
}
return(results)
}
