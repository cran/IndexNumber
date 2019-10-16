laspeyres.index.number <-
function(x,y,name,opt.plot=FALSE,opt.summary=FALSE){

if (is.matrix(x)==FALSE){
message("Please, introduce a matrix with the prices of different products","\n")
}
if (is.vector(y)==FALSE){
message("Please, introduce a matrix with the prices of different products","\n")
}
if (is.character(name)==FALSE){
message("Please, introduce a name of the variable as a character","\n")
}

xmat<-x
n.prod<-ncol(xmat)
ind.index<-matrix(0,ncol=ncol(xmat),nrow=nrow(xmat))

for (i in 1:nrow(x)){
ind.index[i,]<-x[i,]*y
}
ind.index.aux<-apply(ind.index,1,sum)
index.n.laspeyres<-index.number.serie(ind.index.aux,name,opt.plot=F,opt.summary=F)[,3]


if (opt.plot==TRUE){
if (is.character(name)){
min.in<-min(min(index.n.laspeyres)*0.9,100)
max.in<-max(max(index.n.laspeyres)*1.1,100)
plot(index.n.laspeyres,type="l",xaxt='n',ylim=c(min.in,max.in),xlab="Stages",ylab=name,col="red")
                  axis(side = 1, at =seq(1,length(x),by=1),labels=0:(length(x)-1))
abline(h=100,col="gray",lty=2)
}else{
message("Please, introduce a name for the representation of the variable", "\n")
}
}
name.vector<-c()
for (i in 1:n.prod){name.vector[i]<-paste(name,i)}

Stages<-0:(nrow(x)-1)
      table.index<-data.frame(cbind(Stages,x,index.n.laspeyres))
colnames(table.index)<-c("Stages",name.vector,"Agg. index number")
row.names(table.index)<-c()

results<-table.index
      message(" ", "\n")

      message("Laspeyres index number", "\n")


message(" ","\n")
if (opt.summary){
message("Summary","\n")
      message("Min.=",min(index.n.laspeyres[-1]),"Stage=",which.min(index.n.laspeyres[-1]),"\n")
message("Max.=",max(index.n.laspeyres),"Stage=",which.max(index.n.laspeyres),"\n")
results<-list(summary(index.n.laspeyres),results)
names(results)<-c("Summary","Agg. index number")
}
return(results)
}
