index.number.serie <-
function(x,name,opt.plot=FALSE,opt.summary=FALSE){

if (is.character(name)==FALSE){
message("Please, introduce a name of the variable as a character","\n")
}

x0<-x[1]
index.n.serie<-x/x0*100

if (opt.plot==TRUE){
if (is.character(name)){
min.in<-min(min(index.n.serie)*0.9,100)
max.in<-max(max(index.n.serie)*1.1,100)
plot(index.n.serie,type="l",xaxt='n',
     ylim=c(min.in,max.in),xlab="Stages",ylab=name,col="red")
                  axis(side = 1, at =seq(1,length(x),by=1),labels=0:(length(x)-1))
abline(h=100,col="gray",lty=2)
}else{
message("Please, introduce a name for the representation of the variable", "\n")
}
}

Stages<-0:(length(x)-1)
table.index<-data.frame(cbind(Stages,x,index.n.serie))
colnames(table.index)<-c("Stages",name,"Index number")
row.names(table.index)<-c()

results<-table.index
      message("Index number in serie", "\n")
if (opt.summary){
message("Summary","\n")
      message("Min.=",min(index.n.serie[-1]),"Stage=",which.min(index.n.serie[-1]),"\n")
message("Max.=",max(index.n.serie),"Stage=",which.max(index.n.serie),"\n")
results<-list(summary(index.n.serie),results)
names(results)<-c("Summary","Index number")
}
return(results)

}
