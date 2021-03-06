paasche.index.number<-function(x,y,name,opt.plot=FALSE,opt.summary=FALSE){

  x<-as.matrix(x)
  y<-as.matrix(y)

  opt<-TRUE
  if (is.matrix(x)==FALSE|!dim(x)[2]>=2|is.numeric(as.matrix(x))==FALSE){
    opt<-FALSE
    message("Please, introduce a matrix with the prices of different products","\n")
  }
  if (is.matrix(y)==FALSE|!dim(y)[2]>=2|is.numeric(as.matrix(y))==FALSE){
    opt<-FALSE
    message("Please, introduce a matrix with the prices of different products","\n")
  }
  if (is.character(name)==FALSE){
    opt<-FALSE
    message("Please, introduce a name of the variable as a character","\n")
  }
  if (is.logical(opt.plot)==FALSE|is.logical(opt.summary)==FALSE){
    opt<-FALSE
    message("Please, revise the logical options opt.plot and opt.summary","\n")
  }

  if (opt==TRUE){
    xmat<-x
n.prod<-ncol(xmat)
ind.index<-rep(0,nrow(xmat))

for (i in 1:nrow(x)){
ind.index[i]<-sum(x[i,]*y[i,])/sum(x[1,]*y[i,])*100
}
index.n.paasche<-ind.index

if (opt.plot==TRUE){
if (is.character(name)){
min.in<-min(min(index.n.paasche)*0.9,100)
max.in<-max(max(index.n.paasche)*1.1,100)
plot(index.n.paasche,type="l",xaxt='n',ylim=c(min.in,max.in),xlab="Stages",ylab= paste(name,"Index Number",sep=": "),col="red")
axis(side = 1, at =seq(1,length(x),by=1),labels=0:(length(x)-1))
abline(h=100,col="gray",lty=2)
}else{
message("Please, introduce a name for the representation of the variable", "\n")
}
}
name.vector<-c()
for (i in 1:n.prod){name.vector[i]<-paste(name,i)}

Stages<-0:(nrow(x)-1)
      table.index<-data.frame(cbind(Stages,x,index.n.paasche))
colnames(table.index)<-c("Stages",name.vector,"Agg. index number")
row.names(table.index)<-c()

results<-table.index
message(" ", "\n")

message("Paasche index number", "\n")


message(" ","\n")
if (opt.summary){
message("Summary","\n")
message("Min.=",min(index.n.paasche[-1]),"\n")
message("Stage=",which.min(index.n.paasche[-1]),"\n")
message("Max.=",max(index.n.paasche[-1]),"\n")
message("Stage=",which.max(index.n.paasche[-1]),"\n")
results<-list(summary(index.n.paasche[-1]),results)
names(results)<-c("Summary","Agg. index number")
}
return(results)
}
}
