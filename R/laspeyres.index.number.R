laspeyres.index.number <-
function(x,y,name,opt.plot=FALSE,opt.summary=FALSE){

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
ind.index<-matrix(0,ncol=ncol(xmat),nrow=nrow(xmat))

for (i in 1:nrow(x)){
ind.index[i,]<-x[i,]*y[1,]
}
ind.index.aux<-apply(ind.index,1,sum)
index.n.laspeyres<-index.number.serie(ind.index.aux,name,opt.plot=F,opt.summary=F)[,3]


if (opt.plot==TRUE){
if (is.character(name)){
min.in<-min(min(index.n.laspeyres)*0.9,100)
max.in<-max(max(index.n.laspeyres)*1.1,100)
plot(index.n.laspeyres,type="l",xaxt='n',ylim=c(min.in,max.in),xlab="Stages",ylab= paste(name,"Index Number",sep=": "),col="red")
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
message("Min.=",min(index.n.laspeyres[-1]),"\n")
message("Stage=",which.min(index.n.laspeyres[-1]),"\n")
message("Max.=",max(index.n.laspeyres[-1]),"\n")
message("Stage=",which.max(index.n.laspeyres[-1]),"\n")
results<-list(summary(index.n.laspeyres[-1]),results)
names(results)<-c("Summary","Agg. index number")
}
return(results)
}
}
