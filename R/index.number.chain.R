index.number.chain <-
function(x,name,opt.plot=FALSE,opt.summary=FALSE){


opt<-TRUE

if (is.character(name)==FALSE){
opt<-FALSE
message("Please, introduce a name of the variable as a character","\n")
}
if (is.logical(opt.plot)==FALSE|is.logical(opt.summary)==FALSE){
opt<-FALSE
message("Please, revise the logical options opt.plot and opt.summary","\n")
}

if (opt==TRUE){
xaux<-x[-length(x)]
index.n.chain<-c(1,x[-1]/xaux)*100

if (opt.plot==TRUE){
if (is.character(name)){
min.in<-min(min(index.n.chain)*0.9,100)
max.in<-max(max(index.n.chain)*1.1,100)
plot(index.n.chain,type="l",xaxt='n',
     ylim=c(min.in,max.in),xlab="Stages",ylab=name,col="red")
                  axis(side = 1, at =seq(1,length(x),by=1),labels=0:(length(x)-1))
abline(h=100,col="gray",lty=2)
}else{
message("Please, introduce a name for the representation of the variable", "\n")
}
}

Stages<-0:(length(x)-1)
table.index<-data.frame(cbind(Stages,x,index.n.chain))
colnames(table.index)<-c("Stages",name,"Index number")
row.names(table.index)<-c()

results<-table.index
if (opt.summary){
message("Index number in chain", "\n")
message("Summary","\n")
message("Min.=",min(index.n.chain[-1]),"\n")
message("Stage=",which.min(index.n.chain[-1]),"\n")
message("Max.=",max(index.n.chain),"\n")
message("Stage=",which.max(index.n.chain),"\n")
results<-list(summary(index.n.chain[-1]),results)
names(results)<-c("Summary","Index number")
}
return(results)
}
}
