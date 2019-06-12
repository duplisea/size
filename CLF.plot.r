library(size)
tmp= CLF.f("all",codeqc=193)
class(tmp)= "CLF"

plot.CLF= function(CLF.object, min.len=0, max.len=10^6){
  CLF.object= as.list(CLF.object[[1]]) #I initially wrote it for a list and not a dataframe
  years= sort(unique(CLF.object$year))
  cols= floor(sqrt(length(years)))
  rows= ceiling(sqrt(length(years)))
  ab.stats= data.frame(ncol=2,nrow=length(years))
  names(ab.stats)= c("intercepts","slopes")
  if (max.len==10^6) max.len= max(CLF.object$length)
  if (min.len==0) min.len= min(CLF.object$length)
  par(mfcol=c(rows,cols),mar=c(1,1,1,1),omi=c(.5,.5,.01,.01))
  counter=1
  for (y in years){
    plot.data.selection= CLF.object$year==y & CLF.object$length>=min.len & CLF.object$length<=max.len
    plot.year= CLF.object$year[plot.data.selection]
    plot.length= CLF.object$length[plot.data.selection]
    plot.abundance= log(CLF.object$abundance[plot.data.selection])
    plot(plot.length,plot.abundance,type="p",xlim=c(min.len,max.len), ylim=c(0,log(max(CLF.object$abundance))), xlab="", ylab="", pch=20)
    nss= lm(plot.abundance~ plot.length)
    ab.stats[counter,]= coef(nss)
    lines(plot.length,predict(nss),lwd=2,col="blue")
    legend("topright",legend=y,cex=0.6,bty="n",text.col="blue")
    counter= counter+1
  }
  mtext("Length (cm)",side=1,outer=T,line=2)
  mtext("Log abundance",side=2,outer=T,line=2)
  ab.stats$year= years
  #layout(1)
  par(ask=TRUE)
  par(mfcol=c(1,1),mar=c(3,3,2,2))
  plot(ab.stats$year, ab.stats$slopes, pch=20)
  title(paste("Lmin =",min.len,",","Lmax =",max.len))
  gam.trend.line.f(ab.stats$year, ab.stats$slopes, lwd=2, col="blue")
  mtext("Year",side=1,outer=T,line=1)
  mtext("LF slope",side=2,outer=T,line=1)
  par(ask=FALSE)
}

plot(tmp,10,100)

