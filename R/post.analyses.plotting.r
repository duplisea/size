#' Plot the community length frequency showing the slope fit line
#'
#' @param CLF.object the list resulting from running the function CLF.f on the survey dataset
#' @param min.len the minimum length to consider in the plot and for the linear model trend
#' @param max.len the maximum length to consider in the plot and for the linear model trend
#' @description Plots the data for all years in a matrix of plots, one year per plot.
#' @example 
#'         tmp= CLF.f("all")
#'         plot(tmp, 10, 120)
#' @export
plot.CLF= function(CLF.object, min.len=0, max.len=10^6){
  CLF.object= as.list(CLF.object[[1]]) #I initially wrote it for a list and not a dataframe
  years= sort(unique(CLF.object$year))
  cols= floor(sqrt(length(years)))
  rows= ceiling(sqrt(length(years)))
  ab.stats= data.frame(ncol=2,nrow=length(years))
  names(ab.stats)= c("intercepts","slopes")
  if (max.len==10^6) max.len= max(CLF.object$length)
  if (min.len==0) min.len= min(CLF.object$length)
  par(mfcol=c(rows,cols),mar=c(1,1,1,1),omi=c(.5,.5,.5,.01))
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
  title(paste("Lmin =",min.len,",","Lmax =",max.len),outer=T)
}



#' Fits linear models for each year for a community length frequency distribution
#'
#' @param CLF.object the list resulting from running the function CLF.f on the survey dataset
#' @param min.len the minimum length to consider in the plot and for the linear model trend
#' @param max.len the maximum length to consider in the plot and for the linear model trend
#' @description this treats missing abundance values as missing while they may be real zeros. Calculate linear models from the CCF data object
#'        filling in the real zeros if you would like it done otherwise. Chances are that for multispecies size spectra, you will always
#'        have non-zero abundance values for each length class if you chose length classes that are somewhat well sampled by the gear. 
#'        The value of this kind of analysis of lm model parameters is questionable anyway if you are choosing a rather restricted subset 
#'        of species for analysis whos sizes do not cover the length selection length range you have chosen and/or which are not well sampled 
#'        by the gear.
#' @return a list where the first element is a dataframe of ab model parameters for each year. Minimum and maximum length considered 
#'       in the model fits are also elements of the list. The outputs a "CLFab" class object.
#' @example tmp= CLF.f("all")
#'         tmp2=CLF.lm.fit(tmp,10,120)
#' @export
CLF.lm.fit= function(CLF.object, min.len=0, max.len=10^6){
  CLF.object= CLF.object[[1]] #I initially wrote it for a list and not a dataframe
  CLF.object$labundance= log(CLF.object$abundance)
  years= sort(unique(CLF.object$year))
  ab.stats= data.frame(year=years,intercepts=NA,slopes=NA)
  if (max.len==10^6) max.len= max(CLF.object$length)
  if (min.len==0) min.len= min(CLF.object$length)
  counter=1
  for (y in years){
    data.sel= CLF.object[CLF.object$year==y & CLF.object$length>=min.len & CLF.object$length<=max.len,]
    nss= lm(labundance~ length, data=data.sel)
    ab.stats[counter,2:3]= coef(nss)
    counter= counter+1
  }
  ab.stats=list(ab.stats= ab.stats) 
  ab.stats$min.len= min.len
  ab.stats$max.len= max.len
  class(ab.stats)= "CLFab"
  ab.stats
}


#' Plots linear model slopes over time fitted to community length frequency distribution and runs a gam smoother through them
#'
#' @param x a CLFab class object which results from fitting linear models to length frequencies with the function CLF.lm.fit
#' @description A plot of slopes over time with a gam trend run through them. The GAM is from mgcv with defaults.
#' @example tmp= CLF.f("all")
#'         tmp2=CLF.lm.fit(tmp,10,120)
#'         plot(tmp2)
#' @export
plot.CLFab= function(x){
  ab.stats= x[[1]]
  plot(ab.stats$year, ab.stats$slopes, pch=20,xlab= "Year", ylab= "LF slope")
  min.len= x$min.len
  max.len= x$max.len
  title(paste("Lmin =",min.len,",","Lmax =",max.len))
  gam.trend.line.f(ab.stats$year, ab.stats$slopes, lwd=2, col="blue")
  mtext("Year",side=1,outer=T,line=1)
  mtext("LF slope",side=2,outer=T,line=1)
}


#' A wrapper to use sliders to examine impact of length cutoffs on the trend in linear slopes over time
#'
#' @param x a CLFab class object which results from fitting linear models to length frequencies with the function CLF.lm.fit
#' @param min.len the minimum length to consider in the plot and for the linear model trend
#' @param max.len the maximum length to consider in the plot and for the linear model trend
#' @description A plot of slopes over time with a gam trend run through them. The GAM is from mgcv with defaults.
#' @examples library(manipulate)
#'         tmp= CLF.f("all")
#'         manipulate(slope.slider.f(tmp, min.len=x.min, max.len=x.max), x.min= slider(1,35),x.max= slider(70,150))
#' @export
slope.slider.f= function(CLF.object, min.len=0, max.len=10^6){
  tmp2= CLF.lm.fit(CLF.object,min.len=min.len,max.len=max.len)
  plot(tmp2)
}
