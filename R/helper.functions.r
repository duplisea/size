#' Count distinct values in a vector
#'
#' @param vector.name name of your vector
#' @param model.type the kind of model to fit ("poly", "gam", "gam.adaptive","avg")
#' @description Works the same as sql function count(distinct x) when using a groupby statement
#' @export
count.distinct= function(vector.name){
  n.dist= length(unique(vector.name))
  n.dist
}

#' Convert survey lat and long positions to decimal degress
#'
#' @param degminsec in one number e.g. 44 38' 52", input as 443852
#' @description Decimal degrees
#' @export
decdeg.f= function(degminsec){
  # convert geographic coordinates in degree minutes seconds into decimal decgrees
  # in one number e.g. 44 38' 52", input as 443852
  deg= floor(degminsec/10000)
  min= floor((degminsec - deg*10000)/100)/60
  sec= (degminsec-100*floor(degminsec/100))/3600
  dd= deg+min+sec
  dd
}


#' Wrapper for the PBSmapping plotMap function
#'
#' @param longs longitude coordinates for map corners (western hempshere is -)
#' @param lats latitude coordinates for map corners (southern hemisphere is -)
#' @param land.colour colour of the land on the map
#' @param sea.colour colour of the sea on the map
#' @keywords helper function
#' @export
map.f= function(longs=c(-74,-50),lats=c(43,52.25),land.colour="sienna3",sea.colour="lightblue"){
  data(worldLLhigh)
  worldLLhigh$X=(worldLLhigh$X+180)%%360-180
  xlim=longs
  ylim=lats
  map.data=clipPolys(worldLLhigh,xlim=xlim,ylim=ylim)
  plotMap(map.data,xlim=xlim,ylim=ylim,lty=1,lwd=.05 ,col="tan",
          bg=rgb(224,253,254,maxColorValue=255),las=1,xaxt="n", yaxt="n",
          xlab="",ylab="")
  xint= seq(longs[1],longs[2],length=5)
  yint= seq(lats[1],lats[2],length=6)
  mtext("Longitude west",side=1,line=3)
  mtext("Latitude north",side=2,line=3)
  axis(1, at=xint, labels=xint*-1, lty=1,lwd=1,lwd.ticks= 1, cex.axis=.7)
  axis(2, at=yint, labels=yint*1, lty=1,lwd=1,lwd.ticks=1,las=1, cex.axis=.7)
  box()
}


#' Put a smoothed gam trends line on a two dimensional graph
#'
#' @param x independent variable
#' @param y dependent variable
#' @param ... graphical parameters accepted by the function "lines"
#' @keywords helper function gam
#' @export
gam.trend.line.f= function(x, y, ...){
  library(mgcv)
  gf= gam(y~ s(x))
  newdata= data.frame(x=seq(min(x),max(x),length=length(x)*10))
  lines(newdata$x,predict(gf, newdata=newdata), ...)
}


#' A double logistic selectivity function
#'
#' @param L a length or length vector for which you want a selectivity estimates
#' @param inflection1 the inflection point of the first part of the selectivity curve
#' @param inflection2 the inflection point of the second part of the selectivity curve
#' @param slope1 the slope of the first part of the curve
#' @param slope2 the slope of the second part of the curve
#' @param max.sel the maximum selectivity level
#' @param minsel.upper the asymptotic minimum selectivity on the right side of the curve. Dome shaped curves between 0 and max.sel, =max.sel for a regular logistic curve
#' @param plot T if you want a plot of the curve
#' @description This is a useful function because it can not only mimic standard sigmoidal selectivity but also
#'         a dome shaped selectivity curve if necessary. Set minsell.upper=max.sel for a simple logisitic curve. To correct a survey series
#'         you should divide numbers by this curve and set max.sel= catchability.
#' @export
dublogistic.f= function(L,inflection1=38, inflection2=1, slope1=.3, slope2=.1, max.sel=1, minsel.upper=.4, plot=F){
  minsel.upper= 1-minsel.upper
  logistic1= max.sel/(1+exp(-slope1*(L-inflection1)))
  logistic2= 1-minsel.upper/(1+exp(-slope2*(L-inflection2)))
  sel= logistic1*logistic2
  selectivity= data.frame(Length=L,selectivity=sel)
  if(plot){
    plot(L,sel,type="l",lwd=2,xlab="Length",ylab="Selectivity")
  }
  selectivity
}
