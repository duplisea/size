#' Compute the proportion of large fish from the survey data
#'
#' @param dataset the mean length frequency dataset
#' @param cutoff the size (cm) marking the ditinction between large and small fish. Large fish are >= to this size.
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed only for demersals
#'      If "code", then you need to provide the Quebec species numerical code.
#' @param code.qc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish
#' @description The proporption of large fish indicator by abundance.
#' @export
#' @example
#' 
PLF.f= function(dataset=ngsl.plf.data, cutoff=30, species.group="demersal", code.qc=792){
  if (species.group=="all"){
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample==1,]
  }
  if (species.group=="demersal"){ #all bottom dwellers from the survey including invertebrates
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample==1 & ngsl.lf.mean$demersal==1,]
  }
  if (species.group=="groundfish"){# only demersal fish excluding invertebrates
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample==1 & ngsl.lf.mean$groundfish==1,]
  }
  if (species.group=="pelagic"){
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample==1 & ngsl.lf.mean$pelagic==1,]
  }
  if (species.group=="code"){
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$espece==code.qc,]
  }

  large.data= dataset[dataset$length>=cutoff,]
  large.fish= aggregate(large.data$abundance,by=list(large.data$year),FUN=sum)
  all.fish= aggregate(dataset$abundance,list(by=dataset$year),FUN=sum)
  PLF= data.frame(year= all.fish$by, plf= large.fish$x/all.fish$x,N.large=large.fish$x, N.all=all.fish$x)
  PLF
}
