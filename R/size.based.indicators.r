#' Compute the proportion of large fish from the survey data
#'
#' @param cutoff the size (cm) marking the ditinction between large and small fish. Large fish are >= to this size.
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed only for demersals
#'      If "code", then you need to provide the Quebec species numerical code.
#' @param code.qc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish
#' @description The proporption of large fish indicator by abundance.
#' @export
#' @example
#' 
PLF.f= function(cutoff=30, species.group="demersal", code.qc=792){

  ngsl.lf.mean= merge(ngsl.plf.data,species.groups,by="english")
  
  if (species.group=="all"){
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample==1,]
  }
  if (species.group=="demersal"){ #all bottom dwellers from the survey including invertebrates
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample==1 && ngsl.lf.mean$demersal==1,]
  }
  if (species.group=="groundfish"){# only demersal fish excluding invertebrates
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample.fish==1 && ngsl.lf.mean$groundfish==1,]
  }
  if (species.group=="pelagic"){
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$representative.sample==1 && ngsl.lf.mean$pelagic==1,]
  }
  if (species.group=="code"){
    ngsl.lf.mean= ngsl.lf.mean[ngsl.lf.mean$code.qc==code.qc,]
  }

  large.data= ngsl.lf.mean[ngsl.lf.mean$length>=cutoff,]
  large.fish= aggregate(large.data$abundance,by=list(large.data$year),FUN=sum)
  all.fish= aggregate(ngsl.lf.mean$abundance,list(by=ngsl.lf.mean$year),FUN=sum)
  PLF= data.frame(year= all.fish$by, plf= large.fish$x/all.fish$x,N.large=large.fish$x, N.all=all.fish$x)
  PLF
}
