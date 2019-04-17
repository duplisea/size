#' Compute the proportion of large fish from the survey data
#'
#' @param cutoff the size (cm) marking the ditinction between large and small fish. Large fish are >= to this size.
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed only for demersals
#'      If "code", then you need to provide the Quebec species numerical code.
#' @param codeqc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish
#' @description The proporption of large fish indicator by abundance.
#' @export
#' @example
#' 
PLF.f= function(cutoff=30, species.group="demersal", codeqc=792){

  ngsl.plf= merge(ngsl.plf.data,species.groups,by="codeqc")
  
  if (species.group=="all"){
    ngsl.plf= ngsl.plf[ngsl.plf$representative.sample==1,]
  }
  if (species.group=="demersal"){ #all bottom dwellers from the survey including invertebrates
    ngsl.plf= ngsl.plf[ngsl.plf$representative.sample==1 & ngsl.plf$demersal==1,]
  }
  if (species.group=="groundfish"){# only demersal fish excluding invertebrates
    ngsl.plf= ngsl.plf[ngsl.plf$representative.sample.fish==1 & ngsl.plf$groundfish==1,]
  }
  if (species.group=="pelagic"){
    ngsl.plf= ngsl.plf[ngsl.plf$representative.sample==1 & ngsl.plf$pelagic==1,]
  }
  if (species.group=="code"){
    ngsl.plf= ngsl.plf[ngsl.plf$codeqc==codeqc,]
  }

  large.data= ngsl.plf[ngsl.plf$length>=cutoff,]
  large.fish= aggregate(large.data$abundance,by=list(large.data$year),FUN=sum)
  all.fish= aggregate(ngsl.plf$abundance,list(by=ngsl.plf$year),FUN=sum)
  PLF= data.frame(year= all.fish$by, plf= large.fish$x/all.fish$x,N.large=large.fish$x, N.all=all.fish$x)
  PLF
}
