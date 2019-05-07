#' Community length frequency
#'
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed only for demersals. If "code", then you need to provide the Quebec species numerical code.
#' @param codeqc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish. Look at the species 
#'         table (codeqc) for other species
#' @description The Community length frequency distribution or numbers at length size spectrum. 1cm length bins are used
#'      where the value represents the midpoint of the length class (3= 2.5-3.5 cm). Length is generically converted to weight with W=0.1*L^3.
#' @export
CLF.f= function(species.group, codeqc){
  ngsl.CLF= datasel.f(ngsl.comm.data=ngsl.comm.data, species.groups=species.groups, species.group=species.group, codeqc=codeqc)
  CLF= aggregate(ngsl.CLF$abundance, list(ngsl.CLF$year,ngsl.CLF$length),sum)
  names(CLF)= c("year","length","abundance")
  CLF= CLF[CLF$length>0,]
  CLF$wgt=0.1*CLF$length^3
  CLF= CLF[order(CLF$year,CLF$length),]
  CLF
}


#' Numbers at log2 weight size spectrum
#'
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed only for demersals. If "code", then you need to provide the Quebec species numerical code.
#' @param codeqc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish. Look at the species 
#'         table (codeqc) for other species
#' @description The numbers an biomass at log2 weight class size spectrum for the community components you select. 
#'         This is the classic size spectrum of Sheldon & Dickie
#' @references 
#'         Duplisea, D.E. and Castonguay, M. 2006. Comparison and utility of different size-based metrics of fish communities for detecting fishery impacts. Canadian Journal of Fisheries and Aquatic Sciences 63: 810-820.
#' @export
SS.f= function(species.group, codeqc){
  CLF= CLF.f(species.group=species.group, codeqc=codeqc)
  CLF$lg2W= floor(log2(CLF$wgt))
  CLF$biomass= CLF$abundance*CLF$wgt
  SS= aggregate(list(CLF$abundance,CLF$biomass),list(CLF$year,CLF$lg2W),sum)
  names(SS)= c("year","lg2W","abundance","biomass")
  SS= SS[SS$lg2W>0,]
  SS
}


#' Predation size spectrum
#'
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed only for demersals
#'      If "code", then you need to provide the Quebec species numerical code.
#' @param codeqc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish. Look at the species 
#'         table (codeqc) for other species
#' @param PPWR the mean predator/prey weight ratio. If 10, then the predator on average weighs 10x more than its prey
#' @param PPWR.cv the coefficient of variation of the predator prey weight ratio
#' @param minPPWR the minimum predator/prey weight ratio allowed. e.g. if 3 then the prey is never allowed to be more than 1/3 of the 
#'         predator weight
#' @param QWb the exponent of the allometric relationship describing food consumption rate as a function of a fish's weight
#' @description The predator-prey weight ratio is modelled as a log-normal distribution. The energy demand of a predator is a 
#'         function of the predator size. This is not actually the amount of predation that has occured but a predation risk or 
#'         preference, i.e. given the size distribution of predators in the system, the PSS shows where they would most like to 
#'         take their prey. Since the index is relative, a QWa parameter is not needed as it is just a scaling parameter.
#'         
#'         The PPWR CV 0.25 is from from Hahm and Langton (1984). Their PPWR is likely too large for predators in the Northern
#'         Gulf which are eating other fish and large invertebrates such as shrimp. Our default value is 30 which means that a
#'         predator is 30 times larger its prey (by weight) on average.
#'         
#'         The log normal density function will produce NaNs for some size ranges and warnings will be produced. This is generally 
#'         nothing to worry about as NaNs are converted to 0. The warnings have not be suppressed, however.
#' @return A list is returned: element 1: the year vector; element 2: the prey length vector; element 3: the prey weight vector; 
#'         element 4: a matrix of the predation pressure over all prey sizes (rows) by year (columns). You will likely work with 
#'         element 1 and 4 for plotting or creation of predation indices on particular prey size classes. The values in the matrix are 
#'         not scaled but this can be done easily by dividing the matrix by the maximum value of the matrix. They can be scaled annually 
#'         using the apply function and dividing each value in a column (year) by the maximum in that column. The whole PSS is normalised 
#'         by dividing by the maximum value.
#' @example
#'         pss.out= PSS.f("all")
#'         pss.scaled= pss.out$pred.risk/max(pss.out$pred.risk)
#'         plot(pss.out$year, pss.scaled[10,],type="l") #the time series of predation risk on prey size 10cm
#' @references 
#'         Duplisea, D.E. 2005. Running the gauntlet: the predation environment of small fish in the northern Gulf of St Lawrence, Canada. ICES Journal of Marine Science 62: 412-416.
#'         
#'         Hahm, W. and Langton, R. 1984. Prey selection based on predator/prey weight ratios for some Northwest Atlantic fish. Marine Ecology Progress Series 19: 1-5.
#' @export
PSS.f= function(species.group, codeqc, PPWR=30, PPWR.cv=0.25, minPPWR=5, QWb=0.75){
  CLF= CLF.f(species.group=species.group,codeqc=codeqc)
  years= sort(unique(CLF$year))
  preylen= 1:200
  preywgt= 0.1*(preylen)^3
  pred.risk= as.data.frame(matrix(ncol=length(years),nrow=length(preylen)))
  counter= 1
  for (y in years){
    subdata= CLF[CLF$year==y,]
    predwgt= subdata$wgt
    N= subdata$abundance
    p= matrix(nrow=length(predwgt),ncol=length(preywgt))
    for (i in 1:length(predwgt)){
      # this is the probability that a predator of size wgt[i] wants to it prey of different sizes
      # this produces positive probabilities for prey that are larger than predators so these probabilities need 
      # to be truncated and renormalised.
      consumption= predwgt^.75 # consumption rate of a predators of weight wgt
      mu=log(predwgt[i]/PPWR)
      nsd= PPWR.cv*mu
      pw= dlnorm(predwgt,mu,nsd)
      pw[is.nan(pw)]=0
      pw[preywgt/predwgt[i]>=1/minPPWR]= 0 #remove situations where is prey is too large (default = predator weighs 3X prey)
      p[i,]= pw
      pp= p/apply(p,1,sum,na.rm=T) #normalise by dividing by the sum of probabilities
      ppp= pp*consumption*N # bump up by the consumption and abundance
      pred.risk.year= apply(ppp,2,sum,na.rm=T)
      #just save it to its own vector
    }
    pred.risk[,counter]= pred.risk.year
    counter= counter+1
  }
  names(pred.risk)= paste("y",years,sep="")
  row.names(pred.risk)= paste(preylen,"cm", sep="")
  pss.out= list(year=years, preylen=preylen, preywgt=preywgt, pred.risk=pred.risk)
  pss.out
}

#' Compute the proportion of large fish from the survey data
#'
#' @param cutoff the size (cm) marking the ditinction between large and small fish. Large fish are >= to this size.
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed only for demersals
#'         If "code", then you need to provide the Quebec species numerical code.
#' @param codeqc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish. Look at the species 
#'         table (codeqc) for other species
#' @description The proporption of large fish indicator by abundance.
#' @references 
#'         Greenstreet, S.P., Rogers, S.I., Rice, J.C., Piet, G.J., Guirey, E.J., Fraser, H.M. and Fryer, R.J. 2010. Development of the EcoQO for the North Sea fish community. ICES Journal of Marine Science 68: 1-11.
#' @export
PLF.f= function(cutoff=30, species.group="demersal", codeqc=792){
  
  ngsl.plf= datasel.f(ngsl.comm.data=ngsl.comm.data, species.groups=species.groups, species.group=species.group, codeqc=codeqc)
  large.data= ngsl.plf[ngsl.plf$length>=cutoff,]
  large.fish= aggregate(large.data$abundance,by=list(large.data$year),FUN=sum)
  all.fish= aggregate(ngsl.plf$abundance,list(by=ngsl.plf$year),FUN=sum)
  PLF= data.frame(year= all.fish$by, plf= large.fish$x/all.fish$x,N.large=large.fish$x, N.all=all.fish$x)
  PLF
}
