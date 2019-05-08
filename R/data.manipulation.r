#' Import data from the PACES output into R
#'
#' @param data.directory where your PACES .csv files are stored, e.g. "s:/releves..."
#' @param save.rda saves the paces data to .rda files which are compact and easy to work with in later sessions
#' @description Imports data into R from the PACES .csv output. It also saves them as R binary data files in your working directory.
#'         It also harmonises field names and selects on type 1 and 2 sets and changes some field names and create new useful fields.
#'         It create other data tables for computing the PLF indicators and basic data tables for other calculations. You will only
#'         run this function when there is an update to the PACES data. once it is done, just load the datafiles before running your
#'         analysis.         
#'         Note that the package comes with the data but it might get out of date. In this case once you import updated data you need 
#'         to make sure you keep using that updated data since there will always be older data available in the package. i.e. you might
#'         not get an error and your analysis will continue to run fine but with the data that comes with the package.
#' @export
import.paces.f= function(data.directory, save.rda=T){
  # import data from csv, noting delimiters
  #if (!exists("ngsl.set")) ngsl.set= read.csv("NGSL_Set.csv",header=T,sep=";")
  #if (!exists("ngsl.catch")) ngsl.catch= read.csv("NGSL_Capt.csv",header=T,sep=";")
  #if (!exists("ngsl.cbio")) ngsl.cbio= read.csv("NGSL_CBio.csv",header=T,sep=";")
  #if (!exists("ngsl.strat")) ngsl.strat= read.csv("ngsl.strat.csv",header=T,sep=",")
  #if (!exists("species")) species= read.csv("species.codes.csv",header=T,sep=",")
  oldwd= getwd()
  setwd(data.directory)
  ngsl.set= read.csv("NGSL_Set.csv",header=T,sep=";")
  ngsl.catch= read.csv("NGSL_Capt.csv",header=T,sep=";")
  ngsl.cbio= read.csv("NGSL_CBio.csv",header=T,sep=";")
  setwd(oldwd)

  # for some reason the csv files are extracted with different cases in field names which varies depending
  # on which table you are working with, harmonise them to lower case
  names(ngsl.set)= tolower(names(ngsl.set))
  names(ngsl.catch)= tolower(names(ngsl.catch))
  names(ngsl.cbio)= tolower(names(ngsl.cbio))
  names(ngsl.strat)= tolower(names(ngsl.strat))
  names(species)= tolower(names(species))

  # do quality control data selection
  ngsl.set= ngsl.set[ngsl.set$resultat<3,] #keep only result 1 and 2 tows
  ngsl.set= ngsl.set[!(ngsl.set$annee==2004 & ngsl.set$nbpc==34 | ngsl.set$annee==2005 & ngsl.set$nbpc==34),] #remove needler in comparative tows
  ngsl.set= ngsl.set[!is.na(ngsl.set$no_str),]
  ngsl.strat= data.frame(no_str= ngsl.strat$no_str, surfkm91= ngsl.strat$surfkm91)
  species$espece= species$codeqc
  
  if(save.rda){
    save(ngsl.set, file="ngsl.set.rda")
    save(ngsl.catch, file="ngsl.catch.rda")
    save(ngsl.cbio, file="ngsl.bio.rda")
    save(ngsl.strat, file="ngsl.strat.rda")
    save(species, file="species.rda")
  }

  #aggregate individual table to level of category
  bycols=c("source", "no_rel", "nbpc","no_stn","espece","categ","longueur")
  bycolno= match(bycols,names(ngsl.cbio))
  datacols=c("pds_tot","nb_cor")
  datacolno= match(datacols,names(ngsl.cbio))
  tmp= aggregate(ngsl.cbio[,datacolno],by=ngsl.cbio[,bycolno],FUN=sum)

# merge the catch and set table
  ngsl= merge(ngsl.catch,ngsl.set, by= c("source","no_rel","nbpc","no_stn"))
  ngsl= merge(ngsl,tmp, by = c("source","no_rel","nbpc","no_stn","espece","categ"))

# recode data
  ngsl$source[ngsl$source==16]="Teleost" #code vessels by name
  ngsl$source[ngsl$source==6]="Needler"

  #ngsl= ngsl[ngsl$Espece<1000,] #remove invertebrates

  # there are NA in the above owing to not recording number of individuals. Mostly for non-commercial
  # species which are not well caught, e.g. lussion, hagfish, molasse.
  # if no total number caught then remove. If a total but no sample then make sample=total.

  # calculate the catch subsample factor (biomass is same as numbers)
  #ngsl$catch.subsample= ngsl$nb_capt_cor/ngsl$nb_ech_cor
  ngsl$catch.subsample= ngsl$pds_capt_cor/ngsl$pds_ech_cor

  # bump up counts of individuals by the catch subsample
  ngsl$abundance= ngsl$nb_cor*ngsl$catch.subsample

  #create 1 cm length classes for LF distribution
  ngsl$lenclass= floor((ngsl$longueur-5)/10)+1 # e.g. lenclass 12 is all fish between 11.5 and 12.4 cm

  # aggregate abundance into length classes for each tow and species as baseline lf distribution
  mycols=c("source", "no_rel", "nbpc","annee","no_str", "no_stn", "espece","lenclass")
  colno= match(mycols,names(ngsl))
  ngsl.lf.base= aggregate(ngsl$abundance,by=ngsl[,colno],FUN=sum)
  names(ngsl.lf.base)[match("x",names(ngsl.lf.base))]= "abundance"
  # merge the base lf with the stratum area to calculate strap averages for gulf if desired
  ngsl.lf.base= merge(ngsl.strat,ngsl.lf.base, by = c("no_str"))
  ngsl.lf.base$setid= paste(ngsl.lf.base$source, ngsl.lf.base$no_rel, ngsl.lf.base$nbpc,ngsl.lf.base$annee,ngsl.lf.base$no_str,ngsl.lf.base$no_stn,sep="")
  ngsl.lf.base= ngsl.lf.base[!is.na(ngsl.lf.base$abundance),]
  save(ngsl.lf.base,file="ngsl.lf.base.rdata")
  
  # station count per stratum by vessel and year. For computing a mean haul for a stratum
  mycols=c("source", "no_rel", "nbpc","annee","no_str")
  colno= match(mycols,names(ngsl.lf.base))
  stn.cnt= aggregate(ngsl.lf.base$setid,by=ngsl.lf.base[,colno],FUN=count.distinct)
  names(stn.cnt)[match("x",names(stn.cnt))]= "station.count"

  #compute a survey wide stratum average abundance by species and length
  mycols=c("source", "no_rel", "nbpc","annee","no_str", "espece","lenclass")
  colno= match(mycols,names(ngsl.lf.base))
  lf.strat.sum= aggregate(ngsl.lf.base$abundance,by=ngsl.lf.base[,colno],FUN=sum)
  names(lf.strat.sum)[match("x",names(lf.strat.sum))]= "sumabundance"
  lf.strat.mean= merge(lf.strat.sum,stn.cnt, by = c("source", "no_rel", "nbpc","annee","no_str"))
  lf.strat.mean$mean.abundance= lf.strat.mean$sumabundance/lf.strat.mean$station.count

  # merge the base lf with the stratum area to calculate strap averages for gulf if desired
  lf.strat.mean= merge(lf.strat.mean, ngsl.strat, by = c("no_str"))
  lf.strat.mean$weighted.sum= lf.strat.mean$mean.abundance*lf.strat.mean$surfkm91
  strat.bootstrap.data= lf.strat.mean[,c(1,5,6,7,10,11)]
  names(strat.bootstrap.data)= c("no_str","years","species","length","abundance","stratum.area")

  ################  it is the above to use. Calculate PLF by stratum and
  ################  year. Then bootstrap, compute strap quantiles
  # for bootstrap, create a sampling matrix of N*length(unique(no_str))
  # you will need this to sum up weights

  # sum the weight and weighted abundance by species, size class and year
  bycols=c("source", "no_rel", "nbpc","annee","espece","lenclass")
  aggcols= c("weighted.sum","surfkm91")
  bycolno= match(bycols,names(lf.strat.mean))
  aggcolno= match(aggcols,names(lf.strat.mean))
  lf.tmp= aggregate(lf.strat.mean[,aggcolno],by=lf.strat.mean[,bycolno],FUN=sum)
  lf.tmp= merge(lf.tmp,species, by = c("espece"))

  # then compute survey wide stratified random mean LF for a year and species
  keep.cols= c("source", "annee","espece","english","latin","lenclass")
  keepcolno= match(keep.cols,names(lf.tmp))
  ngsl.lf.mean= lf.tmp[,keepcolno]
  ngsl.lf.mean$abundance= lf.tmp$weighted.sum/lf.tmp$surfkm91
  tmp= order(ngsl.lf.mean$source,ngsl.lf.mean$annee,ngsl.lf.mean$espece,ngsl.lf.mean$lenclass)
  ngsl.lf.mean= ngsl.lf.mean[tmp,]
  save(ngsl.lf.mean,file= "ngsl.lf.mean.rdata")
  
  ngsl.comm.cols= c("annee","espece","english","lenclass","abundance")
  cols= match(ngsl.comm.cols,names(ngsl.lf.mean))
  ngsl.comm.data= ngsl.lf.mean[,cols]
  names(ngsl.comm.data)= c("year","codeqc","english","length","abundance")
  save(ngsl.comm.data,file="ngsl.comm.data.rda")
}

#' Selects sub data based on species group or a species code for further analysis
#' @param ngsl.comm.data ngsl community data
#' @param species.groups ngsl species groups with codes (codeqc)
#' @param species.group the species group you want to be represented in the data. The PLF is generally computed 
#'      only for demersals. Choices are "groundfish", "demersal", "pelagic". If "code", then you need to 
#'      provide the Quebec species numerical code (codeqc).
#' @param codeqc the species numerical code used in Quebec region for a species. 792 is unspeciated redfish
#' @description Called by community indicator functions. It is unlikely that you will want to call this on its own
#' @export
datasel.f=function(ngsl.comm.data, species.groups, species.group, codeqc){
  ngsl.sub= merge(ngsl.comm.data,species.groups,by="codeqc")
  
  if (species.group=="all"){
    ngsl.sub= ngsl.sub[ngsl.sub$representative.sample==1,]
  }
  if (species.group=="demersal"){ #all bottom dwellers from the survey including invertebrates
    ngsl.sub= ngsl.sub[ngsl.sub$representative.sample==1 & ngsl.sub$demersal==1,]
  }
  if (species.group=="groundfish"){# only demersal fish excluding invertebrates
    ngsl.sub= ngsl.sub[ngsl.sub$representative.sample.fish==1 & ngsl.sub$groundfish==1,]
  }
  if (species.group=="pelagic"){
    ngsl.sub= ngsl.sub[ngsl.sub$representative.sample==1 & ngsl.sub$pelagic==1,]
  }
  if (species.group=="code"){
    ngsl.sub= ngsl.sub[ngsl.sub$codeqc==codeqc,]
  }
  ngsl.sub
}
