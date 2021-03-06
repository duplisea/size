% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post.analyses.plotting.r
\name{CLF.lm.fit}
\alias{CLF.lm.fit}
\title{Fits linear models for each year for a community length frequency distribution}
\usage{
CLF.lm.fit(CLF.object, min.len = 0, max.len = 10^6)
}
\arguments{
\item{CLF.object}{the list resulting from running the function CLF.f on the survey dataset}

\item{min.len}{the minimum length to consider in the plot and for the linear model trend}

\item{max.len}{the maximum length to consider in the plot and for the linear model trend}
}
\value{
a list where the first element is a dataframe of ab model parameters for each year. Minimum and maximum length considered 
      in the model fits are also elements of the list. The outputs a "CLFab" class object.
}
\description{
this treats missing abundance values as missing while they may be real zeros. Calculate linear models from the CCF data object
       filling in the real zeros if you would like it done otherwise. Chances are that for multispecies size spectra, you will always
       have non-zero abundance values for each length class if you chose length classes that are somewhat well sampled by the gear. 
       The value of this kind of analysis of lm model parameters is questionable anyway if you are choosing a rather restricted subset 
       of species for analysis whos sizes do not cover the length selection length range you have chosen and/or which are not well sampled 
       by the gear.
}
