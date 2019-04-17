Install and load package
------------------------

    library(devtools)
    install_github("duplisea/size")

Load the package

    library(size)

Plot the survey locations on a map of the Gulf
----------------------------------------------

    map.f()
    points(-decdeg.f(ngsl.set$lon_deb*100),decdeg.f(ngsl.set$lat_deb*100),pch=".",col="red")

![](README_files/figure-markdown_strict/mapsurvey-1.png)

PLF analysis with the installed dataset
---------------------------------------

The PLF for all species with a 30 cm threshold between small and large
individuals

    PLF.all= PLF.f(species.group="all",cutoff=30)

The PLF for all species with a 30 cm threshold between small and large
individuals

    PLF.dem= PLF.f(species.group="demersal",cutoff=30)

The PLF for all species with a 30 cm threshold between small and large
individuals

    PLF.gf= PLF.f(species.group="groundfish",cutoff=30)

The PLF for all species with a 30 cm threshold between small and large
individuals

    PLF.rf= PLF.f(species.group="code",codeqc=792,cutoff=20)

Plot the various PLF community indicators

    ymax=max(c(PLF.all$plf,PLF.dem$plf,PLF.gf$plf))
    plot(PLF.all$year, PLF.all$plf,xlab="Year",ylab="Proportion of individuals >=30cm",type="l",lwd=2,
      ylim=c(0,ymax))
    lines(PLF.dem$year,PLF.dem$plf,col="blue",lwd=2)
    lines(PLF.gf$year,PLF.gf$plf,col="green",lwd=2)
    legend("topright",lwd=2,col=c("black","blue","green"),legend=c("everything","demersal","groundfish"),bty="n",cex=0.6)

![](README_files/figure-markdown_strict/PLFplots-1.png)
