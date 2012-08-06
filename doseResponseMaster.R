#dose response analysis master file. calls the data sorting and the plotting scripts
#
#-------------------------------------------------------------------------------

#reference point in the titration series
ref = 18
#call the calculation script
source('~/Dropbox/R/pcr/doseRespAnalysis/doseResponse.R')
#create the corresponding plots

ylim = c(0,4)
ylimMean = c(0,4)

#choose which gene to plot
selectGREB1 = 1
selectTFF1  = 1

#plotting script
source('~/Dropbox/R/pcr/doseRespAnalysis/doseRespPlot.R')