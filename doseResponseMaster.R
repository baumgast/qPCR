#dose response analysis master file. calls the data sorting and the plotting scripts
#
#-------------------------------------------------------------------------------

#reference point in the titration series
ref = 3
#call the calculation script
source('~/Dropbox/R/pcr/doseRespAnalysis/doseResponse.R')
#create the corresponding plots

ylim = c(0,150)
ylimMean = c(0,150)
lim = c(-30,100)

#choose which gene to plot
selectGREB1 = 1
selectTFF1  = 1

#soritng out outliers manually and set correspinding Ct values to NA

#plotting script
source('~/Dropbox/R/pcr/doseRespAnalysis/doseRespPlot.R')