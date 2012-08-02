#dose response analysis master file. calls the data sorting and the plotting scripts
#
#-------------------------------------------------------------------------------

#reference point in the titration series
ref = 4
#call the calculation script
source('~/Dropbox/R/pcr/doseRespAnalysis/doseResponse.R')
#create the corresponding plots

mfrow = c(1,3)
ylim = c(0,3)