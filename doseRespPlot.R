#plotting dose response curves
#
#input: ddCt matrices calculated with the doseResponse.R script
#------------------------------------------------------------------------------
#reference point in the titration series
ref = 4
#call the calculation script
source('~/Dropbox/R/pcr/doseRespAnalysis/doseResponse.R')

#create concentration values from the titration names
titer = as.numeric(titers[4:18])

#plotting
par(mfrow = c(1,2))

ylim = c(0,3)
plot(titer,2^-ddCtGREB1a[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
     main = 'first biological replicate',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
points(titer,2^-ddCtGREB1a[2,4:18], pch = 19, type = 'b')
points(titer,2^-ddCtGREB1a[3,4:18], pch = 15, type = 'b')

# points(titer,2^-ddCtTFF1.4[1,4:18], pch = 17, type = 'b', col = 'blue')
# points(titer,2^-ddCtTFF1.4[2,4:18], pch = 19, type = 'b', col = 'blue')
# points(titer,2^-ddCtTFF1.4[3,4:18], pch = 15, type = 'b', col = 'blue')
# 
# points(titer,2^-ddCtTFF1.5[1,4:18], pch = 17, type = 'b', col = 'red')
# points(titer,2^-ddCtTFF1.5[2,4:18], pch = 19, type = 'b', col = 'red')
# points(titer,2^-ddCtTFF1.5[3,4:18], pch = 15, type = 'b', col = 'red')

legend('topleft', c('GREB1','TFF1-4','TFF1-5'), lwd = c(2,2,2), col = c('black','blue','red'), bty = 'n')

plot(titer,2^-ddCtGREB1b[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
     main = 'second biological replicate',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
points(titer,2^-ddCtGREB1b[2,4:18], pch = 19, type = 'b')
points(titer,2^-ddCtGREB1b[3,4:18], pch = 15, type = 'b')

# points(titer,2^-ddCtTFF1[1,4:18], pch = 17, type = 'b', col = 'blue')
# points(titer,2^-ddCtTFF1[2,4:18], pch = 19, type = 'b', col = 'blue')
# points(titer,2^-ddCtTFF1[3,4:18], pch = 15, type = 'b', col = 'blue')

legend('topleft',c('GREB1','TFF1'), lwd = c(2,2), col = c('black','blue'), bty = 'n')

#barplot(2^-ddControl1, beside = T, names.arg = c('DMEM','ICI','stripped','DMEM','ICI','stripped','DMEM','ICI','stripped'))
#barplot(2^-ddControl2, beside = T, names.arg = c('DMEM','ICI','stripped','DMEM','ICI','stripped'))