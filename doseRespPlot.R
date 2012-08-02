#plotting dose response curves
#
#input: ddCt matrices calculated with the doseResponse.R script
#------------------------------------------------------------------------------
#create concentration values from the titration names
titer = as.numeric(titers[4:18])

#plotting
par(mfrow = c(1,3))

ylim = c(0,150)

#first biological replicate
plot(titer,2^-ddCtGREB1.1[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
     main = 'first biological replicate',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
points(titer,2^-ddCtGREB1.1[2,4:18], pch = 19, type = 'b')
points(titer,2^-ddCtGREB1.1[3,4:18], pch = 15, type = 'b')

points(titer,2^-ddCtTFF1.4.1[1,4:18], pch = 17, type = 'b', col = 'blue')
points(titer,2^-ddCtTFF1.4.1[2,4:18], pch = 19, type = 'b', col = 'blue')
points(titer,2^-ddCtTFF1.4.1[3,4:18], pch = 15, type = 'b', col = 'blue')

points(titer,2^-ddCtTFF1.5.1[1,4:18], pch = 17, type = 'b', col = 'red')
points(titer,2^-ddCtTFF1.5.1[2,4:18], pch = 19, type = 'b', col = 'red')
points(titer,2^-ddCtTFF1.5.1[3,4:18], pch = 15, type = 'b', col = 'red')

legend('topleft', c('GREB1','TFF1-4','TFF1-5', paste('ref = ',titers[ref])), lwd = c(2,2,2,0), col = c('black','blue','red'), bty = 'n')

#second biological replicate
plot(titer,2^-ddCtGREB1.2[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
     main = 'second biological replicate',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
points(titer,2^-ddCtGREB1.2[2,4:18], pch = 19, type = 'b')
points(titer,2^-ddCtGREB1.2[3,4:18], pch = 15, type = 'b')

points(titer,2^-ddCtTFF1.2[1,4:18], pch = 17, type = 'b', col = 'blue')
points(titer,2^-ddCtTFF1.2[2,4:18], pch = 19, type = 'b', col = 'blue')
points(titer,2^-ddCtTFF1.2[3,4:18], pch = 15, type = 'b', col = 'blue')

legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue'), bty = 'n')

#third biological replicate
plot(titer,2^-ddCtGREB1.3[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
     main = 'third biological replicate',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
points(titer,2^-ddCtGREB1.3[2,4:18], pch = 19, type = 'b')
points(titer,2^-ddCtGREB1.3[3,4:18], pch = 15, type = 'b')

points(titer,2^-ddCtTFF1.3[1,4:18], pch = 17, type = 'b', col = 'blue')
points(titer,2^-ddCtTFF1.3[2,4:18], pch = 19, type = 'b', col = 'blue')
points(titer,2^-ddCtTFF1.3[3,4:18], pch = 15, type = 'b', col = 'blue')

legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue'), bty = 'n')

#barplot(2^-ddControl1, beside = T, names.arg = c('DMEM','ICI','stripped','DMEM','ICI','stripped','DMEM','ICI','stripped'))
#barplot(2^-ddControl2, beside = T, names.arg = c('DMEM','ICI','stripped','DMEM','ICI','stripped'))