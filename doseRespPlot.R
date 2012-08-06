#plotting dose response curves
#
#input: ddCt matrices calculated with the doseResponse.R script
#------------------------------------------------------------------------------
#create concentration values from the titration names
titer = as.numeric(titers[4:18])

#plotting
par(mfrow = c(2,3))

#first biological replicate
if (selectGREB1 == 1) {
  plot(titer,2^-ddCtGREB1.1[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
       main = 'first biological replicate',
       xlab = 'E2 concentration (mol)',
       ylab = 'fold induction')
  grid()
  points(titer,2^-ddCtGREB1.1[2,4:18], pch = 19, type = 'b')
  points(titer,2^-ddCtGREB1.1[3,4:18], pch = 15, type = 'b')
}

if (selectTFF1 == 1) {
  points(titer,2^-ddCtTFF1.4.1[1,4:18], pch = 17, type = 'b', col = 'blue')
  points(titer,2^-ddCtTFF1.4.1[2,4:18], pch = 19, type = 'b', col = 'blue')
  points(titer,2^-ddCtTFF1.4.1[3,4:18], pch = 15, type = 'b', col = 'blue')
  
  points(titer,2^-ddCtTFF1.5.1[1,4:18], pch = 17, type = 'b', col = 'red')
  points(titer,2^-ddCtTFF1.5.1[2,4:18], pch = 19, type = 'b', col = 'red')
  points(titer,2^-ddCtTFF1.5.1[3,4:18], pch = 15, type = 'b', col = 'red')
}

legend('topleft', c('GREB1','TFF1-4','TFF1-5', paste('ref = ',titers[ref])), lwd = c(2,2,2,0), col = c('black','blue','red','white'), bty = 'n')

#second biological replicate
if (selectGREB1 == 1) {
  plot(titer,2^-ddCtGREB1.2[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
       main = 'second biological replicate',
       xlab = 'E2 concentration (mol)',
       ylab = 'fold induction')
  grid()
  points(titer,2^-ddCtGREB1.2[2,4:18], pch = 19, type = 'b')
  points(titer,2^-ddCtGREB1.2[3,4:18], pch = 15, type = 'b')
}

if (selectTFF1 == 1) {
  points(titer,2^-ddCtTFF1.2[1,4:18], pch = 17, type = 'b', col = 'blue')
  points(titer,2^-ddCtTFF1.2[2,4:18], pch = 19, type = 'b', col = 'blue')
  points(titer,2^-ddCtTFF1.2[3,4:18], pch = 15, type = 'b', col = 'blue')
}
legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue','white'), bty = 'n')

#third biological replicate
if (selectGREB1 == 1) {
  plot(titer,2^-ddCtGREB1.3[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
       main = 'third biological replicate',
       xlab = 'E2 concentration (mol)',
       ylab = 'fold induction')
  grid()
  points(titer,2^-ddCtGREB1.3[2,4:18], pch = 19, type = 'b')
  points(titer,2^-ddCtGREB1.3[3,4:18], pch = 15, type = 'b')
}
if (selectTFF1 == 1) {
  points(titer,2^-ddCtTFF1.3[1,4:18], pch = 17, type = 'b', col = 'blue')
  points(titer,2^-ddCtTFF1.3[2,4:18], pch = 19, type = 'b', col = 'blue')
  points(titer,2^-ddCtTFF1.3[3,4:18], pch = 15, type = 'b', col = 'blue')
}
legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue','white'), bty = 'n')
#-------------------------------------------------------------------------------
#plot mean values
plot(titer,2^-ddCtGREB1.1.mean[4:18], pch = 20, log = 'x', ylim = ylim, main = 'mean', type = 'b',
     xlab = 'E2 concentration (mol)',
     ylab = ' fold induction')
grid()
segments(titer,2^-ddCtGREB1.1.mean[4:18] - errGREB1.1[4:18],titer,2^-ddCtGREB1.1.mean[4:18] + errGREB1.1[4:18])
points(titer,2^-ddCtTFF1.4.1.mean[4:18], pch = 17, type = 'b', col = 'blue')
segments(titer,2^-ddCtTFF1.4.1.mean[4:18] - errTFF1.4.1[4:18], titer,2^-ddCtTFF1.4.1.mean[4:18] + errTFF1.4.1[4:18], col = 'blue')
points(titer,2^-ddCtTFF1.5.1.mean[4:18], pch = 15, type = 'b', col = 'red')
segments(titer,2^-ddCtTFF1.5.1.mean[4:18] - errTFF1.5.1[4:18], titer,2^-ddCtTFF1.5.1.mean[4:18] + errTFF1.5.1[4:18], col = 'blue')

legend('topleft', c('GREB1','TFF1-4','TFF1-5', paste('ref = ',titers[ref])), lwd = c(2,2,2,0), col = c('black','blue','red','white'), bty = 'n')

plot(titer,2^-ddCtGREB1.2.mean[4:18], type = 'b', ylim = ylim, pch = 19, log = 'x',main = 'mean',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
segments(titer,2^-ddCtGREB1.2.mean[4:18] - errGREB1.2[4:18],titer,2^-ddCtGREB1.2.mean[4:18] + errGREB1.2[4:18])
points(titer,2^-ddCtTFF1.2.mean[4:18], type = 'b',pch = 17, col = 'blue')
segments(titer,2^-ddCtTFF1.2.mean[4:18] - errTFF1.2[4:18],titer,2^-ddCtTFF1.2.mean[4:18] + errTFF1.2[4:18], col = 'blue')

legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue','white'), bty = 'n')


plot(titer,2^-ddCtGREB1.3.mean[4:18], type = 'b', ylim = ylim, pch = 19, log = 'x',main = 'mean',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
segments(titer,2^-ddCtGREB1.3.mean[4:18] - errGREB1.3[4:18],titer,2^-ddCtGREB1.3.mean[4:18] + errGREB1.3[4:18])
points(titer,2^-ddCtTFF1.3.mean[4:18], type = 'b',pch = 17, col = 'blue')
segments(titer,2^-ddCtTFF1.3.mean[4:18] - errTFF1.3[4:18],titer,2^-ddCtTFF1.3.mean[4:18] + errTFF1.3[4:18], col = 'blue')

legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue','white'), bty = 'n')
#-------------------------------------------------------------------------------
#plot the mean values of both genes of all three replicates into one graph
par(mfrow = c(1,2))

plot(titer,2^-ddCtGREB1.1.mean[4:18], pch = 19, type = 'b', log = 'x', main = 'GREB1 mean, all biological replicates', ylim = ylimMean,
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
segments(titer,2^-ddCtGREB1.1.mean[4:18] - errGREB1.1[4:18],titer,2^-ddCtGREB1.1.mean[4:18] + errGREB1.1[4:18])
points(titer,2^-ddCtGREB1.2.mean[4:18], pch = 17, type = 'b', col = 'red')
segments(titer,2^-ddCtGREB1.2.mean[4:18] - errGREB1.2[4:18],titer,2^-ddCtGREB1.2.mean[4:18] + errGREB1.2[4:18])
points(titer,2^-ddCtGREB1.3.mean[4:18], pch = 15, type = 'b', col = 'blue')
segments(titer,2^-ddCtGREB1.3.mean[4:18] - errGREB1.3[4:18],titer,2^-ddCtGREB1.3.mean[4:18] + errGREB1.3[4:18])

legend('topleft',c('1st Replicate','2nd Replicate','3rd Replicate', paste('ref =',titers[ref])), lwd = c(2,2,2,0), 
       col = c('black','red','blue','white'), bty = 'n')

plot(titer,2^-ddCtTFF1.4.1.mean[4:18], pch = 19, type = 'b', log = 'x', main = 'TFF1 mean, all biological replicates', ylim = ylimMean,
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
segments(titer,2^-ddCtTFF1.4.1.mean[4:18] - errTFF1.4.1[4:18], titer,2^-ddCtTFF1.4.1.mean[4:18] + errTFF1.4.1[4:18], col = 'blue')
points(titer,2^-ddCtTFF1.5.1.mean[4:18], pch = 18, type = 'b', col = 'green')
segments(titer,2^-ddCtTFF1.5.1.mean[4:18] - errTFF1.5.1[4:18], titer,2^-ddCtTFF1.5.1.mean[4:18] + errTFF1.5.1[4:18], col = 'blue')
points(titer,2^-ddCtTFF1.2.mean[4:18], pch = 17, type = 'b', col = 'red')
segments(titer,2^-ddCtTFF1.2.mean[4:18] - errTFF1.2[4:18],titer,2^-ddCtTFF1.2.mean[4:18] + errTFF1.2[4:18], col = 'blue')
points(titer,2^-ddCtTFF1.3.mean[4:18], pch = 15, type = 'b', col = 'blue')
segments(titer,2^-ddCtTFF1.3.mean[4:18] - errTFF1.3[4:18],titer,2^-ddCtTFF1.3.mean[4:18] + errTFF1.3[4:18], col = 'blue')

legend('topleft',c('1st Replicate TFF1-4','1st replicate TFF1-5','2nd Replicate','3rd Replicate', paste('ref =',titers[ref])), lwd = c(2,2,2,2,0), 
       col = c('black','green','red','blue','white'), bty = 'n')
#-------------------------------------------------------------------------------
#plotting biological replicats against each other
par(mfrow = c(2,3))
lim = c(-30,100)

plot(2^-ddCtGREB1.1.mean,2^-ddCtGREB1.2.mean, pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-1',
     ylab = 'fold induction GREB1-2')
grid()
segments(2^-ddCtGREB1.1.mean - errGREB1.1,2^-ddCtGREB1.2.mean,2^-ddCtGREB1.1.mean + errGREB1.1,2^-ddCtGREB1.2.mean)
segments(2^-ddCtGREB1.1.mean,2^-ddCtGREB1.2.mean - errGREB1.2,2^-ddCtGREB1.1.mean,2^-ddCtGREB1.2.mean + errGREB1.2)
abline(0,1, lty = 2)

plot(2^-ddCtGREB1.1.mean,2^-ddCtGREB1.3.mean, pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-1',
     ylab = 'fold induction GREB1-3')
grid()
segments(2^-ddCtGREB1.1.mean - errGREB1.1,2^-ddCtGREB1.3.mean,2^-ddCtGREB1.1.mean + errGREB1.1,2^-ddCtGREB1.3.mean)
segments(2^-ddCtGREB1.1.mean,2^-ddCtGREB1.3.mean - errGREB1.3,2^-ddCtGREB1.1.mean,2^-ddCtGREB1.3.mean + errGREB1.3)
abline(0,1, lty = 2)

plot(2^-ddCtGREB1.2.mean,2^-ddCtGREB1.3.mean, pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-2',
     ylab = 'fold induction GREB1-3')
segments(2^-ddCtGREB1.2.mean - errGREB1.2,2^-ddCtGREB1.3.mean,2^-ddCtGREB1.2.mean + errGREB1.2,2^-ddCtGREB1.3.mean)
segments(2^-ddCtGREB1.2.mean,2^-ddCtGREB1.3.mean - errGREB1.3,2^-ddCtGREB1.2.mean,2^-ddCtGREB1.3.mean + errGREB1.3)
grid()
abline(0,1, lty = 2)

#------------------------------------------------------------------------------
plot(2^-ddCtTFF1.4.1.mean,2^-ddCtTFF1.2.mean, pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-1',
     ylab = 'fold induction GREB1-2')
grid()
segments(2^-ddCtTFF1.4.1.mean - errTFF1.4.1,2^-ddCtTFF1.2.mean,2^-ddCtTFF1.4.1.mean + errTFF1.4.1,2^-ddCtTFF1.2.mean)
segments(2^-ddCtTFF1.4.1.mean,2^-ddCtTFF1.2.mean - errTFF1.2,2^-ddCtTFF1.4.1.mean,2^-ddCtTFF1.2.mean + errTFF1.2)
abline(0,1, lty = 2)

plot(2^-ddCtTFF1.4.1.mean,2^-ddCtTFF1.3.mean, pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-1',
     ylab = 'fold induction GREB1-3')
grid()
segments(2^-ddCtTFF1.4.1.mean - errTFF1.4.1,2^-ddCtTFF1.3.mean,2^-ddCtTFF1.4.1.mean + errTFF1.4.1,2^-ddCtTFF1.3.mean)
segments(2^-ddCtTFF1.4.1.mean,2^-ddCtTFF1.3.mean - errTFF1.3,2^-ddCtTFF1.4.1.mean,2^-ddCtTFF1.3.mean + errTFF1.3)
abline(0,1, lty = 2)

plot(2^-ddCtTFF1.2.mean,2^-ddCtTFF1.3.mean, pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-2',
     ylab = 'fold induction GREB1-3')
grid()
segments(2^-ddCtTFF1.2.mean - errTFF1.2,2^-ddCtTFF1.3.mean,2^-ddCtTFF1.2.mean + errTFF1.2,2^-ddCtTFF1.3.mean)
segments(2^-ddCtTFF1.2.mean,2^-ddCtTFF1.3.mean - errTFF1.3,2^-ddCtTFF1.2.mean,2^-ddCtTFF1.3.mean + errTFF1.3)
abline(0,1, lty = 2)