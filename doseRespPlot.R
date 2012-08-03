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

#plot mean values
plot(titer,2^-ddCtGREB1.1.mean[4:18], pch = 19, log = 'x', ylim = ylim, main = 'mean', type = 'b',
     xlab = 'E2 concentration (mol)',
     ylab = ' fold induction')
points(titer,2^-ddCtTFF1.4.1.mean[4:18], pch = 17, type = 'b', col = 'blue')
points(titer,2^-ddCtTFF1.5.1.mean[4:18], pch = 15, type = 'b', col = 'red')

legend('topleft', c('GREB1','TFF1-4','TFF1-5', paste('ref = ',titers[ref])), lwd = c(2,2,2,0), col = c('black','blue','red','white'), bty = 'n')

plot(titer,2^-ddCtGREB1.2.mean[4:18], type = 'b', ylim = ylim, pch = 19, log = 'x',main = 'mean',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
points(titer,2^-ddCtTFF1.2.mean[4:18], type = 'b',pch = 17, col = 'blue')

legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue','white'), bty = 'n')


plot(titer,2^-ddCtGREB1.3.mean[4:18], type = 'b', ylim = ylim, pch = 19, log = 'x',main = 'mean',
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
points(titer,2^-ddCtTFF1.3.mean[4:18], type = 'b',pch = 17, col = 'blue')

legend('topleft',c('GREB1','TFF1',paste('ref = ',titers[ref])), lwd = c(2,2,0), col = c('black','blue','white'), bty = 'n')
