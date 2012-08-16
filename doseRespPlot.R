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

plot(2^-ddCtGREB1.1.mean[4:18],2^-ddCtGREB1.2.mean[4:18], pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-1',
     ylab = 'fold induction GREB1-2')
grid()
fold1 = 2^-ddCtGREB1.1.mean[4:18]
fold2 = 2^-ddCtGREB1.2.mean[4:18]
fmGREB1.1.2 = lm(fold2 ~ fold1 - 1)
segments(2^-ddCtGREB1.1.mean[4:18] - errGREB1.1[4:18],2^-ddCtGREB1.2.mean[4:18],2^-ddCtGREB1.1.mean[4:18] + errGREB1.1[4:18],2^-ddCtGREB1.2.mean[4:18])
segments(2^-ddCtGREB1.1.mean[4:18],2^-ddCtGREB1.2.mean[4:18] - errGREB1.2[4:18],2^-ddCtGREB1.1.mean[4:18],2^-ddCtGREB1.2.mean[4:18] + errGREB1.2[4:18])
abline(0,1, lty = 2)
abline(fmGREB1.1.2, col = 'grey', lwd = 2)
legend('bottomright',c(paste('slope =',round(fmGREB1.1.2$coefficients,4)), paste('ref =',titers[ref])), bty = 'n')

plot(2^-ddCtGREB1.1.mean[4:18],2^-ddCtGREB1.3.mean[4:18], pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-1',
     ylab = 'fold induction GREB1-3')
grid()
fold1 = 2^-ddCtGREB1.1.mean[4:18]
fold2 = 2^-ddCtGREB1.3.mean[4:18]
fmGREB1.1.3 = lm(fold2 ~ fold1 - 1)
segments(2^-ddCtGREB1.1.mean[4:18] - errGREB1.1[4:18],2^-ddCtGREB1.3.mean[4:18],2^-ddCtGREB1.1.mean[4:18] + errGREB1.1[4:18],2^-ddCtGREB1.3.mean[4:18])
segments(2^-ddCtGREB1.1.mean[4:18],2^-ddCtGREB1.3.mean[4:18] - errGREB1.3[4:18],2^-ddCtGREB1.1.mean[4:18],2^-ddCtGREB1.3.mean[4:18] + errGREB1.3[4:18])
abline(0,1, lty = 2)
abline(fmGREB1.1.3, col = 'grey', lwd = 2)
legend('bottomright',c(paste('slope =',round(fmGREB1.1.3$coefficients,4)), paste('ref =',titers[ref])), bty = 'n')

plot(2^-ddCtGREB1.2.mean[4:18],2^-ddCtGREB1.3.mean[4:18], pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction GREB1-2',
     ylab = 'fold induction GREB1-3')
grid()
fold1 = 2^-ddCtGREB1.2.mean[4:18]
fold2 = 2^-ddCtGREB1.3.mean[4:18]
fmGREB1.2.3 = lm(fold2 ~ fold1 - 1)
segments(2^-ddCtGREB1.2.mean[4:18] - errGREB1.2[4:18],2^-ddCtGREB1.3.mean[4:18],2^-ddCtGREB1.2.mean[4:18] + errGREB1.2[4:18],2^-ddCtGREB1.3.mean[4:18])
segments(2^-ddCtGREB1.2.mean[4:18],2^-ddCtGREB1.3.mean[4:18] - errGREB1.3[4:18],2^-ddCtGREB1.2.mean[4:18],2^-ddCtGREB1.3.mean[4:18] + errGREB1.3[4:18])
abline(0,1, lty = 2)
abline(fmGREB1.2.3, col = 'grey', lwd = 2)
legend('bottomright',c(paste('slope =',round(fmGREB1.2.3$coefficients,4)), paste('ref =',titers[ref])), bty = 'n')
#------------------------------------------------------------------------------
plot(2^-ddCtTFF1.5.1.mean[4:18],2^-ddCtTFF1.2.mean[4:18], pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction TFF1-1',
     ylab = 'fold induction TFF1-2')
grid()
fold1 = 2^-ddCtTFF1.5.1.mean[4:18]
fold2 = 2^-ddCtTFF1.2.mean[4:18]
fmTFF1.1.2 = lm(fold2 ~ fold1 - 1)
segments(2^-ddCtTFF1.5.1.mean[4:18] - errTFF1.5.1[4:18],2^-ddCtTFF1.2.mean[4:18],2^-ddCtTFF1.5.1.mean[4:18] + errTFF1.5.1[4:18],2^-ddCtTFF1.2.mean[4:18])
segments(2^-ddCtTFF1.5.1.mean[4:18],2^-ddCtTFF1.2.mean[4:18] - errTFF1.2[4:18],2^-ddCtTFF1.5.1.mean[4:18],2^-ddCtTFF1.2.mean[4:18] + errTFF1.2[4:18])
abline(0,1, lty = 2)
abline(fmTFF1.1.2, lwd = 2, col = 'grey')
legend('bottomright',c(paste('slope =',round(fmTFF1.1.2$coefficients,4)), paste('ref =',titers[ref])), bty = 'n')

plot(2^-ddCtTFF1.5.1.mean[4:18],2^-ddCtTFF1.3.mean[4:18], pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction TFF1-1',
     ylab = 'fold induction TFF1-3')
grid()
fold1 = 2^-ddCtTFF1.5.1.mean[4:18]
fold2 = 2^-ddCtTFF1.3.mean[4:18]
fmTFF1.1.3 = lm(fold2 ~ fold1 - 1)
segments(2^-ddCtTFF1.5.1.mean[4:18] - errTFF1.5.1[4:18],2^-ddCtTFF1.3.mean[4:18],2^-ddCtTFF1.5.1.mean[4:18] + errTFF1.5.1[4:18],2^-ddCtTFF1.3.mean[4:18])
segments(2^-ddCtTFF1.5.1.mean[4:18],2^-ddCtTFF1.3.mean[4:18] - errTFF1.3[4:18],2^-ddCtTFF1.5.1.mean[4:18],2^-ddCtTFF1.3.mean[4:18] + errTFF1.3[4:18])
abline(0,1, lty = 2)
abline(fmTFF1.1.3, lwd = 2, col = 'grey')
legend('bottomright',c(paste('slope =',round(fmTFF1.1.3$coefficients,4)), paste('ref =',titers[ref])), bty = 'n')

plot(2^-ddCtTFF1.2.mean[4:18],2^-ddCtTFF1.3.mean[4:18], pch = 20, xlim = lim, ylim = lim,
     xlab = 'fold induction TFF1-2',
     ylab = 'fold induction TFF1-3')
grid()
fold1 = 2^-ddCtTFF1.2.mean[4:18]
fold2 = 2^-ddCtTFF1.3.mean[4:18]
fmTFF1.2.3 = lm(fold2 ~ fold1 - 1)
segments(2^-ddCtTFF1.2.mean[4:18] - errTFF1.2[4:18],2^-ddCtTFF1.3.mean[4:18],2^-ddCtTFF1.2.mean[4:18] + errTFF1.2[4:18],2^-ddCtTFF1.3.mean[4:18])
segments(2^-ddCtTFF1.2.mean[4:18],2^-ddCtTFF1.3.mean[4:18] - errTFF1.3[4:18],2^-ddCtTFF1.2.mean[4:18],2^-ddCtTFF1.3.mean[4:18] + errTFF1.3[4:18])
abline(0,1, lty = 2)
abline(fmTFF1.2.3, lwd = 2, col = 'grey')
legend('bottomright',c(paste('slope =',round(fmTFF1.2.3$coefficients,4)), paste('ref =',titers[ref])), bty = 'n')

#-------------------------------------------------------------------------------
#rescaling to adjust the lines to each other and rescaling to a range from 0 to 100
#GREB1.1 as standard for GREB1
GREB1.1    = 2^-ddCtGREB1.1.mean[4:18]
GREB1.2    = 2^-ddCtGREB1.2.mean[4:18]/fmGREB1.1.2$coefficients
ErrGREB1.2 = errGREB1.2[4:18]/fmGREB1.1.2$coefficients
GREB1.3    = 2^-ddCtGREB1.3.mean[4:18]/fmGREB1.1.3$coefficients
ErrGREB1.3 = errGREB1.3[4:18]/fmGREB1.1.3$coefficients

TFF1.1    = 2^-ddCtTFF1.5.1.mean[4:18]
TFF1.2    = 2^-ddCtTFF1.2.mean[4:18]/fmTFF1.1.2$coefficients
ErrTFF1.2 = errTFF1.2[4:18]/fmTFF1.1.2$coefficients
TFF1.3    = 2^-ddCtTFF1.3.mean[4:18]/fmTFF1.1.3$coefficients
ErrTFF1.3 = errTFF1.3[4:18]/fmTFF1.1.3$coefficients

ylim = c(0,50)
par(mfrow = c(1,2))

plot(titer,GREB1.1, type = 'p', log = 'x', ylim = ylim, pch = 19,
     main = 'GREB1',
     xlab = 'E2 concentration (mol)',
     ylab = 'Fold induction')
grid()
segments(titer,GREB1.1-errGREB1.1[4:18],titer,GREB1.1+errGREB1.1[4:18])
points(titer,GREB1.2, type = 'p', col = 'red', pch = 17)
segments(titer,GREB1.2 - ErrGREB1.2,titer,GREB1.2+ErrGREB1.2, col = 'red')
points(titer,GREB1.3, type = 'p', col = 'blue', pch = 15)
segments(titer,GREB1.3-ErrGREB1.3,titer,GREB1.3+ErrGREB1.3, col = 'blue')

plot(titer,TFF1.1, type = 'p', log = 'x', ylim = ylim, pch = 19,
     main = 'TFF1',
     xlab = 'E2 concentration (mol)',
     ylab = 'Fold induction')
grid()
segments(titer,TFF1.1-errTFF1.5.1[4:18],titer,TFF1.1+errTFF1.5.1[4:18])
points(titer,TFF1.2, type = 'p', pch = 17, col = 'red')
segments(titer,TFF1.2-ErrTFF1.2,titer,TFF1.2+ErrTFF1.2, col = 'red')
points(titer,TFF1.3, type = 'p', pch = 15, col = 'blue')
segments(titer,TFF1.3-ErrTFF1.3,titer,TFF1.3+ErrTFF1.3, col = 'blue')

#-------------------------------------------------------------------------------
#plot the control experiments as barplot
ControlGREB1 = matrix(NA,nr = 3,nc = 2)
ErrContGREB1 = matrix(NA,nr = 3, nc = 2)
colnames(ControlGREB1) = c('DMEM','ICI')
colnames(ErrContGREB1) = c('DMEM','ICI')

ControlGREB1[1,1] = 2^-ddCtGREB1.1.mean[1]
ControlGREB1[2,1] = 2^-ddCtGREB1.2.mean[1]
ControlGREB1[3,1] = 2^-ddCtGREB1.3.mean[1]

ControlGREB1[1,2] = 2^-ddCtGREB1.1.mean[2]
ControlGREB1[2,2] = 2^-ddCtGREB1.2.mean[2]
ControlGREB1[3,2] = 2^-ddCtGREB1.3.mean[2]

ErrContGREB1[1,1] = errGREB1.1[1]
ErrContGREB1[2.1] = errGREB1.2[1]
ErrContGREB1[3,1] = errGREB1.3[1]

ErrContGREB1[1,2] = errGREB1.1[2]
ErrContGREB1[2,2] = errGREB1.2[2]
ErrContGREB1[3,2] = errGREB1.3[2]

par(mfrow = c(1,1))
ylim = c(0,90)
bar = barplot(ControlGREB1, beside = T, names.arg = c('DMEM', 'ICI'),
              ylim = ylim,
              main = 'Controls GREB1',
              ylab = 'Fold induction',
              border  = 'white',
              col = 'gray')

segments(bar,ControlGREB1 - ErrContGREB1, bar, ControlGREB1 + ErrContGREB1, lwd = 2)

#------------------------------------------------------------------------------
ControlTFF1 = matrix(NA,nr = 3, nc = 2)
ErrContTFF1 = matrix(NA,nr = 3, nc = 2)
colnames(ControlTFF1) = c('DMEM','ICI')
colnames(ErrContTFF1) = c('DMEM','ICI')

ControlTFF1[1,1] = 2^-ddCtTFF1.5.1.mean[1]
ControlTFF1[2,1] = 2^-ddCtTFF1.2.mean[1]
ControlTFF1[3,1] = 2^-ddCtTFF1.3.mean[1]

ControlTFF1[1,2] = 2^-ddCtTFF1.5.1.mean[2]
ControlTFF1[2,2] = 2^-ddCtTFF1.2.mean[2]
ControlTFF1[3,2] = 2^-ddCtTFF1.3.mean[2]

ErrContTFF1[1,1] = errTFF1.5.1[1]
ErrContTFF1[2,1] = errTFF1.2[1]
ErrContTFF1[3,1] = errTFF1.3[1]

ErrContTFF1[1,2] = errTFF1.5.1[2]
ErrContTFF1[2,2] = errTFF1.2[2]
ErrContTFF1[3,2] = errTFF1.3[2]

par(mfrow = c(1,1))

bar = barplot(ControlTFF1, beside = T, names.arg = c('DMEM','ICI'),
              ylim = ylim,
              main = 'Controls TFF1',
              ylab = 'Fold induction',
              border = 'white',
              col = 'gray')

segments(bar,ControlTFF1 - ErrContTFF1,bar,ControlTFF1+ErrContTFF1, lwd = 2)