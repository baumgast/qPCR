#dose response qPCR data analysis

setwd('~/Dropbox/qPCR results/dose response/dense sampling/')
a = read.csv('1st series/2012-07-24-dense-dose-response-new-TFF1-primer-Results.csv', header = T, na.strings = 'Undetermined')
b = read.csv('2nd series/2012-07-27-doseResp_dense-2ndSeries-Results.csv', header = T, na.strings = 'Undetermined')

titers    = c('DMEM','ICI','stripped','1e-8','1e-9','5e-10','2.5e-10','1.3e-10','6.3e-11','3.1e-11','1.6e-11','7.8e-12',
             '3.9e-12','2e-12','1e-12','5e-13','2.5e-13','1.3e-13','control')
controls  = c('DMEM','ICI','stripped')
targetsa  = c('GREB1','TFF1-4','TFF1-5','GAPDH')
targetsb  = c('GREB1','TFF1','GAPDH')
targets1  = c('GREB1','TFF1.4','TFF1.5','GAPDH')

#extract Ct values for all four targets
for (i in 1:length(targetsa)) {
  dd = matrix(NA,nc = length(titers), nr = 3)
  colnames(dd) = titers
  Name = paste('Ct',targets1[i],sep = '')
  for (j in 1:length(titers)) {
    rows = which(a$Target.Name == targetsa[i])
    selA = (a[rows,])
    rowsTiter = which(selA[,4] == titers[j])

    dd[,j] = selA[rowsTiter,12]
  }
  assign(Name,dd)
}
#delta delta Ct calculation: (Ct_target - Ct_GAPDH) - (Ct_target_ref - Ct_GAPDH_ref)
#ref = stripped
ref = 4
ddCtGREB1a  = CtGREB1 - CtGAPDH -(CtGREB1[,ref] - CtGAPDH[,ref])
ddCtTFF1.4 = CtTFF1.4 - CtGAPDH - (CtTFF1.4[,ref] - CtGAPDH[,ref])
ddCtTFF1.5 = CtTFF1.5 - CtGAPDH - (CtTFF1.5[,ref] - CtGAPDH[,ref])

control1 = matrix(NA,nr = 3,nc = 3)
ddControl1 = matrix(NA,nr = 3,nc = 9)
control1[,1] = CtGREB1[,1]
control1[,2] = CtGREB1[,2]
control1[,3] = CtGREB1[,3]

ddControl1[,1:3] = control1 - CtGAPDH[,1:3] -(CtGREB1[,ref] - CtGAPDH[,ref])

control1[,1] = CtTFF1.4[,1]
control1[,2] = CtTFF1.4[,2]
control1[,3] = CtTFF1.4[,3]

ddControl1[,4:6] = control1 - CtGAPDH[,1:3] -(CtTFF1.4[,ref] - CtGAPDH[,ref])

control1[,1] = CtTFF1.5[,1]
control1[,2] = CtTFF1.5[,2]
control1[,3] = CtTFF1.5[,3]

ddControl1[,7:9] = control1 - CtGAPDH[,1:3] -(CtTFF1.5[,ref] - CtGAPDH[,ref])

for (i in 1:length(targetsb)) {
  dd = matrix(NA,nc = length(titers), nr = 3)
  colnames(dd) = titers
  Name = paste('Ct',targetsb[i],sep = '')
  for (j in 1:length(titers)) {
    rows = which(b$Target.Name == targetsb[i])
    selB = (b[rows,])
    rowsTiter = which(selB[,4] == titers[j])
    
    dd[,j] = selB[rowsTiter,12]
  }
  assign(Name,dd)
}
#delta delta Ct calculation: (Ct_target - Ct_GAPDH) - (Ct_target_ref - Ct_GAPDH_ref)
#ref = stripped
ddCtGREB1b = CtGREB1 - CtGAPDH -(CtGREB1[,ref] - CtGAPDH[,ref])
ddCtTFF1   = CtTFF1 - CtGAPDH - (CtTFF1[,ref] - CtGAPDH[,ref])

control1     = matrix(NA,nr = 3,nc = 3)
ddControl2   = matrix(NA,nr = 3,nc = 6)
control1[,1] = CtGREB1[,1]
control1[,2] = CtGREB1[,2]
control1[,3] = CtGREB1[,3]

ddControl2[,1:3] = control1 - CtGAPDH[,1:3] -(CtGREB1[,ref] - CtGAPDH[,ref])

control1[,1] = CtTFF1[,1]
control1[,2] = CtTFF1[,2]
control1[,3] = CtTFF1[,3]

ddControl2[,4:6] = control1 - CtGAPDH[,1:3] -(CtTFF1[,ref] - CtGAPDH[,ref])

titer = as.numeric(titers[4:18])

par(mfrow = c(2,2))

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

barplot(2^-ddControl1, beside = T, names.arg = c('DMEM','ICI','stripped','DMEM','ICI','stripped','DMEM','ICI','stripped'))
barplot(2^-ddControl2, beside = T, names.arg = c('DMEM','ICI','stripped','DMEM','ICI','stripped'))