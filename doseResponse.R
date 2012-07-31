#dose response qPCR data analysis

setwd('~/Dropbox/qPCR results/dose response/dense sampling/')
a = read.csv('1st series/2012-07-24-dense-dose-response-new-TFF1-primer-Results.csv', header = T, na.strings = 'Undetermined')

titers   = c('DMEM','ICI','stripped','1e-8','1e-9','5e-10','2.5e-10','1.3e-10','6.3e-11','3.1e-11','1.6e-11','7.8e-12',
             '3.9e-12','2e-12','1e-12','5e-13','2.5e-13','1.3e-13','control')
controls = c('DMEM','ICI','stripped')
targets  = c('GREB1','TFF1-4','TFF1-5','GAPDH')
targets1 = c('GREB1','TFF1.4','TFF1.5','GAPDH')

#extract Ct values for all four targets
for (i in 1:length(targets)) {
  dd = matrix(NA,nc = length(titers), nr = 3)
  colnames(dd) = titers
  Name = paste('Ct',targets1[i],sep = '')
  for (j in 1:length(titers)) {
    rows = which(a$Target.Name == targets[i])
    selA = (a[rows,])
    rowsTiter = which(selA[,4] == titers[j])

    dd[,j] = selA[rowsTiter,12]
  }
  assign(Name,dd)
}
#delta delta Ct calculation: (Ct_target - Ct_GAPDH) - (Ct_target_ref - Ct_GAPDH_ref)
#ref = stripped
ddCtGREB1  = CtGREB1 - CtGAPDH -(CtGREB1[,3] - CtGAPDH[,3])
ddCtTFF1.4 = CtTFF1.4 - CtGAPDH - (CtTFF1.4[,3] - CtGAPDH[,3])
ddCTTFF1.5 = CtTFF1.5 - CtGAPDH - (CtTFF1.5[,3] - CtGAPDH[,3])

titer = as.numeric(titers[4:18])

ylim = c(0,50)
plot(titer,2^-ddCtGREB1[1,4:18], pch = 17, log = 'x', type = 'b', ylim = ylim,
     xlab = 'E2 concentration (mol)',
     ylab = 'fold induction')
grid()
points(titer,2^-ddCtGREB1[2,4:18], pch = 19, type = 'b')
points(titer,2^-ddCtGREB1[3,4:18], pch = 15, type = 'b')
