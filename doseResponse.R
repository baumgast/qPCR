#dose response qPCR data analysis

setwd('~/Dropbox/qPCR results/dose response/dense sampling/')
a = read.csv('1st series/2012-07-24-dense-dose-response-new-TFF1-primer-Results.csv', header = T)

titers = c('1e-8','1e-9','5e-10','2.5e-10','1.3e-10','6.3e-11','3.1e-11','1.6e-11','7.8e-12','3.9e-12','2e-12','1e-12','5e-13','2.5e-13','1.3e-13')
controls = c('DMEM','ICI','stripped')
targets = c('GREB1','TFF1-4','TFF1-5')

#a$Sample.Name = as.character(a$Sample.Name)

par(mfrow = c(2,1), xlog = F)

xlim = range(as.numeric(titers))
ylim = c(2^-max(a$Delta.Delta.Ct,na.rm = T),2^-min(a$Delta.Delta.Ct,na.rm = T))
col = c('darkblue','orange','tomato')

plot.new()
plot.window(xlim,ylim)
axis(1)
axis(2)
grid()
box()

title(main = 'dose response',
      xlab = 'E2 concentration (mol)',
      ylab = 'fold inducteion')

for (i in 1:length(targets)) {
  for (j in 1:length(titers)) {
    indexTarget = which(a$Target.Name == targets[i])
    selA = a[indexTarget,]
    indexTiter = which(selA$Sample.Name == (titers[j]))
    selAfin = selA[indexTiter,]
    t = as.numeric(titers[j])
    x = c(t,t,t)
    matpoints(x,2^-selAfin$Delta.Delta.Ct, pch = 20)
  }
}
