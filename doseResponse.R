#dose response qPCR data analysis

setwd('~/Dropbox/qPCR results/dose response/dense sampling/')
a = read.csv('1st series/2012-07-24-dense-dose-response-new-TFF1-primer-Results.csv', header = T)

titers = c('DMEN','ICI','stripped','1e-8','1e-9','5e-10','2.5e-10','1.3e-10','6.3e-11','3.1e-11','1.6e-11','7.8e-12','3.9e-12','2e-12','1e-12','5e-13','2.5e-13','1.3e-13','control')
controls = c('DMEM','ICI','stripped')
targets = c('GREB1','TFF1-4','TFF1-5','GAPDH')

#a$Sample.Name = as.character(a$Sample.Name)
#extract Ct values for all four targets
for (i in 1:length(targets)) {
  for (i in 1:length(titers)) {
    rows = which(a$Target.Name == targets[i])
    selA = as.matrix(a[rows,])
    
    
    
  }
  dd = matrix(NA, nr = rows, nc = )
}
#delta delta Ct calculation
ddCtGREB1 = matrix(NA,nc = length(titers),nr = 3)
ddCtTFF1.4 = matrix(NA,nc = length(titers), nr = 3)
ddCTTFF1.5 = matrix(NA,nc = length(titers), nr = 3)