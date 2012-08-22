#dose response qPCR data analysis
#read in csv files with the Ct values
setwd('~/Dropbox/qPCR results/dose response/dense sampling/')
a = read.csv('1st series/2012-07-24-dense-dose-response-new-TFF1-primer-Results.csv', header = T, na.strings = 'Undetermined')
b = read.csv('2nd series/2012-07-27-doseResp_dense-2ndSeries-Results.csv', header = T, na.strings = 'Undetermined')
c = read.csv('3rd series/2012-08-02-doseResp-3rdSeries-3-Results.csv', header = T, na.strings = 'Undetermined')

#define the titrations to sort out the data of the above files
titers    = c('DMEM','ICI','stripped','1e-8','1e-9','5e-10','2.5e-10','1.3e-10','6.3e-11','3.1e-11','1.6e-11','7.8e-12',
             '3.9e-12','2e-12','1e-12','5e-13','2.5e-13','1.3e-13','control')
controls  = c('DMEM','ICI','stripped')
#targets for the first biological replicate
targetsa  = c('GREB1','TFF1-4','TFF1-5','GAPDH')
#target names of the first series for further procession
targets1  = c('GREB1','TFF1.4','TFF1.5','GAPDH')
#targets of the 2nd and 3rd replicate
targetsb  = c('GREB1','TFF1','GAPDH')

#-------------------------------------------------------------------------------
#extract Ct values for all four targets
for (i in 1:length(targetsa)) {
  dd = matrix(NA,nc = length(titers), nr = 3)
  colnames(dd) = titers
  Name = paste('Ct',targets1[i],'1',sep = '.')
  for (j in 1:length(titers)) {
    rows = which(a$Target.Name == targetsa[i])
    selA = (a[rows,])
    rowsTiter = which(selA[,4] == titers[j])

    dd[,j] = selA[rowsTiter,12]
  }
  assign(Name,dd)
}

for (i in 1:length(targetsb)) {
  dd2 = matrix(NA,nc = length(titers), nr = 3)
  dd3 = matrix(NA,nc = length(titers), nr = 3)
  colnames(dd2) = titers
  colnames(dd3) = titers  
  Name2 = paste('Ct',targetsb[i],'2', sep = '.')
  Name3 = paste('Ct',targetsb[i],'3', sep = '.')
  for (j in 1:length(titers)) {
    rows = which(b$Target.Name == targetsb[i])
    selB = (b[rows,])
    rowsTiter = which(selB[,4] == titers[j])
    dd2[,j] = selB[rowsTiter,12]
    
    rows = which(c$Target.Name == targetsb[i])
    selC = c[rows,]
    rowsTiter = which(selC[,4] == titers[j])
    dd3[,j] = selC[rowsTiter,12]
  }
  assign(Name2,dd2)
  assign(Name3,dd3)
}
#-------------------------------------------------------------------------------

#sort out outliers manually
#GREB1.1
Ct.GAPDH.1[3,15] = NA
#TFF1
Ct.TFF1.4.1[2,15] = NA
#mean and std Ct values
Ct.GAPDH.1.mean = apply(Ct.GAPDH.1,2,mean,na.rm = T)
Ct.GAPDH.2.mean = apply(Ct.GAPDH.2,2,mean,na.rm = T)
Ct.GAPDH.3.mean = apply(Ct.GAPDH.3,2,mean,na.rm = T)
Ct.GAPDH.3.mean[9] = NA

Ct.GAPDH.1.sd = apply(Ct.GAPDH.1,2,sd,na.rm = T)
Ct.GAPDH.2.sd = apply(Ct.GAPDH.2,2,sd,na.rm = T)
Ct.GAPDH.3.sd = apply(Ct.GAPDH.3,2,sd,na.rm = T)

Ct.GREB1.1.mean = apply(Ct.GREB1.1,2,mean,na.rm = T)
Ct.GREB1.2.mean = apply(Ct.GREB1.2,2,mean,na.rm = T)
Ct.GREB1.3.mean = apply(Ct.GREB1.3,2,mean,na.rm = T)

Ct.GREB1.1.sd = apply(Ct.GREB1.1,2,sd,na.rm = T)
Ct.GREB1.2.sd = apply(Ct.GREB1.2,2,sd,na.rm = T)
Ct.GREB1.3.sd = apply(Ct.GREB1.3,2,sd,na.rm = T)

Ct.TFF1.4.1.mean = apply(Ct.TFF1.4.1,2,mean,na.rm = T)

Ct.TFF1.4.1.sd = apply(Ct.TFF1.4.1,2,sd,na.rm = T)

Ct.TFF1.5.1.mean = apply(Ct.TFF1.5.1,2,mean,na.rm = T)

Ct.TFF1.5.1.sd = apply(Ct.TFF1.5.1,2,sd,na.rm = T)

Ct.TFF1.2.mean = apply(Ct.TFF1.2,2,mean,na.rm = T)
Ct.TFF1.3.mean = apply(Ct.TFF1.3,2,mean,na.rm = T)

Ct.TFF1.2.sd = apply(Ct.TFF1.2,2,sd,na.rm = T)
Ct.TFF1.3.sd = apply(Ct.TFF1.3,2,sd,na.rm = T)
#-------------------------------------------------------------------------------
#delta delta Ct calculation: (Ct_target - Ct_GAPDH) - (Ct_target_ref - Ct_GAPDH_ref)
#single curves
ddCtGREB1.1  = Ct.GREB1.1 - Ct.GAPDH.1 -(Ct.GREB1.1[,ref] - Ct.GAPDH.1[,ref])
ddCtGREB1.2  = Ct.GREB1.2 - Ct.GAPDH.2 -(Ct.GREB1.2[,ref] - Ct.GAPDH.2[,ref])
ddCtGREB1.3  = Ct.GREB1.3 - Ct.GAPDH.3 -(Ct.GREB1.3[,ref] - Ct.GAPDH.3[,ref])

ddCtTFF1.4.1 = Ct.TFF1.4.1 - Ct.GAPDH.1 - (Ct.TFF1.4.1[,ref] - Ct.GAPDH.1[,ref])
ddCtTFF1.5.1 = Ct.TFF1.5.1 - Ct.GAPDH.1 - (Ct.TFF1.5.1[,ref] - Ct.GAPDH.1[,ref])
ddCtTFF1.2   = Ct.TFF1.2 - Ct.GAPDH.2 - (Ct.TFF1.2[,ref] - Ct.GAPDH.2[,ref])
ddCtTFF1.3   = Ct.TFF1.3 - Ct.GAPDH.3 - (Ct.TFF1.3[,ref] - Ct.GAPDH.3[,ref])

#mean curves for biologial replicates
ddCtGREB1.1.mean = Ct.GREB1.1.mean - Ct.GAPDH.1.mean - (Ct.GREB1.1.mean[ref] - Ct.GAPDH.1.mean[ref])
ddCtGREB1.2.mean = Ct.GREB1.2.mean - Ct.GAPDH.2.mean - (Ct.GREB1.2.mean[ref] - Ct.GAPDH.2.mean[ref])
ddCtGREB1.3.mean = Ct.GREB1.3.mean - Ct.GAPDH.3.mean - (Ct.GREB1.3.mean[ref] - Ct.GAPDH.3.mean[ref])
ddCtGREB1.3.mean[4] = NA

ddCtTFF1.4.1.mean = Ct.TFF1.4.1.mean - Ct.GAPDH.1.mean - (Ct.TFF1.4.1.mean[ref] - Ct.GAPDH.1.mean[ref])

ddCtTFF1.5.1.mean = Ct.TFF1.5.1.mean - Ct.GAPDH.1.mean - (Ct.TFF1.5.1.mean[ref] - Ct.GAPDH.1.mean[ref])

ddCtTFF1.2.mean = Ct.TFF1.2.mean - Ct.GAPDH.2.mean - (Ct.TFF1.2.mean[ref] - Ct.GAPDH.2.mean[ref])
ddCtTFF1.3.mean = Ct.TFF1.3.mean - Ct.GAPDH.3.mean - (Ct.TFF1.3.mean[ref] - Ct.GAPDH.3.mean[ref])

#-------------------------------------------------------------------------------
#calculating statistical errors based on gaussian error propagation
errGREB1.1 = sqrt(2^-ddCtGREB1.1.mean*log(2)*(Ct.GREB1.1.sd^2 + Ct.GAPDH.1.sd^2 + Ct.GREB1.1.sd[ref]^2 + Ct.GAPDH.1.sd[ref]^2))
errGREB1.2 = sqrt(2^-ddCtGREB1.2.mean*log(2)*(Ct.GREB1.2.sd^2 + Ct.GAPDH.2.sd^2 + Ct.GREB1.2.sd[ref]^2 + Ct.GAPDH.2.sd[ref]^2))
errGREB1.3 = sqrt(2^-ddCtGREB1.3.mean*log(2)*(Ct.GREB1.3.sd^2 + Ct.GAPDH.3.sd^2 + Ct.GREB1.3.sd[ref]^2 + Ct.GAPDH.3.sd[ref]^2))

errTFF1.4.1 = sqrt(2^-ddCtTFF1.4.1.mean*log(2)*(Ct.TFF1.4.1.sd^2 + Ct.GAPDH.1.sd^2 + Ct.TFF1.4.1.sd[ref] + Ct.GAPDH.1.sd[ref]^2))
errTFF1.5.1 = sqrt(2^-ddCtTFF1.5.1.mean*log(2)*(Ct.TFF1.5.1.sd^2 + Ct.GAPDH.1.sd^2 + Ct.TFF1.5.1.sd[ref] + Ct.GAPDH.1.sd[ref]^2))

errTFF1.2 = sqrt(2^-ddCtTFF1.2.mean*log(2)*(Ct.TFF1.2.sd^2 + Ct.GAPDH.2.sd^2 + Ct.TFF1.2.sd[ref]^2 + Ct.GAPDH.2.sd[ref]^2))
errTFF1.3 = sqrt(2^-ddCtTFF1.3.mean*log(2)*(Ct.TFF1.3.sd^2 + Ct.GAPDH.3.sd^2 + Ct.TFF1.3.sd[ref]^2 + Ct.GAPDH.3.sd[ref]^2))
