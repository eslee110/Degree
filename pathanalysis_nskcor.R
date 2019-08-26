#subset data
depsn3 <- subset(w1d, select=c("sex", "age", "edu", "cesd_r",
                               "n_size", "n_kcore_all", 
                               "Mstatus", "Work", "sf_1"))
##data="depsn"(yangsa+buleun)
library(lavaan)
library(semPlot)
#covariance table
cov(depsn3, use = "complete.obs")
data.cov3 <- lav_matrix_lower2full(c(.243, 
                                     -.217, 66.22, 
                                     -.260, -4.262, 2.291, 
                                     .288, 6.346, -2.959, 67.887, 
                                     -.044, -.19, .368, -3.537, 2.312, 
                                     -.0546, -1.929, .017, -.203, .257, .966,
                                     -.066, -1.223, .221, -.509, .191, .137, .191,
                                     -.021, -.865, .024, -.674, .111, .086, .043, .209,
                                     .045, 1.301, -0.334, 2.852, -.206, -.053, -.054, -.062, .699))
rownames(data.cov3)<-colnames(data.cov3)<-c("sex", "age", "edu", "depression", "nsize", "kcore", "mstatus", "work","health")
View(data.cov3)

model4='
depression ~ b1*nsize + c2*mstatus + c1*work + b2*kcore + sex + age + edu + health
nsize ~ a1*work + a3*mstatus
kcore ~ a2*work + a4*mstatus
indirect := a1*b1
indirect2 := a2*b2
indirect3 := a3*b1
indirect4 := a4*b2
direct1 := c1
direct2 := c2
'
model.fit4 <-sem(model4, sample.cov=data.cov3, sample.nobs=1751)
summary(model.fit4, standardized=T, fit=T, rsquar=T)
