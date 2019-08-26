#subset data
depsn2 <- subset(w1d, select=c("sex", "age", "edu", "cesd_r",
                              "n_degree_all", "n_kcore_all", 
                              "Mstatus", "Work", "sf_1"))
##data="depsn"(yangsa+buleun)
library(lavaan)
library(semPlot)
#covariance table
cov(depsn2, use = "complete.obs")
data.cov2 <- lav_matrix_lower2full(c(.243, 
                                    -.217, 66.22, 
                                    -.260, -4.262, 2.291, 
                                    .288, 6.346, -2.959, 67.887, 
                                    -.120, -2.858, .028, -1.172, 5.347, 
                                    -.0546, -1.929, .017, -.203, 1.73, .966,
                                    -.066, -1.223, .221, -.509, .256, .137, .191,
                                    -.021, -.865, .024, -.674, .236, .086, .043, .209,
                                    .045, 1.3, -0.334, 2.852, -.182, -.053, -.054, -.062, .699))
rownames(data.cov2)<-colnames(data.cov2)<-c("sex", "age", "edu", "depression", "degree", "kcore", "mstatus", "work","health")
View(data.cov2)

model3='
depression ~ b1*degree + c2*mstatus + c1*work + b2*kcore + sex + age + edu + health
degree ~ a1*work + a3*mstatus
kcore ~ a2*work + a4*mstatus
indirect := a1*b1
indirect2 := a2*b2
indirect3 := a3*b1
indirect4 := a4*b2
direct1 := c1
direct2 := c2
'
model.fit3 <-sem(model3, sample.cov=data.cov2, sample.nobs=1751)
summary(model.fit3, standardized=T, fit=T, rsquar=T)
