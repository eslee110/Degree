#subset data
depsn <- subset(realdata, select=c("sex", "age", "edu", "cesd_r",
                            "n_degree_out", "n_degree_in", 
                            "mstatus", "work", "chrdisease", "myeon"))
##data="depsn"(yangsa+buleun)
library(lavaan)
library(semPlot)
#covariance table
cov(depsn, use = "complete.obs")
data.cov <- lav_matrix_lower2full(c(.243, 
                                    -.221, 59.401, 
                                    -.184, -2.976, 1.188, 
                                    .305, 10.273, -2.071, 67.608,
                                    -.086, -.792, -.039, -.899, 2.082,
                                    -.089, -1.896, .025, -.487, .521, 2.258,
                                    .065, 1.198, -.148, .509, -.103, -.144, .192,
                                    .021, .914, -.001, .669, -.095, -.121, .044, .209,
                                    .048, .404, -.125, 1.506, -.005, .042, .01, .031, .561,
                                    -1.666, 74.412, -11.472, 21.816, 6.354, 6.871, 1.45, -2.541, 1.05, 687.545))
rownames(data.cov)<-colnames(data.cov)<-c("sex", "age", "edu", "depression", "outdegree", "indegree", "mstatus", "work","chrdisease", "residence")
View(data.cov)

model='
depression ~ b1*outdegree + c2*mstatus + c1*work + b2*indegree + sex + age + edu + chrdisease + residence
outdegree ~ a1*work + a3*mstatus
indegree ~ a2*work + a4*mstatus
indirect := a1*b1
indirect2 := a2*b2
indirect3 := a3*b1
indirect4 := a4*b2
direct1 := c1
direct2 := c2
total1 := c1 + (a1*b1)
total2 := c1 + (a2*b2)
total3 := c2 + (a3*b1)
total4 := c2 + (a4*b2)
'
model.fit <-sem(model, sample.cov=data.cov, sample.nobs=1751)
summary(model.fit, standardized=T, fit=T, rsquar=T)


#semPaths(model.fit, whatLabels = "par", curvePivot = TRUE)
#semPaths(model.fit, "std", "hide", edge.label.cex = 0.5, curvePivot = TRUE)