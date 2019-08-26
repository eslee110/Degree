##realdata
yangbul$cesd_m <- yangbul$cesd_1+yangbul$cesd_2+yangbul$cesd_3+yangbul$cesd_4+(5-(yangbul$cesd_5))+yangbul$cesd_6+yangbul$cesd_7+yangbul$cesd_8+yangbul$cesd_9+(5-(yangbul$cesd_10))+
  yangbul$cesd_11+yangbul$cesd_12+yangbul$cesd_13+yangbul$cesd_14+(5-(yangbul$cesd_15))+yangbul$cesd_16+yangbul$cesd_17+yangbul$cesd_18+yangbul$cesd_19+yangbul$cesd_20
yangbul$cesd_r <- (yangbul$cesd_m - 20) #real_cesd
realdata <- subset(yangbul, cesd_m>=20) #missing
View(realdata)

#hierarchical regression
depb <- lm(cesd_r ~ sex+age+edu+myeon+chrdisease, data=realdata)
summary(depb)
depm <- lm(cesd_r ~ sex+age+edu+myeon+chrdisease+mstatus, data=realdata)
summary(depm)
depw <- lm(cesd_r ~ sex+age+edu+myeon+chrdisease+work, data=realdata)
summary(depw)
depdeg <- lm(cesd_r ~sex+age+edu+myeon+chrdisease+n_degree_all, data=realdata)
summary(depdeg)
depmw <-lm(cesd_r ~sex+age+edu+myeon+chrdisease+mstatus+work, data=realdata)
summary(depmw)
depms <-lm(cesd_r ~sex+age+edu+myeon+chrdisease+mstatus+n_degree_all, data=realdata)
summary(depms)
depws <-lm(cesd_r ~sex+age+edu+myeon+chrdisease+work+n_degree_all, data=realdata)
summary(depws)
depmws <-lm(cesd_r ~sex+age+edu+myeon+chrdisease+mstatus+work+n_degree_all, data=realdata)
summary(depmws)
depout <-lm(cesd_r ~sex+age+edu+myeon+chrdisease+mstatus+work+n_degree_out, data=realdata)
summary(depout)
depin <-lm(cesd_r ~sex+age+edu+myeon+chrdisease+mstatus+work+n_degree_in, data=realdata)
summary(depin)

