#descriptive_groupdiff
mar <- subset(realdata, mstatus==1)
nmar <- subset(realdata, mstatus==2)
wor <- subset(realdata, work==1)
nwor <-subset(realdata, work==2)

#descriptive
hist(mar$cesd_r)
hist(nmar$cesd_r)

hist(w1c$cesd_r, 
     xlab="CES-D Score", main="Wave 1 CES-D Histogram")

#desc
table(mar$sex==1); table(nmar$sex==1)
mean(mar$age); sd(mar$age)
mean(mar$myeon, na.rm=TRUE); sd(mar$myeon, na.rm=TRUE)
mean(mar$chrdisease, na.rm=TRUE); sd(mar$chrdisease, na.rm=TRUE)
table(mar$Work==1)
table(mar$edu==1)
table(mar$edu==2)
table(mar$edu==3)
table(mar$edu==4)
table(mar$edu==5)
table(mar$edu>5)
mean(mar$n_degree_out); sd(mar$n_degree_out)
mean(mar$n_degree_in); sd(mar$n_degree_in)
mean(mar$cesd_r); sd(mar$cesd_r)
hist(mar$cesd_r, 
     xlab="CES-D Score", main="Depression: Married")

mean(nmar$age); sd(nmar$age)
mean(nmar$myeon); sd(nmar$myeon)
mean(nmar$chrdisease, na.rm=TRUE); sd(nmar$chrdisease, na.rm=TRUE)
table(nmar$Work==1)
table(nmar$edu==1)
table(nmar$edu==2)
table(nmar$edu==3)
table(nmar$edu==4)
table(nmar$edu==5)
table(nmar$edu>5)
mean(nmar$n_degree_out); sd(nmar$n_degree_out)
mean(nmar$n_degree_in); sd(nmar$n_degree_in)
mean(nmar$cesd_r); sd(nmar$cesd_r)
hist(nmar$cesd_r, 
     xlab="CES-D Score", main="Depression: Not Married")

table(wor$sex==1); table(nwor$sex==1)
table(wor$Mstatus==1)
mean(wor$age); sd(wor$age)
mean(wor$myeon); sd(wor$myeon)
mean(wor$chrdisease, na.rm=TRUE); sd(wor$chrdisease, na.rm=TRUE)
table(wor$Work==1)
table(wor$edu==1)
table(wor$edu==2)
table(wor$edu==3)
table(wor$edu==4)
table(wor$edu==5)
table(wor$edu>5)
mean(wor$n_degree_out); sd(wor$n_degree_out)
mean(wor$n_degree_in); sd(wor$n_degree_in)
mean(wor$cesd_r); sd(wor$cesd_r)
hist(wor$cesd_r, 
     xlab="CES-D Score", main="Depression: Working")


table(nwor$Mstatus==1)
mean(nwor$age); sd(nwor$age)
mean(nwor$myeon, na.rm=TRUE); sd(nwor$myeon, na.rm=TRUE)
mean(nwor$chrdisease, na.rm=TRUE); sd(nwor$chrdisease, na.rm=TRUE)
table(nwor$edu==1)
table(nwor$edu==2)
table(nwor$edu==3)
table(nwor$edu==4)
table(nwor$edu==5)
table(nwor$edu>5)
mean(nwor$n_degree_out); sd(nwor$n_degree_out)
mean(nwor$n_degree_in); sd(nwor$n_degree_in)
mean(nwor$cesd_r); sd(nwor$cesd_r)
hist(nwor$cesd_r, 
     xlab="CES-D Score", main="Depression: Not Working")

#partial correlation table
library(corpcor)
pc<- cor2pcor(data.cor)
rownames(pc)<-colnames(pc)<-c("sex", "age", "edu", "depression", "outdegree", "indegree", "mstatus", "work")
View(pc)

library(reshape2)
meltedpc <- melt(pc)
head(meltedpc)
library(ggplot2)
ggplot(data = meltedpc, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
lowertri<-function(pc){
  pc[upper.tri(pc)] <- NA
  return(pc)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(pc){
  pc[lower.tri(pc)]<- NA
  return(pc)
}
uppertri <- get_upper_tri(pc)
uppertri

# Melt the correlation matrix
library(reshape2)
melted_pc <- melt(uppertri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_pc, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
