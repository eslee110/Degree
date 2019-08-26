library(readxl)
library(xlsx)
library(haven)

##############
yang.sn <- read_stata("D:/Dropbox/Degree/analysis/yangsa_SN201712.dta")
yang.sn.w3 <- yang.sn[yang.sn$wave==3,]
yang.sn.w2 <- yang.sn[yang.sn$wave==2,]
yang.sn.w1 <- yang.sn[yang.sn$wave==1,]
# educ(old school) -> no schooling
edcod <- which(yang.sn.w1$edu == 2) 
yang.sn.w1$edu[edcod] <- 3 # (old school) to (elementary)
edcod <- which(yang.sn.w1$edu == 1)
yang.sn.w1$edu[edcod] <- 2
yang.sn.w1$edu <- yang.sn.w1$edu-1


bul.sn <- read_excel("D:/Dropbox/Degree/analysis/buleun_SN201804.xlsx")
bul.sn.info <- read_excel("D:/Dropbox/Degree/analysis/불은면_데이터공유.xlsx")
bul.sn$age <- bul.sn.info$age
# ( >=5 university) aggregate recode , oldschool -> noschool
edcod <- which(bul.sn$edu >= 6) # coerce to match yang education coding
bul.sn$edu[edcod] <- 6
edcod <- which(bul.sn$edu == 2)
bul.sn$edu[edcod] <- 3 # (old school) to (elementary)
edcod <- which(bul.sn$edu == 1)
bul.sn$edu[edcod] <- 2
bul.sn$edu <- bul.sn$edu - 1

bul.sn <- cbind(bul.sn,bul.sn.info[,c(133,137,138,144,149,206:225)]) #combine cesd mood, chronic disease

#### Select and subset variables
yang.names <- c("dbid", "age", "sex", "edu","work","myeon","sf_1", "mstatus", 
                "n_meet","n_feel", "n_comm","n_size","n_size_d", "n_size_s","n_degree_in","n_degree_out", "n_degree_all", "n_constraint","n_kcore_all","n_between",
                "cesd_1", "cesd_2", "cesd_3", "cesd_4", "cesd_5", "cesd_6", "cesd_7", "cesd_8", "cesd_9", "cesd_10", "cesd_11","cesd_12","cesd_13", "cesd_14","cesd_15","cesd_16","cesd_17","cesd_18","cesd_19","cesd_20",
                "diabetes", "stroke", "chd", "arthritis","cancer"
) # # hincome_c # "stroke" "cancer","wanttodie", "cesd_tot"  "mmse_c",

bul.names <- c("dbid", "age", "sex", "edu","a3","residence","p1","c2", 
               "n_meet","n_feel","n_comm","n_size","n_size_d","n_size_s","n_degree_in","n_degree_out","n_degree_all","n_constraint", "n_kcore_all","n_between",
               "k1_1","k1_2","k1_3","k1_4","k1_5","k1_6","k1_7","k1_8","k1_9","k1_10","k1_11","k1_12","k1_13","k1_14","k1_15","k1_16","k1_17","k1_18","k1_19","k1_20",
               "j2_1", "j4", "j5", "j10","j15"
) # q4(income)


# outcome - loneliness
#yang.y.name <- c("cesd_14")
#bul.y.name <- c("K1_14")

# outcome suicide
# yang.y.name <- c("wanttodie")
# bul.y.name <- c("k2")

yang1 <- subset(yang.sn.w1, select= c(yang.names))
bul1 <- subset(bul.sn, select = c(bul.names))

# combind yang + bul
colnames(bul1) <- colnames(yang1)
yangbul <- rbind(yang1,bul1)

# marital status categorize (y/n living with)
yangbul$mstatus[which(is.na(yangbul$mstatus)==1)] <- 5  # recode bul.sn have not married 
marcat <- which(yangbul$mstatus>=2)
yangbul$mstatus[marcat] <- 2
# depressive mood
# yangbul$depmood <- with(yangbul, cesd_3 + cesd_6 + cesd_17 + cesd_18)
# chronic disease counts
yangbul$chrdisease <- with(yangbul,(diabetes==1)+(stroke==1)+(chd==1)+(arthritis==1)+(cancer==1))

# years of residence outlier remove
temp <- which(yangbul$myeon > 120)
yangbul$myeon[temp] <- NA
rm(temp)

# subjective net size scale adjustment
temp <- (yangbul$n_size_s>=15)
yangbul$n_size_s[temp] <- 15