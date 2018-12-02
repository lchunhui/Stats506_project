#-----------------------------------------------------------------------------
#Read data
setwd('C:/Users/36303/Desktop/Proj1')
library(data.table)
library(dplyr)
library(haven)
library(CCA)
liver <- read_dta("1999/liver/DS0129/25501-0129-Data.dta")
vocs <- read_dta("1999/vocs/25501-0133-Data.dta")

#Name used for all data
livers_name <- c('SEQN','LBDSALSI','LBXSATSI','LBXSAPSI','LBXSASSI','LBXSGTSI','LBXSLDSI','LBDSTBSI')
vocs_name <- c('SEQN','LBXZDB','LBDZDBLC','LBXZBZ','LBDZBZLC','LBXZOX','LBDZOXLC','LBXZXY','LBDZXYLC','LBXZEB','LBDZEBLC','LBXZMB','LBDZMBLC','LBXZTO','LBDZTOLC','LBXZCF','LBDZCFLC','LBXZTE','LBDZTELC','LBXZTI','LBDZTILC')
drink_name <-c('SEQN','ALQ100')
smoke_name <- c('SEQN','SMQ040','SMD030','SMD090')
stroke_name <- c('SEQN','MCQ220','MCQ160B','MCQ160C','MCQ160D','MCQ160E','MCQ160F','MCQ160L')
diabete_name <- c('SEQN','DIQ010')
hepatitis_name <-c('SEQN', 'LBDHCV')
livers_ori = c('Albumin','ALT','ALP','AST','GGT','LDH','TB')
vocs_ori = c('1,4-Dichlorobenzene','1,4-DichlorobenzeneC','Benzene','BenzeneC','o-Xylene','o-XyleneC','m,p-Xylene','m,p-XyleneC','Ethylbenzene','EthylbenzeneC','MTBE','MTBEC','Toluene','TolueneC','Chloroform','ChloroformC','Tetrachloroethene','TetrachloroetheneC','Trichloroethene','TrichloroetheneC')

#Supplements data--------------------------------------------------------
stroke <- read_dta("1999/DS0226/25501-0226-Data.dta")
diabete <- read_dta("1999/diabete/DS0210/25501-0210-Data.dta")
hepatitis <- read_dta("1999/hepatitis/DS0113/25501-0113-Data.dta")
smoke <- read_dta("1999/smoke/DS0242/25501-0242-Data.dta")
drink <- read_dta("1999/drink/DS0202/25501-0202-Data.dta")

#Narrow data and combine-------------------------------------------------
liver = liver[livers_name]
vocs = vocs[vocs_name]
stroke = stroke[stroke_name]
diabete = diabete[diabete_name]
hepatitis = hepatitis[hepatitis_name]
smoke = smoke[smoke_name]
drink = drink[drink_name]

combine = left_join(vocs, liver, by = 'SEQN')
combine = left_join(combine, stroke,by = 'SEQN')
combine = as.data.table(left_join(combine, diabete, by ='SEQN'))
combine = as.data.table(left_join(combine, hepatitis, by ='SEQN'))

#Exclude heart disease, stroke, diabete, antibody C-----------------------
combine = combine %>% filter(MCQ160L != 1 | is.na(MCQ160L)) %>% 
  filter(MCQ160B != 1 | is.na(MCQ160B)) %>% 
  filter(MCQ160C != 1 | is.na(MCQ160C)) %>% 
  filter(MCQ160D != 1 | is.na(MCQ160D)) %>% 
  filter(MCQ160E != 1 | is.na(MCQ160E)) %>% 
  filter(MCQ160F != 1 | is.na(MCQ160F)) %>% 
  filter(MCQ220 != 1 | is.na(MCQ220)) %>% 
  filter(DIQ010 != 1 | is.na(DIQ010)) %>% 
  filter(LBDHCV != 1 | is.na(LBDHCV))

combine = left_join(combine, smoke, by='SEQN')
combine = left_join(combine, drink, by='SEQN')

#####Further solving NA
livers_ori = c('Albumin','ALT','ALP','AST','GGT','LDH','TB')
total_name = c('SEQN',vocs_ori,livers_ori )

#Remove observation is all vocs are NA---------------------------------------
combine = as.data.table(combine)
keep = c('SEQN','LBXZDB','LBDZDBLC','LBXZBZ','LBDZBZLC','LBXZOX','LBDZOXLC','LBXZXY','LBDZXYLC','LBXZEB','LBDZEBLC','LBXZMB','LBDZMBLC','LBXZTO','LBDZTOLC','LBXZCF','LBDZCFLC','LBXZTE','LBDZTELC','LBXZTI','LBDZTILC','LBDSALSI','LBXSATSI','LBXSAPSI','LBXSASSI','LBXSGTSI','LBXSLDSI','LBDSTBSI')
combine = combine[,..keep]
combine = combine[apply(combine[,2:21], 1, function(x) !all(is.na(x)))] #Exclude if NA
names(combine) = total_name #Rename

#exclude extreme values
extreme_index1 = which.max(combine[['ALT']])
extreme_index2 = which.max(combine[['AST']])
combine = combine[-extreme_index1,]
combine = as.data.table(combine)

#Imputed data below limit using detection limit/sqrt(2)------------------------------

##By citation, the detection limits are values below
combine1 = combine
combine1$Benzene[combine1$Benzene < 1.774] = 1.774/sqrt(2)
combine1$Ethylbenzene[combine1$Ethylbenzene <0.277] = 0.277/sqrt(2)
combine1$Toluene[combine1$Toluene < 3.808] = 3.808/sqrt(2)
combine1$`o-Xylene`[combine1$`o-Xylene` < 0.434] = 0.434/sqrt(2)
combine1$`m,p-Xylene`[combine1$`m,p-Xylene` < 0.48] = 0.48/sqrt(2)
combine1$Chloroform[combine1$Chloroform < 0.45] = 0.45/sqrt(2)
combine1$Tetrachloroethene[combine1$Tetrachloroethene < 0.257] =0.257/sqrt(2)
combine1$Trichloroethene[combine1$Trichloroethene < 0.383] = 0.383/sqrt(2)
combine1$`1,4-Dichlorobenzene`[combine1$`1,4-Dichlorobenzene` < 0.882] = 0.882/sqrt(2)
combine1$MTBE[combine1$MTBE < 0.846]=0.846/sqrt(2)

#For our data
adjust= function(data = combine){
  adjust_name_list = c('1,4-Dichlorobenzene','Benzene','o-Xylene','m,p-Xylene','Ethylbenzene','MTBE','Toluene','Chloroform','Tetrachloroethene','Trichloroethene')
  criterion_list = paste0(adjust_name_list,'C')
  min = rep(0,10)
  for (i in 1:10){
    minimum = min(data[[adjust_name_list[i]]][data[[criterion_list[i]]]==0],na.rm=TRUE)
    data[[adjust_name_list[i]]][data[[adjust_name_list[i]]] < minimum] = minimum/sqrt(2)
    min[i] = minimum
  }
  min
}

min=adjust(combine1)
##Compute Bolm score################------------------------------------------------------------------
# y <- qnorm((r-c)/(N-2c+1))       #
# n = total number of observations #
# c = 3/8                          #
# r = rank                         #
####################################
n = nrow(combine1)
c = 3/8
combine2 = combine1[, lapply(.SD, function(x) qnorm({rank(x)-c}/{n+1/4})), .SDcols = total_name[2:28]]

vocs_ori2 = c('1,4-Dichlorobenzene','Benzene','o-Xylene','m,p-Xylene','Ethylbenzene','MTBE','Toluene','Chloroform','Tetrachloroethene','Trichloroethene')
X = as.matrix(combine2[, ..vocs_ori2])
Y = as.matrix(combine2[, ..livers_ori])

# ###Check
# vocs_ori_check = c('SEQN','1,4-Dichlorobenzene','1,4-DichlorobenzeneC','Benzene','BenzeneC','o-Xylene','o-XyleneC','m,p-Xylene','m,p-XyleneC','Ethylbenzene','EthylbenzeneC','MTBE','MTBEC','Toluene','TolueneC','Chloroform','ChloroformC','Tetrachloroethene','TetrachloroetheneC','Trichloroethene','TrichloroetheneC')
# 
# names(vocs) = vocs_ori
# vocs = as.data.table(vocs)
# check = vocs[,list(Benzene, BenzeneC)]
# vocs = na.omit(vocs)
# nrow(vocs)
# min(check[BenzeneC == 0]$Benzene)*sqrt(2)*(48/56.4)
# check = combine1[,list(Ethylbenzene, EthylbenzeneC)]
# min(check[EthylbenzeneC == 0]$Ethylbenzene)*sqrt(2)*(48/56.4)

#First pair
#Second pair
library(yacca)
cca.fit = cca(X,Y)
F.test.cca(cca.fit)
cca.fit$xstructcorr
#Further analysis by focusing on people who have more than 12 drinks per year
moredrink = left_join(combine1, drink, by='SEQN')
summary(as.factor(moredrink$ALQ100))
moredrink = as.data.table(moredrink)
moredrink = moredrink[ALQ100==1]
#Form normal score
n = nrow(moredrink)
c = 3/8
moredrink1 = moredrink[, lapply(.SD, function(x) qnorm({rank(x)-c}/{n+1/4})), .SDcols = total_name[2:28]]
X1 = as.matrix(moredrink1[, ..vocs_ori2])
Y1 = as.matrix(moredrink1[, ..livers_ori])
cca.fit1 = cca(X1, Y1)
F.test.cca(cca.fit1)

fwrite(combine2, file = './combine2.csv')
fwrite(moredrink, file = './moredrink.csv')

#Description
combine1 = as.data.table(combine1)
vocs_full_name = c('1,4-Dichlorobenzene','Benzene','o-Xylene','m,p-Xylene','Ethylbenzene','MTBE','Toluene','Chloroform','Tetrachloroethene','Trichloroethene')

descriptive = function( data = combine1, full_name = vocs_full_name){
  n = data[, lapply(.SD, function(x) length(which(!is.na(x)))),.SDcols = full_name]
  min = data[,lapply(.SD, min, na.rm = TRUE),.SDcols = full_name]
  mean = data[,lapply(.SD, mean, na.rm = TRUE),.SDcols = full_name]
  median = data[,lapply(.SD, median, na.rm = TRUE),.SDcols = full_name]
  quantile90 = data[,lapply(.SD, quantile, 0.9, na.rm = TRUE),.SDcols = full_name]
  quantile95 = data[,lapply(.SD, quantile, 0.95, na.rm = TRUE),.SDcols = full_name]
  tb = as.data.frame(t(rbind(n,min,mean,median,quantile90,quantile95)))
  colnames(tb) = c('n','min','mean','median','90th percentile','95th percentile')
  rownames(tb) = full_name 
  tb
}

#Descriptive statistics of personal exposure to 10 VOCs
table1_1 = descriptive(combine1)
table2_1 = descriptive(moredrink)

#Biochemical liver tests
table1_2 = descriptive(combine1, livers_ori)
table2_2 = descriptive(moredrink, livers_ori)

##For the full group
#Canonical structures of the first pair of canonical variate
as.matrix(cca.fit$xcoef[,1],ncol=1)
as.matrix(cca.fit$ycoef[,1],ncol=1)
F.test.cca(cca.fit)

##For people drink more than 12 times per year
as.matrix(cca.fit1$xcoef[,1],ncol=1)
as.matrix(cca.fit1$ycoef[,1],ncol=1)
F.test.cca(cca.fit1)

