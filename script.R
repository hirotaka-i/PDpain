rm(list=ls())
setwd('/home/rstudio/test/dulo/anlysis/')
library(tidyverse)
library(data.table)
#################################################
assign = read_csv('CymbaltaTrial_Assign.csv', locale = locale(encoding = "cp932"))
vAssign = c('id', 'grp', 'day0', 'dob', 'age', 'sex', 'vas')
colnames(assign)[c(1,5, 7, 9, 10, 11, 12)] = vAssign
assign = assign %>% 
  mutate(sex = as.numeric(as.factor(sex))) %>%
  mutate(sex = ifelse(sex==2, 'male', 'female')) %>%
  mutate(vas=as.numeric(as.character(vas)))

assign$grp = rnorm(nrow(assign))<0 # temporary

##################################################
patient = read_csv('CymbaltaTrial_Patient.csv', na = c("@", ""),
                   locale = (locale(encoding = 'cp932')))

sfpain=vector(); bdi=vector(); updrs=vector()
for (i in 1:15){sfpain[i]=paste('sfpain_', i, sep='')}
for (i in 1:21){bdi[i]=paste('bdi_', i, sep='')}
for (i in 1:27) {updrs[i]=paste('updrs3_', i, sep='')}

vPatient =   c('id', 'date_motor', 'pddrug_effective', 'painpart', 'painpart2', 
               'date_pain', 'height', 'bw', 'ldbz', 'ldcd',
               'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
               'bromo', 'caber', 'pergo', 'seleg', 'amant',
               'zonis', 'istra', 'dorox', 'trihe', 'poultice',
               'yahr_on', 'yahr_off',
               sfpain, 'sfpain', 'sfpain_intense',
               bdi, 'bdi',
               'status_updrs', updrs, 'updrs3',
               'pdq_mobility', 'pdq_adl', 'pdq_ewb', 'pdq_stigma', 'pdq_social', 'pdq_cog', 'pdq_commu', 'pdq_body',
               'walkm', 'walks', 'walkms')


colnames(patient)[c(1, 13, 14, 15, 16, 17, 18, 19, 34, 39,
                    42, 52, 57, 62, 67, 72, 77, 82, 87, 92,
                    97, 102, 107, 112, 306,
                    356,357,
                    360:374, 375, 376,
                    378:398, 399,
                    401:428, 429,
                    441, 448, 455, 460, 465, 470, 474, 478,
                    479:481)] = vPatient
patient$EVENT='BL'

##########################################
w10 = read_csv('CymbaltaTrial_FollowW10.csv', na = c("@", ""), 
               locale = (locale(encoding = 'cp932')))
vW10 =   c('id', 'date_visit', 'bw', 'compliance', 'drug_change', 'ldbz', 'ldcd',
           'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
           'bromo', 'caber', 'pergo', 'seleg', 'amant',
           'zonis', 'istra', 'dorox', 'trihe', 'poultice',
           'yahr_on', 'yahr_off',
           sfpain, 'sfpain', 'vas', 'sfpain_intense',
           bdi, 'bdi',
           'status_updrs', updrs, 'updrs3',
           'pdq_mobility', 'pdq_adl', 'pdq_ewb', 'pdq_stigma', 'pdq_social', 'pdq_cog', 'pdq_commu', 'pdq_body',
           'walkm', 'walks', 'walkms')

colnames(w10)[c(1, 7, 9, 20, 21,
                24, 29, 32, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97, 102,
                296,
                347, 348,
                351:368,
                370:391,
                393:421,
                433, 440, 447, 452, 457, 462, 466, 470,
                471:473)] = vW10

w10$EVENT = 'W10'

##### cancel table merge and create FAS and PPS indicator
cancel = read_csv('CymbaltaTrial_Cancel.csv', na = c("@", ""), 
                  locale = locale(encoding='cp932'))
vCancel =   c('id', 'date_visit', 'bw', 'compliance',
              'yahr_on', 'yahr_off',
              sfpain, 'sfpain', 'vas', 'sfpain_intense',
              bdi, 'bdi',
              'status_updrs', updrs, 'updrs3',
              'pdq_mobility', 'pdq_adl', 'pdq_ewb', 'pdq_stigma', 'pdq_social', 'pdq_cog', 'pdq_commu', 'pdq_body',
              'walkm', 'walks', 'walkms')

colnames(cancel)[c(1, 7, 9, 19, 351:352, 355:372, 374:395, 397:425,
                   437, 444, 451, 456, 461, 466, 470, 474, 475:477)] = vCancel

cancel$EVENT='CANCEL'

### W14
w14= read_csv('CymbaltaTrial_Summary.csv', na = c("@", ""), 
              locale = locale(encoding='cp932'))
vW14 =   c('id', 'date_visit', 'bw','drug_change', 'ldbz', 'ldcd',
           'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
           'bromo', 'caber', 'pergo', 'seleg', 'amant',
           'zonis', 'istra', 'dorox', 'trihe', 
           'poultice',
           'yahr_on', 'yahr_off',
           sfpain, 'sfpain', 'vas', 'sfpain_intense')

colnames(w14)[c(1, 7, 9, 17,
                20, 25, 30, seq(38, 98, 5),
                292,
                343, 344,
                346:363)]= vW14


w14$EVENT='W14'

## Merge and save
patient = patient %>%
  mutate(walkm = as.numeric(as.character(walkm)),
         walks = as.numeric(as.character(walks)),
         walkms = as.numeric(as.character(walkms)))
w10 = w10 %>% 
  mutate(walkm = as.numeric(as.character(walkm)),
         walks = as.numeric(as.character(walks)),
         walkms = as.numeric(as.character(walkms)))

vall = unique(c(vPatient, vW10, vCancel, vW14))
df = bind_rows(patient, w10, cancel, w14) %>%
  select(c(vall, EVENT)) %>%
  rename(VAS=vas) %>%
  left_join(assign %>% select(id, grp, age, sex, vas, day0) %>% rename(first_vas=vas), ., by = 'id') %>%
  mutate(VAS=ifelse(EVENT=='BL', first_vas, VAS),
         date_visit=ifelse(EVENT=='BL', day0, date_visit))



write.csv(df, 'toAnalyze.csv', fileEncoding = 'cp932', row.names = F)



###############################
# Analysis Part
###############################
drugs = c('ldbz', 'ldcd','ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
          'bromo', 'caber', 'pergo', 'seleg', 'amant','zonis', 'istra', 
          'dorox', 'trihe', 'poultice')

df = read_csv('toAnalyze.csv', locale = locale(encoding = 'cp932')) %>%
  # filter cancel without compliance
  filter(!(EVENT=='CANCEL'&is.na(compliance))) %>%
  # calculate drug equivalent dose, get the summary score
  mutate_at(vars(drugs), list(function(x){
    ifelse(is.na(x), 0, x)
  })) %>%
  mutate(sfpain = ifelse(sfpain==0, NA, sfpain),
         bdi = ifelse(bdi==0, NA, bdi),
         updrs3 = ifelse(updrs3==3, NA, updrs3)) %>%
  mutate(levodopa = ldbz + ldcd + ldcden,
         led = ldbz + ldcd + ldcden + ropin * 20 +
           pramp * 100 + rotig * 40/3 + apomo * 10 + bromo * 10 +
           caber * 67 + pergo * 100 + seleg * 100 + amant * 1,
         walking = walkm*60 + walks + walkms/100, 
         dur_pd = (as.numeric(as.Date(day0)) -as.numeric(as.Date(date_motor)))/365.25,
         dur_pain = (as.numeric(as.Date(day0)) -as.numeric(as.Date(date_pain)))/365.25,
         BMI = bw/((height/100)^2)) %>%
  group_by(id) %>%
  mutate(start = date_visit - as.numeric(as.Date(day0))) %>%
  ungroup()
  

idDrop = df %>% filter(EVENT=='CANCEL') %>% .$id %>% unique

# write.csv(df,  'toCheck.csv', fileEncoding = 'cp932', row.names = F)
dfa = df %>% 
  mutate(drop = ifelse(id%in% idDrop, 1, 0)) %>%
  select(id, age, sex, dur_pd, dur_pain, 
         grp, drop, start, EVENT, VAS,
         yahr_on, yahr_off, levodopa, led, 
         sfpain, bdi, status_updrs, updrs3, 
         pdq_mobility, pdq_adl, pdq_ewb, pdq_stigma,
         pdq_social, pdq_cog, pdq_commu, pdq_body,
         first_vas, walking)

library(tableone)



df = df %>%
  mutate(LDOPA = sum(ldbz, ldcd, ldcden, na.rm = T),
         )


w14[c('ldbz', 'ldcd',
      'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
      'bromo', 'caber', 'pergo', 'seleg', 'amant',
      'zonis', 'istra', 'dorox', 'trihe', 
      'poultice')][is.na(w14[c('ldbz', 'ldcd',
                               'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
                               'bromo', 'caber', 'pergo', 'seleg', 'amant',
                               'zonis', 'istra', 'dorox', 'trihe', 
                               'poultice')])] = 0








cancelPPS = cancel %>% filter(!is.na(compliance)) %>%





w10 = merge(w10, cancel[c('id', 'fas','pps')], by="id", all.x=T)
w10 = transform(w10,
                fas = ifelse(is.na(fas), 1, fas),
                pps = ifelse(is.na(pps), 1, pps))
w10 %>% select(pps)
w10[w10$pps==0,vars] <- cancel[vars]

# levodopa and led
w10[c('ldbz', 'ldcd',
      'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
      'bromo', 'caber', 'pergo', 'seleg', 'amant',
      'zonis', 'istra', 'dorox', 'trihe', 
      'poultice')][is.na(w10[c('ldbz', 'ldcd',
                               'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
                               'bromo', 'caber', 'pergo', 'seleg', 'amant',
                               'zonis', 'istra', 'dorox', 'trihe', 
                               'poultice')])] = 0

w10 = transform(w10,
                    levodopa = ldbz + ldcd + ldcden,
                    led = ldbz + ldcd + ldcden + ropin * 20 + 
                      pramp * 100 + rotig * 40/3 + apomo * 10 + bromo * 10 +
                      caber * 67 + pergo * 100 + seleg * 100 + amant * 1,
                    walking = walkm*60 + walks + walkms/100)




##################################################

w14= read.csv('CymbaltaTrial_Summary.csv')
colnames(w14)[c(1, 7, 9, 17,
                20, 25, 30, seq(38, 98, 5),
                292,
                343, 344,
                346:363)]=
  c('id', 'date_visit', 'bw','drug_change', 'ldbz', 'ldcd',
    'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
    'bromo', 'caber', 'pergo', 'seleg', 'amant',
    'zonis', 'istra', 'dorox', 'trihe', 
    'poultice',
    'yahr_on', 'yahr_off',
    sfpain, 'sfpain', 'vas', 'sfpain_intense')

w14[c('ldbz', 'ldcd',
          'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
          'bromo', 'caber', 'pergo', 'seleg', 'amant',
          'zonis', 'istra', 'dorox', 'trihe', 
          'poultice')][is.na(w14[c('ldbz', 'ldcd',
                     'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
                     'bromo', 'caber', 'pergo', 'seleg', 'amant',
                     'zonis', 'istra', 'dorox', 'trihe', 
                     'poultice')])] = 0

w14 = transform(w14, 
                levodopa = ldbz + ldcd + ldcden,
                led = ldbz + ldcd + ldcden + ropin * 20 + pramp * 100 + 
                  rotig * 40/3 + apomo * 10 + bromo * 10 + caber * 67 + 
                  pergo * 100 + seleg * 100 + amant * 1)


#####################################################

# merge
data0 = merge(assign, patient, by = 'id')
data1 = merge(w10[c('id', 'date_visit', 'sfpain', 'vas', 'updrs3', 
                    'bdi', 'drug_change', 'levodopa', 'led', 'walking', 'fas', 'pps') ], 
              w14[c('id', 'date_final', 'sfpain', 'vas', 'drug_change', 'levodopa', 'led')], by = 'id' )
data  = merge(data0, data1, by = 'id' )

## Convert to factors
for (i in 1:ncol(data)){
  if(is.character(data[,i])){
    data[,i]=factor(data[,i])
  }
}

## last ovservation carry forward
data = transform(data, 
                 vas.x = ifelse(is.na(vas.x), vas, vas.x),
                 sfpain.x = ifelse(is.na(sfpain.x), sfpain, sfpain.x),
                 bdi.y = ifelse(is.na(bdi.y), bdi.x, bdi.y),
                 updrs3.y = ifelse(is.na(updrs3.y), updrs3.x, updrs3.y),           
                 levodopa.x = ifelse(drug_change.x=="?ŏI?m??", levodopa.x, levodopa ),
                 led.x = ifelse(drug_change.x=="?ŏI?m??", led.x, led ))
data = transform(data,
                 vas.y = ifelse(is.na(vas.y), vas.x, vas.y),
                 sfpain.y = ifelse(is.na(sfpain.y), sfpain.x, sfpain.y),
                 levodopa.y = ifelse(drug_change.y=="?ŏI?m??", levodopa.y, levodopa.x ),
                 led.y = ifelse(drug_change.y=="?ŏI?m??", led.y, led.x ))
data$levodopa.y
## age and disease duration
data$date_motor = strptime(data$date_motor, "%Y/%m/%d")
data$dob = strptime(data$dob, "%Y/%m/%d")
data$day0 = strptime(data$day0, "%Y/%m/%d")

data$age = (data$day0-data$dob)/365.25
data$age = as.numeric(data$age)
data$duration = (data$day0 - data$date_motor)/365.25
data$duration = as.numeric(data$duration)
## BMI
data = transform(data, bmi = bw / (height/100)^2)



##############################################################

write.csv(data, 'output_complete_data.csv', quote = F, row.names = T)
write.csv(colnames(data), 'output_complete_data(colnames).csv', quote = F, row.names = T)
write.csv(data, 'output_cancelled.csv', quote = F, row.names = T)
############################################################

## Data quality check
vars = c('sex', 'age', 'bmi', 'duration', 
         'yahr_on', 'yahr_off', 'levodopa', 'levodopa.x', 'levodopa.y', 'led', 'led.x', 'led.y',
         'updrs3.x', 'updrs3.y',
         'pddrug_effective', 'sfpain_intense', 
         'vas', 'vas.x', 'vas.y', 'sfpain', 'sfpain.x',  'sfpain.y', 
         'bdi.x', 'bdi.y',
         'walking.x', 'walking.y')

select = data[vars]
continous = select[c(2:5, 7:14, 17:26)]
check = scale(continous)
histgrams = function(x, y, data){
  for (i in x:y){
    hist(data[,i], main = colnames(data)[i])
  }
}
par(mfrow=c(3,3))
histgrams(1,8, data = check)
histgrams(9,16, data = check)
histgrams(17,22, data = check)
par(mfrow=c(2,2))
categorical = select[c(1,6,15,16)]
for (i in 1:4){
  plot(categorical[i],main = colnames(categorical)[i])
  
}

#########################################################

# Table 1
library(tableone)
table1_fas = CreateTableOne(vars = vars, data=data[data$fas==1,], includeNA = F)
table1_fas = print(table1_fas)
write.csv(table1_fas, 'output_table1_fas.csv', quote = F, row.names = T)

table1_pps = CreateTableOne(vars = vars, data=data[data$pps==1,], includeNA = F)
table1_pps = print(table1_pps)
write.csv(table1_pps, 'output_table1_pps.csv', quote = F, row.names = T)

######################################################

# Analysis
data = transform(data,
                 change_vas = vas.x - vas,
                 change_walk = walking.y - walking.x,
                 change_bdi = bdi.y - bdi.x,
                 change_sfpain = sfpain.x - sfpain,
                 change_updrs3 = updrs3.y - updrs3.x)

cat_indicator <- function(vector, cutpoint=4){
  vec = (quantile(vector, prob=seq(0,1, 1/cutpoint)))
  indicator = cut(vector, breaks = vec, right = T, include.lowest = T)
  indicator
}

data$cat_age = cat_indicator(data$age, 4)
data$cat_vas = cat_indicator(data$vas,4)
data$grp = sample(0:1, size=nrow(data), replace = T)
plot(data$grp, data$change_vas)

pv=vector()
se=vector()
for (i in 1:500){
  data$grp = sample(0:1, size=nrow(data), replace = T)
  test = lm(change ~ grp + I(scale(vas)) + I(scale(age)) + I(scale(bdi.x)), data = data[data$fas==1,])
  res=summary(test)
  se[i]=res$coefficients['grp',2]
  pv[i]=res$coefficients['grp',4]
}
hist(se)
data.frame(mean(se), sd(se))
hist(pv)

test = lm(change ~ grp, data = data[data$fas==1,])
summary(test)
test = lm(change ~ grp + vas, data = data[data$fas==1,])
summary(test)

test = lm(change ~ grp + vas + age + I(age^2), data = data[data$fas==1,])
summary(test)



test = lm(change ~ grp + sex + vas + scale(age) + bdi.x , data = data[data$pps==1,])
summary(test)

test = lm(change ~ grp + vas + sex + cat_age, data = data[data$fas==1,])
summary(test)


test = lm(change ~ grp + vas + sex + age + I(age^2), data = data[data$fas==1,])
summary(test)

test = lm(change ~ vas + I(age<65), data = data[data$fas==1,])
summary(test)

test = lm(change ~ grp + sex + scale(vas) + I(scale(vas)^2) + I(scale(age)) + I(scale(age)^2), data = data[data$fas==1,])
summary(test)
          
test = lm(change ~ grp + sex + scale(vas) + I(scale(age)), data = data[data$fas==1,])
summary(test)

test = lm(change ~ grp + scale(vas) + I(scale(age)), data = data[data$fas==1,])
summary(test)

test = lm(change ~ grp + sex + cat_vas + cat_age, data = data[data$fas==1,])
summary(test)

test = lm(change ~ grp, data = data[data$fas==1,])
summary(test)

test = lm(change ~ grp + sex + scale(vas) + I(scale(vas)^2) + I(scale(age)) + I(scale(age)^2), data = data[data$fas==1,])
summary(test)

## pps
test = lm(change ~ grp + sex + cat_vas + cat_age, data = data[data$pps==1,])
summary(test)

test = lm(change ~ grp, data = data[data$pps==1,])
summary(test)