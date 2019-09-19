rm(list=ls())
setwd('/home/rstudio/wkdir/PDpain/')
library(tidyverse)
library(data.table)
#################################################
assign = read_csv('data/CymbaltaTrial_Assign.csv', locale = locale(encoding = "cp932"))
vAssign = c('id', 'grp', 'day0', 'dob', 'age', 'sex', 'vas')
colnames(assign)[c(1,5, 7, 9, 10, 11, 12)] = vAssign
assign = assign %>% 
  mutate(sex = as.numeric(as.factor(sex))) %>%
  mutate(sex = ifelse(sex==2, 'male', 'female')) %>%
  mutate(vas=as.numeric(as.character(vas)))

##################################################
patient = read_csv('data/CymbaltaTrial_Patient.csv', na = c("@", ""),
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
w10 = read_csv('data/CymbaltaTrial_FollowW10.csv', na = c("@", ""), 
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
cancel = read_csv('data/CymbaltaTrial_Cancel.csv', na = c("@", ""), 
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
w14= read_csv('data/CymbaltaTrial_Summary.csv', na = c("@", ""), 
              locale = locale(encoding='cp932'))
names(w14) %>% print
vW14 =   c('id', 'date_visit', 'bw','drug_change', 'ldbz', 'ldcd',
           'ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
           'bromo', 'caber', 'pergo', 'seleg', 'amant',
           'zonis', 'istra', 'dorox', 'trihe', 
           'poultice',
           'yahr_on', 'yahr_off',
           sfpain, 'sfpain', 'vas', 'sfpain_intense')

colnames(w14)[c(1, 7, 8, 13,
                18, 23, 28, seq(38, 98, 5),
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
  dplyr::select(c(vall, 'EVENT')) %>%
  rename(VAS=vas) %>%
  left_join(assign %>% dplyr::select(id, grp, age, sex, vas, day0) %>% rename(first_vas=vas), ., by = 'id') %>%
  mutate(VAS=ifelse(EVENT=='BL', first_vas, VAS),
         date_visit=ifelse(EVENT=='BL', day0, date_visit))



write.csv(df, 'temp/toAnalyze.csv', fileEncoding = 'cp932', row.names = F)



###############################
# Analysis Part
###############################
drugs = c('ldbz', 'ldcd','ldcden', 'ropin', 'pramp', 'rotig', 'apomo',
          'bromo', 'caber', 'pergo', 'seleg', 'amant','zonis', 'istra', 
          'dorox', 'trihe', 'poultice')
basev = c('date_motor', 'pddrug_effective', 'painpart', 'painpart2', 'date_pain', 'height', 'bw')

library(zoo)
df = read_csv('temp/toAnalyze.csv', locale = locale(encoding = 'cp932')) %>%
  # filter cancel without compliance
  filter(!(EVENT=='CANCEL' & is.na(compliance))) %>%
  # correct missed coding
  mutate(pergo=ifelse(id=='01-00027'&EVENT=='BL', 0.75, pergo)) %>%
  # lost observation carry forward
  group_by(id) %>%
  mutate_at(vars(c(drugs, basev)), na.locf0) %>%
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
         dur_pd = (as.numeric(as.Date(day0)) -as.numeric(date_motor))/365.25,
         dur_pain = (as.numeric(as.Date(day0)) -as.numeric(date_pain))/365.25,
         BMI = bw/((height/100)^2)) %>%
  group_by(id) %>%
  mutate(start = as.numeric(date_visit) - as.numeric(as.Date(day0))) %>%
  ungroup()
df %>% select(start)
idDrop = df %>% filter(EVENT=='CANCEL') %>% .$id %>% unique


df$painpart %>% unique
df = df %>% mutate(
  pain_lwrExtrimities=case_when(
    grepl('下肢', painpart) | grepl('下肢', painpart2) ~ 1,
    TRUE~0),
  pain_uprExtrimities=case_when(
    grepl('上肢', painpart) | grepl('上肢', painpart2) ~ 1,
    TRUE~0),
  pain_Body = case_when(
    grepl('体幹', painpart) | grepl('体幹', painpart2) ~ 1,
    TRUE~0),
  pain_NeckSholder = case_when(
    grepl('頚|肩', painpart) | grepl('頚|肩', painpart2) ~ 1,
    TRUE~0),
  pain_Head=case_when(
    grepl('顔|頭', painpart) | grepl('顔|頭', painpart2) ~ 1,
    TRUE~0)) %>%
  mutate(sfpain_intense = case_when(
    sfpain_intense=='わずかな痛み'~ '1:Mild',
    sfpain_intense=='わずらわしい痛み'~ '2:Discomforting',
    sfpain_intense=='やっかいで情けない痛み'~ '3:Distressing',
    sfpain_intense=='激しい痛み'~ '4:Horrible',
    sfpain_intense=='耐え難い痛み'~ '5:Excruciating')) %>%
  mutate(pddrug_effective = ifelse(pddrug_effective=='有り', 1, 0) %>% as.factor)

# write.csv(df,  'toCheck.csv', fileEncoding = 'cp932', row.names = F)
dfa = df %>% 
  mutate(dropI = ifelse(id%in% idDrop, 1, 0)) %>%
  mutate_at(vars(starts_with('pain_')), as.factor) %>% 
  dplyr::select(id, age, sex, dur_pd, pddrug_effective,
         dur_pain, pain_Head, pain_NeckSholder, pain_Body, pain_uprExtrimities, pain_lwrExtrimities, 
         sfpain_intense,
         grp, dropI, start, EVENT, VAS,
         yahr_on, yahr_off, levodopa, led, 
         sfpain, bdi, status_updrs, updrs3, 
         pdq_mobility, pdq_adl, pdq_ewb, pdq_stigma,
         pdq_social, pdq_cog, pdq_commu, pdq_body,
         first_vas, walking)

library(tableone)
dfa %>% filter(EVENT=='BL') %>%
  mutate(dropI=as.factor(dropI)) %>%
  select(-id) %>%
  CreateTableOne(data=., strata = 'grp')

dfa %>% filter(EVENT=='BL') %>%
  mutate(dropI=as.factor(dropI)) %>%
  select(-id) %>%
  select(starts_with('pain_'), sfpain_intense) %>%
  CreateTableOne(data=., strata='sfpain_intense')

options(max.print=2000)
skim_tee(df)


df %>% filter(!(id %in% idDrop)) %>% with(boxplot(VAS~EVENT))
df %>% filter(!(id %in% idDrop)) %>% with(boxplot(log(walking)~EVENT))
hist(df$walking)
hist(df$VAS)

#######
FAS = df %>% filter(id != '01-00011') %>%
  filter(((id %in% idDrop) & (EVENT=='CANCEL')) | (!(id %in% idDrop) & (EVENT=='W10')))
PPS = df %>% filter(EVENT=='W10')

data2 %>% filter(is.na(BMI)) %>% .[,1:5]
# Analysis
data = FAS
outcome = 'VAS'
data2 = left_join(dfa %>% filter(EVENT=='BL'), data, by = 'id') 
change = with(data2, eval(parse(text = paste0(outcome, '.x - ', outcome, '.y '))))
hist(change)
mean(change, na.rm =T)
mod1 = lm(change ~ age.x+sex.x+pddrug_effective.x+
            dur_pd.x+dur_pain.x+BMI+
            VAS.x+updrs3.x+bdi.x+sfpain.x+
            led.x+levodopa.x, data = data2)

# data2 %>% dplyr::select(
#   age.x, sex.x, pddrug_effective.x, 
#   dur_pd.x, dur_pain.x, BMI, 
#   VAS.x, walking.x, updrs3.x, bdi.x, sfpain.x,
#   led.x, levodopa.x) %>% skim_tee()

summary(mod1)
library(MASS)
mod_fin = stepAIC(mod1, trace = F)
mod_fin %>% summary
lm(change ~ grp.x+age.x+VAS.x+sex.x, data = data2) %>% summary
ggplot(data=df, aes(x=EVENT, y=VAS, colour=grp)) + geom_boxplot()

# mod2 = lm(change ~ dur_pain.y+BMI+VAS.x+
#             pddrug_effective.y+bdi.y, data = data2)
# mod2 %>% summary

# Analysis
data = FAS
outcome = 'walking'
data2 = left_join(dfa %>% filter(EVENT=='BL'), data, by = 'id') 
change = with(data2, eval(parse(text = paste0(outcome, '.x - ', outcome, '.y '))))
hist(change)
mean(change, na.rm =T)
mod1 = lm(change ~ age.x+sex.x+pddrug_effective.x+
            dur_pd.x+dur_pain.x+BMI+
            VAS.x+updrs3.x+bdi.x+sfpain.x+walking.x+
            led.x+levodopa.x, data = data2)
mod_fin = stepAIC(mod1, trace = F)
mod_fin %>% summary
lm(change ~ grp.x+walking.x, data = data2) %>% summary


# Analysis
outcome = 'updrs3'
data2 = left_join(dfa %>% filter(EVENT=='BL'), data, by = 'id') 
change = with(data2, eval(parse(text = paste0(outcome, '.x - ', outcome, '.y '))))
hist(change)
mean(change, na.rm =T)
lm(change ~ grp.x+updrs3.x+led.x, data = data2) %>% summary
mod1 = lm(change ~ age.x+sex.x+pddrug_effective.x+
            dur_pd.x+dur_pain.x+BMI+
            VAS.x+updrs3.x+bdi.x+sfpain.x+walking.x+
            led.x+levodopa.x, data = data2)
mod_fin = stepAIC(mod1, trace = F)
mod_fin %>% summary


outcome = 'sfpain'
data2 = left_join(dfa %>% filter(EVENT=='BL'), data, by = 'id') 
change = with(data2, eval(parse(text = paste0(outcome, '.x - ', outcome, '.y '))))
hist(change)
mean(change, na.rm =T)
lm(change ~ grp.x+sfpain.x, data = data2) %>% summary
mod1 = lm(change ~ age.x+sex.x+pddrug_effective.x+
            dur_pd.x+dur_pain.x+BMI+
            VAS.x+updrs3.x+bdi.x+sfpain.x+walking.x+
            led.x+levodopa.x, data = data2)
mod_fin = stepAIC(mod1, trace = F)
mod_fin %>% summary


outcome = 'bdi'
data2 = left_join(dfa %>% filter(EVENT=='BL'), data, by = 'id') 
change = with(data2, eval(parse(text = paste0(outcome, '.x - ', outcome, '.y '))))
hist(change)
mean(change, na.rm =T)
lm(change ~ grp.x+bdi.x, data = data2) %>% summary
mod1 = lm(change ~ age.x+sex.x+pddrug_effective.x+
            dur_pd.x+dur_pain.x+BMI+
            VAS.x+updrs3.x+bdi.x+sfpain.x+walking.x+
            led.x+levodopa.x, data = data2)
mod_fin = stepAIC(mod1, trace = F)
mod_fin %>% summary
d



















































cancelPPS = cancel %>% filter(!is.na(compliance)) %>
  


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