rm(list=ls())
setwd('/home/rstudio/wkdir/PDpain/')
library(tidyverse)
library(data.table)


# PD drugs
pddrug = c('ldbz', 'ldcd', 'ldcden', 'entca', 
           'ropin', 'pramp', 'rotig', 'apomo', 
           'bromo', 'caber', 'pergo', 'seleg', 
           'amant', 'zonis', 'istra', 'dorox', 'trihe')

# functions
listFunc = list()
listFunc[[1]] = function(df){
  # Get the LEDD for PD drug
  dfdrug = df[, grep('LED', names(df))]
  names(dfdrug) = pddrug
  return(dfdrug)
}

getOne = function(df, item, item_name){
  d = df[,grep(item, names(df))]
  names(d) = item_name
  return(d)
}

listFunc[[2]] = function(df){
 # get VAS
 getOne(df, '痛みの評価VAS', 'vas') 
}

listFunc[[3]] = function(df){
 # get BDI
 getOne(df, "BDI合計点数", 'bdi') 
}

listFunc[[4]] = function(df){
  # get sfpain
  d = bind_cols(
    getOne(df, "痛みの評価合計点数", 'sfpain'),
    getOne(df, "痛みの評価痛みの現在の強さ", 'sfpain_intense'))
  return(d)
}


listFunc[[5]] = function(df){
  # get UPDRS
  d = bind_cols(
    getOne(df, "UPDRS検査施行時の状態", 'status_updrs'),
    getOne(df, "UPDRS合計得点", 'updrs3'))
  return(d)
}
  

listFunc[[6]] = function(df){
  # get PDQ
  PDQ = c('pdq_mobility', 'pdq_adl', 'pdq_ewb', 'pdq_stigma', 
          'pdq_social', 'pdq_cog', 'pdq_commu', 'pdq_body')
  dfpdq = df[, grep('PDQ ', names(df))]
  names(dfpdq) = PDQ
  return(dfpdq)
}

listFunc[[7]] = function(df){
  # get seconds for 3m Up and go
  dfupgo = df[, grep('TUG', names(df))] %>% data.matrix
  a = data.frame(walking = dfupgo[,1]*60 + dfupgo[,2] + dfupgo[,3]/100)
  return(a)
}

listFunc[[8]] = function(df){
  # get pd status
  pdstatus = c('yahr_on', 'yahr_off', 'haveWO')
  d = df[,grep('パーキンソン病評価', names(df))]
  names(d) = pdstatus
  d$haveWO = case_when(d$haveWO=='無し'~'No',
                       d$haveWO=='有り'~'Yes')
  return(d)
}


listFunc[[9]] = function(df){
  # get extra comments
  getOne(df, '備考', 'comments')
}

getReady = function(df, list_of_func=list(), extraVarNames, extraVarColumns){
  d = df[, extraVarColumns]
  print(names(d))
  names(d) = extraVarNames
  if(length(list_of_func)==0){return(d)}
  for (i in 1:length(list_of_func)){
    print(i)
    d = bind_cols(d, list_of_func[[i]](df))
  }
  return(d)
}

# assignment
assign = getReady(read_csv('data/CymbaltaTrial_Assign.csv', 
                           locale = locale(encoding = "cp932")),
                  extraVarNames = c('id', 'grp', 'date_visit', 'age', 'sex', 'vas'),
                  extraVarColumns = c(1, 5, 7, 10, 11, 12))
assign = assign %>% 
  mutate(sex = as.numeric(as.factor(sex))) %>%
  mutate(sex = ifelse(sex==2, 'male', 'female')) %>%
  mutate(date_visit = as.Date(date_visit)) %>%
  mutate(vas=as.numeric(as.character(vas))) %>%
  mutate(grp = ifelse(grp=='Group-1', 'placebo', 'duloxetine'))




# Baseline
dfbl = getReady(read_csv('data/CymbaltaTrial_Patient.csv', na = c("@", ""),
                  locale = (locale(encoding = 'cp932'))), 
         list_of_func = listFunc[c(1,3:9)], 
         extraVarNames = c('id', 'date_motor', 'pddrug_effective', 
                           'painpart', 'painpart2', 
                           'date_pain', 'height', 'bw'), 
         extraVarColumns = c(1, 13:19))
dfbl = left_join(assign, dfbl, by = 'id') # basic parameters plus vas is added. 
dfbl$EVENT='BL'

# only keep basic data
assign$vas = NULL
assign$date_visit = NULL


# Week2
dfw2 = getReady(read_csv('data/CymbaltaTrial_FollowW2.csv', na = c("@", ""),
                   locale = (locale(encoding = 'cp932'))),
                list_of_func = listFunc[c(1,9)],
                extraVarNames = c('id', 'date_visit', 'bw'),
                extraVarColumns = c(1,7,9))
dfw2 = dfw2 %>% filter(!is.na(date_visit)) %>%
  left_join(., assign, by = 'id')
dfw2$EVENT='W2'

# Week 10
dfw10 = getReady(read_csv('data/CymbaltaTrial_FollowW10.csv', na = c("@", ""),
                          locale = (locale(encoding = 'cp932'))),
                 list_of_func = listFunc,
                 extraVarNames = c('id', 'date_visit', 'bw'),
                 extraVarColumns = c(1,7,9))
dfw10 = dfw10 %>% filter(!is.na(date_visit)) %>%
  left_join(., assign, by = 'id')

dfw10$EVENT='W10'



# Week 12
dfw12 = getReady(read_csv('data/CymbaltaTrial_Summary.csv', na = c("@", ""),
                          locale = (locale(encoding = 'cp932'))),
                 list_of_func = listFunc[c(1,2,4,9)],
                 extraVarNames = c('id', 'date_visit', 'bw'),
                 extraVarColumns = c(1,7,8))
dfw12 = dfw12 %>% filter(!is.na(date_visit)) %>%
  left_join(., assign, by = 'id')
dfw12$EVENT='W12'

# drop observation 
dfdrop = getReady(read_csv('data/CymbaltaTrial_Cancel.csv', na = c("@", ""),
                          locale = (locale(encoding = 'cp932'))),
                 list_of_func = listFunc,
                 extraVarNames = c('id', 'date_visit', 'bw', 'compliance'),
                 extraVarColumns = c(1,7,9, 19))
dfdrop = dfdrop %>% filter(!is.na(compliance)) %>%
  left_join(., assign, by = 'id')
dfdrop$EVENT='Drop'

# All combined
df = bind_rows(dfbl, dfw2, dfw10, dfw12, dfdrop)

# Data correction
df$pergo[df$id=='01-00027'&df$EVENT=='BL'] = 0.75 # unit mistake
df$vas[df$id=='01-00047'&df$EVENT=='BL'] = 41 # measurement mistake

# LEDD calculation and other new variables
library(zoo)
t = df %>% 
  filter(id !='01-00011') %>% # No medication
  arrange(date_visit) %>%
  group_by(id) %>%
  mutate_at(c(pddrug, 'date_motor', 'pddrug_effective', 
              'painpart', 'painpart2', 
              'date_pain', 'height'),  na.locf0) %>%
  # recode pain parts
  mutate(
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
  # recode sfpain_intense
  mutate(sfpain_intense = case_when(
    sfpain_intense=='わずかな痛み'~ '1:Mild',
    sfpain_intense=='わずらわしい痛み'~ '2:Discomforting',
    sfpain_intense=='やっかいで情けない痛み'~ '3:Distressing',
    sfpain_intense=='激しい痛み'~ '4:Horrible',
    sfpain_intense=='耐え難い痛み'~ '5:Excruciating')) %>%
  # others
  mutate(
    pddrug_effective = ifelse(pddrug_effective=='有り', 1, 0) %>% as.factor,
    BMI = bw/((height/100)^2),
    dur_pd = (as.numeric(first(date_visit)) -as.numeric(date_motor))/365.25,
    dur_pain = (as.numeric(first(date_visit)) -as.numeric(date_pain))/365.25,
    dropI = ifelse(id %in% dfdrop$id, 1, 0) %>% as.factor) %>%
  ungroup() %>% 
  arrange(id, date_visit)
  
t$led = rowSums(t[pddrug], na.rm = T)
t[c(pddrug, 'painpart','painpart2', 'comments', 'height', 'bw', 'date_motor', 'date_pain', 'compliance')]=NULL


# Table 1
library(tableone)
t %>% filter(EVENT=='BL') %>%
  select(-id) %>%
  CreateTableOne(data= ., strata = 'grp')

# Analysis
# Torelance
fisher.test(matrix(c(21,2,16,7), ncol=2))
# VAS
testfunc = function(df, test_item, cov_items=c()){
  d1 = df %>% filter(EVENT=='BL') %>%
    mutate(duloxetine = ifelse(grp=='duloxetine', 1, 0))
  d2 = df %>% filter(EVENT=='W10')
  d3 = df %>% filter(EVENT=='Drop')
  covs=''
  if(length(cov_items)>0){
    covs = paste0(paste(cov_items, collapse ='.y+'), '.y+')
  }
  # d3 = data.frame()
  d = inner_join(d1, bind_rows(d2, d3), by = 'id')
  m = paste0(test_item, '.y ~ ', covs, 'duloxetine + ', test_item, '.x')
  print(m)
  a = lm(m, data = d)
  return(summary(a))
}
testfunc(t, 'vas', c('sex', 'age', 'led'))
testfunc(t, test_item = 'walking', c('BMI'))
testfunc(t, 'sfpain')
testfunc(t, 'bdi')

testfunc(t, 'updrs3', c('sex', 'age', 'led'))

