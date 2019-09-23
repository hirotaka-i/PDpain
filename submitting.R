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
  d1 = df[, grep('痛みの評価No', names(df))]
  d$sfpainSens = rowSums(d1[,1:11], na.rm = T)
  d$sfpainAffe = rowSums(d1[,12:15], na.rm = T)
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
  dfpdq$pdq_mobility = dfpdq$pdq_mobility/40*100
  dfpdq$pdq_adl = dfpdq$pdq_adl/24*100
  dfpdq$pdq_ewb = dfpdq$pdq_ewb/24*100
  dfpdq$pdq_stigma = dfpdq$pdq_stigma/16*100
  dfpdq$pdq_social = dfpdq$pdq_social/12*100
  dfpdq$pdq_cog = dfpdq$pdq_cog/12*100
  dfpdq$pdq_commu = dfpdq$pdq_commu/12*100
  dfpdq$pdq_body = dfpdq$pdq_body/12*100
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
  mutate_at(vars(starts_with('pain_')), as.factor) %>%
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


# Create PPS and FAS
names(t)
outcomes = names(t)[c(6,8,9,11,12,14:23)]
units = c('mm', 'Score', 'PRI', 'PRI', 'PRI', 'Score',
          rep('%', 8), 'seconds')
testnames = c('Visual Analogue Scale', "Beck's Depression Inventory", 
              'SF_MPQ, Total', 'SF-MPQ, Sensory', 'SF-MPQ, Affective',
              'MDS-UPDRS part III', 
              paste('PDQ39', c('Mobility', 'ADL', 'Emotional Well-Being',
                               'Stigma', 'Social Support', 'Cognition', 
                               'Communication', 'Bodily Discomfort'), sep=', '),
              '3m Up and Go Test') 
pps = t %>% filter(dropI==0, EVENT %in% c('BL', 'W10'))

fas = t %>% 
  filter(EVENT!='W12') %>%
  arrange(id, date_visit) %>%
  group_by(id) %>%
  mutate_at(vars(outcomes), na.locf0) %>%
  mutate(V = 1:n()) %>%
  mutate(idx = (V==min(V)) | (V==max(V))) %>%
  ungroup() %>%
  filter(idx) %>%
  select(names(pps))

fas %>% with(table(EVENT))


# Table 1
library(tableone)
fas %>% filter(EVENT=='BL') %>%
  select(-id) %>%
  select(-date_visit) %>%
  CreateTableOne(data= ., strata = 'grp') %>%
  print(quote = FALSE, noSpaces = TRUE, printToggle = FALSE) %>%
  write.csv(file='output/table1.csv')

# Table 2
dfdrop %>% select(id, compliance, grp)

# Fisher's exact test
fisher.test(matrix(c(23-2,2,23-7,7), ncol=2))

# SupTable for adverse event
getAE = function(df){
  # get pd status
  AEs = c('id', 'input', 'Dstart', 'status', 'Dend', 'Dcontin', 'contStatus', 'diagnosis',
          'PD', 'pain', 'lab', 'diagnosisOth', 'seriousness', 'severity', 'causality', 
          'drugAction', 'otherAction', 'detail')
  d1 = df[,c(1, grep('有害事象\\[No.1\\]', names(df)))]
  d2 = df[,c(1, grep('有害事象\\[No.2\\]', names(df)))]
  d3 = df[,c(1, grep('有害事象\\[No.3\\]', names(df)))]
  d4 = df[,c(1, grep('有害事象\\[No.4\\]', names(df)))]
  d5 = df[,c(1, grep('有害事象\\[No.5\\]', names(df)))]
  names(d1) = AEs
  names(d2) = AEs
  names(d3) = AEs
  names(d4) = AEs
  names(d5) = AEs
  return(bind_rows(d1,d2,d3,d4,d5))
}
dfae1 = getAE(read_csv('data/CymbaltaTrial_Adverse.csv', na = c("@", ""),
                       locale = (locale(encoding = 'cp932')))) %>%
  filter(input!='未入力') %>% 
  left_join(., assign %>% select(id, grp), by = 'id')
dfae2 = dfae1 %>%
  mutate(event = ifelse(diagnosis=='その他', diagnosisOth, diagnosis)) %>%
  group_by(grp, severity, event) %>%
  mutate(N = n()) %>%
  distinct(grp, severity, event, N) %>%
  arrange(grp, severity, desc(N))
dfae2 %>% write_csv('output/SupTable1.csv')


# Efficacy Analysis
# VAS
testfunc = function(df, test_item, cov_items=c()){
  # if cov_items are given, 
  # the model would be regressed by the baseline cov_items' values
  d = df %>% mutate(duloxetine=ifelse(grp=='duloxetine', 1, 0))
  d[, 'outcome'] = d[, test_item]
  d = d %>% 
    arrange(id, date_visit) %>%
    group_by(id) %>%
    mutate(change = last(outcome) - first(outcome)) %>% 
    distinct(id, .keep_all = T)
  if(length(cov_items)>0){
    covs = paste0('+', paste(cov_items, collapse ='+'))
  }else{covs=''}
  m = paste0('change ~ duloxetine', covs)
  a = lm(m, data = d)
  return(summary(a))
}
testfunc(fas, 'vas')

# Table 3
giveRes = function(df, outcome){
  t = testfunc(df, outcome)
  res = coef(t) %>% .['duloxetine',]
  n = length(t$residuals)
  b = res[1]
  se = res[2]
  l = b - 1.96 * se
  u = b + 1.96 * se
  p = res[4]
  ci =   sprintf('%.1f [%.1f, %.1f]', b,l,u)
  o = data.frame(outcome = outcome, beta=b, se = se, p = p, ci = ci, n = n)
  return(o)
}

lapply(outcomes, function(x){giveRes(fas, x)}) %>%
  bind_rows() %>%
  fwrite('output/table3_fas.csv')


lapply(outcomes, function(x){giveRes(pps, x)}) %>%
  bind_rows() %>%
  fwrite('output/table3_pps.csv')


# Figure 1
pd <- position_dodge(1)
plotWeek=function(df, outcome){
  df[, 'outcome'] = df[, outcome]
  df %>% filter(dropI!=1, EVENT!='W2') %>% 
    rename(Arm = grp) %>%
    mutate(Week = case_when(
      EVENT=='BL'~0,
      EVENT=='W10'~10,
      EVENT=='W12'~12
    )) %>% 
    filter(!is.na(outcome)) %>%
    group_by(Week, Arm) %>%
    do(lm(outcome ~ 1, data=.) %>% broom::tidy()) %>%
    .[, 1:5] %>%
    rename(Mean = estimate, SE = std.error) %>%
    ggplot(aes(x=Week, y = Mean, colour=Arm)) + 
    geom_line(position=pd, show.legend = FALSE) + 
    geom_point(position=pd, size=3, show.legend = FALSE) +
    geom_errorbar(aes(ymin= Mean - 1 * SE, ymax = Mean + 1 * SE), 
                  width=1, position=pd, show.legend = FALSE) +
    expand_limits (x=c(0, 12), y=0) + 
    scale_x_discrete(limits=c(0, 10, 12))+
    theme_bw() +
    labs(title = testnames[which(outcomes==outcome)],
         x = "Week from baseline", y = units[which(outcomes==outcome)])
}
plotWeek(fas, outcomes[1])
figs = list()
for (i in 1:length(outcomes)){
  cat(i, outcomes[i], '\n')
  figs[[i]]=plotWeek(t, outcomes[i])
}
p = gridExtra::grid.arrange(grobs=figs[c(1,15,2,3:14)], ncol=5)
ggsave(plot = p, filename = 'output/fig1.png', width=15, height = 9)



# SupFig1
plotFig1=function(df, outcome){
  d = df %>% 
    mutate(Week = case_when(
      EVENT=='BL'~0,
      EVENT!='BL'~10))
  d[, 'outcome'] = d[, outcome]
  # print(d %>% filter(!is.na(outcome)) %>% with(table(grp, EVENT)))
  ggplot(data=d, aes(y=outcome, x=Week, group = id)) +
    # geom_boxplot( aes(y=outcome, x=Week)) + 
    geom_line(show.legend = F, colour='grey') +
    facet_grid(.~grp) +
    geom_point(colour='grey') +
    stat_summary(aes(group = 1), geom = 'line', fun.y = mean,
                 size = 2, colour='black') +
    scale_x_discrete(limits=c(0, 10))+
    theme_bw() +
    labs(title = testnames[which(outcomes==outcome)],
         x = "Week from baseline", y = units[which(outcomes==outcome)])
}
plotFig1(pps, outcomes[1])
figs = list()
for (i in 1:length(outcomes)){
  cat(i, outcomes[i], '\n')
  figs[[i]]=plotFig1(fas, outcomes[i])
}
p = gridExtra::grid.arrange(grobs=figs, ncol=3, )
ggsave(plot = p, filename = 'output/supfig1.png', width=9, height = 12)
