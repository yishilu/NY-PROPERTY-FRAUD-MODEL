library(dplyr)
library(ggplot2)
library(tidyr)

nyprop=read.csv('NY property data.csv')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## clean missing value of zip
zipmode=nyprop %>% arrange(BBLE)
wizzip=zipmode %>% fill(ZIP)
count_lte_10=wizzip %>% filter(LTFRONT!=0,LTDEPTH!=0,BLDFRONT!=0,BLDDEPTH!=0,!is.na(STORIES)) %>%
  group_by(BLDGCL) %>%
  summarise(count=n(),
            medianfront=median(LTFRONT),
            mediandepth=median(LTDEPTH),
            medianbdfront=median(BLDFRONT),
            medianbddepth=median(BLDDEPTH),
            medianstory=median(STORIES)) %>%
  filter(count>=10)


## clean missing value of ltfront
ltfmissing=wizzip %>% filter(LTFRONT==0)
ltfvalue=wizzip %>% filter(LTFRONT!=0)
count_lte_10_ltf=count_lte_10 %>% select(BLDGCL,medianfront)
wizltf=left_join(ltfmissing,count_lte_10_ltf, by=c('BLDGCL'))
wizltf1=wizltf %>% select(-LTFRONT)
colnames(wizltf1)[32]='LTFRONT'
newltf=rbind(ltfvalue,wizltf1)
newltfmissing=newltf %>% filter(is.na(LTFRONT))
newltfvalue=newltf %>% filter(!is.na(LTFRONT))
count_naltf=newltfvalue %>% group_by(TAXCLASS) %>%
  summarise(medianfront=median(LTFRONT))
wizltf3=left_join(newltfmissing,count_naltf,by='TAXCLASS')
wizltf4=wizltf3 %>% select(-LTFRONT)
colnames(wizltf4)[32]='LTFRONT'
cleanltf=rbind(newltfvalue,wizltf4)


## clean missing value of ltdepth
ltdmissing=cleanltf %>% filter(LTDEPTH==0)
ltdvalue=cleanltf %>% filter(LTDEPTH!=0)
count_lte_10_ltd=count_lte_10 %>% select(BLDGCL,mediandepth)
wizltd=left_join(ltdmissing,count_lte_10_ltd, by=c('BLDGCL'))
wizltd1=wizltd %>% select(-LTDEPTH)
colnames(wizltd1)[32]='LTDEPTH'
newltd=rbind(ltdvalue,wizltd1)
newltdmissing=newltd %>% filter(is.na(LTDEPTH))
newltdvalue=newltd %>% filter(!is.na(LTDEPTH))
count_naltd=newltdvalue %>% group_by(TAXCLASS) %>%
  summarise(mediandepth=median(LTDEPTH))
wizltd3=left_join(newltdmissing,count_naltd,by='TAXCLASS')
wizltd4=wizltd3 %>% select(-LTDEPTH)
colnames(wizltd4)[32]='LTDEPTH'
cleanltd=rbind(newltdvalue,wizltd4)

## clean missing value of stories
smissing=cleanltd %>% filter(is.na(STORIES))
svalue=cleanltd %>% filter(!is.na(STORIES))
count_lte_10_s=count_lte_10 %>% select(BLDGCL,medianstory)
wizs=left_join(smissing,count_lte_10_s, by=c('BLDGCL'))
wizs1=wizs %>% select(-STORIES)
colnames(wizs1)[32]='STORIES'
news=rbind(svalue,wizs1)
newsmissing=news %>% filter(is.na(STORIES))
newsvalue=news %>% filter(!is.na(STORIES))
count_nas=newsvalue %>% group_by(TAXCLASS) %>%
  summarise(medianstory=median(STORIES))
wizs3=left_join(newsmissing,count_nas,by='TAXCLASS')
wizs4=wizs3 %>% select(-STORIES)
colnames(wizs4)[32]='STORIES'
cleans=rbind(newsvalue,wizs4)


## clean missing value of BLDFRONT
bdfmissing=cleans %>% filter(BLDFRONT==0)
bdfvalue=cleans %>% filter(BLDFRONT!=0)
count_lte_10_bdf=count_lte_10 %>% select(BLDGCL,medianfront)
wizbdf=left_join(bdfmissing,count_lte_10_bdf, by=c('BLDGCL'))
wizbdf1=wizbdf %>% select(-BLDFRONT)
colnames(wizbdf1)[32]='BLDFRONT'
newbdf=rbind(bdfvalue,wizbdf1)
newbdfmissing=newbdf %>% filter(is.na(BLDFRONT))
newbdfvalue=newbdf %>% filter(!is.na(BLDFRONT))
count_nabdf=newbdfvalue %>% group_by(TAXCLASS) %>%
  summarise(medianfront=median(BLDFRONT))
wizbdf3=left_join(newbdfmissing,count_nabdf,by='TAXCLASS')
wizbdf4=wizbdf3 %>% select(-BLDFRONT)
colnames(wizbdf4)[32]='BLDFRONT'
cleanbdf=rbind(newbdfvalue,wizbdf4)

## clean missing value of BLDDEPTH
bldmissing=cleanbdf %>% filter(BLDDEPTH==0)
bldvalue=cleanbdf %>% filter(BLDDEPTH!=0)
count_lte_10_bld=count_lte_10 %>% select(BLDGCL,mediandepth)
wizbld=left_join(bldmissing,count_lte_10_bld, by=c('BLDGCL'))
wizbld1=wizbld %>% select(-BLDDEPTH)
colnames(wizbld1)[32]='BLDDEPTH'
newbld=rbind(bldvalue,wizbld1)
newbldmissing=newbld %>% filter(is.na(BLDDEPTH))
newbldvalue=newbld %>% filter(!is.na(BLDDEPTH))
count_nabld=newbldvalue %>% group_by(TAXCLASS) %>%
  summarise(mediandepth=median(BLDDEPTH))
wizbld3=left_join(newbldmissing,count_nabld,by='TAXCLASS')
wizbld4=wizbld3 %>% select(-BLDDEPTH)
colnames(wizbld4)[32]='BLDDEPTH'
cleanbld=rbind(newbldvalue,wizbld4)



count_lte_10_1=cleanbld %>% filter(FULLVAL!=0,AVTOT!=0,AVLAND!=0) %>%
  group_by(ZIP,TAXCLASS) %>%
  summarise(count=n(),
            medianfull=median(FULLVAL),
            mediantot=median(AVTOT),
            medianland=median(AVLAND)) %>%
  filter(count>=10)

## Get median value for ltdepth, ltfront, fullval regarding to lotarea
medianvalue1=cleanbld %>% select(B,ZIP,LTDEPTH,LTFRONT,FULLVAL) %>% filter(FULLVAL!=0) %>% group_by(B,ZIP) %>% 
  summarise(mediandepth=getmode(LTDEPTH),
            medianfront=getmode(LTFRONT),
            medianval=getmode(FULLVAL)) %>%
  mutate(lotpercentage=medianval/(mediandepth*medianfront)) %>% select(B,ZIP,lotpercentage)

medianvalue2=cleanbld %>% select(B,ZIP,LTDEPTH,LTFRONT,AVTOT) %>% filter(AVTOT!=0) %>% group_by(B,ZIP) %>% 
  summarise(mediandepth=getmode(LTDEPTH),
            medianfront=getmode(LTFRONT),
            mediantot=getmode(AVTOT)) %>%
  mutate(lotpercentage=mediantot/(mediandepth*medianfront)) %>% select(B,ZIP,lotpercentage)

medianvalue3=cleanbld %>% select(B,ZIP,LTDEPTH,LTFRONT,AVLAND) %>% filter(AVLAND!=0) %>% group_by(B,ZIP) %>% 
  summarise(mediandepth=getmode(LTDEPTH),
            medianfront=getmode(LTFRONT),
            medianland=getmode(AVLAND)) %>%
  mutate(lotpercentage=medianland/(mediandepth*medianfront)) %>% select(B,ZIP,lotpercentage)

## clean missing value of fullvalue
fullmissing=cleanbld %>% filter(FULLVAL==0)
fullvalue=cleanbld %>% filter(FULLVAL!=0)
count_lte_10_1_full=count_lte_10_1 %>% select(ZIP,TAXCLASS,medianfull)
wizfull=left_join(fullmissing,count_lte_10_1_full, by=c('ZIP','TAXCLASS'))
wizfull1=wizfull %>% select(-FULLVAL)
colnames(wizfull1)[32]='FULLVAL'
newfull=rbind(fullvalue,wizfull1)
newfullmissing=newfull %>% filter(is.na(FULLVAL))
newfullvalue=newfull %>% filter(!is.na(FULLVAL))
count_nafull=newfullvalue %>% group_by(B,TAXCLASS) %>%
  summarise(medianfull=median(FULLVAL))
wizfull3=left_join(newfullmissing,count_nafull,by=c('B','TAXCLASS'))
wizfull4=wizfull3 %>% select(-FULLVAL)
colnames(wizfull4)[32]='FULLVAL'
cleanfull=rbind(newfullvalue,wizfull4)
cleanfullvalue=cleanfull %>% filter(!is.na(FULLVAL))
cleanfullna=cleanfull %>% filter(is.na(FULLVAL))
cleanfullna1=left_join(cleanfullna,medianvalue1,by=c('B','ZIP'))
cleanfullna2=cleanfullna1%>%select(-FULLVAL)%>%mutate(FULLVAL=lotpercentage*LTDEPTH*LTFRONT) %>%select(-lotpercentage)
cleanfull2=rbind(cleanfullvalue,cleanfullna2)

## clean missing value of avtot
totmissing=cleanfull2 %>% filter(AVTOT==0)
totvalue=cleanfull2 %>% filter(AVTOT!=0)
count_lte_10_1_tot=count_lte_10_1 %>% select(ZIP,TAXCLASS,mediantot)
wiztot=left_join(totmissing,count_lte_10_1_tot, by=c('ZIP','TAXCLASS'))
wiztot1=wiztot %>% select(-AVTOT)
colnames(wiztot1)[32]='AVTOT'
newtot=rbind(totvalue,wiztot1)
newtotmissing=newtot %>% filter(is.na(AVTOT))
newtotvalue=newtot %>% filter(!is.na(AVTOT))
count_natot=newtotvalue %>% group_by(B,TAXCLASS) %>%
  summarise(mediantot=median(AVTOT))
wiztot3=left_join(newtotmissing,count_natot,by=c('B','TAXCLASS'))
wiztot4=wiztot3 %>% select(-AVTOT)
colnames(wiztot4)[32]='AVTOT'
cleantot=rbind(newtotvalue,wiztot4)
cleantotvalue=cleantot %>% filter(!is.na(AVTOT))
cleantotna=cleantot %>% filter(is.na(AVTOT))
cleantotna1=left_join(cleantotna,medianvalue2,by=c('B','ZIP'))
cleantotna2=cleantotna1%>%select(-AVTOT)%>%mutate(AVTOT=lotpercentage*LTDEPTH*LTFRONT) %>%select(-lotpercentage)
cleantot2=rbind(cleantotvalue,cleantotna2)


## clean missing value of avland
landmissing=cleantot2 %>% filter(AVLAND==0)
landvalue=cleantot2 %>% filter(AVLAND!=0)
count_lte_10_1_land=count_lte_10_1 %>% select(ZIP,TAXCLASS,medianland)
wizland=left_join(landmissing,count_lte_10_1_land, by=c('ZIP','TAXCLASS'))
wizland1=wizland %>% select(-AVLAND)
colnames(wizland1)[32]='AVLAND'
newland=rbind(landvalue,wizland1)
newlandmissing=newland %>% filter(is.na(AVLAND))
newlandvalue=newland %>% filter(!is.na(AVLAND))
count_naland=newlandvalue %>% group_by(B,TAXCLASS) %>%
  summarise(medianland=median(AVLAND))
wizland3=left_join(newlandmissing,count_naland,by=c('B','TAXCLASS'))
wizland4=wizland3 %>% select(-AVLAND)
colnames(wizland4)[32]='AVLAND'
final=rbind(newlandvalue,wizland4)

## final cleaned version
finalvalue=final %>% filter(!is.na(AVLAND))
finalna=final %>% filter(is.na(AVLAND))
finalna1=left_join(finalna,medianvalue3,by=c('B','ZIP'))
finalna2=finalna1%>%select(-AVLAND)%>%mutate(AVLAND=lotpercentage*LTDEPTH*LTFRONT) %>%select(-lotpercentage)
final2=rbind(finalvalue,finalna2)

