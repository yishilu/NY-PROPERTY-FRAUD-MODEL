library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)


## Z-scale and perform PCA to reduce dimensionality
nyprop = fread('new_variable.csv')
nyprop=nyprop %>% select(-V1)
nyprop1=scale(nyprop[,-1],center=TRUE,scale=TRUE)
nyprop.pca=prcomp(nyprop1,center=TRUE,scale= FALSE)
summary(nyprop.pca)

## heuristic algorithm, fraudscore 1
pc1=cbind(nyprop1,nyprop.pca$x[,1:8])
pc1=data.frame(pc1)
pc2=data.frame(nyprop$RECORD,scale(pc1[,46:53],center=TRUE,scale=TRUE))
pc3=pc2 %>% mutate(score1=abs(PC1)+abs(PC2)+abs(PC3)+abs(PC4)+abs(PC5)+abs(PC6)+abs(PC7)+abs(PC8))
pc3=pc3 %>% mutate(score1.2=sqrt((PC1)**2+(PC2)**2+(PC3)**2+(PC4)**2+(PC5)**2+(PC6)**2+(PC7)**2+(PC8)**2))


## autoencoder, fraudscore2
library(h2o)
h2o.init()
pc_8=pc3[,2:9]
pc_8.hf=as.h2o(pc_8)
anomaly_model=h2o.deeplearning(x=names(pc_8.hf),training_frame=pc_8.hf,autoencoder=TRUE)
recon_error=h2o.anomaly(anomaly_model,pc_8.hf,per_feature=FALSE)
recon_error=as.data.frame(recon_error)
finalscore=data.frame(pc3$nyprop.RECORD,pc3[,10:11],recon_error)


## quantile binning two scores and combine to final fraud score
finalscore2=finalscore %>% select(pc3.nyprop.RECORD,score1.2) %>% arrange(score1.2) %>% mutate(rank1.2=seq(1,1070994,1))
finalscore3=finalscore %>% select(pc3.nyprop.RECORD,Reconstruction.MSE) %>% arrange(Reconstruction.MSE) %>% mutate(rank2.2=seq(1,1070994,1))
final=left_join(finalscore2,finalscore3,by='pc3.nyprop.RECORD')
final=final%>%mutate(score=rank1.2+rank2.2) %>% select(pc3.nyprop.RECORD,score)%>% arrange(desc(score))

## join to original dataset and find the characteristics of anomalies
ny=fread('NY property data.csv')
anomaly=left_join(final,ny,by=c('pc3.nyprop.RECORD'='RECORD'))

