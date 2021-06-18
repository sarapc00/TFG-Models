library(dplyr)
library(tidyverse)
library(missForest)
library(naniar)
library(rockchalk)

dataset <- read.csv("datasett.csv")

#filter data
#eliminate irrelevant variables
drop <- (c("X","NUMID","NHC","Study.Group","surgery_date","Surgery.Category","readmision_ICD","totcom_6f81b9","tcl_cua3_d20ae6",
           "seguiment_ingrs_inicial_complete","CO_RENAL", "CO_RESPIRATORY","CO_CARDIAC", "CO_ENDOCRINO", "CO_HEPATIC", "CO_NEUROLOGIC", "CO_PSYCHIATRIC", "CO_VASCULAR",
           "AREA_RESIDENCE", "TRANSPORT_TO_HOSPITAL","WEIGHT_BASELINE","HEIGHT_BASELINE","START_PROGRAM","DAYS_PROGRAM","TABACO_DICOTOMICA", "filter_.", 
           "ID", "arr", 'iam1', "angor", "insufcardio", "shockhemodin", "aturadacardioresp", "shockhemoragic", "hemopostquir", "altrescardio", 
           "insufrespi", "fugaaerea", "empiema", "hemotorax", "fsitulabronco", "emfisemasubcutani", "infeccrespi", "atelectasi", "tep1", 
           "vesspleural", "penumotorax", "altresrespi", "insufren", 'rao1', 'altresrenals', "nausees", "ilipara", "coleccintrab", "pancreatit", 
           "biliar", "hda1", "rectorragia", 'descompdm', "descomphepat", "altresdiges", "deliri", 'avc1', "convulsio", 'altresneuro', "infurinaria", 
           'sepsis', "infferida", 'shockseptic', "infcateter", "altresinfecc", 'dehiscencia', 'tvp1', 'cefalea', 'complcateter', "dolor1", "altresmiscelania", 
           "HANDGRIP_NON_DOM_BASELINE", "HAD_DEP_BASELINE", "HAD_ANX_BASELINE", "Walkin_REF_Troosters", "Walking_REF_Enright"))

dataset<-dataset[ , !(names(dataset) %in% drop)]


#dataset<-rename(dataset, "GMA" = 'GMA_Pes')

dataset$MINDFULNESS<-as.factor(dataset$MINDFULNESS)#there is a number 2 where there shouldn't be
dataset$MINDFULNESS<-na_if(dataset$MINDFULNESS, '2.0')#set NA where the 2s are
dataset$MINDFULNESS<-droplevels(dataset$MINDFULNESS)#drop unused level 2, we are left with only 2 levels: yes and no

#combine levels into 1 due to under-representation
dataset$disease_group<-combineLevels(dataset$disease_group,levs = c("diseases of the genitourinary system", "endocrine, nutritional and metabolic diseases, and immunity disorders",
                                     "injury and poisoning", "diseases of the digestive system", "factors influencing health status", "diseases of the respiratory system"), newLabel = c("Other") )
summary(dataset$disease_group)
attach(dataset)
colnames(dataset)
#estratificacio risc poblacional (cutoff at 9.360, 22.098, 40.387)
Pstrat<-seq(0,0,length.out=nrow(dataset))
Pstrat<-factor(Pstrat)
dataset<-cbind(dataset,Pstrat)
levels(dataset$Pstrat)<-c("Risc molt alt","Risc alt","Risc moderat","Risc baix")
for (i in 1:nrow(dataset)){
  if(dataset$GMA[i]>40.387){
    dataset$Pstrat[i]<-"Risc molt alt"
  } else if (dataset$GMA[i]>22.098){
    dataset$Pstrat[i]<-"Risc alt"
  } else if (dataset$GMA[i]>9.360){
    dataset$Pstrat[i]<-"Risc moderat"
  } else{
    dataset$Pstrat[i]<-"Risc baix"
  }
}
dataset$Pstrat<-as.factor(dataset$Pstrat)


#estratificacio risc cohort (cutoff at 0.5, 0.8, 0.95)
Cstrat<-seq(0,0,length.out=nrow(dataset))
Cstrat<-factor(Cstrat)
dataset<-cbind(dataset,Cstrat)
levels(dataset$Cstrat)<-c("Risc molt alt","Risc alt","Risc moderat","Risc baix")
for (i in 1:nrow(dataset)){
  if(dataset$GMA[i]>quantile(dataset$GMA,.95)){
    dataset$Cstrat[i]<-"Risc molt alt"
  } else if (dataset$GMA[i]>quantile(dataset$GMA,.8)){
    dataset$Cstrat[i]<-"Risc alt"
  } else if (dataset$GMA[i]>quantile(dataset$GMA,.5)){
    dataset$Cstrat[i]<-"Risc moderat"
  } else{
    dataset$Cstrat[i]<-"Risc baix"
  }
}
dataset$Cstrat<-as.factor(dataset$Cstrat)



#Calcul complicacions per sistema
#comp_tot_cardio<-seq(0,0,length.out=nrow(dataset))
#dataset<-cbind(dataset,comp_tot_cardio)
#dataset$comp_tot_cardio<-rowSums(dataset[,c('arr', 'iam1', 'angor', 'insufcardio','shockhemodin', 'aturadacardioresp', 'shockhemoragic', 'hemopostquir', 'altrescardio')])

#PUEDES AÑADIR LAS OTRAS CATEGORIAS AQUI? respiratorio, digestivo, neuro, infecciosas..........

#comp_tot_resp <- seq(0,0,length.out=nrow(dataset))
#dataset<-cbind(dataset, comp_tot_resp)
#dataset$comp_tot_resp <- rowSums(dataset[,c("insufrespi", "fugaaerea", "empiema", "hemotorax", "fsitulabronco", "emfisemasubcutani", "infeccrespi", 
#                                            "atelectasi", "tep1", "vesspleural", "penumotorax", "altresrespi")])
#comp_tot_ren <- seq(0,0,length.out=nrow(dataset))
#dataset<-cbind(dataset, comp_tot_ren)
#dataset$comp_tot_ren <- rowSums(dataset[,c("insufren", "rao1", "altresrenals")])

#c<omp_tot_diges <- seq(0,0,length.out=nrow(dataset))
#d<ataset<-cbind(dataset, comp_tot_diges)
#dataset$comp_tot_diges <- rowSums(dataset[,c("nausees", "ilipara", "coleccintrab", "pancreatit", "biliar", "hda1", "rectorragia", "descompdm","descomphepat", "altresdiges")])

#comp_tot_neuro <- seq(0,0,length.out=nrow(dataset))
#dataset<-cbind(dataset, comp_tot_neuro)
#d<ataset$comp_tot_neuro <- rowSums(dataset[,c("deliri", "avc1", "convulsio", "altresneuro")])

#comp_tot_inf <- seq(0,0,length.out=nrow(dataset))
#dataset<-cbind(dataset, comp_tot_inf)
#dataset$comp_tot_inf <- rowSums(dataset[,c("infurinaria", "sepsis", "infferida", "shockseptic", "infcateter", "altresinfecc")])

#comp_tot_misc <- seq(0,0,length.out=nrow(dataset))
#dataset<-cbind(dataset, comp_tot_misc)
#dataset$comp_tot_misc <- rowSums(dataset[,c("dehiscencia","tvp1","cefalea","complcateter","dolor1","altresmiscelania")])


# #separate data depending on the time of extraction
# day1prehab <- subset(data, select = c('ID','Study_Subgroup', 'Completers', 'ABS', 'sex', 'age', 'num_diag', 'disease_group',
#                                       'Surgery.Subcategory', 'GMA_Pes', 'DespesaPrevia', 'nFarmacs', 'ASA', 'BMI', 
#                                       'TABACCO', 'DASI_BASELINE', 'CHARLSON_BASELINE', 'MUST_BASELINE', 'CSHA_BASELINE'))
# post_surgical <- subset(data, select = c('ID','comedic_1b6168', 'coquirurgic_b719de', 'arr', 'iam1', 'angor', 'insufcardio', 
#                                          'shockhemodin', 'aturadacardioresp', 'shockhemoragic', 'hemopostquir', 'altrescardio', 'insufrespi', 'fugaaerea', 'empiema', 'hemotorax', 'fsitulabronco', 
#                                          'emfisemasubcutani', 'infeccrespi', 'tep1', 'vesspleural', 'penumotorax', 'altresrespi', 'insufren', 'rao1', 'altresrenals', 'nausees', 'ilipara', 'coleccintrab',
#                                          'pancreatit', 'biliar', 'hda1', 'rectorragia', 'descompdm', 'descomphepat', 'altresdiges',
#                                          'deliri', 'avc1', 'convulsio', 'altresneuro', 'infurinaria', 'sepsis', 'infferida', 'shockseptic', 
#                                          'infcateter', 'altresinfecc', 'dehiscencia', 'tvp1', 'cefalea', 'complcateter', 'dolor1', 
#                                          'altresmiscelania', 'comprehensive_complication', 'ONCOLOGIC_SURGERY', 'NEOADJUVANCE', 
#                                          'MINDFULNESS'))
# 
# post_dicharge_30 <- subset(data, select = c('ID','readmision_30', 'reintervention_30', 'emergency_room_30'))
# 

colnames(dataset)

#missing values
n_var_miss(dataset) #20 variables with missing values
gg_miss_which(dataset)+ geom_text(aes(label = miss_var_summary(dataset)$pct_miss), hjust = -0.25) #shows which ones contain NAs
miss_var_summary(dataset)#percentage of missing values in each column
ggplot(miss_var_summary(dataset)$pct_miss) + labs(title = "Variable Importance" ) + geom_text(aes(label =round(Importance, digits = 3)), hjust = -0.25)
pct_miss(dataset) #total % missing

vis_miss(dataset, sort_miss = TRUE) 
gg_miss_var(dataset, show_pct = TRUE)


#remove variables to predict
predicted_v <- dataset[,c ("readmision_30", 'reintervention_30', "emergency_room_30")]
dataset<-dataset[ , !(names(dataset) %in% c("readmision_30", 'reintervention_30', "emergency_room_30"))]

#input values
options(stringsAsFactors = TRUE)
set.seed(44)
dataset.imp<-missForest(dataset)

dataset.imp$ximp$CSHA_BASELINE<-round(dataset.imp$ximp$CSHA_BASELINE)
dataset.imp$ximp$MUST_BASELINE<-round(dataset.imp$ximp$MUST_BASELINE)
dataset.imp$ximp$SIX_MIN_WALK_BASELINE<-round(dataset.imp$ximp$SIX_MIN_WALK_BASELINE)
dataset.imp$ximp$HANDGRIP_DOM_BASELINE<-round(dataset.imp$ximp$HANDGRIP_DOM_BASELINE)
dataset.imp$ximp$HAD_TOTAL_BASELINE<-round(dataset.imp$ximp$HAD_TOTAL_BASELINE)
dataset.imp$ximp$YPAS_BASELINE<-round(dataset.imp$ximp$YPAS_BASELINE)
dataset.imp$ximp$ALBUMINA_BASELINE<-round(dataset.imp$ximp$ALBUMINA_BASELINE)

dataset<-dataset.imp$ximp


#attach variables to predict
dataset<-cbind(dataset,predicted_v)
#dataset<-rename(dataset, "Queralt_procediments" = 'Queralt_procediments.1')

colnames(dataset)
#dataset<-rename(dataset, "Queralt_procediments" = 'Queralt_procediments.1')
dataset$readmision_30


#save data
write.csv(dataset,file="dataset_imputed.csv",row.names=FALSE)
