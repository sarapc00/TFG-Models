dat<-read.csv("var_imp.csv")
colnames(dat)<- c('MODEL', 'VARIABLE', 'VALUE')
dat$MODEL
ggplot(dat = subset(dat, MODEL == 'READMISSION'), aes(y=VALUE, x= reorder(VARIABLE, VALUE))) + 
  geom_bar(position="identity", stat="identity", fill='blue') + coord_flip() + 
  geom_text(aes(label= round(as.numeric(VALUE), digits = 2)), colour = "black", size = 3, hjust = -0.05, vjust= 0)  + 
  ggtitle('Variable Importance Readmission (%)') + xlab("Variable")

ggplot(dat = subset(dat, MODEL == 'EMERGENCY_ROOM'), aes(y=VALUE, x= reorder(VARIABLE, VALUE))) + 
  geom_bar(position="identity", stat="identity", fill='red') + coord_flip() + 
  geom_text(aes(label= round(as.numeric(VALUE), digits = 2)), colour = "black", size = 3, hjust = -0.05, vjust= 0)  + 
  ggtitle('Variable Importance Emergency Room (%)') + xlab("Variable")

ggplot(dat = subset(dat, MODEL == 'ADHERENCE'), aes(y=VALUE, x= reorder(VARIABLE, VALUE))) + 
  geom_bar(position="identity", stat="identity", fill='orange') + coord_flip() + 
  geom_text(aes(label= round(as.numeric(VALUE), digits = 2)), colour = "black", size = 3, hjust = -0.05, vjust= 0)  + 
  ggtitle('Variable Importance Adherence (%)') + xlab("Variable")

ggplot(dat,aes(x=reorder(VARIABLE, VALUE),y=VALUE,fill=MODEL, label=prettyNum(as.numeric(VALUE),digits = 3)))+
  geom_bar(stat="identity")+
  geom_text(position="stack", angle= -45)+ coord_flip()

summary(em_rf)

#AUC comparison

#ggplot(AUCtable, aes(x=Model, y=value)) + geom_point(aes(color=Model), size = 6) + labs(title = 'AUC (mean) per Model') + theme(axis.text.x = element_text(angle = 45)) + scale_color_brewer(palette="Dark2")
#ggplot(AUCtable, aes(x=Model, y=value, fill = Model)) + geom_bar(stat = 'identity') + labs(title = 'AUC (mean) per Model') + theme(axis.text.x = element_text(angle = 45))

#random forest comparison between models
roc.rf <- roc(data_test$readmision_30, predict(re_rf, data_test, type = 'prob')[, 'Yes'])
plot(roc.rf, col = "blue")
roc.em.rf <- roc(data_test$emergency_room_30, predict(em_rf, data_test, type = "prob")[, "Yes"])
lines(roc.em.rf, col = "red")
roc.ad.rf <- roc(data_test$Completers, predict(ad_rf, data_test, type = "prob")[, "Completers"])
lines(roc.ad.rf, col = "orange")
text(title='a')

#readmission model comparison
roc.re.rf <- roc(data_test$readmision_30, predict(re_rf, data_test, type = 'prob')[, 'Yes'])
plot(roc.rf, col = "blue")
roc.re.lr <- roc(data_test$readmision_30, predict(re_lr, data_test, type = "prob")[, "Yes"])
lines(roc.em.rf, col = "red")
roc.re.dt <- roc(data_test$readmision_30, predict(re_dt, data_test, type = "prob")[, "Yes"])
lines(roc.ad.rf, col = "orange")

#all models comparison
#RF dotted; lty = 2
#AD ; lty = 4
#readmission - blue
#emergency room - red
#adherence - orange
#RF - blue
#LR - red
#DT - orange

#readmission

roc.rf <- roc(data_test$readmision_30, predict(re_rf, data_test, type = 'prob')[, 'Yes'])
plot(roc.rf, col = "blue")
roc.rf <- roc(data_test$readmision_30, predict(re_lr, data_test, type = 'prob')[, 'Yes'])
lines(roc.rf, col = "red")
roc.rf <- roc(data_test$readmision_30, predict(re_dt, data_test, type = 'prob')[, 'Yes'])
lines(roc.rf, col = "orange")
legend("bottomright", legend=c("Random Forest", "Logistic Regression", "Decision Trees"),
       fill=c("blue", "red", "orange"), cex=0.8)
title("Readmission")

#emergency room

roc.em.rf <- roc(data_test$emergency_room_30, predict(em_rf, data_test, type = "prob")[, "Yes"])
plot(roc.em.rf, col = "blue")
roc.em.rf <- roc(data_test$emergency_room_30, predict(em_lr, data_test, type = "prob")[, "Yes"])
lines(roc.em.rf, col = "red")
roc.em.rf <- roc(data_test$emergency_room_30, predict(em_dt, data_test, type = "prob")[, "Yes"])
lines(roc.em.rf, col = "orange")
legend("bottomright", legend=c("Random Forest", "Logistic Regression", "Decision Trees"),
       fill=c("blue", "red", "orange"), cex=0.8)
title("Emergency room")

#adherence

roc.ad.rf <- roc(data_test$Completers, predict(ad_rf, data_test, type = "prob")[, "Completers"])
plot(roc.ad.rf, col = "blue")
roc.ad.rf <- roc(data_test$Completers, predict(ad_lr, data_test, type = "prob")[, "Completers"])
lines(roc.ad.rf, col = "red")
roc.ad.rf <- roc(data_test$Completers, predict(mod_rf, data_test, type = "prob")[, "Completers"])
lines(roc.ad.rf, col = "orange") 

legend("bottomright", legend=c("Random Forest", "Logistic Regression", "Decision Trees"),
       fill=c("blue", "red", "orange"), cex=0.8)
title("Adherence")
