
#===Script used in the manuscript: Rootstock–scion combination contributes to shape diversity and composition of microbial communities associated with grapevine root system
#===Ramona Marasco et al.


#load packages
library(mvabund)
library(randomForest)
library(indicspecies)


#Alphadiversity contribution using Random Forest Approach
rf <- randomForest(
  Observed ~ Fraction + Rootstock + Scion +Combo+Estate+ Field,
  data=tabella,
  ntree=1000, mtry=3, importance=TRUE
)
rf
pred = predict(rf, newdata=tabella)

table(pred)
varImpPlot(rf)
importance(rf)
str(tabella)

#For each compartment
rf_comparto <- randomForest(
  Observed ~   Rootstock + Scion +Combo+Estate+Field,
  data=tabella_comparto,ntree=1000, mtry=3, importance=TRUE
)
rf_bulk
varImpPlot(rf_comparto)
importance(rf_comparto)


rf_comparto <- randomForest(
  Shannon ~   Rootstock + Scion +Combo+Estate+Field,
  data=tabella_comparto,ntree=1000, mtry=3, importance=TRUE
)
rf_bulk
varImpPlot(rf_comparto)
importance(rf_comparto)



#Indicator Species for the combination Rootstock and Scion

indval = multipatt(df, Rootstock:Scion ,func = "r.g",
                   control = how(nperm=999))
summary(indval, indvalcomp=TRUE)



#Compositional analysis using Mvabund packages
#Factor contribution
best.sq.r(comparto_abund~Estate+Field+Rootstock:Scion)

model<-manyglm(comparto_abund~Estate+Field+Rootstock:Scion)
anova_model<-anova(model,p.uni="adjused",show.time="all")


#Retrieval each OTU contribution to the model
a<-data.frame(t(model$uni.test))
b<-data.frame(t(model$uni.p))

head(a)

df<-data.frame((cbind(rownames(a),a[,2],b[,2])))

head(df)
colnames(df)<-c("OTU","Deviance","p")
head(df)
df_ordered<-df[order(df$p),]
head(df_ordered)
write.table(df_ordered, "~/anova_model.txt",sep='\t',row.names = FALSE)


