

df<-read.csv(file.choose())

require(ggplot2)
require(glmmTMB)
require(visreg)
require(car)
require(DHARMa)


dfn<-subset(df, sex.species!='latipinnaNA')

b<-ggplot(dfn, aes(sex.species,totalpara,  fill=sex))+
  geom_boxplot()+
  facet_wrap(~collection.site)+
  theme_classic()
print(b)

b<-ggplot(dfn, aes(sex.species,totalpara,  fill=sex))+
  geom_boxplot()+
  theme_classic()
print(b)


m<-glmmTMB(log(totalpara+1)~sex.species*collection.site, data=dfn)
summary(m)
Anova(m)

visreg(m, partial=T, scale='response')
visreg(m, 'sex.species', by='collection.site', partial=T, scale='response')

#Model fit
sim_residuals_glmmTMB <- simulateResiduals(m, 1000)  
plot(sim_residuals_glmmTMB) 
DHARMa::testDispersion(sim_residuals_glmmTMB)
