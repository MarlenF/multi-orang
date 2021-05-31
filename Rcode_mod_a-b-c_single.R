#R script: GLMMs for testing the single modalities and articulators

rm(list=ls())
setwd("C:/Users/Marlen Fröhlich/Documents/R")
int <- read.table ("ESM_2_data.csv", header=TRUE, sep=",", stringsAsFactors=TRUE)
library(arm)
library(car)
test.data=as.data.frame(na.omit(int[, c("modality_vis2","modality_tac","artic_gaze", "modality_aud", "modality_seis", "artic_bod","artic_man", "artic_voc", "artic_fac","Behavior", "age", "sex", "context_fs", "context_pl", "species", "kinship", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))


test.data$age.dep=as.numeric(test.data$age==levels(test.data$age)[2])
test.data$age.imm=as.numeric(test.data$age==levels(test.data$age)[3])
test.data$sex.code=as.numeric(test.data$sex==levels(test.data$sex)[2])
test.data$kinship.mk=as.numeric(test.data$kinship==levels(test.data$kinship)[1])
test.data$kinship.mo=as.numeric(test.data$kinship==levels(test.data$kinship)[2])


contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

# collinearity: max vif = 2.5
vif(lm(rnorm(nrow(test.data)) ~  species + setting +  age.dep + age.imm + sex.code + context_pl + context_fs + kinship, data = test.data))

# modality visual
mod.vis = glmer(formula = modality_vis2 ~  setting * species +  age.dep + age.imm + sex.code +   kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

red.vis = glmer(formula = modality_vis2 ~  setting + species +  age.dep + age.imm + sex.code  + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

null.vis = glmer(formula = modality_vis2 ~   age.dep + age.imm + sex.code  + context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.vis)) #7587
length(residuals(null.vis)) #7587
as.data.frame(anova(null.vis, mod.vis, test="Chisq"))

round(summary(mod.vis)$coefficients, 3)
round(summary(red.vis)$coefficients, 3)
drop1(red.vis,  test ="Chisq")

# modality tactile
mod.tac = glmer(formula = modality_tac ~  setting * species +  age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

null.tac = glmer(formula = modality_tac ~   age.dep + age.imm + sex.code + context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.tac)) #7587
length(residuals(null.tac)) #7587
as.data.frame(anova(null.tac, mod.tac, test="Chisq"))

round(summary(mod.tac)$coefficients, 3)
drop1(mod.tac,  test ="Chisq")


# modality auditory
mod.aud = glmer(formula = modality_aud ~ setting * species +  age.dep + age.imm + sex.code +   kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)


null.aud = glmer(formula = modality_aud ~  age.dep + age.imm  + sex.code +  context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.aud)) #7587
length(residuals(null.aud)) #7587
as.data.frame(anova(null.aud, mod.aud, test="Chisq"))

# modality seismic
mod.seis = glmer(formula = modality_seis ~  setting * species + age.dep + age.imm + sex.code +   kinship.mo + kinship.mk + context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

null.seis = glmer(formula = modality_seis ~   age.dep + age.imm + sex.code + context_fs + context_pl +
                    (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                    (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(null.seis)) #7587
as.data.frame(anova(null.seis, mod.seis, test="Chisq"))

# articulator manual
mod.man = glmer(formula = artic_man ~  setting * species +  age.dep + age.imm + sex.code +   kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

red.man = glmer(formula = artic_man ~  setting + species +  age.dep + age.imm + sex.code +kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

null.man = glmer(formula = artic_man ~   age.dep + age.imm + sex.code +    context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(null.man)) #7587
length(residuals(man.man)) #7587

drop1(mod.man,  test ="Chisq")
drop1(red.man,  test ="Chisq")
round(summary(red.man)$coefficients, 3)

# articulator bodily
mod.bod = glmer(formula = artic_bod ~  setting * species +  age.dep + age.imm + sex.code + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

null.bod = glmer(formula = artic_bod ~   age.dep + age.imm + sex.code + context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)
length(residuals(mod.bod)) #7587
length(residuals(null.bod)) #7587
as.data.frame(anova(null.bod, mod.bod, test="Chisq"))

drop1(mod.bod,  test ="Chisq")
round(summary(mod.bod)$coefficients, 3)

# articulator gaze
mod.gaz = glmer(formula = artic_gaze ~  setting * species +  age.dep +age.imm + sex.code + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

null.gaz = glmer(formula = artic_gaze ~  age.dep + age.imm + sex.code  + context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)


length(residuals(mod.gaz)) #7587
length(residuals(null.gaz)) #7587
as.data.frame(anova(null.gaz, mod.gaz, test="Chisq"))


round(summary(mod.gaz)$coefficients, 3)
drop1(mod.gaz,  test ="Chisq")

# articulator vocal
mod.voc = glmer(formula = artic_voc ~  setting * species +  age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

red.voc = glmer(formula = artic_voc ~  setting + species +  age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

null.voc = glmer(formula = artic_voc ~    age.dep + age.imm  + sex.code +  context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(null.voc)) #8824 model did not converge
as.data.frame(anova(null.voc, mod.voc, test="Chisq"))

drop1(mod.voc,  test ="Chisq")
drop1(red.voc,  test ="Chisq")
round(summary(red.voc)$coefficients, 3)

# articulator facial
mod.fac = glmer(formula = artic_fac ~  setting * species +  age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)


null.fac = glmer(formula = artic_fac ~   age.dep + age.imm + sex.code +  context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.fac)) #7587
length(residuals(null.fac)) #7587 model did not converge
as.data.frame(anova(null.fac, mod.fac, test="Chisq"))


