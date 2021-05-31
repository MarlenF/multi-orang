rm(list=ls())
setwd("C:/Users/Marlen Fröhlich/Documents/R")
library(lme4)
library(car)

test.data <- read.table ("orangutanR_Apr2021_rev2.csv", header=TRUE, sep=",", stringsAsFactors=TRUE)

test.data$age.dep=as.numeric(test.data$age==levels(test.data$age)[2])
test.data$age.imm=as.numeric(test.data$age==levels(test.data$age)[3])
test.data$sex.code=as.numeric(test.data$sex==levels(test.data$sex)[2])
test.data$kinship.mk=as.numeric(test.data$kinship==levels(test.data$kinship)[1])
test.data$kinship.mo=as.numeric(test.data$kinship==levels(test.data$kinship)[2])

ms.uc=as.data.frame(na.omit(test.data[, c("ms_uc","modality_vis2","modality_tac","modality_aud", "modality_seis", "artic_bod","artic_man", "artic_voc", "artic_fac","Behavior", "context_fs", "context_pl", "species","age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mc.us=as.data.frame(na.omit(test.data[, c("mc_us","modality_vis2","modality_tac","modality_aud", "modality_seis", "artic_bod","artic_man", "artic_voc", "artic_fac","Behavior",  "context_fs", "context_pl", "species", "age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
MS.mc=as.data.frame(na.omit(test.data[, c("MS_mc","modality_vis2","modality_tac","modality_aud", "modality_seis", "artic_bod","artic_man", "artic_voc", "artic_fac","Behavior",  "context_fs", "context_pl", "species", "age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
MC.ms=as.data.frame(na.omit(test.data[, c("MC_ms","modality_vis2","modality_tac","modality_aud", "modality_seis", "artic_bod","artic_man", "artic_voc", "artic_fac","Behavior",  "context_fs", "context_pl", "species", "age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))


contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

# collinearity: max vif = 2.5
vif(lm(rnorm(nrow(test.data)) ~  species + setting +  age.dep + age.imm + sex.code + context_pl + context_fs + agediff + kinship, data = test.data))


# multisensory - unicomp
mod.ms_uc = glmer(formula = ms_uc ~  setting * species +  age.dep + age.imm + sex.code  + kinship.mo + kinship.mk + context_fs + context_pl +
                   (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (1|Dyad) +(1|ID_obs), family = binomial, data = ms.uc,  control=contr)

null.ms_uc = glmer(formula = ms_uc ~  age.dep + age.imm + sex.code  + context_fs + context_pl +
                     (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                     (1|Dyad) +(1|ID_obs), family = binomial, data = ms.uc,  control=contr)

length(residuals(mod.ms_uc)) #4954
length(residuals(null.ms_uc)) #4954
as.data.frame(anova(null.ms_uc, mod.ms_uc, test="Chisq"))

round(summary(mod.ms_uc)$coefficients, 3)
drop1(mod.ms_uc,  test ="Chisq")

require(lsmeans)
lsm <- lsmeans(mod.ms_uc, ~ setting * species, adjust ="sidak")
lsm1 <- lsmeans(mod.ms_uc, list(pairwise ~ setting|species, pairwise ~ species|setting))
lsm1


## multicomp -  unisensory
mod.mc_us = glmer(formula = mc_us ~  setting * species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                 (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (1|Dyad) +(1|ID_obs), family = binomial, data = mc.us,  control=contr)

null.mc_us = glmer(formula = mc_us ~  age.dep + age.imm + sex.code +   context_fs + context_pl +
                     (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                     (1|Dyad) +(1|ID_obs), family = binomial, data = mc.us,  control=contr)

length(residuals(mod.mc_us)) #5239
length(residuals(null.mc_us)) #5239
as.data.frame(anova(null.mc_us, mod.mc_us, test="Chisq"))

round(summary(mod.mc_us)$coefficients, 3)
drop1(mod.mc_us,  test ="Chisq")

lsm <- lsmeans(mod.mc_us, ~ setting * species, adjust ="sidak")
lsm1 <- lsmeans(mod.mc_us, list(pairwise ~ setting|species, pairwise ~ species|setting))
lsm1  

# multicomp multisensory (MC acts only)
mod.MC_ms = glmer(formula = MC_ms ~  setting * species + age.dep + age.imm + sex.code +    kinship.mo + kinship.mk + context_fs + context_pl +
                    (0+context_fs|Dyad)+ 
                    (1|Dyad) +(1|ID_obs), family = binomial, data = MC.ms,  control=contr)

null.MC_ms = glmer(formula = MC_ms ~  age.dep + age.imm + sex.code +  context_fs + context_pl +
                     (0+context_fs|Dyad)+ 
                     (1|Dyad) +(1|ID_obs), family = binomial, data = MC.ms,  control=contr)
length(residuals(mod.MC_ms)) #2633
length(residuals(null.MC_ms)) #2633
as.data.frame(anova(null.MC_ms, mod.MC_ms, test="Chisq"))


round(summary(mod.MC_ms)$coefficients, 3)
drop1(mod.MC_ms,  test ="Chisq")


lsm <- lsmeans(mod.MC_ms, ~ setting * species, adjust ="sidak")
lsm1 <- lsmeans(mod.MC_ms, list(pairwise ~ setting|species, pairwise ~ species|setting))
lsm1  

 
# multicomp multisensory (MS acts only)
mod.MS_mc = glmer(formula = MS_mc ~  setting * species + age.dep + age.imm + sex.code +   kinship.mo + kinship.mk + context_fs + context_pl +
                    (0+context_fs|Dyad)+ 
                    (1|Dyad) +(1|ID_obs), family = binomial, data = MS.mc,  control=contr)

null.MS_mc = glmer(formula = MS_mc ~  age.dep + age.imm + sex.code +  context_fs + context_pl +
                     (0+context_fs|Dyad)+ 
                     (1|Dyad) +(1|ID_obs), family = binomial, data = MS.mc,  control=contr)

length(residuals(mod.MS_mc)) #2348
length(residuals(null.MS_mc)) #2348
as.data.frame(anova(null.MS_mc, mod.MS_mc, test="Chisq"))

round(summary(mod.MS_mc)$coefficients, 3)
drop1(mod.MS_mc,  test ="Chisq")


lsm <- lsmeans(mod.MS_mc, ~ setting * species, adjust ="sidak")
lsm1 <- lsmeans(mod.MS_mc, list(pairwise ~ setting|species, pairwise ~ species|setting))
lsm1


