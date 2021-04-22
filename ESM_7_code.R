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

mm.vis.aso=as.data.frame(na.omit(test.data[, c("effect", "vis_mm", "ms_uc", "Behavior", "age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mm.tac.aso=as.data.frame(na.omit(test.data[, c("effect", "tac_mm", "ms_uc", "Behavior", "age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))

mm.vis.dom=as.data.frame(na.omit(test.data[, c("match_tight", "vis_mm", "ms_uc", "Behavior", "age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mm.tac.dom=as.data.frame(na.omit(test.data[, c("match_tight", "tac_mm", "ms_uc", "Behavior", "age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))


mc.bod.aso=as.data.frame(na.omit(test.data[, c("effect", "bodily_mp", "mc_us", "Behavior","age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mc.gaz.aso=as.data.frame(na.omit(test.data[, c("effect", "gaze_mp", "mc_us", "Behavior","age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mc.man.aso=as.data.frame(na.omit(test.data[, c("effect", "manual_mp", "mc_us", "Behavior","age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))

mc.bod.dom=as.data.frame(na.omit(test.data[, c("match_tight", "bodily_mp", "mc_us", "Behavior","age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mc.gaz.dom=as.data.frame(na.omit(test.data[, c("match_tight", "gaze_mp", "mc_us", "Behavior","age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mc.man.dom=as.data.frame(na.omit(test.data[, c("match_tight", "manual_mp", "mc_us", "Behavior","age.dep","age.imm", "kinship.mo","kinship.mk", "sex.code", "context_fs", "context_pl", "species",  "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))


contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000000))
contr2=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000000))
# collinearity: max vif = 2.6 (effect tac_mm)
vif(lm(rnorm(nrow(test.data)) ~  vis_mm+ species * setting +  age.dep + age.imm + sex.code + context_pl + agediff + kinship, data = test.data))

#########################################################
#multicomponent EFFECT Gaze

mod.suc.gaz = glmer(formula = effect ~ gaze_mp +setting + species  + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                       (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                      (0+gaze_mp|ID_sign)  +
                      (1|Dyad)+ (1|ID_obs), family = binomial, data = mc.gaz.aso,  control=contr)

null.suc.gaz = glmer(formula = effect ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                       (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                       (0+gaze_mp|ID_sign)  +
                       (1|Dyad)+ (1|ID_obs), family = binomial, data = mc.gaz.aso,  control=contr)

length(residuals(mod.suc.gaz)) #manual: 3037, bodily: 1498, gaze2: 3083, gaze1: 4513, manual: 3037, bodily: 1498, vocal: 219
length(residuals(null.suc.gaz)) # 4513
as.data.frame(anova(null.suc.gaz, mod.suc.gaz, test="Chisq"))


drop1(mod.suc.gaz,  test ="Chisq")
round(summary(mod.suc.gaz)$coefficients, 3)


#multicomponent EFFECT Bodily

mod.suc.bod = glmer(formula = effect ~ bodily_mp +setting + species  + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                      (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                      (1|Dyad)+ (1|ID_obs), family = binomial, data = mc.bod.aso,  control=contr)

null.suc.bod = glmer(formula = effect ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                        (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                        +
                        (1|Dyad)+ (1|ID_obs), family = binomial, data = mc.bod.aso,  control=contr)

length(residuals(mod.suc.bod)) # 1498,
length(residuals(null.suc.bod)) # 1498
as.data.frame(anova(null.suc.bod, mod.suc.bod, test="Chisq"))

#multicomponent EFFECT Manual

mod.suc.man = glmer(formula = effect ~ manual_mp +setting + species  + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                       (0+context_pl|Dyad)+
                      (0+manual_mp|ID_sign)  +
                      (1|Dyad)+ (1|ID_obs), family = binomial, data = mc.man.aso,  control=contr)

null.suc.man = glmer(formula = effect ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                       (0+context_pl|Dyad)+
                       (0+manual_mp|ID_sign)  +
                       (1|Dyad)+ (1|ID_obs), family = binomial, data = mc.man.aso,  control=contr)

length(residuals(mod.suc.man)) #manual: 3037, bodily: 1498, gaze2: 3083, gaze1: 4513, manual: 3037, bodily: 1498, vocal: 219
length(residuals(null.suc.man)) # 4513
as.data.frame(anova(null.suc.man, mod.suc.man, test="Chisq"))


#########################################################
#multicomp MATCH bodily
mod.mat.bod = glmer(formula = match_tight~ bodily_mp +setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                     (0 + bodily_mp | ID_sign) + (1 | ID_sign)  + (1|Behavior), family = binomial, data = mc.bod.dom,  control=contr)

null.mat.bod = glmer(formula = match_tight ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                       (0 + bodily_mp | ID_sign) + (1 | ID_sign)  + (1|Behavior), family = binomial, data = mc.bod.dom,  control=contr)


length(residuals(mod.mat.bod)) # bodily: 1429
length(residuals(null.mat.bod)) 
as.data.frame(anova(null.mat.bod, mod.mat.bod, test="Chisq"))


#multicomp MATCH gaze
mod.mat.gaz = glmer(formula = match_tight~ gaze_mp +setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                      (0+gaze_mp|ID_sign)  +
                      (1|ID_sign) + (1|Behavior), family = binomial, data = mc.gaz.dom,  control=contr)

null.mat.gaz = glmer(formula = match_tight ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                       (0+gaze_mp|ID_sign)  +
                       (1|ID_sign)+ (1|Behavior), family = binomial, data = mc.gaz.dom,  control=contr)


length(residuals(mod.mat.gaz)) #  gaze: 3869, 
length(residuals(null.mat.gaz)) 
as.data.frame(anova(null.mat.gaz, mod.mat.gaz, test="Chisq"))


drop1(mod.mat.gaz,  test ="Chisq")

round(summary(mod.mat.gaz)$coefficients, 3)


#multicomp MATCH manual
mod.mat.man = glmer(formula = match_tight~ manual_mp +setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                      (0+manual_mp|ID_sign)  +
                      (1|ID_sign)+ (1|Behavior), family = binomial, data = mc.man.dom,  control=contr)

null.mat.man = glmer(formula = match_tight ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +   context_fs + context_pl +
                       (0+manual_mp|ID_sign)  +
                       (1|ID_sign)+ (1|Behavior), family = binomial, data = mc.man.dom,  control=contr)

length(residuals(mod.mat.man)) # manual: 2590
length(residuals(null.mat.man)) 

as.data.frame(anova(null.mat.man, mod.mat.man, test="Chisq"))


#########################################################

#multisensory effect - visual
mod.suc.vis = glmer(formula = effect ~vis_mm +setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                    (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                     (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ 
                     (0+vis_mm|ID_sign)  +
                     (1|ID_rec) + (1|ID_coder) + (1|ID_obs), family = binomial, data = mm.vis.aso,  control=contr)

null.suc.vis = glmer(formula = effect ~ setting + species +age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                       (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                       (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ 
                       (0+vis_mm|ID_sign)  +
                       (1|ID_rec) + (1|ID_coder) + (1|ID_obs), family = binomial, data = mm.vis.aso,  control=contr)

length(residuals(mod.suc.vis)) # 2301,
length(residuals(null.suc.vis)) # 2301
as.data.frame(anova(null.suc.vis, mod.suc.vis, test="Chisq"))


round(summary(mod.suc.vis)$coefficients, 3)
drop1(mod.suc.vis,  test ="Chisq")

#multisensory effect tactile
mod.suc.tac = glmer(formula = effect ~tac_mm +setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                      (0+context_pl|ID_sign) + 
                      (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ 
                      (0+tac_mm|ID_sign)  +
                      (1|ID_rec) + (1|ID_coder) + (1|ID_obs), family = binomial, data = mm.tac.aso,  control=contr)

null.suc.tac = glmer(formula = effect ~ setting + species +age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                       (0+context_pl|ID_sign) + 
                       (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ 
                       (0+tac_mm|ID_sign)  +
                       (1|ID_rec) + (1|ID_coder) + (1|ID_obs), family = binomial, data = mm.tac.aso,  control=contr)

length(residuals(mod.suc.tac)) #3743
length(residuals(null.suc.tac)) #3743
as.data.frame(anova(null.suc.tac, mod.suc.tac, test="Chisq"))


round(summary(mod.suc.tac)$coefficients, 3)
drop1(mod.suc.tac,  test ="Chisq")


##########################################################
#multisensory mtach visual
mod.mat.vis = glmer(formula = match_tight ~vis_mm + setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                    (0+context_pl|ID_sign) + 
                    #(0+kinship.mk|ID_rec) +
                    (0+vis_mm|ID_sign)  +  
                    (1|Behavior), family = binomial, data = mm.vis.dom,  control=contr)

null.mat.vis = glmer(formula = match_tight ~ setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                      (0+context_pl|ID_sign) + 
                      #(0+kinship.mk|ID_rec) +
                      (0+vis_mm|ID_sign)  +  
                      (1|Behavior), family = binomial, data = mm.vis.dom,  control=contr)
                     
length(residuals(mod.mat.vis)) # 1674
length(residuals(null.mat.vis)) # 1674
as.data.frame(anova(null.mat.vis, mod.mat.vis, test="Chisq"))


#multisensory match - tactile
mod.mat.tac = glmer(formula = match_tight ~tac_mm + setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk +  context_fs + context_pl +
                      (0+context_pl|ID_sign) + 
                      (0+kinship.mk|ID_rec) +
                      (0+tac_mm|ID_sign)  +  
                      (1|Behavior), family = binomial, data = mm.tac.dom,  control=contr)

null.mat.tac = glmer(formula = match_tight ~ setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + context_fs + context_pl +
                       (0+context_pl|ID_sign) + 
                       (0+kinship.mk|ID_rec) +
                       (0+tac_mm|ID_sign)  +  
                       (1|Behavior), family = binomial, data = mm.tac.dom,  control=contr)

length(residuals(mod.mat.tac)) # tactile: 3129
length(residuals(null.mat.tac)) #
as.data.frame(anova(null.mat.tac, mod.mat.tac, test="Chisq"))

