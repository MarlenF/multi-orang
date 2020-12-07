rm(list=ls())
setwd("C:/Users/Marlen Fröhlich/Documents/R")
int <- read.table ("orangutanR_Sep2020_rev.csv", header=TRUE, sep=",")
library(arm)
library(car)
mm=as.data.frame(na.omit(int[, c("modality_vis","modality_tac","modality_aud", "modality_seis","multimod", "artic_bod","artic_man", "artic_voc", "artic_fac","Behavior", "multiplex", "age", "sex", "context_fs", "context_pl", "species", "kinship","agediff", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))

test.data=mm
test.data$age.dep=as.numeric(test.data$age==levels(test.data$age)[2])
test.data$age.imm=as.numeric(test.data$age==levels(test.data$age)[3])
test.data$sex.code=as.numeric(test.data$sex==levels(test.data$sex)[2])
test.data$kinship.mk=as.numeric(test.data$kinship==levels(test.data$kinship)[1])
test.data$kinship.mo=as.numeric(test.data$kinship==levels(test.data$kinship)[2])
test.data$agediff.ol=as.numeric(test.data$agediff==levels(test.data$agediff)[1])
test.data$agediff.yo=as.numeric(test.data$agediff==levels(test.data$agediff)[3])
"2"


contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

# collinearity: max vif = 2.5
vif(lm(rnorm(nrow(test.data)) ~  species * setting +  age.dep + age.imm + sex.code + context_pl + agediff + kinship, data = test.data))

# modality visual
mod.vis = glmer(formula = modality_vis ~  setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.vis = glmer(formula = modality_vis ~   age.dep + age.imm + sex.code  + context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)
length(residuals(mod.vis)) #7587
length(residuals(null.vis)) #7587
as.data.frame(anova(null.vis, mod.vis, test="Chisq"))
#Df      AIC      BIC    logLik deviance    Chisq Chi Df   Pr(>Chisq)
#null.vis 28 6342.486 6536.643 -3143.243 6286.486       NA     NA           NA
#mod.vis  35 6302.401 6545.097 -3116.200 6232.401 54.08544      7 2.262128e-09

round(summary(mod.vis)$coefficients, 3)
#(Intercept)               0.129      0.360   0.358    0.720
#settingwild              -2.290      0.318  -7.195    0.000
#speciesSum               -0.542      0.274  -1.980    0.048
#age.dep                  -0.628      0.307  -2.043    0.041
#age.imm                  -0.558      0.301  -1.856    0.063
#sex.code                 -0.338      0.159  -2.129    0.033
#agediff.yo               -0.417      0.285  -1.460    0.144
#agediff.ol                0.030      0.203   0.146    0.884
#kinship.mo               -0.953      0.188  -5.078    0.000
#kinship.mk               -0.175      0.194  -0.899    0.368
#context_fs                0.877      0.181   4.841    0.000
#context_pl                0.011      0.142   0.079    0.937
#settingwild:speciesSum    2.594      0.390   6.647    0.000

drop1(mod.vis,  test ="Chisq")
#<none>             6302.4                      
#age.dep          1 6304.6  4.2280   0.03976 *  
#age.imm          1 6303.9  3.5159   0.06078 .  
#sex.code         1 6304.5  4.0891   0.04316 *  
#agediff.yo       1 6302.5  2.1043   0.14689    
#agediff.ol       1 6300.4  0.0214   0.88380    
#kinship.mo       1 6322.9 22.5076 2.093e-06 ***
#kinship.mk       1 6301.2  0.8147   0.36675    
#context_fs       1 6319.3 18.8978 1.379e-05 ***
#context_pl       1 6300.4  0.0062   0.93712    
#setting:species  1 6324.3 23.9377 9.950e-07 ***


# modality tactile
mod.tac = glmer(formula = modality_tac ~  setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.tac = glmer(formula = modality_tac ~   age.dep + age.imm + sex.code + context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.tac)) #7587
length(residuals(null.tac)) #7587
as.data.frame(anova(null.tac, mod.tac, test="Chisq"))
#Df      AIC      BIC    logLik deviance    Chisq Chi Df   Pr(>Chisq)
#null.tac 28 6655.279 6849.436 -3299.639 6599.279       NA     NA           NA
#mod.tac  35 6618.631 6861.328 -3274.316 6548.631 50.64716      7 1.077913e-08

round(summary(mod.tac)$coefficients, 3)
#(Intercept)              -0.208      0.410  -0.507    0.612
#settingwild               2.235      0.358   6.249    0.000
#speciesSum                0.240      0.330   0.728    0.467
#age.dep                   0.528      0.343   1.541    0.123
#age.imm                   0.675      0.327   2.063    0.039
#sex.code                  0.466      0.185   2.519    0.012
#agediff.yo                0.610      0.318   1.916    0.055
#agediff.ol               -0.034      0.224  -0.153    0.878
#kinship.mo                0.896      0.200   4.483    0.000
#kinship.mk                0.248      0.221   1.124    0.261
#context_fs               -0.641      0.187  -3.435    0.001
#context_pl                0.072      0.151   0.475    0.635
#settingwild:speciesSum   -2.735      0.437  -6.265    0.000

drop1(mod.tac,  test ="Chisq")
#age.dep          1 6619.0  2.3634  0.124209    
#age.imm          1 6621.0  4.3595  0.036804 *  
#sex.code         1 6623.1  6.4524  0.011080 *  
#agediff.yo       1 6620.3  3.6327  0.056655 .  
#agediff.ol       1 6616.7  0.0234  0.878338    
#kinship.mo       1 6634.8 18.1884 2.001e-05 ***
#kinship.mk       1 6617.9  1.2612  0.261427    
#context_fs       1 6627.1 10.4806  0.001206 ** 
#context_pl       1 6616.9  0.2244  0.635732    
#setting:species  1 6637.9 21.2249 4.084e-06 ***#


# modality auditory
mod.aud = glmer(formula = modality_aud ~ setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.aud = glmer(formula = modality_aud ~  age.dep + age.imm  + sex.code +  context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.aud)) #7587
length(residuals(null.aud)) #7587
as.data.frame(anova(null.aud, mod.aud, test="Chisq"))
#Df      AIC      BIC    logLik deviance    Chisq Chi Df Pr(>Chisq)
#null.aud 12 1003.972 1088.995 -489.9860 979.9721       NA     NA         NA
#mod.aud  19 1009.876 1144.496 -485.9382 971.8764 8.095645      7  0.3242357

# modality seismic
mod.seis = glmer(formula = modality_seis ~  setting * species + age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.seis = glmer(formula = modality_seis ~   age.dep + age.imm + sex.code + context_fs + context_pl +
                    (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                    (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                    (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                    (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                    (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(null.seis)) #7587
#as.data.frame(anova(null.seis, mod.seis, test="Chisq"))
#Df      AIC      BIC    logLik deviance    Chisq Chi Df Pr(>Chisq)
#null.seis 28 547.5422 741.6996 -245.7711 491.5422       NA     NA         NA
#mod.seis  35 552.0391 794.7358 -241.0195 482.0391 9.503123      7   0.218522


# multimodal
mod.mm = glmer(formula = multimod ~  setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                 (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                 (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                 (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                 (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                 (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.mm = glmer(formula = multimod ~   age.dep + age.imm + sex.code +   context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.mm)) #7587
length(residuals(null.mm)) #7587
as.data.frame(anova(null.mm, mod.mm, test="Chisq"))
#Df      AIC      BIC    logLik deviance    Chisq Chi Df   Pr(>Chisq)
#null.mm 28 8194.593 8388.750 -4069.296 8138.593       NA     NA           NA
#mod.mm  35 8162.808 8405.505 -4046.404 8092.808 45.78471      7 9.627011e-08

round(summary(mod.mm)$coefficients, 3)
#(Intercept)              -4.168      0.367 -11.342    0.000
#settingwild               2.864      0.299   9.592    0.000
#speciesSum                1.732      0.298   5.817    0.000
#age.dep                   0.208      0.231   0.901    0.368
##age.imm                   0.105      0.218   0.482    0.630
#sex.code                  0.057      0.124   0.460    0.646
#agediff.yo                0.196      0.211   0.930    0.352
#agediff.ol                0.351      0.180   1.948    0.051
#kinship.mo               -0.042      0.173  -0.243    0.808
#kinship.mk               -0.427      0.162  -2.642    0.008
#context_fs                1.200      0.161   7.431    0.000
#context_pl                0.248      0.106   2.332    0.020
#settingwild:speciesSum   -2.032      0.347  -5.854    0.000

drop1(mod.mm,  test ="Chisq")


# articulator manual
mod.man = glmer(formula = artic_man ~  setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + 
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

red.man = glmer(formula = artic_man ~  setting + species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + 
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.man = glmer(formula = artic_man ~   age.dep + age.imm + sex.code +    context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) +
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(null.man)) #7587
as.data.frame(anova(null.man, mod.man, test="Chisq"))
#null.man 26 8668.648 8848.937 -4308.324 8616.648       NA     NA           NA
#mod.man  33 8648.411 8877.239 -4291.205 8582.411 34.23769      7 1.554703e-05

drop1(mod.man,  test ="Chisq")
#(Intercept)    0.340      0.336   1.013    0.311
#settingwild    0.373      0.229   1.629    0.103
#speciesSum    -0.245      0.226  -1.087    0.277
#age.dep        0.490      0.300   1.637    0.102
#age.imm        0.322      0.262   1.228    0.219
#sex.code       0.230      0.177   1.296    0.195
#agediff.yo     0.605      0.247   2.448    0.014
#agediff.ol    -0.422      0.247  -1.704    0.088
#kinship.mo     0.798      0.210   3.800    0.000
#kinship.mk     0.488      0.218   2.235    0.025
#context_fs    -0.945      0.171  -5.542    0.000
#context_pl    -0.393      0.150  -2.614    0.009

drop1(red.man,  test ="Chisq")
#setting     1 8648.9  2.5875 0.1077085    
#species     1 8647.5  1.1731 0.2787738    
#age.dep     1 8648.9  2.6185 0.1056247    
#age.imm     1 8647.7  1.4492 0.2286573    
#sex.code    1 8648.0  1.6862 0.1940982    
#agediff.yo  1 8652.1  5.8336 0.0157227 *  
#  agediff.ol  1 8649.0  2.7582 0.0967582 .  
#kinship.mo  1 8660.2 13.9086 0.0001919 ***
#  kinship.mk  1 8651.0  4.7491 0.0293136 *  
#  context_fs  1 8668.1 21.8219 2.992e-06 ***
#  context_pl  1 8652.8  6.5443 0.0105222 *   

round(summary(red.man)$coefficients, 3)
#Intercept)              -0.377      0.380  -0.991    0.322
#settingwild              -0.089      0.351  -0.254    0.800
#speciesSum               -0.348      0.319  -1.090    0.276
#age.dep                   0.200      0.296   0.676    0.499
#age.imm                   0.088      0.288   0.306    0.759
#sex.code                  0.324      0.172   1.888    0.059
#agediff.yo                0.264      0.286   0.920    0.357
#agediff.ol               -0.243      0.221  -1.100    0.271
#kinship.mo                0.621      0.198   3.133    0.002
#kinship.mk                0.490      0.185   2.652    0.008
#context_fs               -0.181      0.144  -1.260    0.208
#context_pl                0.285      0.126   2.262    0.024
#settingwild:speciesSum    0.132      0.475   0.277    0.781


# articulator bodily
mod.bod = glmer(formula = artic_bod ~  setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.bod = glmer(formula = artic_bod ~   age.dep + age.imm + sex.code + context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)
length(residuals(mod.bod)) #7587
length(residuals(null.bod)) #7587
as.data.frame(anova(null.bod, mod.bod, test="Chisq"))
null.bod 28 7564.531 7758.688 -3754.265 7508.531       NA     NA           NA
mod.bod  35 7549.965 7792.662 -3739.983 7479.965 28.56524      7 0.0001736205

drop1(mod.bod,  test ="Chisq")
#age.dep          1 7549.9  1.979   0.15948    
#age.imm          1 7549.4  1.427   0.23223    
#sex.code         1 7548.1  0.151   0.69778    
#agediff.yo       1 7550.2  2.275   0.13146    
#agediff.ol       1 7550.1  2.179   0.13988    
#kinship.mo       1 7548.4  0.401   0.52646    
#kinship.mk       1 7548.1  0.175   0.67591    
#context_fs       1 7583.6 35.675 2.331e-09 ***
#context_pl       1 7577.0 29.008 7.210e-08 ***
#setting:species  1 7552.8  4.796   0.02852 * 

round(summary(mod.bod)$coefficients, 3)
#(Intercept)              -1.817      0.370  -4.918    0.000
#settingwild              -0.723      0.288  -2.511    0.012
#speciesSum                0.680      0.289   2.350    0.019
#age.dep                  -0.439      0.316  -1.389    0.165
#age.imm                  -0.338      0.289  -1.170    0.242
#sex.code                  0.071      0.180   0.396    0.692
#agediff.yo               -0.412      0.278  -1.484    0.138
#agediff.ol                0.326      0.220   1.485    0.137
#kinship.mo               -0.116      0.181  -0.643    0.520
#kinship.mk                0.085      0.204   0.418    0.676
#context_fs                1.485      0.185   8.035    0.000
#context_pl                0.816      0.146   5.583    0.000
#settingwild:speciesSum   -0.845      0.373  -2.264    0.024


# articulator vocal
mod.voc = glmer(formula = artic_voc ~  setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.voc = glmer(formula = artic_voc ~    age.dep + age.imm  + sex.code +  context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(null.voc)) # model did not converge
as.data.frame(anova(null.voc, mod.voc, test="Chisq"))


# articulator facial
mod.fac = glmer(formula = artic_fac ~  setting * species +  age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.fac = glmer(formula = artic_fac ~   age.dep + age.imm + sex.code +  context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.fac)) #7587
length(residuals(null.fac)) #7587 model did not converge

# multiplex
mod.mp = glmer(formula = multiplex ~  setting * species + age.dep + age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                 (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                 (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                 (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                 (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                 (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.mp = glmer(formula = multiplex ~  age.dep + age.imm + sex.code +  context_fs + context_pl +
                  (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                  (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                  (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                  (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                  (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)
length(residuals(mod.mp)) #7587
length(residuals(null.mp)) #7587
as.data.frame(anova(null.mp, mod.mp, test="Chisq"))
#Df      AIC      BIC    logLik deviance    Chisq Chi Df   Pr(>Chisq)
#null.mp 28 8514.356 8708.514 -4229.178 8458.356       NA     NA           NA
#mod.mp  35 8475.526 8718.223 -4202.763 8405.526 52.83034      7 4.004369e-09

round(summary(mod.mp)$coefficients, 3)
#(Intercept)              -2.377      0.341  -6.968    0.000
#settingwild               2.224      0.293   7.591    0.000
#speciesSum                2.196      0.285   7.717    0.000
#age.dep                  -0.459      0.277  -1.657    0.098
#age.imm                  -0.007      0.232  -0.030    0.976
#sex.code                 -0.117      0.169  -0.692    0.489
#agediff.yo               -0.105      0.217  -0.485    0.628
#agediff.ol               -0.084      0.204  -0.411    0.681
#kinship.mo               -0.860      0.175  -4.912    0.000
#kinship.mk               -0.160      0.179  -0.892    0.372
#context_fs               -0.540      0.166  -3.250    0.001
##context_pl                0.186      0.129   1.438    0.150
#settingwild:speciesSum   -1.521      0.336  -4.521    0.000

drop1(mod.mp,  test ="Chisq")
#age.dep          1 8476.3  2.7959 0.0945061 .  
#age.imm          1 8473.5  0.0009 0.9764161    
#sex.code         1 8474.0  0.4639 0.4957979    
#agediff.yo       1 8473.8  0.2346 0.6281485    
#agediff.ol       1 8473.7  0.1684 0.6815362    
#kinship.mo       1 8495.2 21.7162 3.161e-06 ***
#kinship.mk       1 8474.3  0.7274 0.3937328    
#context_fs       1 8484.2 10.6985 0.0010723 ** 
#context_pl       1 8475.5  1.9277 0.1650154    
#setting:species  1 8486.3 12.7747 0.0003513 ***

# articulator gaze
mod.gaz = glmer(formula = gaze ~  setting * species +  age.dep +age.imm + sex.code +   agediff.yo + agediff.ol + kinship.mo + kinship.mk + context_fs + context_pl +
                    (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                    (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                    (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                    (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                    (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.gaz = glmer(formula = gaze ~  age.dep + age.imm + sex.code  + context_fs + context_pl +
                   (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                   (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                   (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                   (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                   (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)


length(residuals(mod.gaz)) #7096
length(residuals(null.gaz)) #7096
as.data.frame(anova(null.gaz, mod.gaz, test="Chisq"))
#Df      AIC      BIC    logLik deviance    Chisq Chi Df   Pr(>Chisq)
#null.gaz 28 6717.850 6910.134 -3330.925 6661.850       NA     NA           NA
#mod.gaz  35 6698.764 6939.119 -3314.382 6628.764 33.08653      7 2.551159e-05

round(summary(mod.gaz)$coefficients, 3)
#(Intercept)              -1.902      0.515  -3.695    0.000
#settingwild               3.586      0.539   6.652    0.000
#speciesSum                2.735      0.478   5.728    0.000
#age.dep                  -0.014      0.350  -0.041    0.967
#age.imm                   0.397      0.308   1.290    0.197
#sex.code                  0.210      0.249   0.844    0.399
#agediff.yo                0.078      0.287   0.272    0.786
#agediff.ol                0.073      0.244   0.301    0.764
#kinship.mo               -0.832      0.227  -3.658    0.000
#kinship.mk               -0.131      0.227  -0.575    0.565
#context_fs               -1.598      0.218  -7.330    0.000
#context_pl                0.648      0.156   4.145    0.000
#settingwild:speciesSum   -2.432      0.655  -3.712    0.000


drop1(mod.gaz,  test ="Chisq")
#age.dep          1 6696.8  0.002 0.9673494    
#age.imm          1 6698.4  1.654 0.1983489    
#sex.code         1 6697.5  0.734 0.3914699    
#agediff.yo       1 6696.8  0.074 0.7854872    
#agediff.ol       1 6696.9  0.091 0.7627516    
#kinship.mo       1 6709.3 12.549 0.0003964 ***
 # kinship.mk       1 6697.1  0.333 0.5638416    
#context_fs       1 6735.0 38.199 6.388e-10 ***
#  context_pl       1 6711.6 14.869 0.0001152 ***
#  setting:species  1 6704.5  7.704 0.0055108 ** 



#####################plot site differences in monopol#################################3

############################################################################################################################
modv=aggregate(x=test.data$modality_vis, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
modt=aggregate(x=test.data$modality_tac, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
modt_kin=aggregate(x=test.data$modality_tac, by=test.data[, c("ID_sign", "species", "setting", "kinship.mo")], FUN=mean)

moda=aggregate(x=test.data$modality_seis, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
mmod=aggregate(x=test.data$multimod, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
mmod_kin=aggregate(x=test.data$multimod, by=test.data[, c("ID_sign", "species", "setting", "kinship.mo")], FUN=mean)
mplex=aggregate(x=test.data$multiplex, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
mplex_kin=aggregate(x=test.data$multiplex, by=test.data[, c("ID_sign", "species", "setting", "kinship.mo")], FUN=mean)
bod=aggregate(x=test.data$artic_voc, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
bod_kin=aggregate(x=test.data$artic_bod, by=test.data[, c("ID_sign", "species", "setting", "kinship.mo")], FUN=mean)

manual=aggregate(x=test.data$artic_man, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
gaze=aggregate(x=test.data$gaze, by=test.data[, c("ID_sign", "species", "setting")], FUN=mean)
gaze_kin=aggregate(x=test.data$gaze, by=test.data[, c("ID_sign", "species", "setting","kinship.mo")], FUN=mean)

#####################plot site differences in monopol#################################3
library(ggplot2)
theme_marlen_ss <- theme(panel.background = element_blank(),
                         panel.border =element_rect(colour="black", fill=NA),
                         
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 12,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 12,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour=NA),
                         axis.title.x = element_text(size = 12, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 12, vjust = 1.5, family="sans"),
                         legend.text=  element_text(size = 11, family="sans", margin = margin(t = 10)),
                         legend.key = element_blank(),
                         legend.position = "right",
                         legend.spacing.x = unit(1.0, 'cm'),
                         strip.text = element_text(size = 12))

levels(mmod_kin$setting) <- c("Zoo", "Wild")
mmod_kin$kinship.mo=as.factor(mmod_kin$kinship.mo)
mmod_kin$kinship.mo <- relevel(mmod_kin$kinship.mo, "1")

dodge.posn <- position_dodge(.9)
mod.site <- ggplot(mmod_kin, aes(x = species, y = x))
mod.site + geom_boxplot(aes(fill = kinship.mo), width = 0.9) +
  geom_point(aes(fill = kinship.mo), width = 1,position= dodge.posn, shape = 1, colour = "black", alpha = 0.5) +
  theme_marlen_ss +
  scale_y_continuous("Proportion of multi-sensory acts") +
  scale_x_discrete("Orang-utan species",
                   limits = c("Bor", "Sum"),
                   labels = c("Bornean", "Sumatran"))+
  scale_fill_manual(values=c("blue", "orange"),name="Interaction dyad",
                    breaks=c("1","0"),
                    labels=c("mother-offspring", "other dyad"))+
  facet_wrap(~setting)+
  stat_summary(fun.y=mean, geom="point",shape =23, fill ="black",aes(group=kinship.mo), position=position_dodge(.9), 
               color="black", size=3)

                            
save.image("C:/Users/Marlen Fröhlich/Documents/R/orangutan_multi.RData")
