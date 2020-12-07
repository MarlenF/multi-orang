rm(list=ls())
setwd("C:/Users/Marlen Fröhlich/Documents/R")
int <- read.table ("orangutanR_Sep2020_rev.csv", header=TRUE, sep=",")
library(arm)
library(car)
mm=as.data.frame(na.omit(int[, c("effect","tac_mm", "multiplex", "Behavior", "age", "sex", "context_fs", "context_pl", "species", "kinship","agediff", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))
mp=as.data.frame(na.omit(int[, c("match_tight", "gaze_mp", "multimod", "Behavior","age", "sex", "context_fs", "context_pl", "species", "kinship","agediff", "group", "setting", "ID_coder", "ID_obs", "Dyad", "ID_sign","ID_rec")]))


test.data=mm
test.data <- test.data[ which(test.data$multiplex=='0'), ] # for all mm models
#test.data <- test.data[ which(test.data$multimod=='0'), ] # for all mp models

test.data$age.dep=as.numeric(test.data$age==levels(test.data$age)[2])
test.data$age.imm=as.numeric(test.data$age==levels(test.data$age)[3])
test.data$sex.code=as.numeric(test.data$sex==levels(test.data$sex)[2])
test.data$kinship.mk=as.numeric(test.data$kinship==levels(test.data$kinship)[1])
test.data$kinship.mo=as.numeric(test.data$kinship==levels(test.data$kinship)[2])
test.data$agediff.ol=as.numeric(test.data$agediff==levels(test.data$agediff)[1])
test.data$agediff.yo=as.numeric(test.data$agediff==levels(test.data$agediff)[3])


contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000000))
contr2=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1000000000))

# collinearity: max vif = 2.6 (effect tac_mm)
vif(lm(rnorm(nrow(test.data)) ~  vis_mm+ species * setting +  age.dep + age.imm + sex.code + context_pl + agediff + kinship, data = test.data))

#########################################################
#multiplex EFFECT
mod.suc.mp = glmer(formula = effect ~ gaze_mp +setting + species  + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                     (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                     (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                     (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                     (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                     (0+gaze_mp|ID_sign)  +
                     (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.suc.mp = glmer(formula = effect ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                      (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                      (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                      (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                      (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                      (0+gaze_mp|ID_sign)  +
                      (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)
length(residuals(mod.suc.mp)) #manual: 3037, bodily: 1498, gaze: 4513, 
length(residuals(null.suc.mp)) #
as.data.frame(anova(null.suc.mp, mod.suc.mp, test="Chisq"))



# LRT gaze
Df      AIC      BIC    logLik deviance    Chisq Chi Df   Pr(>Chisq)
null.suc.mp 35 5447.229 5671.744 -2688.614 5377.229       NA     NA           NA
mod.suc.mp  36 5436.415 5667.345 -2682.208 5364.415 12.81328      1 0.0003441671

# LRT manual
Df      AIC      BIC    logLik deviance      Chisq Chi Df Pr(>Chisq)
null.suc.mp 35 3648.963 3859.615 -1789.482 3578.963         NA     NA         NA
mod.suc.mp  36 3650.890 3867.561 -1789.445 3578.890 0.07281961      1  0.7872748

# LRT bodily
Df      AIC      BIC    logLik deviance    Chisq Chi Df Pr(>Chisq)
null.suc.mp 35 1734.924 1920.840 -832.4620 1664.924       NA     NA         NA
mod.suc.mp  36 1732.636 1923.863 -830.3178 1660.636 4.288419      1 0.03837284

drop1(mod.suc.mp,  test ="Chisq")


#LRT vocal/facial: model did not converge

#sum gaze
round(summary(mod.suc.mp)$coefficients, 3)
(Intercept)    0.239      0.445   0.537    0.591
gaze_mp        0.472      0.124   3.800    0.000
settingwild    0.416      0.351   1.183    0.237
speciesSum    -0.967      0.362  -2.674    0.008
age.dep        0.241      0.375   0.643    0.520
age.imm        0.380      0.315   1.206    0.228
sex.code       0.169      0.222   0.761    0.447
kinship.mo     0.454      0.242   1.879    0.060
kinship.mk    -0.218      0.231  -0.947    0.344
agediff.yo     0.330      0.294   1.121    0.262
agediff.ol    -0.188      0.251  -0.747    0.455
context_fs    -1.020      0.203  -5.014    0.000
context_pl     0.010      0.189   0.050    0.960

### drop1 gaze
gaze_mp     1 5447.2 12.8133 0.0003442 ***
setting     1 5435.7  1.3232 0.2500258    
species     1 5438.5  4.0922 0.0430807 *  
age.dep     1 5434.8  0.3836 0.5356613    
age.imm     1 5435.9  1.4430 0.2296493    
sex.code    1 5435.0  0.5630 0.4530462    
kinship.mo  1 5438.0  3.5954 0.0579398 .  
kinship.mk  1 5435.3  0.8798 0.3482593    
agediff.yo  1 5435.6  1.2112 0.2710857    
agediff.ol  1 5434.9  0.5141 0.4733905    
context_fs  1 5452.8 18.3974 1.793e-05 ***
context_pl  1 5434.4  0.0022 0.9626236 


#sum bodily
(Intercept)   -0.044      0.667  -0.066    0.948
bodily_mp     -0.403      0.192  -2.097    0.036
settingwild    1.515      0.719   2.108    0.035
speciesSum    -1.039      0.675  -1.541    0.123
age.dep        0.623      0.439   1.418    0.156
age.imm        0.745      0.448   1.662    0.096
sex.code      -0.106      0.293  -0.361    0.718
kinship.mo     0.327      0.289   1.129    0.259
kinship.mk    -0.507      0.328  -1.544    0.123
agediff.yo     0.381      0.416   0.915    0.360
agediff.ol    -0.131      0.310  -0.424    0.672
context_fs    -0.822      0.260  -3.165    0.002
context_pl    -0.441      0.464  -0.951    0.342


### drop1 bodily
bodily_mp   1 1734.9 4.2884 0.03837 * 
setting     1 1734.4 3.7333 0.05334 . 
species     1 1732.7 2.0216 0.15508   
age.dep     1 1732.6 1.9544 0.16212   
age.imm     1 1733.3 2.7126 0.09956 . 
sex.code    1 1730.8 0.1253 0.72339   
kinship.mo  1 1731.9 1.2478 0.26397   
kinship.mk  1 1732.9 2.2318 0.13520   
agediff.yo  1 1731.4 0.8086 0.36852   
agediff.ol  1 1730.8 0.1711 0.67913   
context_fs  1 1738.9 8.2612 0.00405 **
context_pl  1 1731.5 0.8608 0.35350 


  
#########################################################
#multiplex MATCH (convergence troubles)
mod.mat.mp = glmer(formula = match_tight~ bodily_mp +setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                                          #(0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                                           #(0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                                           #(0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                                           #(0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                                           (0+bodily_mp|ID_sign)  + 
                                           (1|ID_sign) +#(1|ID_rec) +#(1|Dyad)+
                                           (1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.mat.mp = glmer(formula = match_tight ~setting + species + age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                      #(0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                                            #(0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                                            #(0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                                            #(0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                                            (0+bodily_mp|ID_sign)  + 
                                            (1|ID_sign) +#(1|ID_rec) +#(1|Dyad)+
                                            (1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)


length(residuals(mod.mat.mp)) # manual: 2590, gaze: 3869, bodily: 1429
length(residuals(null.mat.mp)) 
as.data.frame(anova(null.mat.mp, mod.mat.mp, test="Chisq"))

#LRT manual
#Df      AIC      BIC    logLik deviance    Chisq Chi Df Pr(>Chisq)
#null.mat.mp 36 925.6758 1136.615 -426.8379 853.6758       NA     NA         NA
#mod.mat.mp  37 926.5748 1143.373 -426ywwaA .2874 852.5748 1.101021      1  0.2940421

#LRT bodily
#Df      AIC      BIC    logLik deviance    Chisq Chi Df  Pr(>Chisq)
#null.mat.mp 17 651.5217 741.0221 -308.7609 617.5217       NA     NA          NA
#mod.mat.mp  18 646.8575 741.6226 -305.4287 610.8575 6.664266      1 0.009836513

#LRT facial, vocal
# model did not converge - sample size

#LRT gaze1*
#Df      AIC      BIC    logLik deviance    Chisq Chi Df  Pr(>Chisq)
#null.mat.mp 36 1896.789 2122.176 -912.3947 1824.789       NA     NA          NA
#mod.mat.mp  37 1892.088 2123.735 -909.0438 1818.088 6.701837      1 0.009631361


drop1(mod.mat.mp,  test ="Chisq")
#drop1 bodily
#bodily_mp   1 651.18   6.324 0.0119094 *  
#setting     1 647.37   2.514 0.1128653    
#species     1 644.97   0.113 0.7371386    
#age.dep     1 645.15   0.290 0.5900659    
#age.imm     1 644.86   0.002 0.9648311    
#sex.code    1 649.28   4.421 0.0354915 *  
#kinship.mo  1 649.07   4.210 0.0401874 *  
#kinship.mk  1 659.72  14.860 0.0001158 ***
#agediff.yo  1 647.96   3.099 0.0783198 .  
#agediff.ol  1 644.92   0.065 0.7989295    
#context_fs  1 836.40 191.543 < 2.2e-16 ***
#context_pl  1 856.56 211.703 < 2.2e-16 ***

round(summary(mod.mat.mp)$coefficients, 3)


#sum bodily
#(Intercept)   -6.034      1.722  -3.503    0.000
#bodily_mp     -1.889      0.730  -2.587    0.010
#settingwild   -1.536      0.987  -1.557    0.120
#speciesSum    -0.351      1.048  -0.335    0.738
#age.dep       -0.773      1.450  -0.533    0.594
#age.imm        0.064      1.394   0.046    0.964
#sex.code       1.714      0.902   1.900    0.057
#kinship.mo     1.251      1.114   1.123    0.261
#kinship.mk     0.950      1.033   0.919    0.358
#agediff.yo    -2.168      1.301  -1.666    0.096
#agediff.ol    -0.273      1.074  -0.254    0.800
#context_fs    17.153      1.468  11.688    0.000
#context_pl    16.712      1.424  11.739    0.000#

#drop1 gaze
#gaze_mp     1 1896.8   6.702  0.009631 ** 
#setting     1 1898.6   8.502  0.003547 ** 
#species     1 1890.1   0.001  0.969714    
#age.dep     1 1890.5   0.457  0.499083    
#age.imm     1 1891.6   1.506  0.219730    
#sex.code    1 1893.0   2.947  0.086017 .  
#kinship.mo  1 1890.2   0.072  0.788537    
#kinship.mk  1 1898.9   8.839  0.002949 ** 
#agediff.yo  1 1890.7   0.578  0.446980    
#agediff.ol  1 1890.3   0.219  0.639602    
#context_fs  1 1891.9   1.799  0.179877    
#context_pl  1 2038.4 148.329 < 2.2e-16 ***

#sum gaze
#(Intercept)   -0.174      0.643  -0.271    0.787
#gaze_mp       -0.691      0.265  -2.604    0.009
#settingwild   -1.371      0.344  -3.989    0.000
#speciesSum     0.014      0.380   0.038    0.970
#age.dep       -0.342      0.511  -0.671    0.503
#age.imm       -0.585      0.486  -1.203    0.229
#sex.code       0.523      0.302   1.734    0.083
#kinship.mo     0.085      0.315   0.270    0.787
#kinship.mk     1.074      0.359   2.993    0.003
#agediff.yo    -0.348      0.458  -0.759    0.448
#agediff.ol     0.206      0.440   0.467    0.640
#context_fs     0.409      0.316   1.296    0.195
#context_pl     6.973      0.655  10.639    0.000



#########################################################
#multimodal EFFECT

#VISUAL MM 
mod.suc.vis = glmer(formula = effect ~vis_mm +setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                     (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                     (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                     (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                     (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                     (0+vis_mm|ID_sign)  +
                     (1|ID_sign) +(1|ID_rec) +(1|Dyad)+ (1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.suc.vis = glmer(formula = effect ~ setting + species +age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                      (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                      (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                      (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                      (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                      (0+vis_mm|ID_sign) + 
                      (1|ID_sign) +(1|ID_rec) +(1|Dyad)+ (1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.suc.vis)) #tac_mm: 2301,
length(residuals(null.suc.vis)) #
as.data.frame(anova(null.suc.vis, mod.suc.vis, test="Chisq"))

#LRT visual effect*
#null.suc.vis 35 2503.049 2703.987 -1216.524 2433.049       NA     NA           NA
#mod.suc.vis  36 2490.869 2697.549 -1209.434 2418.869 14.17991      1 0.0001661352

#sum visual effect
#(Intercept)    0.027      0.656   0.041    0.968
#vis_mm         0.878      0.212   4.140    0.000
#settingwild   -0.118      0.491  -0.240    0.811
#speciesSum    -1.316      0.523  -2.515    0.012
#age.dep        0.141      0.465   0.303    0.762
#age.imm        0.214      0.434   0.493    0.622
#sex.code       0.464      0.253   1.838    0.066
#kinship.mo     0.807      0.313   2.579    0.010
#kinship.mk     0.354      0.319   1.111    0.267
#agediff.yo     0.927      0.404   2.296    0.022
#agediff.ol    -0.291      0.323  -0.902    0.367
#context_fs    -0.887      0.281  -3.155    0.002
#context_pl     0.166      0.285   0.583    0.560

drop1(mod.suc.vis,  test ="Chisq")
#vis_mm      1 2503.1 14.1799 0.0001661 ***
#setting     1 2488.9  0.0575 0.8105607    
#species     1 2492.6  3.7468 0.0529087 .  
#age.dep     1 2489.0  0.0918 0.7618606    
#age.imm     1 2489.1  0.2425 0.6224145    
#sex.code    1 2492.3  3.3938 0.0654419 .  
#kinship.mo  1 2495.7  6.8374 0.0089270 ** 
#kinship.mk  1 2490.1  1.2317 0.2670860    
#agediff.yo  1 2494.2  5.3232 0.0210435 *  
#agediff.ol  1 2489.6  0.7708 0.3799771    
#context_fs  1 2498.1  9.2379 0.0023706 ** 
#context_pl  1 2489.2  0.3419 0.5587438  

#TACTILE MM 
mod.suc.tac = glmer(formula = effect ~tac_mm +setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                      (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                      (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                      (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                      (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                      (0+tac_mm|ID_sign)  +
                      (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

null.suc.tac = glmer(formula = effect ~ setting + species +age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                       (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                       (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                       (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                       (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                       (0+tac_mm|ID_sign) + 
                       (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

length(residuals(mod.suc.tac)) #tac_mm: 3743
length(residuals(null.suc.tac)) #
as.data.frame(anova(null.suc.tac, mod.suc.tac, test="Chisq"))

#LRT tactile effect*
#Df      AIC      BIC    logLik deviance    Chisq Chi Df  Pr(>Chisq)
#null.suc.tac 35 4395.951 4613.919 -2162.976 4325.951       NA     NA          NA
#mod.suc.tac  36 4387.867 4612.062 -2157.933 4315.867 10.08464      1 0.001495105



#LRT audible effect: no model convergence

round(summary(mod.suc.vis)$coefficients, 3)

#sum tactile effect
#(Intercept)   -1.057      0.522  -2.026    0.043
#tac_mm         0.558      0.162   3.453    0.001
#settingwild    0.699      0.414   1.688    0.091
#speciesSum    -0.556      0.415  -1.340    0.180
#age.dep        0.819      0.422   1.942    0.052
#age.imm        0.504      0.383   1.315    0.189
##sex.code       0.356      0.253   1.409    0.159
#k#inship.mo     0.550      0.259   2.120    0.034
#kinship.mk     0.128      0.253   0.507    0.612
#agediff.yo     0.879      0.353   2.492    0.013
#agediff.ol    -0.091      0.301  -0.302    0.762
#context_fs    -0.186      0.194  -0.957    0.339
#context_pl     0.283      0.229   1.235    0.217

drop1(mod.suc.tac,  test ="Chisq")
#tac_mm      1 4396.0 10.0846 0.001495 **
#setting     1 4388.4  2.5630 0.109394   
#species     1 4387.6  1.6856 0.194188   
#age.dep     1 4389.5  3.6498 0.056076 . 
#age.imm     1 4387.6  1.7017 0.192063   
#sex.code    1 4387.9  2.0663 0.150585   
#kinship.mo  1 4390.4  4.5289 0.033326 * 
#kinship.mk  1 4386.1  0.2560 0.612892   
#agediff.yo  1 4392.0  6.1424 0.013197 * 
#agediff.ol  1 4386.0  0.0888 0.765732   
#context_fs  1 4386.7  0.8823 0.347572   
#context_pl  1 4387.4  1.4865 0.222756#

##########################################################
#multimodal MATCH
                                                                                                                                                 
mod.mat.mm = glmer(formula = match_tight ~tac_mm + setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                     (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                     (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                     (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                     (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                     (0+tac_mm|ID_sign)  +  
                     (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs)+ (1|Behavior), family = binomial, data = test.data,  control=contr)

null.mat.mm = glmer(formula = match_tight ~ setting + species +   age.dep + age.imm + sex.code +  kinship.mo + kinship.mk + agediff.yo + agediff.ol + context_fs + context_pl +
                      (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                      (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                      (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                      (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                      (0+tac_mm|ID_sign)  +  
                    (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs)+ (1|Behavior), family = binomial, data = test.data,  control=contr
                    
                  length(residuals(mod.mat.mm)) # visual: 1674, tactile: 3129
length(residuals(null.mat.mm)) #
as.data.frame(anova(null.mat.mm, mod.mat.mm, test="Chisq"))

#LRT tactile match
#Df      AIC      BIC    logLik deviance    Chisq Chi Df Pr(>Chisq)
#null.mat.mm 36 947.6380 1165.383 -437.8190 875.6380       NA     NA         NA
#mod.mat.mm  37 946.9804 1170.774 -436.4902 872.9804 2.657595      1  0.1030564

#LRT visual match (no model convergence)

#audible/seismic match: sample size too small




#####################plot site differences in monopol#################################3

# dummycoding for plot

test.data$setting=as.numeric(test.data$setting)
test.data$species=as.numeric(test.data$species)
setting.c= test.data$setting- mean(test.data$setting)
species.c= test.data$species- mean(test.data$species)
age.dep.c= test.data$age.dep- mean(test.data$age.dep)
age.imm.c= test.data$age.imm- mean(test.data$age.imm)
sex.code.c= test.data$sex.code- mean(test.data$sex.code)
kinship.mo.c= test.data$kinship.mo- mean(test.data$kinship.mo)
kinship.mk.c= test.data$kinship.mk- mean(test.data$kinship.mk)
agediff.ol.c= test.data$agediff.ol- mean(test.data$agediff.ol)
agediff.yo.c= test.data$agediff.yo- mean(test.data$agediff.yo)
context_fs.c= test.data$context_fs- mean(test.data$context_fs)
context_pl.c= test.data$context_pl- mean(test.data$context_pl)



#plot models

plot.suc.tac = glmer(formula = effect ~tac_mm +setting.c + species.c +   age.dep.c + age.imm.c + sex.code.c +  kinship.mo.c + kinship.mk.c + agediff.yo.c + agediff.ol.c + context_fs.c + context_pl.c +
                      (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                      (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                      (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                      (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                      (0+tac_mm|ID_sign)  +
                      (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr)

plot.suc.vis = glmer(formula = effect ~vis_mm +setting.c + species.c +   age.dep.c + age.imm.c + sex.code.c +  kinship.mo.c + kinship.mk.c + agediff.yo.c + agediff.ol.c + context_fs.c + context_pl.c +
                       (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                       (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                       (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                       (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                       (0+vis_mm|ID_sign)  +
                       (1|ID_sign) +(1|ID_rec) +(1|Dyad) +(1|group) + (1|ID_coder) + (1|ID_obs), family = binomial, data = test.data,  control=contr2)

plot.mat.gaz = lmer(formula = match_tight~ gaze_mp +setting.c + species.c +   age.dep.c + age.imm.c + sex.code.c +  kinship.mo.c + kinship.mk.c + agediff.yo.c + agediff.ol.c + context_fs.c + context_pl.c +
                     (0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                     (0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                     (0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                     (0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                     (0+gaze_mp|ID_sign)  + 
                     (1|ID_sign) +(1|ID_rec) +(1|Dyad)+(1|group) + (1|ID_coder) + (1|ID_obs)+ (1|Behavior), data = test.data,  control=contr2)

plot.mat.bod = lmer(formula = match_tight~ bodily_mp +setting.c + species.c +   age.dep.c + age.imm.c + sex.code.c +  kinship.mo.c + kinship.mk.c + agediff.yo.c + agediff.ol.c + context_fs.c + context_pl.c +
                     #(0+age.dep|ID_rec)  + (0+sex.code|ID_rec) +  (0+context_fs|ID_sign) +  (0+context_pl|ID_sign) + (0+context_fs|ID_rec)+ 
                     #(0+context_pl|ID_rec) + (0+context_fs|Dyad)+ (0+context_pl|Dyad)+
                     #(0+agediff.yo|ID_sign)+  (0+agediff.yo|ID_rec)+  (0+agediff.ol|ID_sign)+ (0+agediff.ol|ID_rec)+
                     #(0+kinship.mo|ID_sign)+ (0+kinship.mk|ID_sign) + (0+kinship.mo|ID_rec) + (0+kinship.mk|ID_rec) +
                     #(0+bodily_mp|ID_sign)  + 
                     (1|ID_sign) +#(1|ID_rec) +(1|Dyad)+
                     (1|group) + (1|ID_coder) + (1|ID_obs),  data = test.data,  control=contr2)


############################################################################################################################


                              )

save.image("C:/Users/Marlen Fröhlich/Documents/R/orangutan_effect.RData")
