surv.des <- svrepdesign( weights=~W_FSTUWT, data=pisa15Mas,repweights="W_FSTURWT[0-9]+", 
                         type="Fay", rho=0.5,combined.weights = TRUE)

ibteach <- '
        science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
        
        IBTEACH  ~ giveIdeas +doingPractExp +scieQuest +conclExp +techExpl +designOwnExp +classDebate +techExplCon
        
        science ~ IBTEACH + ESCS + Gender

        '

fit.ibteach <- lavaan(ibteach, data=pisa15Mas, auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
               int.ov.free=TRUE,estimator="MLM")
final.ibteach <-lavaan.survey(lavaan.fit=fit.ibteach, survey.design=surv.des)

#sink("analysis1.txt")
summary(final.ibteach, fit.measures = TRUE, standardized = TRUE,rsquare=TRUE)
fitmeasures(final.ibteach,c("rmsea.ci.lower","rmsea.ci.upper","rmsea","chisq","srmr","tli","cfi","srmr_bentler"))
#sink()
fitted(final.ibteach)
coef(final.ibteach)
resid(final.ibteach)

# Standardized parameters:
semPaths(final.ibteach, "std","est", style = "lisrel", curve = 0.8, nCharNodes = 0,
         sizeLat = 12, sizeLat2 = 6, title = TRUE,
         mar = c(5, 1, 5, 1), curvePivot = FALSE,
         edge.label.cex = 0.5)


ibtEnv <- '
        science =~ PV1SCIE + PV2SCIE + PV3SCIE + PV4SCIE + PV5SCIE + PV6SCIE + PV7SCIE + PV8SCIE + PV9SCIE + PV10SCIE
        
        IBTEACH  ~ giveIdeas +doingPractExp +scieQuest +conclExp +techExpl +designOwnExp +classDebate +techExplCon
        ENVAWARE ~ greenhouseA+geneticA+nuclearA+deforestA +airPollutA+extinctionA+waterShortA
        ENVOPT ~ airPollutO+extinctionO+deforestO+waterShortO+nuclearO+greenhouseO+geneticO
        
        science ~ IBTEACH +ENVAWARE+ENVOPT+ ESCS + Gender

        '
fit.ibtEnv<- lavaan(ibtEnv, data=pisa15Mas, auto.var=TRUE, std.lv=TRUE, meanstructure=TRUE, 
                      int.ov.free=TRUE,estimator="MLM")
final.ibtEnv <-lavaan.survey(lavaan.fit=fit.ibtEnv, survey.design=surv.des)
summary(final.ibtEnv, fit.measures = TRUE, standardized = TRUE,rsquare=TRUE)
fitmeasures(final.ibtEnv,c("rmsea.ci.lower","rmsea.ci.upper","rmsea","chisq","srmr","tli","cfi","srmr_bentler"))
semPaths(final.ibtEnv, "std","est", style = "lisrel", curve = 0.8, nCharNodes = 0,
         sizeLat = 12, sizeLat2 = 6, title = TRUE,
         mar = c(5, 1, 5, 1), curvePivot = FALSE,
         edge.label.cex = 0.5)
