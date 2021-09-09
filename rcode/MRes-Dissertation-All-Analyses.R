data_DB <- subset(data, Condition=="Dialectical Bootstrapping")
data_RE <- subset(data, Condition=="Repeated Estimates")
data$scaledOMC <- scale(data$OMC, scale=TRUE, center=TRUE)
data$scaledNFCS <- scale(data$NFCS, scale=TRUE, center=TRUE)
data_long <- reshape(data, direction='long', #long data by time
                     varying=list(Error=c("MeanFE_Error", "MeanSE_Error"), 
                                  HRCont=c("HRContT1", "HRContT2"),
                                  HRAmbiv=c("HRAmbivT1", "HRAmbivT2"),
                                  HRStr=c("HRStrT1", "HRStrT2"),
                                  XRCont=c("XRContT1", "XRContT2"),
                                  XRAmbiv=c("XRAmbivT1", "XRAmbivT2"),
                                  XRStr=c("XRStrT1", "XRStrT2")),
                     timevar='Time',
                     times=c('T1', 'T2'),
                     v.names=c("Error", 
                               "HRCont", "HRAmbiv", "HRStr",
                               "XRCont", "XRAmbiv", "XRStr"),
                     idvar="ResponseId")
data_DB_long <- subset(data_long, Condition=="Dialectical Bootstrapping")
data_RE_long <- subset(data_long, Condition=="Repeated Estimates")
data_t1 <- subset(data_long, Time=="T1")
data_t2 <- subset(data_long, Time=="T2")

### Preliminary - raw accuracy between estimates
anovaAccuracy <- aov(Error ~ Condition*Time + Error(ResponseId/Time), 
                   data=data_long)
summary(anovaAccuracy)

### RQ1: Does DB improve accuracy?
RQ1_levene <- leveneTest(MeanAG~Condition, data=data)
RQ1_levene_apa <- apa_print(RQ1_levene)
RQ1_t <- t.test(MeanAG~Condition, data=data)
summary(RQ1_t)
RQ1_t_apa <- apa_print(RQ1_t)

### RQ2: Does DB make people change estimates more?
RQ2_levene <- leveneTest(MeanE_Change~Condition, data=data)
RQ2_levene_apa <- apa_print(RQ2_levene)
RQ2_t <- t.test(MeanE_Change~Condition, data=data)
RQ2_t_apa <- apa_print(RQ2_t)

### RQ3: Is attitude ambivalence/ certainty change bigger in the DB condition?

data_long_DB <- subset(data_long, Condition=="Dialectical Bootstrapping")
data_long_RE <- subset(data_long, Condition=="Repeated Estimates")
data_long_T1 <- subset(data_long, Time=="T1")
data_long_T2 <- subset(data_long, Time=="T2")
data_long_DB_T1 <- subset(data_long, 
                          Condition=="Dialectical Bootstrapping" & Time=="T1")
data_long_DB_T2 <- subset(data_long, 
                          Condition=="Dialectical Bootstrapping" & Time=="T2")
data_long_RE_T1 <- subset(data_long, 
                          Condition=="Repeated Estimates" & Time=="T1")
data_long_RE_T2 <- subset(data_long, 
                          Condition=="Repeated Estimates" & Time=="T2")

data_long$Id2 <- paste(data_long$ResponseId, data_long$Time, sep="")
data_long_att <- reshape(data_long, direction='long', #long data by time and topic
                     varying=list(Cont=c("HRCont", "XRCont"),
                                  Ambiv=c("HRAmbiv","XRAmbiv"),
                                  Str=c("HRStr","XRStr")),
                     timevar='Topic',
                     times=c('HR', 'XR'),
                     v.names=c("Cont", "Ambiv", "Str"),
                     idvar="Id2")

#Correlation between HR and XR
cor_cont <- cor.test(data_long$HRCont, data_long$XRCont)
cor_str <- cor.test(data_long$HRStr, data_long$XRStr)
cor_ambiv <- cor.test(data_long$HRAmbiv, data_long$XRAmbiv)

anovaHRCont <- aov(HRCont ~ Condition*Time + Error(ResponseId/Time), 
                   data=data_long)
summary(anovaHRCont)
anovaHRCont_apa <- apa_print(anovaHRCont)

anovaHRStr <- aov(HRStr ~ Condition*Time + Error(ResponseId/Time), 
                  data=data_long)
summary(anovaHRStr)
model.tables(anovaHRStr, "means")
anovaHRStr_apa <- apa_print(anovaHRStr)

anovaHRAmbiv <- aov(HRAmbiv ~ Condition*Time + Error(ResponseId/Time), 
                    data=data_long)
summary(anovaHRAmbiv)
model.tables(anovaHRAmbiv, "means")
anovaHRAmbiv_apa <- apa_print(anovaHRAmbiv)
#post hoc - interaction
anovaHRAmbivDB <- aov(HRAmbiv ~ Time + Error(ResponseId/Time), 
                     data=data_DB_long)
summary(anovaHRAmbivDB)
anovaHRAmbivDB_apa <- apa_print(anovaHRAmbivDB)
anovaHRAmbivRE <- aov(HRAmbiv ~ Time + Error(ResponseId/Time), 
                     data=data_RE_long)
summary(anovaHRAmbivRE)
anovaHRAmbivRE_apa <- apa_print(anovaHRAmbivRE)
anovaHRAmbivT1 <- aov(HRAmbiv ~ Condition, 
                     data=data_t1)
summary(anovaHRAmbivT1)
anovaHRAmbivT1_apa <- apa_print(anovaHRAmbivT1)
anovaHRAmbivT2 <- aov(HRAmbiv ~ Condition, 
                     data=data_t2)
summary(anovaHRAmbivT2)
anovaHRAmbivT2_apa <- apa_print(anovaHRAmbivT2)

anovaXRCont <- aov(XRCont ~ Condition*Time + Error(ResponseId/Time), 
                   data=data_long)
summary(anovaXRCont)
model.tables(anovaXRCont, "means")
anovaXRCont_apa <- apa_print(anovaXRCont)
#post hoc - interaction
anovaXRContDB <- aov(XRCont ~ Time + Error(ResponseId/Time), 
                      data=data_DB_long)
summary(anovaXRContDB)
anovaXRContDB_apa <- apa_print(anovaXRContDB)
anovaXRContRE <- aov(XRCont ~ Time + Error(ResponseId/Time), 
                      data=data_RE_long)
summary(anovaXRContRE)
anovaXRContRE_apa <- apa_print(anovaXRContRE)
anovaXRContT1 <- aov(XRCont ~ Condition, 
                      data=data_t1)
summary(anovaXRContT1)
anovaXRContT1_apa <- apa_print(anovaXRContT1)
anovaXRContT2 <- aov(XRCont ~ Condition, 
                      data=data_t2)
summary(anovaXRContT2)
anovaXRContT2_apa <- apa_print(anovaXRContT2)

## XR Ambiv
anovaXRAmbiv <- aov(XRAmbiv ~ Condition*Time + Error(ResponseId/Time), 
                    data=data_long)
summary(anovaXRAmbiv)
model.tables(anovaXRAmbiv, "means")
anovaXRAmbiv_apa <- apa_print(anovaXRAmbiv)
#Posthoc
anovaXRAmbivDB <- aov(XRAmbiv ~ Time + Error(ResponseId/Time), 
                      data=data_DB_long)
summary(anovaXRAmbivDB)
anovaXRAmbivDB_apa <- apa_print(anovaXRAmbivDB)
anovaXRAmbivRE <- aov(XRAmbiv ~ Time + Error(ResponseId/Time), 
                      data=data_RE_long)
summary(anovaXRAmbivRE)
anovaXRAmbivRE_apa <- apa_print(anovaXRAmbivRE)
anovaXRAmbivT1 <- aov(XRAmbiv ~ Condition, 
                      data=data_t1)
summary(anovaXRAmbivT1)
anovaXRAmbivT1_apa <- apa_print(anovaXRAmbivT1)
anovaXRAmbivT2 <- aov(XRAmbiv ~ Condition, 
                      data=data_t2)
summary(anovaXRAmbivT2)
anovaXRAmbivT2_apa <- apa_print(anovaXRAmbivT2)


## XR Strength
anovaXRStr <- aov(XRStr ~ Condition*Time + Error(ResponseId/Time), 
                  data=data_long)
summary(anovaXRStr)
model.tables(anovaXRStr, "means")
anovaXRStr_apa <- apa_print(anovaXRStr)


### RQ4: Do OMC/ NFCS affect RQs 1, 2 and 3?
#RQ4.1 Do OMC/ NFCS affect RQ1
RQ4.1 <- lm(data=data, MeanAG ~ Condition * scaledOMC * scaledNFCS)
summary(RQ4.1)
vif(RQ4.1)
RQ4.1_apa <- apa_print(RQ4.1)

#RQ4.2 Do OMC/ NFCS affect RQ2
RQ4.2 <- lm(data=data, MeanE_Change ~ Condition * scaledOMC * scaledNFCS)
summary(RQ4.2)
RQ4.2_vif <- vif(RQ4.2)
RQ4.2_apa <- apa_print(RQ4.2)

#RQ4.3 Do OMC/ NFC affect RQ3?
lm <- lm(data=data_long, HRCont ~ Condition*Time*(scaledNFCS+scaledOMC))
summary(lm)
lm <- lm(data=data_long, XRCont ~ Condition*Time*(scaledNFCS+scaledOMC))
summary(lm)
lm <- lm(data=data_long, HRAmbiv ~ Condition*Time*(scaledNFCS+scaledOMC))
summary(lm)
lm <- lm(data=data_long, XRAmbiv ~ Condition*Time*(scaledNFCS+scaledOMC))
summary(lm)
lm <- lm(data=data_long, HRStr ~ Condition*Time*(scaledNFCS+scaledOMC))
summary(lm)
lm <- lm(data=data_long, XRStr ~ Condition*Time*(scaledNFCS+scaledOMC))
summary(lm)
