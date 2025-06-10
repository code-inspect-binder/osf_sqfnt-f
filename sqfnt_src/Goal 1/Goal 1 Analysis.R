#--------------Load package-----------------------------------------------------
library(metafor)
library(devtools)
library(flextable)
#if (!require("devtools")) {
        install.packages("devtools")
}
#devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
#install.packages("https://tinyurl.com/dt4y5drs")
library(meta)
library(metasens)
library(readxl)
library(pimeta)
#--------------Read data--------------------------------------------------------
Goal_1_Data <- read_excel("Goal 1 Data.xlsx")
Overall_Analysis <- Goal_1_Data[order(-Goal_1_Data$`Correlation_(r)`),]
View(Overall_Analysis)

#--------------Main meta-analysis-----------------------------------------------
m.cor <- metacor(Overall_Analysis$`Correlation_(r)`, 
                 N, 
                 data = Overall_Analysis,
                 studlab = Overall_Analysis$Author,
                 sm = "COR",
                 method.tau = "REML",
                 prediction = T,
                 comb.fixed=FALSE)
m.cor

#Forest Plot
pdf("Self_control_Forest.pdf",width=9,height=20)
forest(m.cor,
       xlim = c(-1,1),
       rightlabs = c("Correlation", "95% CI"),
       leftlabs = c("Author (Year)", "N"),
       pooled.totals = FALSE,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = FALSE,
       col.diamond = "grey",
       col.diamond.lines = "black",
       col.predict = "black",
       col.inside = "black",
       print.I2.ci = TRUE,
       digits.sd = 2, na.rm = T)
dev.off()

#pcurve
#pcurve(m.cor)

#Funnel plot, symmetry test (Egger's regression), fail-safe n
pdf("Self_control_Funnel.pdf",width=5,height=5)
funnel(m.cor,xlab = "Correlation")
dev.off()

eggers.test(x = m.cor)
fsn(yi=m.cor$TE, sei=m.cor$seTE, type="Rosenthal")
plot(copas(m.cor))
copas(m.cor)

#-----------Meta-analysis with reliability-corrected correlations---------------
WithCorrected = which(Overall_Analysis$Correlation_corrected !='NA')
CorrectedDat = Overall_Analysis[WithCorrected,]
CorrectedData = CorrectedDat[order(-CorrectedDat$Correlation_corrected),]

m.cor2 <- metacor(Correlation_corrected, 
                 N, 
                 data = CorrectedData,
                 studlab = CorrectedData$Author,
                 sm = "COR",
                 method.tau = "REML",
                 prediction = T,
                 comb.fixed=FALSE)
m.cor2

#Corresponding Forest Plot
pdf("Self_control_Forest_Corrected.pdf",width=9,height=20)
forest(m.cor2,
       xlim = c(-1,1),
       rightlabs = c("Correlation", "95% CI"),
       leftlabs = c("Author (Year)", "N"),
       pooled.totals = FALSE,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = FALSE,
       col.diamond = "grey",
       col.diamond.lines = "black",
       col.predict = "black",
       col.inside = "black",
       print.I2.ci = TRUE,
       digits.sd = 2, na.rm = T)
dev.off()

#-----------Meta_analysis Without reliability imputation------------------------
WithCorrected = which(Overall_Analysis$Correlation_corrected_o !='NA')
CorrectedDat = Overall_Analysis[WithCorrected,]
CorrectedData = CorrectedDat[order(-CorrectedDat$Correlation_corrected),]

m.cor_imp <- metacor(Correlation_corrected, 
                 N, 
                 data = CorrectedData,
                 studlab = CorrectedData$Author,
                 sm = "COR",
                 method.tau = "REML",
                 comb.fixed=FALSE)
m.cor_imp

#----------------------Meta_analysis without Tangney et al 2004-----------------
ExceptTangney = which(Overall_Analysis$UniqueID !='1')
CorrectedDatWT = Overall_Analysis[ExceptTangney,]
CorrectedDataWT = CorrectedDatWT[order(-CorrectedDatWT$`Correlation_(r)`),]

m.cor3 <- metacor(CorrectedDataWT$`Correlation_(r)`, 
                  N, 
                  data = CorrectedDataWT,
                  studlab = CorrectedDataWT$Author,
                  sm = "COR",
                  method.tau = "REML",
                  comb.fixed=FALSE)
m.cor3

#----------------------Meta_analysis with Attenuatted correlation---------------
m.cor4 <- metacor(CorrectedDataWT$Correlation_corrected,
                  N, 
                  data = CorrectedDataWT,
                  studlab = CorrectedDataWT$Author,
                  sm = "COR",
                  method.tau = "REML",
                  comb.fixed=FALSE)
m.cor4

#----------------------Meta_analysis on the Reliability of SC/SD----------------
SCSD_Re_data = Overall_Analysis[!(is.na(Overall_Analysis$SC_Reliability)) & !(is.na(Overall_Analysis$SD_Reliability)),]
m.cor_SCRe <- metacor(SCSD_Re_data$SC_Reliability, 
                 N, 
                 data = SCSD_Re_data,
                 studlab = SCSD_Re_data$Author,
                 sm = "COR",
                 method.tau = "REML",
                 comb.fixed=FALSE)
m.cor_SCRe

m.cor_SDRe <- metacor(SCSD_Re_data$SD_Reliability, 
                      N, 
                      data = SCSD_Re_data,
                      studlab = SCSD_Re_data$Author,
                      sm = "COR",
                      method.tau = "REML",
                      comb.fixed=FALSE)
m.cor_SDRe


#----------------------moderator analysis---------------------------------------
#---Q statistics----------------------------------------------------------------
#data collection setting
metareg(m.cor2,Data_Collection_Setting)

#SC Measure
metareg(m.cor2,SC_scale)

#SD Measure
metareg(m.cor2,SD_scale2)

#Language
metareg(m.cor2,Language2)

#Individual Language
metareg(m.cor2,Language)

#country
#metareg(m.cor2,Geographic_Location)

#---Averaged Correlations by group----------------------------------------------
#------Mod SD--------------------Averaged Correlation by SD scale---------------
newdataSD = Overall_Analysis[which(Overall_Analysis$SD_scale2=='BIDR' | Overall_Analysis$SD_scale2=='MC'|Overall_Analysis$SD_scale2=='SDS-17'|Overall_Analysis$SD_scale2=='Lie' ), ]
m.corSD <- metacor(newdataSD$`Correlation_(r)`, 
                  N, 
                  data = newdataSD,
                  studlab = newdataSD$`Title/Citation`,
                  sm = "COR",
                  method.tau = "REML",
                  comb.fixed=FALSE)
subgroupSD<-update.meta(m.corSD, byvar=SD_scale2, comb.random = TRUE, comb.fixed = FALSE)
subgroupSD


#-----Mod SC FULL BRIEF----------Averaged Correlation by SD scale---------------
newdataSC = Overall_Analysis[which(Overall_Analysis$SC_scale=='Full' | Overall_Analysis$SC_scale=='Brief'), ]
m.corSc <- metacor(newdataSC$`Correlation_(r)`, 
                   N, 
                   data = newdataSC,
                   studlab = newdataSC$`Title/Citation`,
                   sm = "COR",
                   method.tau = "REML",
                   comb.fixed=FALSE)
subgroupSC<-update.meta(m.corSc, byvar=SC_scale, comb.random = TRUE, comb.fixed = FALSE)
subgroupSC

#---Mod Data Collection Setting--Averaged Correlation by Data collection setting--------
newdataLAB = Overall_Analysis[which(Overall_Analysis$Data_Collection_Setting=='In-person' | Overall_Analysis$Data_Collection_Setting=='Online'), ]
m.corLAB <- metacor(newdataLAB$`Correlation_(r)`, 
                   N, 
                   data = newdataLAB,
                   studlab = newdataLAB$`Title/Citation`,
                   sm = "COR",
                   method.tau = "REML",
                   comb.fixed=FALSE)
subgroupLAB<-update.meta(m.corLAB , byvar=Data_Collection_Setting, comb.random = TRUE, comb.fixed = FALSE)
subgroupLAB

#---Mod Language--Averaged Correlation by Language------------------------------
newdataLang = Overall_Analysis[which(Overall_Analysis$Language2=='English' | Overall_Analysis$Language2=='Translated'), ]
m.corLang <- metacor(newdataLang$`Correlation_(r)`, 
                    N, 
                    data = newdataLang,
                    studlab = newdataLang$`Title/Citation`,
                    sm = "COR",
                    method.tau = "REML",
                    comb.fixed=FALSE)
subgroupLang<-update.meta(m.corLang, byvar=Language2, comb.random = TRUE, comb.fixed = FALSE)
subgroupLang

#-------------Moderation of whether reliability values are provided-------------
Overall_Analysis$SD_Reliability[Overall_Analysis$SD_Reliability > 0 ] <- "Yes"
Overall_Analysis$SD_Reliability[is.na(Overall_Analysis$SD_Reliability)] <- "No"
Overall_Analysis

m.corRelia <- metacor(Overall_Analysis$`Correlation_(r)`, 
                   N, 
                   data = Overall_Analysis,
                   studlab = Overall_Analysis$`Title/Citation`,
                   sm = "COR",
                   method.tau = "REML",
                   comb.fixed=FALSE)
metareg(m.corRelia,SD_Reliability)
subgroupRelia<-update.meta(m.corRelia, byvar=SD_Reliability, comb.random = TRUE, comb.fixed = FALSE)
subgroupRelia
