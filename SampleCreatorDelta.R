##Run first if starting with the raw data

library(edfReader)
library(ggplot2)
library(eegkit) ##if on Mac, requires xquartz for rgl package, installed from xquartz.org

library(tidyverse)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#import all PD and HC EEG files----
pd3.Off <- readEdfHeader("ds002778-1.0.1/sub-pd3/ses-off/eeg/sub-pd3_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd5.Off <- readEdfHeader("ds002778-1.0.1/sub-pd5/ses-off/eeg/sub-pd5_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd6.Off <- readEdfHeader("ds002778-1.0.1/sub-pd6/ses-off/eeg/sub-pd6_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd9.Off <- readEdfHeader("ds002778-1.0.1/sub-pd9/ses-off/eeg/sub-pd9_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd11.Off <- readEdfHeader("ds002778-1.0.1/sub-pd11/ses-off/eeg/sub-pd11_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd12.Off <- readEdfHeader("ds002778-1.0.1/sub-pd12/ses-off/eeg/sub-pd12_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd13.Off <- readEdfHeader("ds002778-1.0.1/sub-pd13/ses-off/eeg/sub-pd13_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd14.Off <- readEdfHeader("ds002778-1.0.1/sub-pd14/ses-off/eeg/sub-pd14_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd16.Off <- readEdfHeader("ds002778-1.0.1/sub-pd16/ses-off/eeg/sub-pd16_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd17.Off <- readEdfHeader("ds002778-1.0.1/sub-pd17/ses-off/eeg/sub-pd17_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd19.Off <- readEdfHeader("ds002778-1.0.1/sub-pd19/ses-off/eeg/sub-pd19_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd22.Off <- readEdfHeader("ds002778-1.0.1/sub-pd22/ses-off/eeg/sub-pd22_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd23.Off <- readEdfHeader("ds002778-1.0.1/sub-pd23/ses-off/eeg/sub-pd23_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd26.Off <- readEdfHeader("ds002778-1.0.1/sub-pd26/ses-off/eeg/sub-pd26_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()
pd28.Off <- readEdfHeader("ds002778-1.0.1/sub-pd28/ses-off/eeg/sub-pd28_ses-off_task-rest_eeg.bdf") %>% readEdfSignals()

hc1 <- readEdfHeader("ds002778-1.0.1/sub-hc1/ses-hc/eeg/sub-hc1_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc2 <- readEdfHeader("ds002778-1.0.1/sub-hc2/ses-hc/eeg/sub-hc2_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc4 <- readEdfHeader("ds002778-1.0.1/sub-hc4/ses-hc/eeg/sub-hc4_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc7 <- readEdfHeader("ds002778-1.0.1/sub-hc7/ses-hc/eeg/sub-hc7_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals() 
hc8 <- readEdfHeader("ds002778-1.0.1/sub-hc8/ses-hc/eeg/sub-hc8_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc10 <- readEdfHeader("ds002778-1.0.1/sub-hc10/ses-hc/eeg/sub-hc10_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc18 <- readEdfHeader("ds002778-1.0.1/sub-hc18/ses-hc/eeg/sub-hc18_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc20 <- readEdfHeader("ds002778-1.0.1/sub-hc20/ses-hc/eeg/sub-hc20_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc21 <- readEdfHeader("ds002778-1.0.1/sub-hc21/ses-hc/eeg/sub-hc21_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc24 <- readEdfHeader("ds002778-1.0.1/sub-hc24/ses-hc/eeg/sub-hc24_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc25 <- readEdfHeader("ds002778-1.0.1/sub-hc25/ses-hc/eeg/sub-hc25_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc29 <- readEdfHeader("ds002778-1.0.1/sub-hc29/ses-hc/eeg/sub-hc29_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc30 <- readEdfHeader("ds002778-1.0.1/sub-hc30/ses-hc/eeg/sub-hc30_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc31 <- readEdfHeader("ds002778-1.0.1/sub-hc31/ses-hc/eeg/sub-hc31_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc32 <- readEdfHeader("ds002778-1.0.1/sub-hc32/ses-hc/eeg/sub-hc32_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()
hc33 <- readEdfHeader("ds002778-1.0.1/sub-hc33/ses-hc/eeg/sub-hc33_ses-hc_task-rest_eeg.bdf") %>% readEdfSignals()

hc.01signal <- data.frame(time = seq(0, nrow(data.frame(hc1$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc1$Fp1$signal, AF3 = hc1$AF3$signal, F7 = hc1$F7$signal, 
                          F3 = hc1$F3$signal, FC1 = hc1$FC1$signal, FC5 = hc1$FC5$signal, T7 = hc1$T7$signal,C3 = hc1$C3$signal, 
                          CP1 = hc1$CP1$signal, CP5 = hc1$CP5$signal, P7 = hc1$P7$signal, P3 = hc1$P3$signal, Pz = hc1$Pz$signal, 
                          PO3 = hc1$PO3$signal, O1 = hc1$O1$signal, Oz = hc1$Oz$signal, O2 = hc1$O2$signal, PO4 = hc1$PO4$signal, 
                          P4 = hc1$P4$signal, P8 = hc1$P8$signal, CP6 = hc1$CP6$signal, CP2 = hc1$CP2$signal, C4 = hc1$C4$signal,
                          T8 = hc1$T8$signal, FC6 = hc1$FC6$signal, FC2 = hc1$FC2$signal, F4 = hc1$F4$signal, F8 = hc1$F8$signal, 
                          AF4 = hc1$AF4$signal, Fp2 = hc1$Fp2$signal, Fz = hc1$Fz$signal, Cz = hc1$Cz$signal)

hc.02signal <- data.frame(time = seq(0, nrow(data.frame(hc2$Fp1$signal))/512 -1/512, 1/512),
                          FP1 = hc2$Fp1$signal, AF3 = hc2$AF3$signal, F7 = hc2$F7$signal, 
                          F3 = hc2$F3$signal, FC1 = hc2$FC1$signal, FC5 = hc2$FC5$signal, T7 = hc2$T7$signal,C3 = hc2$C3$signal, 
                          CP1 = hc2$CP1$signal, CP5 = hc2$CP5$signal, P7 = hc2$P7$signal, P3 = hc2$P3$signal, Pz = hc2$Pz$signal, 
                          PO3 = hc2$PO3$signal, O1 = hc2$O1$signal, Oz = hc2$Oz$signal, O2 = hc2$O2$signal, PO4 = hc2$PO4$signal, 
                          P4 = hc2$P4$signal, P8 = hc2$P8$signal, CP6 = hc2$CP6$signal, CP2 = hc2$CP2$signal, C4 = hc2$C4$signal,
                          T8 = hc2$T8$signal, FC6 = hc2$FC6$signal, FC2 = hc2$FC2$signal, F4 = hc2$F4$signal, F8 = hc2$F8$signal, 
                          AF4 = hc2$AF4$signal, Fp2 = hc2$Fp2$signal, Fz = hc2$Fz$signal, Cz = hc2$Cz$signal)

hc.04signal <- data.frame(time = seq(0, nrow(data.frame(hc4$Fp1$signal))/512 -1/512, 1/512),
                          FP1 = hc4$Fp1$signal, AF3 = hc4$AF3$signal, F7 = hc4$F7$signal, 
                          F3 = hc4$F3$signal, FC1 = hc4$FC1$signal, FC5 = hc4$FC5$signal, T7 = hc4$T7$signal,C3 = hc4$C3$signal, 
                          CP1 = hc4$CP1$signal, CP5 = hc4$CP5$signal, P7 = hc4$P7$signal, P3 = hc4$P3$signal, Pz = hc4$Pz$signal, 
                          PO3 = hc4$PO3$signal, O1 = hc4$O1$signal, Oz = hc4$Oz$signal, O2 = hc4$O2$signal, PO4 = hc4$PO4$signal, 
                          P4 = hc4$P4$signal, P8 = hc4$P8$signal, CP6 = hc4$CP6$signal, CP2 = hc4$CP2$signal, C4 = hc4$C4$signal,
                          T8 = hc4$T8$signal, FC6 = hc4$FC6$signal, FC2 = hc4$FC2$signal, F4 = hc4$F4$signal, F8 = hc4$F8$signal, 
                          AF4 = hc4$AF4$signal, Fp2 = hc4$Fp2$signal, Fz = hc4$Fz$signal, Cz = hc4$Cz$signal)

hc.07signal <- data.frame(time = seq(0, nrow(data.frame(hc7$Fp1$signal))/512 -1/512, 1/512),
                          FP1 = hc7$Fp1$signal, AF3 = hc7$AF3$signal, F7 = hc7$F7$signal, 
                          F3 = hc7$F3$signal, FC1 = hc7$FC1$signal, FC5 = hc7$FC5$signal, T7 = hc7$T7$signal,C3 = hc7$C3$signal, 
                          CP1 = hc7$CP1$signal, CP5 = hc7$CP5$signal, P7 = hc7$P7$signal, P3 = hc7$P3$signal, Pz = hc7$Pz$signal, 
                          PO3 = hc7$PO3$signal, O1 = hc7$O1$signal, Oz = hc7$Oz$signal, O2 = hc7$O2$signal, PO4 = hc7$PO4$signal, 
                          P4 = hc7$P4$signal, P8 = hc7$P8$signal, CP6 = hc7$CP6$signal, CP2 = hc7$CP2$signal, C4 = hc7$C4$signal,
                          T8 = hc7$T8$signal, FC6 = hc7$FC6$signal, FC2 = hc7$FC2$signal, F4 = hc7$F4$signal, F8 = hc7$F8$signal, 
                          AF4 = hc7$AF4$signal, Fp2 = hc7$Fp2$signal, Fz = hc7$Fz$signal, Cz = hc7$Cz$signal)

hc.08signal <- data.frame(time = seq(0, nrow(data.frame(hc8$Fp1$signal))/512 -1/512, 1/512),
                          FP1 = hc8$Fp1$signal, AF3 = hc8$AF3$signal, F7 = hc8$F7$signal, 
                          F3 = hc8$F3$signal, FC1 = hc8$FC1$signal, FC5 = hc8$FC5$signal, T7 = hc8$T7$signal,C3 = hc8$C3$signal, 
                          CP1 = hc8$CP1$signal, CP5 = hc8$CP5$signal, P7 = hc8$P7$signal, P3 = hc8$P3$signal, Pz = hc8$Pz$signal, 
                          PO3 = hc8$PO3$signal, O1 = hc8$O1$signal, Oz = hc8$Oz$signal, O2 = hc8$O2$signal, PO4 = hc8$PO4$signal, 
                          P4 = hc8$P4$signal, P8 = hc8$P8$signal, CP6 = hc8$CP6$signal, CP2 = hc8$CP2$signal, C4 = hc8$C4$signal,
                          T8 = hc8$T8$signal, FC6 = hc8$FC6$signal, FC2 = hc8$FC2$signal, F4 = hc8$F4$signal, F8 = hc8$F8$signal, 
                          AF4 = hc8$AF4$signal, Fp2 = hc8$Fp2$signal, Fz = hc8$Fz$signal, Cz = hc8$Cz$signal)

hc.10signal <- data.frame(time = seq(0, nrow(data.frame(hc10$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc10$Fp1$signal, AF3 = hc10$AF3$signal, F7 = hc10$F7$signal, 
                          F3 = hc10$F3$signal, FC1 = hc10$FC1$signal, FC5 = hc10$FC5$signal, T7 = hc10$T7$signal,C3 = hc10$C3$signal, 
                          CP1 = hc10$CP1$signal, CP5 = hc10$CP5$signal, P7 = hc10$P7$signal, P3 = hc10$P3$signal, Pz = hc10$Pz$signal, 
                          PO3 = hc10$PO3$signal, O1 = hc10$O1$signal, Oz = hc10$Oz$signal, O2 = hc10$O2$signal, PO4 = hc10$PO4$signal, 
                          P4 = hc10$P4$signal, P8 = hc10$P8$signal, CP6 = hc10$CP6$signal, CP2 = hc10$CP2$signal, C4 = hc10$C4$signal,
                          T8 = hc10$T8$signal, FC6 = hc10$FC6$signal, FC2 = hc10$FC2$signal, F4 = hc10$F4$signal, F8 = hc10$F8$signal, 
                          AF4 = hc10$AF4$signal, Fp2 = hc10$Fp2$signal, Fz = hc10$Fz$signal, Cz = hc10$Cz$signal)

hc.18signal <- data.frame(time = seq(0, nrow(data.frame(hc18$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc18$Fp1$signal, AF3 = hc18$AF3$signal, F7 = hc18$F7$signal, 
                          F3 = hc18$F3$signal, FC1 = hc18$FC1$signal, FC5 = hc18$FC5$signal, T7 = hc18$T7$signal,C3 = hc18$C3$signal, 
                          CP1 = hc18$CP1$signal, CP5 = hc18$CP5$signal, P7 = hc18$P7$signal, P3 = hc18$P3$signal, Pz = hc18$Pz$signal, 
                          PO3 = hc18$PO3$signal, O1 = hc18$O1$signal, Oz = hc18$Oz$signal, O2 = hc18$O2$signal, PO4 = hc18$PO4$signal, 
                          P4 = hc18$P4$signal, P8 = hc18$P8$signal, CP6 = hc18$CP6$signal, CP2 = hc18$CP2$signal, C4 = hc18$C4$signal,
                          T8 = hc18$T8$signal, FC6 = hc18$FC6$signal, FC2 = hc18$FC2$signal, F4 = hc18$F4$signal, F8 = hc18$F8$signal, 
                          AF4 = hc18$AF4$signal, Fp2 = hc18$Fp2$signal, Fz = hc18$Fz$signal, Cz = hc18$Cz$signal)

hc.20signal <- data.frame(time = seq(0, nrow(data.frame(hc20$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc20$Fp1$signal, AF3 = hc20$AF3$signal, F7 = hc20$F7$signal, 
                          F3 = hc20$F3$signal, FC1 = hc20$FC1$signal, FC5 = hc20$FC5$signal, T7 = hc20$T7$signal,C3 = hc20$C3$signal, 
                          CP1 = hc20$CP1$signal, CP5 = hc20$CP5$signal, P7 = hc20$P7$signal, P3 = hc20$P3$signal, Pz = hc20$Pz$signal, 
                          PO3 = hc20$PO3$signal, O1 = hc20$O1$signal, Oz = hc20$Oz$signal, O2 = hc20$O2$signal, PO4 = hc20$PO4$signal, 
                          P4 = hc20$P4$signal, P8 = hc20$P8$signal, CP6 = hc20$CP6$signal, CP2 = hc20$CP2$signal, C4 = hc20$C4$signal,
                          T8 = hc20$T8$signal, FC6 = hc20$FC6$signal, FC2 = hc20$FC2$signal, F4 = hc20$F4$signal, F8 = hc20$F8$signal, 
                          AF4 = hc20$AF4$signal, Fp2 = hc20$Fp2$signal, Fz = hc20$Fz$signal, Cz = hc20$Cz$signal)

hc.21signal <- data.frame(time = seq(0, nrow(data.frame(hc21$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc21$Fp1$signal, AF3 = hc21$AF3$signal, F7 = hc21$F7$signal, 
                          F3 = hc21$F3$signal, FC1 = hc21$FC1$signal, FC5 = hc21$FC5$signal, T7 = hc21$T7$signal,C3 = hc21$C3$signal, 
                          CP1 = hc21$CP1$signal, CP5 = hc21$CP5$signal, P7 = hc21$P7$signal, P3 = hc21$P3$signal, Pz = hc21$Pz$signal, 
                          PO3 = hc21$PO3$signal, O1 = hc21$O1$signal, Oz = hc21$Oz$signal, O2 = hc21$O2$signal, PO4 = hc21$PO4$signal, 
                          P4 = hc21$P4$signal, P8 = hc21$P8$signal, CP6 = hc21$CP6$signal, CP2 = hc21$CP2$signal, C4 = hc21$C4$signal,
                          T8 = hc21$T8$signal, FC6 = hc21$FC6$signal, FC2 = hc21$FC2$signal, F4 = hc21$F4$signal, F8 = hc21$F8$signal, 
                          AF4 = hc21$AF4$signal, Fp2 = hc21$Fp2$signal, Fz = hc21$Fz$signal, Cz = hc21$Cz$signal)


hc.24signal <- data.frame(time = seq(0, nrow(data.frame(hc24$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc24$Fp1$signal, AF3 = hc24$AF3$signal, F7 = hc24$F7$signal, 
                          F3 = hc24$F3$signal, FC1 = hc24$FC1$signal, FC5 = hc24$FC5$signal, T7 = hc24$T7$signal,C3 = hc24$C3$signal, 
                          CP1 = hc24$CP1$signal, CP5 = hc24$CP5$signal, P7 = hc24$P7$signal, P3 = hc24$P3$signal, Pz = hc24$Pz$signal, 
                          PO3 = hc24$PO3$signal, O1 = hc24$O1$signal, Oz = hc24$Oz$signal, O2 = hc24$O2$signal, PO4 = hc24$PO4$signal, 
                          P4 = hc24$P4$signal, P8 = hc24$P8$signal, CP6 = hc24$CP6$signal, CP2 = hc24$CP2$signal, C4 = hc24$C4$signal,
                          T8 = hc24$T8$signal, FC6 = hc24$FC6$signal, FC2 = hc24$FC2$signal, F4 = hc24$F4$signal, F8 = hc24$F8$signal, 
                          AF4 = hc24$AF4$signal, Fp2 = hc24$Fp2$signal, Fz = hc24$Fz$signal, Cz = hc24$Cz$signal)

hc.25signal <- data.frame(time = seq(0, nrow(data.frame(hc25$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc25$Fp1$signal, AF3 = hc25$AF3$signal, F7 = hc25$F7$signal, 
                          F3 = hc25$F3$signal, FC1 = hc25$FC1$signal, FC5 = hc25$FC5$signal, T7 = hc25$T7$signal,C3 = hc25$C3$signal, 
                          CP1 = hc25$CP1$signal, CP5 = hc25$CP5$signal, P7 = hc25$P7$signal, P3 = hc25$P3$signal, Pz = hc25$Pz$signal, 
                          PO3 = hc25$PO3$signal, O1 = hc25$O1$signal, Oz = hc25$Oz$signal, O2 = hc25$O2$signal, PO4 = hc25$PO4$signal, 
                          P4 = hc25$P4$signal, P8 = hc25$P8$signal, CP6 = hc25$CP6$signal, CP2 = hc25$CP2$signal, C4 = hc25$C4$signal,
                          T8 = hc25$T8$signal, FC6 = hc25$FC6$signal, FC2 = hc25$FC2$signal, F4 = hc25$F4$signal, F8 = hc25$F8$signal, 
                          AF4 = hc25$AF4$signal, Fp2 = hc25$Fp2$signal, Fz = hc25$Fz$signal, Cz = hc25$Cz$signal)

hc.29signal <- data.frame(time = seq(0, nrow(data.frame(hc29$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc29$Fp1$signal, AF3 = hc29$AF3$signal, F7 = hc29$F7$signal, 
                          F3 = hc29$F3$signal, FC1 = hc29$FC1$signal, FC5 = hc29$FC5$signal, T7 = hc29$T7$signal, C3 = hc29$C3$signal, 
                          CP1 = hc29$CP1$signal, CP5 = hc29$CP5$signal, P7 = hc29$P7$signal, P3 = hc29$P3$signal, Pz = hc29$Pz$signal, 
                          PO3 = hc29$PO3$signal, O1 = hc29$O1$signal, Oz = hc29$Oz$signal, O2 = hc29$O2$signal, PO4 = hc29$PO4$signal, 
                          P4 = hc29$P4$signal, P8 = hc29$P8$signal, CP6 = hc29$CP6$signal, CP2 = hc29$CP2$signal, C4 = hc29$C4$signal,
                          T8 = hc29$T8$signal, FC6 = hc29$FC6$signal, FC2 = hc29$FC2$signal, F4 = hc29$F4$signal, F8 = hc29$F8$signal, 
                          AF4 = hc29$AF4$signal, Fp2 = hc29$Fp2$signal, Fz = hc29$Fz$signal, Cz = hc29$Cz$signal)

hc.30signal <- data.frame(time = seq(0, nrow(data.frame(hc30$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc30$Fp1$signal, AF3 = hc30$AF3$signal, F7 = hc30$F7$signal, 
                          F3 = hc30$F3$signal, FC1 = hc30$FC1$signal, FC5 = hc30$FC5$signal, T7 = hc30$T7$signal,C3 = hc30$C3$signal, 
                          CP1 = hc30$CP1$signal, CP5 = hc30$CP5$signal, P7 = hc30$P7$signal, P3 = hc30$P3$signal, Pz = hc30$Pz$signal, 
                          PO3 = hc30$PO3$signal, O1 = hc30$O1$signal, Oz = hc30$Oz$signal, O2 = hc30$O2$signal, PO4 = hc30$PO4$signal, 
                          P4 = hc30$P4$signal, P8 = hc30$P8$signal, CP6 = hc30$CP6$signal, CP2 = hc30$CP2$signal, C4 = hc30$C4$signal,
                          T8 = hc30$T8$signal, FC6 = hc30$FC6$signal, FC2 = hc30$FC2$signal, F4 = hc30$F4$signal, F8 = hc30$F8$signal, 
                          AF4 = hc30$AF4$signal, Fp2 = hc30$Fp2$signal, Fz = hc30$Fz$signal, Cz = hc30$Cz$signal)

hc.31signal <- data.frame(time = seq(0, nrow(data.frame(hc31$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc31$Fp1$signal, AF3 = hc31$AF3$signal, F7 = hc31$F7$signal, 
                          F3 = hc31$F3$signal, FC1 = hc31$FC1$signal, FC5 = hc31$FC5$signal, T7 = hc31$T7$signal,C3 = hc31$C3$signal, 
                          CP1 = hc31$CP1$signal, CP5 = hc31$CP5$signal, P7 = hc31$P7$signal, P3 = hc31$P3$signal, Pz = hc31$Pz$signal, 
                          PO3 = hc31$PO3$signal, O1 = hc31$O1$signal, Oz = hc31$Oz$signal, O2 = hc31$O2$signal, PO4 = hc31$PO4$signal, 
                          P4 = hc31$P4$signal, P8 = hc31$P8$signal, CP6 = hc31$CP6$signal, CP2 = hc31$CP2$signal, C4 = hc31$C4$signal,
                          T8 = hc31$T8$signal, FC6 = hc31$FC6$signal, FC2 = hc31$FC2$signal, F4 = hc31$F4$signal, F8 = hc31$F8$signal, 
                          AF4 = hc31$AF4$signal, Fp2 = hc31$Fp2$signal, Fz = hc31$Fz$signal, Cz = hc31$Cz$signal)

hc.32signal <- data.frame(time = seq(0, nrow(data.frame(hc32$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc32$Fp1$signal, AF3 = hc32$AF3$signal, F7 = hc32$F7$signal, 
                          F3 = hc32$F3$signal, FC1 = hc32$FC1$signal, FC5 = hc32$FC5$signal, T7 = hc32$T7$signal,C3 = hc32$C3$signal, 
                          CP1 = hc32$CP1$signal, CP5 = hc32$CP5$signal, P7 = hc32$P7$signal, P3 = hc32$P3$signal, Pz = hc32$Pz$signal, 
                          PO3 = hc32$PO3$signal, O1 = hc32$O1$signal, Oz = hc32$Oz$signal, O2 = hc32$O2$signal, PO4 = hc32$PO4$signal, 
                          P4 = hc32$P4$signal, P8 = hc32$P8$signal, CP6 = hc32$CP6$signal, CP2 = hc32$CP2$signal, C4 = hc32$C4$signal,
                          T8 = hc32$T8$signal, FC6 = hc32$FC6$signal, FC2 = hc32$FC2$signal, F4 = hc32$F4$signal, F8 = hc32$F8$signal, 
                          AF4 = hc32$AF4$signal, Fp2 = hc32$Fp2$signal, Fz = hc32$Fz$signal, Cz = hc32$Cz$signal)

hc.33signal <- data.frame(time = seq(0, nrow(data.frame(hc33$Fp1$signal))/512 -1/512, 1/512), 
                          FP1 = hc33$Fp1$signal, AF3 = hc33$AF3$signal, F7 = hc33$F7$signal, F3 = hc33$F3$signal, 
                          FC1 = hc33$FC1$signal, FC5 = hc33$FC5$signal, T7 = hc33$T7$signal,C3 = hc33$C3$signal, 
                          CP1 = hc33$CP1$signal, CP5 = hc33$CP5$signal, P7 = hc33$P7$signal, P3 = hc33$P3$signal, 
                          Pz = hc33$Pz$signal, PO3 = hc33$PO3$signal, O1 = hc33$O1$signal, Oz = hc33$Oz$signal, 
                          O2 = hc33$O2$signal, PO4 = hc33$PO4$signal, P4 = hc33$P4$signal, P8 = hc33$P8$signal, 
                          CP6 = hc33$CP6$signal, CP2 = hc33$CP2$signal, C4 = hc33$C4$signal, T8 = hc33$T8$signal, 
                          FC6 = hc33$FC6$signal, FC2 = hc33$FC2$signal, F4 = hc33$F4$signal, F8 = hc33$F8$signal, 
                          AF4 = hc33$AF4$signal, Fp2 = hc33$Fp2$signal, Fz = hc33$Fz$signal, Cz = hc33$Cz$signal)



#Off medication sessions ----

pd.03.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd3.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd3.Off$Fp1$signal, AF3 = pd3.Off$AF3$signal, F7 = pd3.Off$F7$signal, 
                               F3 = pd3.Off$F3$signal, FC1 = pd3.Off$FC1$signal, FC5 = pd3.Off$FC5$signal, T7 = pd3.Off$T7$signal,C3 = pd3.Off$C3$signal, 
                               CP1 = pd3.Off$CP1$signal, CP5 = pd3.Off$CP5$signal, P7 = pd3.Off$P7$signal, P3 = pd3.Off$P3$signal, Pz = pd3.Off$Pz$signal, 
                               PO3 = pd3.Off$PO3$signal, O1 = pd3.Off$O1$signal, Oz = pd3.Off$Oz$signal, O2 = pd3.Off$O2$signal, PO4 = pd3.Off$PO4$signal, 
                               P4 = pd3.Off$P4$signal, P8 = pd3.Off$P8$signal, CP6 = pd3.Off$CP6$signal, CP2 = pd3.Off$CP2$signal, C4 = pd3.Off$C4$signal,
                               T8 = pd3.Off$T8$signal, FC6 = pd3.Off$FC6$signal, FC2 = pd3.Off$FC2$signal, F4 = pd3.Off$F4$signal, F8 = pd3.Off$F8$signal, 
                               AF4 = pd3.Off$AF4$signal, Fp2 = pd3.Off$Fp2$signal, Fz = pd3.Off$Fz$signal, Cz = pd3.Off$Cz$signal)

pd.05.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd5.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd5.Off$Fp1$signal, AF3 = pd5.Off$AF3$signal, F7 = pd5.Off$F7$signal, 
                               F3 = pd5.Off$F3$signal, FC1 = pd5.Off$FC1$signal, FC5 = pd5.Off$FC5$signal, T7 = pd5.Off$T7$signal,C3 = pd5.Off$C3$signal, 
                               CP1 = pd5.Off$CP1$signal, CP5 = pd5.Off$CP5$signal, P7 = pd5.Off$P7$signal, P3 = pd5.Off$P3$signal, Pz = pd5.Off$Pz$signal, 
                               PO3 = pd5.Off$PO3$signal, O1 = pd5.Off$O1$signal, Oz = pd5.Off$Oz$signal, O2 = pd5.Off$O2$signal, PO4 = pd5.Off$PO4$signal, 
                               P4 = pd5.Off$P4$signal, P8 = pd5.Off$P8$signal, CP6 = pd5.Off$CP6$signal, CP2 = pd5.Off$CP2$signal, C4 = pd5.Off$C4$signal,
                               T8 = pd5.Off$T8$signal, FC6 = pd5.Off$FC6$signal, FC2 = pd5.Off$FC2$signal, F4 = pd5.Off$F4$signal, F8 = pd5.Off$F8$signal, 
                               AF4 = pd5.Off$AF4$signal, Fp2 = pd5.Off$Fp2$signal, Fz = pd5.Off$Fz$signal, Cz = pd5.Off$Cz$signal)

pd.06.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd6.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd6.Off$Fp1$signal, AF3 = pd6.Off$AF3$signal, F7 = pd6.Off$F7$signal, 
                               F3 = pd6.Off$F3$signal, FC1 = pd6.Off$FC1$signal, FC5 = pd6.Off$FC5$signal, T7 = pd6.Off$T7$signal,C3 = pd6.Off$C3$signal, 
                               CP1 = pd6.Off$CP1$signal, CP5 = pd6.Off$CP5$signal, P7 = pd6.Off$P7$signal, P3 = pd6.Off$P3$signal, Pz = pd6.Off$Pz$signal, 
                               PO3 = pd6.Off$PO3$signal, O1 = pd6.Off$O1$signal, Oz = pd6.Off$Oz$signal, O2 = pd6.Off$O2$signal, PO4 = pd6.Off$PO4$signal, 
                               P4 = pd6.Off$P4$signal, P8 = pd6.Off$P8$signal, CP6 = pd6.Off$CP6$signal, CP2 = pd6.Off$CP2$signal, C4 = pd6.Off$C4$signal,
                               T8 = pd6.Off$T8$signal, FC6 = pd6.Off$FC6$signal, FC2 = pd6.Off$FC2$signal, F4 = pd6.Off$F4$signal, F8 = pd6.Off$F8$signal, 
                               AF4 = pd6.Off$AF4$signal, Fp2 = pd6.Off$Fp2$signal, Fz = pd6.Off$Fz$signal, Cz = pd6.Off$Cz$signal)

pd.09.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd9.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd9.Off$Fp1$signal, AF3 = pd9.Off$AF3$signal, F7 = pd9.Off$F7$signal, 
                               F3 = pd9.Off$F3$signal, FC1 = pd9.Off$FC1$signal, FC5 = pd9.Off$FC5$signal, T7 = pd9.Off$T7$signal,C3 = pd9.Off$C3$signal, 
                               CP1 = pd9.Off$CP1$signal, CP5 = pd9.Off$CP5$signal, P7 = pd9.Off$P7$signal, P3 = pd9.Off$P3$signal, Pz = pd9.Off$Pz$signal, 
                               PO3 = pd9.Off$PO3$signal, O1 = pd9.Off$O1$signal, Oz = pd9.Off$Oz$signal, O2 = pd9.Off$O2$signal, PO4 = pd9.Off$PO4$signal, 
                               P4 = pd9.Off$P4$signal, P8 = pd9.Off$P8$signal, CP6 = pd9.Off$CP6$signal, CP2 = pd9.Off$CP2$signal, C4 = pd9.Off$C4$signal,
                               T8 = pd9.Off$T8$signal, FC6 = pd9.Off$FC6$signal, FC2 = pd9.Off$FC2$signal, F4 = pd9.Off$F4$signal, F8 = pd9.Off$F8$signal, 
                               AF4 = pd9.Off$AF4$signal, Fp2 = pd9.Off$Fp2$signal, Fz = pd9.Off$Fz$signal, Cz = pd9.Off$Cz$signal)

pd.11.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd11.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd11.Off$Fp1$signal, AF3 = pd11.Off$AF3$signal, F7 = pd11.Off$F7$signal, 
                               F3 = pd11.Off$F3$signal, FC1 = pd11.Off$FC1$signal, FC5 = pd11.Off$FC5$signal, T7 = pd11.Off$T7$signal,C3 = pd11.Off$C3$signal, 
                               CP1 = pd11.Off$CP1$signal, CP5 = pd11.Off$CP5$signal, P7 = pd11.Off$P7$signal, P3 = pd11.Off$P3$signal, Pz = pd11.Off$Pz$signal, 
                               PO3 = pd11.Off$PO3$signal, O1 = pd11.Off$O1$signal, Oz = pd11.Off$Oz$signal, O2 = pd11.Off$O2$signal, PO4 = pd11.Off$PO4$signal, 
                               P4 = pd11.Off$P4$signal, P8 = pd11.Off$P8$signal, CP6 = pd11.Off$CP6$signal, CP2 = pd11.Off$CP2$signal, C4 = pd11.Off$C4$signal,
                               T8 = pd11.Off$T8$signal, FC6 = pd11.Off$FC6$signal, FC2 = pd11.Off$FC2$signal, F4 = pd11.Off$F4$signal, F8 = pd11.Off$F8$signal, 
                               AF4 = pd11.Off$AF4$signal, Fp2 = pd11.Off$Fp2$signal, Fz = pd11.Off$Fz$signal, Cz = pd11.Off$Cz$signal)

pd.12.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd12.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd12.Off$Fp1$signal, AF3 = pd12.Off$AF3$signal, F7 = pd12.Off$F7$signal, 
                               F3 = pd12.Off$F3$signal, FC1 = pd12.Off$FC1$signal, FC5 = pd12.Off$FC5$signal, T7 = pd12.Off$T7$signal,C3 = pd12.Off$C3$signal, 
                               CP1 = pd12.Off$CP1$signal, CP5 = pd12.Off$CP5$signal, P7 = pd12.Off$P7$signal, P3 = pd12.Off$P3$signal, Pz = pd12.Off$Pz$signal, 
                               PO3 = pd12.Off$PO3$signal, O1 = pd12.Off$O1$signal, Oz = pd12.Off$Oz$signal, O2 = pd12.Off$O2$signal, PO4 = pd12.Off$PO4$signal, 
                               P4 = pd12.Off$P4$signal, P8 = pd12.Off$P8$signal, CP6 = pd12.Off$CP6$signal, CP2 = pd12.Off$CP2$signal, C4 = pd12.Off$C4$signal,
                               T8 = pd12.Off$T8$signal, FC6 = pd12.Off$FC6$signal, FC2 = pd12.Off$FC2$signal, F4 = pd12.Off$F4$signal, F8 = pd12.Off$F8$signal, 
                               AF4 = pd12.Off$AF4$signal, Fp2 = pd12.Off$Fp2$signal, Fz = pd12.Off$Fz$signal, Cz = pd12.Off$Cz$signal)

pd.13.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd13.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd13.Off$Fp1$signal, AF3 = pd13.Off$AF3$signal, F7 = pd13.Off$F7$signal, 
                               F3 = pd13.Off$F3$signal, FC1 = pd13.Off$FC1$signal, FC5 = pd13.Off$FC5$signal, T7 = pd13.Off$T7$signal,C3 = pd13.Off$C3$signal, 
                               CP1 = pd13.Off$CP1$signal, CP5 = pd13.Off$CP5$signal, P7 = pd13.Off$P7$signal, P3 = pd13.Off$P3$signal, Pz = pd13.Off$Pz$signal, 
                               PO3 = pd13.Off$PO3$signal, O1 = pd13.Off$O1$signal, Oz = pd13.Off$Oz$signal, O2 = pd13.Off$O2$signal, PO4 = pd13.Off$PO4$signal, 
                               P4 = pd13.Off$P4$signal, P8 = pd13.Off$P8$signal, CP6 = pd13.Off$CP6$signal, CP2 = pd13.Off$CP2$signal, C4 = pd13.Off$C4$signal,
                               T8 = pd13.Off$T8$signal, FC6 = pd13.Off$FC6$signal, FC2 = pd13.Off$FC2$signal, F4 = pd13.Off$F4$signal, F8 = pd13.Off$F8$signal, 
                               AF4 = pd13.Off$AF4$signal, Fp2 = pd13.Off$Fp2$signal, Fz = pd13.Off$Fz$signal, Cz = pd13.Off$Cz$signal)

pd.14.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd14.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd14.Off$Fp1$signal, AF3 = pd14.Off$AF3$signal, F7 = pd14.Off$F7$signal, 
                               F3 = pd14.Off$F3$signal, FC1 = pd14.Off$FC1$signal, FC5 = pd14.Off$FC5$signal, T7 = pd14.Off$T7$signal,C3 = pd14.Off$C3$signal, 
                               CP1 = pd14.Off$CP1$signal, CP5 = pd14.Off$CP5$signal, P7 = pd14.Off$P7$signal, P3 = pd14.Off$P3$signal, Pz = pd14.Off$Pz$signal, 
                               PO3 = pd14.Off$PO3$signal, O1 = pd14.Off$O1$signal, Oz = pd14.Off$Oz$signal, O2 = pd14.Off$O2$signal, PO4 = pd14.Off$PO4$signal, 
                               P4 = pd14.Off$P4$signal, P8 = pd14.Off$P8$signal, CP6 = pd14.Off$CP6$signal, CP2 = pd14.Off$CP2$signal, C4 = pd14.Off$C4$signal,
                               T8 = pd14.Off$T8$signal, FC6 = pd14.Off$FC6$signal, FC2 = pd14.Off$FC2$signal, F4 = pd14.Off$F4$signal, F8 = pd14.Off$F8$signal, 
                               AF4 = pd14.Off$AF4$signal, Fp2 = pd14.Off$Fp2$signal, Fz = pd14.Off$Fz$signal, Cz = pd14.Off$Cz$signal)
pd.16.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd16.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd16.Off$Fp1$signal, AF3 = pd16.Off$AF3$signal, F7 = pd16.Off$F7$signal, 
                               F3 = pd16.Off$F3$signal, FC1 = pd16.Off$FC1$signal, FC5 = pd16.Off$FC5$signal, T7 = pd16.Off$T7$signal,C3 = pd16.Off$C3$signal, 
                               CP1 = pd16.Off$CP1$signal, CP5 = pd16.Off$CP5$signal, P7 = pd16.Off$P7$signal, P3 = pd16.Off$P3$signal, Pz = pd16.Off$Pz$signal, 
                               PO3 = pd16.Off$PO3$signal, O1 = pd16.Off$O1$signal, Oz = pd16.Off$Oz$signal, O2 = pd16.Off$O2$signal, PO4 = pd16.Off$PO4$signal, 
                               P4 = pd16.Off$P4$signal, P8 = pd16.Off$P8$signal, CP6 = pd16.Off$CP6$signal, CP2 = pd16.Off$CP2$signal, C4 = pd16.Off$C4$signal,
                               T8 = pd16.Off$T8$signal, FC6 = pd16.Off$FC6$signal, FC2 = pd16.Off$FC2$signal, F4 = pd16.Off$F4$signal, F8 = pd16.Off$F8$signal, 
                               AF4 = pd16.Off$AF4$signal, Fp2 = pd16.Off$Fp2$signal, Fz = pd16.Off$Fz$signal, Cz = pd16.Off$Cz$signal)


pd.17.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd17.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd17.Off$Fp1$signal, AF3 = pd17.Off$AF3$signal, F7 = pd17.Off$F7$signal, 
                               F3 = pd17.Off$F3$signal, FC1 = pd17.Off$FC1$signal, FC5 = pd17.Off$FC5$signal, T7 = pd17.Off$T7$signal,C3 = pd17.Off$C3$signal, 
                               CP1 = pd17.Off$CP1$signal, CP5 = pd17.Off$CP5$signal, P7 = pd17.Off$P7$signal, P3 = pd17.Off$P3$signal, Pz = pd17.Off$Pz$signal, 
                               PO3 = pd17.Off$PO3$signal, O1 = pd17.Off$O1$signal, Oz = pd17.Off$Oz$signal, O2 = pd17.Off$O2$signal, PO4 = pd17.Off$PO4$signal, 
                               P4 = pd17.Off$P4$signal, P8 = pd17.Off$P8$signal, CP6 = pd17.Off$CP6$signal, CP2 = pd17.Off$CP2$signal, C4 = pd17.Off$C4$signal,
                               T8 = pd17.Off$T8$signal, FC6 = pd17.Off$FC6$signal, FC2 = pd17.Off$FC2$signal, F4 = pd17.Off$F4$signal, F8 = pd17.Off$F8$signal, 
                               AF4 = pd17.Off$AF4$signal, Fp2 = pd17.Off$Fp2$signal, Fz = pd17.Off$Fz$signal, Cz = pd17.Off$Cz$signal)

pd.19.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd19.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd19.Off$Fp1$signal, AF3 = pd19.Off$AF3$signal, F7 = pd19.Off$F7$signal, 
                               F3 = pd19.Off$F3$signal, FC1 = pd19.Off$FC1$signal, FC5 = pd19.Off$FC5$signal, T7 = pd19.Off$T7$signal,C3 = pd19.Off$C3$signal, 
                               CP1 = pd19.Off$CP1$signal, CP5 = pd19.Off$CP5$signal, P7 = pd19.Off$P7$signal, P3 = pd19.Off$P3$signal, Pz = pd19.Off$Pz$signal, 
                               PO3 = pd19.Off$PO3$signal, O1 = pd19.Off$O1$signal, Oz = pd19.Off$Oz$signal, O2 = pd19.Off$O2$signal, PO4 = pd19.Off$PO4$signal, 
                               P4 = pd19.Off$P4$signal, P8 = pd19.Off$P8$signal, CP6 = pd19.Off$CP6$signal, CP2 = pd19.Off$CP2$signal, C4 = pd19.Off$C4$signal,
                               T8 = pd19.Off$T8$signal, FC6 = pd19.Off$FC6$signal, FC2 = pd19.Off$FC2$signal, F4 = pd19.Off$F4$signal, F8 = pd19.Off$F8$signal, 
                               AF4 = pd19.Off$AF4$signal, Fp2 = pd19.Off$Fp2$signal, Fz = pd19.Off$Fz$signal, Cz = pd19.Off$Cz$signal)

pd.22.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd22.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd22.Off$Fp1$signal, AF3 = pd22.Off$AF3$signal, F7 = pd22.Off$F7$signal, 
                               F3 = pd22.Off$F3$signal, FC1 = pd22.Off$FC1$signal, FC5 = pd22.Off$FC5$signal, T7 = pd22.Off$T7$signal,C3 = pd22.Off$C3$signal, 
                               CP1 = pd22.Off$CP1$signal, CP5 = pd22.Off$CP5$signal, P7 = pd22.Off$P7$signal, P3 = pd22.Off$P3$signal, Pz = pd22.Off$Pz$signal, 
                               PO3 = pd22.Off$PO3$signal, O1 = pd22.Off$O1$signal, Oz = pd22.Off$Oz$signal, O2 = pd22.Off$O2$signal, PO4 = pd22.Off$PO4$signal, 
                               P4 = pd22.Off$P4$signal, P8 = pd22.Off$P8$signal, CP6 = pd22.Off$CP6$signal, CP2 = pd22.Off$CP2$signal, C4 = pd22.Off$C4$signal,
                               T8 = pd22.Off$T8$signal, FC6 = pd22.Off$FC6$signal, FC2 = pd22.Off$FC2$signal, F4 = pd22.Off$F4$signal, F8 = pd22.Off$F8$signal, 
                               AF4 = pd22.Off$AF4$signal, Fp2 = pd22.Off$Fp2$signal, Fz = pd22.Off$Fz$signal, Cz = pd22.Off$Cz$signal)

pd.23.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd23.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd23.Off$Fp1$signal, AF3 = pd23.Off$AF3$signal, F7 = pd23.Off$F7$signal, 
                               F3 = pd23.Off$F3$signal, FC1 = pd23.Off$FC1$signal, FC5 = pd23.Off$FC5$signal, T7 = pd23.Off$T7$signal,C3 = pd23.Off$C3$signal, 
                               CP1 = pd23.Off$CP1$signal, CP5 = pd23.Off$CP5$signal, P7 = pd23.Off$P7$signal, P3 = pd23.Off$P3$signal, Pz = pd23.Off$Pz$signal, 
                               PO3 = pd23.Off$PO3$signal, O1 = pd23.Off$O1$signal, Oz = pd23.Off$Oz$signal, O2 = pd23.Off$O2$signal, PO4 = pd23.Off$PO4$signal, 
                               P4 = pd23.Off$P4$signal, P8 = pd23.Off$P8$signal, CP6 = pd23.Off$CP6$signal, CP2 = pd23.Off$CP2$signal, C4 = pd23.Off$C4$signal,
                               T8 = pd23.Off$T8$signal, FC6 = pd23.Off$FC6$signal, FC2 = pd23.Off$FC2$signal, F4 = pd23.Off$F4$signal, F8 = pd23.Off$F8$signal, 
                               AF4 = pd23.Off$AF4$signal, Fp2 = pd23.Off$Fp2$signal, Fz = pd23.Off$Fz$signal, Cz = pd23.Off$Cz$signal)

pd.26.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd26.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd26.Off$Fp1$signal, AF3 = pd26.Off$AF3$signal, F7 = pd26.Off$F7$signal, 
                               F3 = pd26.Off$F3$signal, FC1 = pd26.Off$FC1$signal, FC5 = pd26.Off$FC5$signal, T7 = pd26.Off$T7$signal,C3 = pd26.Off$C3$signal, 
                               CP1 = pd26.Off$CP1$signal, CP5 = pd26.Off$CP5$signal, P7 = pd26.Off$P7$signal, P3 = pd26.Off$P3$signal, Pz = pd26.Off$Pz$signal, 
                               PO3 = pd26.Off$PO3$signal, O1 = pd26.Off$O1$signal, Oz = pd26.Off$Oz$signal, O2 = pd26.Off$O2$signal, PO4 = pd26.Off$PO4$signal, 
                               P4 = pd26.Off$P4$signal, P8 = pd26.Off$P8$signal, CP6 = pd26.Off$CP6$signal, CP2 = pd26.Off$CP2$signal, C4 = pd26.Off$C4$signal,
                               T8 = pd26.Off$T8$signal, FC6 = pd26.Off$FC6$signal, FC2 = pd26.Off$FC2$signal, F4 = pd26.Off$F4$signal, F8 = pd26.Off$F8$signal, 
                               AF4 = pd26.Off$AF4$signal, Fp2 = pd26.Off$Fp2$signal, Fz = pd26.Off$Fz$signal, Cz = pd26.Off$Cz$signal)

pd.28.off.signal <- data.frame(time = seq(0, nrow(data.frame(pd28.Off$Fp1$signal))/512 -1/512, 1/512), 
                               FP1 = pd28.Off$Fp1$signal, AF3 = pd28.Off$AF3$signal, F7 = pd28.Off$F7$signal, 
                               F3 = pd28.Off$F3$signal, FC1 = pd28.Off$FC1$signal, FC5 = pd28.Off$FC5$signal, T7 = pd28.Off$T7$signal,C3 = pd28.Off$C3$signal, 
                               CP1 = pd28.Off$CP1$signal, CP5 = pd28.Off$CP5$signal, P7 = pd28.Off$P7$signal, P3 = pd28.Off$P3$signal, Pz = pd28.Off$Pz$signal, 
                               PO3 = pd28.Off$PO3$signal, O1 = pd28.Off$O1$signal, Oz = pd28.Off$Oz$signal, O2 = pd28.Off$O2$signal, PO4 = pd28.Off$PO4$signal, 
                               P4 = pd28.Off$P4$signal, P8 = pd28.Off$P8$signal, CP6 = pd28.Off$CP6$signal, CP2 = pd28.Off$CP2$signal, C4 = pd28.Off$C4$signal,
                               T8 = pd28.Off$T8$signal, FC6 = pd28.Off$FC6$signal, FC2 = pd28.Off$FC2$signal, F4 = pd28.Off$F4$signal, F8 = pd28.Off$F8$signal, 
                               AF4 = pd28.Off$AF4$signal, Fp2 = pd28.Off$Fp2$signal, Fz = pd28.Off$Fz$signal, Cz = pd28.Off$Cz$signal)

#bandpass butterworth filter of 1 between 0 and 4 ----

hc.01signal[,2:33] <- eegfilter(hc.01signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.02signal[,2:33] <- eegfilter(hc.02signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.04signal[,2:33] <- eegfilter(hc.04signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.07signal[,2:33] <- eegfilter(hc.07signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.08signal[,2:33] <- eegfilter(hc.08signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.10signal[,2:33] <- eegfilter(hc.10signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.18signal[,2:33] <- eegfilter(hc.18signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.20signal[,2:33] <- eegfilter(hc.20signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.21signal[,2:33] <- eegfilter(hc.21signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.24signal[,2:33] <- eegfilter(hc.24signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.25signal[,2:33] <- eegfilter(hc.25signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.29signal[,2:33] <- eegfilter(hc.29signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.30signal[,2:33] <- eegfilter(hc.30signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.31signal[,2:33] <- eegfilter(hc.31signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.32signal[,2:33] <- eegfilter(hc.32signal[,2:33], 512, lower = .005, upper = 4, order = 1)
hc.33signal[,2:33] <- eegfilter(hc.33signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.03.off.signal[,2:33] <- eegfilter(pd.03.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.05.off.signal[,2:33] <- eegfilter(pd.05.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.06.off.signal[,2:33] <- eegfilter(pd.06.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.09.off.signal[,2:33] <- eegfilter(pd.09.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.11.off.signal[,2:33] <- eegfilter(pd.11.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.12.off.signal[,2:33] <- eegfilter(pd.12.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.13.off.signal[,2:33] <- eegfilter(pd.13.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.14.off.signal[,2:33] <- eegfilter(pd.14.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.16.off.signal[,2:33] <- eegfilter(pd.16.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.17.off.signal[,2:33] <- eegfilter(pd.17.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.19.off.signal[,2:33] <- eegfilter(pd.19.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.22.off.signal[,2:33] <- eegfilter(pd.22.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.23.off.signal[,2:33] <- eegfilter(pd.23.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.26.off.signal[,2:33] <- eegfilter(pd.26.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)
pd.28.off.signal[,2:33] <- eegfilter(pd.28.off.signal[,2:33], 512, lower = .005, upper = 4, order = 1)

## save into windows and into raw delta folder

for(i in seq(0, nrow(data.frame(hc1$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.01signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.1.", i, ".csv", sep = "")))
}
for(i in seq(0, nrow(data.frame(hc2$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.02signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.2.", i, ".csv", sep = "")))
}
for(i in seq(0, nrow(data.frame(hc4$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.04signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.4.", i, ".csv", sep = "")))
}
for(i in seq(0, nrow(data.frame(hc7$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.07signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.7.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc8$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.08signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.8.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc10$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.10signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.10.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc18$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.18signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.18.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc20$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.20signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.20.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc21$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.21signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.21.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc24$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.24signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.24.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc25$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.25signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.25.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc29$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.29signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.29.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc30$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.30signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.30.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc31$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.31signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.31.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc32$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.32signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.32.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(hc33$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(hc.33signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/hc.33.", i, ".csv", sep = "")))
}


##Creating the 2s windows of data Off data ----


for(i in seq(0, nrow(data.frame(pd3.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.03.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.3.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd5.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.05.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.5.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd6.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.06.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.6.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd9.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.09.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.9.", i, ".csv", sep = "")))
}


for(i in seq(0, nrow(data.frame(pd11.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.11.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.11.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd12.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.12.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.12.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd13.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.13.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.13.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd14.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.14.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.14.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd16.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.16.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.16.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd17.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.17.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.17.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd19.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.19.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.19.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd22.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.22.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.22.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd23.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.23.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.23.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd26.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.26.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.26.", i, ".csv", sep = "")))
}

for(i in seq(0, nrow(data.frame(pd28.Off$Fp1$signal))/512-2, 2)) {
  write_csv(dplyr::filter(as.data.frame(pd.28.off.signal), time >= i, time <i+2), (paste("~/PG-400/data2/rawFreqs/delta/pd.off.28.", i, ".csv", sep = "")))
}




