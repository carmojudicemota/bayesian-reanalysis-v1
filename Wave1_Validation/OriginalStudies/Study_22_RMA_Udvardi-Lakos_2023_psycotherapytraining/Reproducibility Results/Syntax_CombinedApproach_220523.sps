* Encoding: UTF-8.
*Syntax for data file "LKT_Data_for_publication_220523".
*Information about variables in codebook "LKT_Codebook_220523".

**THIS IS FOR THE EXPLORATORY ANALYSIS EB: texture and EB: Variability
#Step 1: Recoding of variables and calculation of means/sum scores.

#############################.
##Epistemic Beliefs.

###Connotative Aspects of Epistemological Beliefs (Stahl & Bromme, 2007).

RECODE CAEB1_SQ002 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ002_num.
RECODE CAEB1_SQ003 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ003_num.
RECODE CAEB1_SQ005 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ005_num.
RECODE CAEB1_SQ006 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ006_num.
RECODE CAEB1_SQ007 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ007_num.
RECODE CAEB1_SQ008 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ008_num.
RECODE CAEB1_SQ009 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ009_num.
RECODE CAEB1_SQ010 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ010_num.
RECODE CAEB1_SQ011 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ011_num.
RECODE CAEB1_SQ012 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ012_num.
RECODE CAEB1_SQ013 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ013_num.
RECODE CAEB1_SQ014 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ014_num.
RECODE CAEB1_SQ015 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ015_num.
RECODE CAEB1_SQ016 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ016_num.
RECODE CAEB1_SQ017 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ017_num.
RECODE CAEB1_SQ018 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ018_num.
RECODE CAEB1_SQ019 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB1_SQ019_num.

*I DID THIS BECAUSE DATA WAS BLANK FOR SOME REASON. after i did frequencies it showed me the data
FREQUENCIES VARIABLES=CAEB1_SQ002 TO CAEB1_SQ019.
FREQUENCIES VARIABLES=CAEB1_SQ002 TO CAEB1_SQ019.

RECODE CAEB2_SQ006 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ006_num.
RECODE CAEB2_SQ002 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ002_num.
RECODE CAEB2_SQ003 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ003_num.
RECODE CAEB2_SQ005 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ005_num.
RECODE CAEB2_SQ007 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ007_num.
RECODE CAEB2_SQ008 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ008_num.
RECODE CAEB2_SQ009 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ009_num.
RECODE CAEB2_SQ010 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ010_num.
RECODE CAEB2_SQ011 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ011_num.
RECODE CAEB2_SQ012 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ012_num.
RECODE CAEB2_SQ013 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ013_num.
RECODE CAEB2_SQ014 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ014_num.
RECODE CAEB2_SQ015 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ015_num.
RECODE CAEB2_SQ016 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ016_num.
RECODE CAEB2_SQ017 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ017_num.
RECODE CAEB2_SQ018 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ018_num.
RECODE CAEB2_SQ019 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO CAEB2_SQ019_num.

FREQUENCIES VARIABLES=CAEB2_SQ002 TO CAEB2_SQ019.

####Sum scores for two factors.
COMPUTE Texture1_sum = SUM(CAEB1_SQ003_num, CAEB1_SQ005_num, CAEB1_SQ007_num, CAEB1_SQ009_num, 
                           CAEB1_SQ011_num, CAEB1_SQ012_num, CAEB1_SQ014_num, CAEB1_SQ015_num, 
                           CAEB1_SQ016_num).
EXECUTE.

COMPUTE Variab1_sum = SUM(CAEB1_SQ002_num, CAEB1_SQ006_num, CAEB1_SQ008_num, CAEB1_SQ010_num, 
                          CAEB1_SQ013_num, CAEB1_SQ017_num, CAEB1_SQ018_num, CAEB1_SQ019_num).
EXECUTE.

COMPUTE Texture2_sum = SUM(CAEB2_SQ003_num, CAEB2_SQ005_num, CAEB2_SQ007_num, CAEB2_SQ009_num, 
                           CAEB2_SQ011_num, CAEB2_SQ012_num, CAEB2_SQ014_num, CAEB2_SQ015_num, 
                           CAEB2_SQ016_num).
EXECUTE.

COMPUTE Variab2_sum = SUM(CAEB2_SQ002_num, CAEB2_SQ006_num, CAEB2_SQ008_num, CAEB2_SQ010_num, 
                          CAEB2_SQ013_num, CAEB2_SQ017_num, CAEB2_SQ018_num, CAEB2_SQ019_num).
EXECUTE.

*EXPLORATORY ANALYSIS: THIS IS FOR THE EB: texture and EB: Variability results in table 2

DESCRIPTIVES VARIABLES=Texture1_sum Variab1_sum Texture2_sum Variab2_sum
  /STATISTICS=MEAN STDDEV MIN MAX.

GLM Texture1_sum Texture2_sum
  /WSFACTOR=Zeitpunkt 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Zeitpunkt.

GLM Variab1_sum Variab2_sum
  /WSFACTOR=Zeitpunkt 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Zeitpunkt.

*SKIP ALL OF THIS UNTIL YOU REACH THE COMMENT: ***********Prior declarative knowledge.
###Epistemic orientation (Hefter et al., 2014).
####Three items reverse coded.
RECODE EO1_SQ001 (1=5) (2=4) (3=3) (4=2) (5=1) INTO EO1_SQ001_rev.
RECODE EO1_SQ002 (1=5) (2=4) (3=3) (4=2) (5=1) INTO EO1_SQ002_rev.
RECODE EO1_SQ004 (1=5) (2=4) (3=3) (4=2) (5=1) INTO EO1_SQ004_rev.

RECODE EO2_SQ001 (1=5) (2=4) (3=3) (4=2) (5=1) INTO EO2_SQ001_rev.
RECODE EO2_SQ002 (1=5) (2=4) (3=3) (4=2) (5=1) INTO EO2_SQ002_rev.
RECODE EO2_SQ004 (1=5) (2=4) (3=3) (4=2) (5=1) INTO EO2_SQ004_rev.
EXECUTE.


####Sum scores for pre- and posttest.
COMPUTE EO1_All_new = SUM(EO1_SQ001_rev, EO1_SQ002_rev, EO1_SQ003, EO1_SQ004_rev, EO1_SQ005).
COMPUTE EO2_All_new = SUM(EO2_SQ001_rev, EO2_SQ002_rev, EO2_SQ003, EO2_SQ004_rev, EO2_SQ005).
EXECUTE.


#############################.
#############################.

##Need for cognition.

RECODE NFC_SQ001 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ002 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ003 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ009 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ012 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ014 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ015 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ016 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ017 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ019 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ022 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ023 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ028 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ029 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ030 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).
RECODE NFC_SQ031 (1=3) (2=2) (3=1) (4=0) (5=-1) (6=-2) (7=-3) (ELSE=-99).


###Sum score for overall Need for cognition.
COMPUTE NFC_All = SUM(NFC_SQ001,NFC_SQ002,NFC_SQ003,NFC_SQ009,NFC_SQ012,NFC_SQ014,NFC_SQ015,NFC_SQ016,NFC_SQ017,NFC_SQ019,NFC_SQ022,NFC_SQ023,NFC_SQ028,NFC_SQ029,NFC_SQ030,NFC_SQ031).

FREQUENCIES VARIABLES = NFC_All
  /STATISTICS = MEAN STDDEV MIN MAX
  /ORDER = ANALYSIS.


##Cognitive load, questionnaire by Klepsch et al. 2017.

RECODE CognitiveLoadEU_SQ001 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadEU_SQ002 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadEU_SQ003 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadEU_SQ004 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadEU_SQ005 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadEU_SQ006 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadEU_SQ007 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').

RECODE CognitiveLoadMDL_SQ001 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadMDL_SQ002 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadMDL_SQ003 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadMDL_SQ004 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadMDL_SQ005 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadMDL_SQ006 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadMDL_SQ007 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').

RECODE CognitiveLoadAF_SQ001 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadAF_SQ002 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadAF_SQ003 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadAF_SQ004 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadAF_SQ005 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadAF_SQ006 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').
RECODE CognitiveLoadAF_SQ007 ('A1'='1') ('A2'='2') ('A3'='3') ('A4'='4') ('A5'='5') ('A6'='6') ('A7'='7').

###Cognitive load scores for each load type and each skill chapter.
COMPUTE ICL_EU = SUM(CognitiveLoadEU_SQ001,CognitiveLoadEU_SQ002)/2.
COMPUTE ICL_MDL = SUM(CognitiveLoadMDL_SQ001,CognitiveLoadMDL_SQ002)/2.
COMPUTE ICL_AF = SUM(CognitiveLoadAF_SQ001,CognitiveLoadAF_SQ002)/2.
COMPUTE GCL_EU = SUM(CognitiveLoadEU_SQ003,CognitiveLoadEU_SQ004)/2.
COMPUTE GCL_MDL = SUM(CognitiveLoadMDL_SQ003,CognitiveLoadMDL_SQ004)/2.
COMPUTE GCL_AF = SUM(CognitiveLoadAF_SQ003,CognitiveLoadAF_SQ004)/2.
COMPUTE ECL_EU = SUM(CognitiveLoadEU_SQ005,CognitiveLoadEU_SQ006,CognitiveLoadEU_SQ007)/3.
COMPUTE ECL_MDL = SUM(CognitiveLoadMDL_SQ005,CognitiveLoadMDL_SQ006,CognitiveLoadMDL_SQ007)/3.
COMPUTE ECL_AF = SUM(CognitiveLoadAF_SQ005,CognitiveLoadAF_SQ006,CognitiveLoadAF_SQ007)/3.

###Overall cognitive load type scores across all three chapters.
COMPUTE ICL_All = SUM(ICL_EU,ICL_MDL,ICL_AF)/3.
COMPUTE GCL_All = SUM(GCL_EU,GCL_MDL,GCL_AF)/3.
COMPUTE ECL_All = SUM(ECL_EU,ECL_MDL,ECL_AF)/3.



***********Prior declarative knowledge.

RECODE VorwissenEU_SQ001 TO VorwissenEU_SQ015 (1 = 0) (0 = 1).
RECODE VorwissenMDL_SQ001 TO VorwissenMDL_SQ015 (1 = 0) (0 = 1).
RECODE VorwissenAD_SQ001 TO VorwissenAD_SQ015 (1 = 0) (0 = 1).
RECODE VorwissenFW_SQ001 TO VorwissenFW_SQ015 (1 = 0) (0 = 1).

###Scores for each level of prior knowledge (recall, comprehension, application) and overall scores.
COMPUTE Vor_EU_L1=(VorwissenEU_SQ001+VorwissenEU_SQ002+VorwissenEU_SQ003+VorwissenEU_SQ004+
    VorwissenEU_SQ005).
COMPUTE Vor_EU_L2=(VorwissenEU_SQ006+VorwissenEU_SQ007+VorwissenEU_SQ008+VorwissenEU_SQ009+
    VorwissenEU_SQ010).
COMPUTE Vor_EU_L3=(VorwissenEU_SQ011+VorwissenEU_SQ012+VorwissenEU_SQ013+VorwissenEU_SQ014+
    VorwissenEU_SQ015).
COMPUTE Vor_EU_All=SUM(Vor_EU_L1,Vor_EU_L2,Vor_EU_L3).
EXECUTE.

COMPUTE Vor_MDL_L1=(VorwissenMDL_SQ001+VorwissenMDL_SQ002+VorwissenMDL_SQ003+VorwissenMDL_SQ004+
    VorwissenMDL_SQ005).
COMPUTE Vor_MDL_L2=(VorwissenMDL_SQ006+VorwissenMDL_SQ007+VorwissenMDL_SQ008+VorwissenMDL_SQ009+
    VorwissenMDL_SQ010).
COMPUTE Vor_MDL_L3=(VorwissenMDL_SQ011+VorwissenMDL_SQ012+VorwissenMDL_SQ013+VorwissenMDL_SQ014+
    VorwissenMDL_SQ015).
COMPUTE Vor_MDL_All=SUM(Vor_MDL_L1,Vor_MDL_L2,Vor_MDL_L3).

COMPUTE Vor_AD_L1=(VorwissenAD_SQ001+VorwissenAD_SQ002+VorwissenAD_SQ003+VorwissenAD_SQ004+
    VorwissenAD_SQ005).
COMPUTE Vor_AD_L2=(VorwissenAD_SQ006+VorwissenAD_SQ007+VorwissenAD_SQ008+VorwissenAD_SQ009+
    VorwissenAD_SQ010).
COMPUTE Vor_AD_L3=(VorwissenAD_SQ011+VorwissenAD_SQ012+VorwissenAD_SQ013+VorwissenAD_SQ014+
    VorwissenAD_SQ015).
COMPUTE Vor_AD_All=SUM(Vor_AD_L1,Vor_AD_L2,Vor_AD_L3).

COMPUTE Vor_FW_Agg=(VorwissenFW_SQ001+VorwissenFW_SQ002+VorwissenFW_SQ003+VorwissenFW_SQ004+
    VorwissenFW_SQ005).
COMPUTE Vor_FW_Mehr=(VorwissenFW_SQ006+VorwissenFW_SQ007+VorwissenFW_SQ008+VorwissenFW_SQ009+
    VorwissenFW_SQ010).
COMPUTE Vor_FW_Noten=(VorwissenFW_SQ011+VorwissenFW_SQ012+VorwissenFW_SQ013+VorwissenFW_SQ014+
    VorwissenFW_SQ015).
COMPUTE Vor_FW_All=SUM(Vor_FW_Agg,Vor_FW_Mehr,Vor_FW_Noten).

*WHAT IS ABOVE IS COMBINED WITH WHAT IS BELOW, READ FURTHER TO UNDERSTAND

##Posttest declarative knowledge.

RECODE PosttestEU1_SQ001 TO PosttestEU1_SQ015 (1 = 0) (0 = 1).
RECODE PosttestMDL_SQ001 TO PosttestMDL_SQ015 (1 = 0) (0 = 1).
RECODE PosttestAD_SQ001 TO PosttestAD_SQ015 (1 = 0) (0 = 1).
RECODE PosttestFW_SQ001 TO PosttestFW_SQ015 (1 = 0) (0 = 1).


###Scores for each level of posttest declarative knowledge (recall, comprehension, application) and overall scores.
COMPUTE PT_EU_L1=(PosttestEU1_SQ001+PosttestEU1_SQ002+PosttestEU1_SQ003+PosttestEU1_SQ004+
    PosttestEU1_SQ005).
COMPUTE PT_EU_L2=(PosttestEU1_SQ006+PosttestEU1_SQ007+PosttestEU1_SQ008+PosttestEU1_SQ009+
    PosttestEU1_SQ010).
COMPUTE PT_EU_L3=(PosttestEU1_SQ011+PosttestEU1_SQ012+PosttestEU1_SQ013+PosttestEU1_SQ014+
    PosttestEU1_SQ015).
COMPUTE PT_EU_All=SUM(PT_EU_L1,PT_EU_L2,PT_EU_L3).

COMPUTE PT_MDL_L1=(PosttestMDL_SQ001+PosttestMDL_SQ002+PosttestMDL_SQ003+PosttestMDL_SQ004+
    PosttestMDL_SQ005).
COMPUTE PT_MDL_L2=(PosttestMDL_SQ006+PosttestMDL_SQ007+PosttestMDL_SQ008+PosttestMDL_SQ009+
    PosttestMDL_SQ010).
COMPUTE PT_MDL_L3=(PosttestMDL_SQ011+PosttestMDL_SQ012+PosttestMDL_SQ013+PosttestMDL_SQ014+
    PosttestMDL_SQ015).
COMPUTE PT_MDL_All=SUM(PT_MDL_L1,PT_MDL_L2,PT_MDL_L3).

COMPUTE PT_AD_L1=(PosttestAD_SQ001+PosttestAD_SQ002+PosttestAD_SQ003+PosttestAD_SQ004+
    PosttestAD_SQ005).
COMPUTE PT_AD_L2=(PosttestAD_SQ006+PosttestAD_SQ007+PosttestAD_SQ008+PosttestAD_SQ009+
    PosttestAD_SQ010).
COMPUTE PT_AD_L3=(PosttestAD_SQ011+PosttestAD_SQ012+PosttestAD_SQ013+PosttestAD_SQ014+
    PosttestAD_SQ015).
COMPUTE PT_AD_All=SUM(PT_AD_L1,PT_AD_L2,PT_AD_L3).

COMPUTE PT_FW_Agg=(PosttestFW_SQ001+PosttestFW_SQ002+PosttestFW_SQ003+PosttestFW_SQ004+
    PosttestFW_SQ005).
COMPUTE PT_FW_Mehr=(PosttestFW_SQ006+PosttestFW_SQ007+PosttestFW_SQ008+PosttestFW_SQ009+
    PosttestFW_SQ010).
COMPUTE PT_FW_Noten=(PosttestFW_SQ011+PosttestFW_SQ012+PosttestFW_SQ013+PosttestFW_SQ014+
    PosttestFW_SQ015).
COMPUTE PT_FW_All=SUM(PT_FW_Agg,PT_FW_Mehr,PT_FW_Noten).

###Overall scores for prior and posttest declarative knowledge across all three skill chapters.
COMPUTE Vorwissen_All=(Vor_EU_All + Vor_MDL_All + Vor_AD_All).
COMPUTE Posttest_All=(PT_EU_All + PT_MDL_All + PT_AD_All).

**EB KNOWLEDGE, This is for the key result replication

GLM Vor_EU_All PT_EU_All
  /WSFACTOR=Zeitpunkt 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Zeitpunkt.


*BEFORE THE TRAINING MEAN AND SD FOR EB KNOWLEDGE (KEY RESULT)

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=Vor_EU_All
  /STATISTICS=MEAN STDDEV.

*AFTER THE TRAINING MEAN AND SD FOR EB KNOWLEDGE (KEY RESULT)

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=PT_EU_All
  /STATISTICS=MEAN STDDEV.


* Create a variable counting the number of missing values across all variables.
COMPUTE missing_any = NMISS(ALL) > 0.
VARIABLE LABELS missing_any '1 = participant has missing data in any variable'.
VALUE LABELS missing_any 0 'Complete' 1 'Missing data'.
FORMATS missing_any (f1.0).
EXECUTE.

* See how many participants have missing data.
FREQUENCIES VARIABLES=missing_any.


* Flag participants with missing values in any of the specified variables.
COMPUTE missing_any = (NMISS(
    VorwissenEU_SQ001 TO VorwissenEU_SQ015 
    VorwissenMDL_SQ001 TO VorwissenMDL_SQ015 
    VorwissenAD_SQ001 TO VorwissenAD_SQ015 
    VorwissenFW_SQ001 TO VorwissenFW_SQ015
    PosttestEU1_SQ001 TO PosttestEU1_SQ015
    PosttestMDL_SQ001 TO PosttestMDL_SQ015
    PosttestAD_SQ001 TO PosttestAD_SQ015
    PosttestFW_SQ001 TO PosttestFW_SQ015
) > 0).

VARIABLE LABELS missing_any 'Missing in any of the specified 120 variables'.
VALUE LABELS missing_any 0 'Complete' 1 'Has Missing Data'.
FORMATS missing_any (F1.0).
EXECUTE.

* Count how many have missing values.
FREQUENCIES VARIABLES = missing_any.

