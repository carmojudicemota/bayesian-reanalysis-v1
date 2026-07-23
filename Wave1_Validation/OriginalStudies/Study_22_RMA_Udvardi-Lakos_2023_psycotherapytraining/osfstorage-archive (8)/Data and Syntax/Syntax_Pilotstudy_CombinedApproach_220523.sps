* Encoding: UTF-8.
*Syntax for Data File "LKT_Pilotstudy_Data_for_publication_220523".
*Codebook in "LKT_Codebook_220523".

#Step 1: Recoding variables.

##Need for cognition.
RECODE NFC_SQ001 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ002 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ003 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ005 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ011 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ012 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ018 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ020 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ025 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ026 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ027 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ028 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ029 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').
RECODE NFC_SQ033 ('A1'='3') ('A2'='2') ('A3'='1') ('A4'='0') ('A5'='-1') ('A6'='-2') ('A7'='-3') (ELSE='-99').

RECODE NFC_SQ004 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ006 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ007 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ008 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ009 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ010 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ013 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ014 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ015 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ016 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ017 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ019 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ021 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ022 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ023 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ024 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ030 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ031 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
RECODE NFC_SQ032 ('A1'='-3') ('A2'='-2') ('A3'='-1') ('A4'='0') ('A5'='1') ('A6'='2') ('A7'='3') (ELSE='-99').
EXECUTE.


##Posttest declarative knowledge.
RECODE RecallMDL1_SQ001 ('A1'='1') ('A2'='0') (ELSE='0').
RECODE RecallMDL1_SQ002 ('A1'='0') ('A2'='1') (ELSE='0').
RECODE RecallMDL1_SQ003 ('A1'='0') ('A2'='1') (ELSE='0').
RECODE RecallMDL1_SQ004 ('A1'='1') ('A2'='0') (ELSE='0').
RECODE RecallMDL1_SQ005 ('A1'='1') ('A2'='0') (ELSE='0').
RECODE RecallMDL1_SQ006 ('A1'='0') ('A2'='1') (ELSE='0').
RECODE RecallMDL1_SQ007 ('A1'='1') ('A2'='0') (ELSE='0').
RECODE RecallMDL1_SQ008 ('A1'='1') ('A2'='0') (ELSE='0').

RECODE RecallMDL2_SQ001 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallMDL2_SQ002 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallMDL2_SQ003 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallMDL2_SQ004 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallMDL2_SQ005 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallMDL2_SQ006 ('A1'='0') ('A2'='1') (ELSE='-99').

RECODE RecallMDL3_SQ001 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallMDL3_SQ002 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallMDL3_SQ003 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallMDL3_SQ004 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallMDL3_SQ005 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallMDL3_SQ006 ('A1'='1') ('A2'='0') (ELSE='-99').

##Posttest content knowledge.
RECODE RecallFW1_SQ001 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ002 ('A1'='0') ('A2'='0') ('A3'='1') (ELSE='-99').
RECODE RecallFW1_SQ003 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ004 ('A1'='0') ('A2'='1') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ005 ('A1'='0') ('A2'='1') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ006 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ007 ('A1'='0') ('A2'='0') ('A3'='1') (ELSE='-99').
RECODE RecallFW1_SQ008 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ009 ('A1'='0') ('A2'='1') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ010 ('A1'='0') ('A2'='0') ('A3'='1') (ELSE='-99').
RECODE RecallFW1_SQ011 ('A1'='0') ('A2'='1') ('A3'='0') (ELSE='-99').
RECODE RecallFW1_SQ012 ('A1'='0') ('A2'='1') ('A3'='0') (ELSE='-99').

RECODE RecallFW2_SQ001 ('A1'='0') ('A2'='1') ('A3'='0') (ELSE='-99').
RECODE RecallFW2_SQ002 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').
RECODE RecallFW2_SQ003 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').
RECODE RecallFW2_SQ004 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').
RECODE RecallFW2_SQ005 ('A1'='0') ('A2'='1') ('A3'='0') (ELSE='-99').
RECODE RecallFW2_SQ006 ('A1'='1') ('A2'='0') ('A3'='0') (ELSE='-99').


##Posttest presentation skills.
RECODE RecallPR1_SQ001 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR1_SQ002 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallPR1_SQ003 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallPR1_SQ004 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR1_SQ005 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR1_SQ006 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallPR1_SQ007 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR1_SQ008 ('A1'='0') ('A2'='1') (ELSE='-99').

RECODE RecallPR2_SQ001 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallPR2_SQ002 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR2_SQ003 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR2_SQ004 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR2_SQ005 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallPR2_SQ006 ('A1'='0') ('A2'='1') (ELSE='-99').

RECODE RecallPR3_SQ001 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR3_SQ002 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallPR3_SQ003 ('A1'='0') ('A2'='1') (ELSE='-99').
RECODE RecallPR3_SQ004 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR3_SQ005 ('A1'='1') ('A2'='0') (ELSE='-99').
RECODE RecallPR3_SQ006 ('A1'='1') ('A2'='0') (ELSE='-99').

###############################.
#Step 2: Calculating means/sum scores and adding dummy variables for contrasts.

COMPUTE NFC_All = SUM(NFC_SQ001,NFC_SQ002,NFC_SQ003,NFC_SQ004,NFC_SQ005,NFC_SQ006,NFC_SQ007,NFC_SQ008,NFC_SQ009,NFC_SQ010,NFC_SQ011,NFC_SQ012,NFC_SQ013,NFC_SQ014,NFC_SQ015,NFC_SQ016,NFC_SQ017,NFC_SQ018,NFC_SQ019,NFC_SQ020,NFC_SQ021,NFC_SQ022,NFC_SQ023,NFC_SQ024,NFC_SQ025,NFC_SQ026,NFC_SQ027,NFC_SQ028,NFC_SQ029,NFC_SQ030,NFC_SQ031,NFC_SQ032,NFC_SQ033).

COMPUTE VorwissenMDL = SUM(VorC2,VorC4,VorC5).
COMPUTE VorwissenPR = SUM(VorC1,VorC3,VorC6).
COMPUTE FadingAll = SUM(Fading1C, Fading2C,Fading3C,Fading4C).

COMPUTE RecallMDLS1 = SUM(RecallMDL1_SQ001,RecallMDL1_SQ002,RecallMDL1_SQ003,RecallMDL1_SQ004,RecallMDL1_SQ005,RecallMDL1_SQ006,RecallMDL1_SQ007,RecallMDL1_SQ008).
COMPUTE RecallMDLS2 = SUM(RecallMDL2_SQ001,RecallMDL2_SQ002,RecallMDL2_SQ003,RecallMDL2_SQ004,RecallMDL2_SQ005,RecallMDL2_SQ006).
COMPUTE RecallMDLS3 = SUM(RecallMDL3_SQ001,RecallMDL3_SQ002,RecallMDL3_SQ003,RecallMDL3_SQ004,RecallMDL3_SQ005,RecallMDL3_SQ006).
COMPUTE RecallMDL_All = SUM(RecallMDLS1,RecallMDLS2,RecallMDLS3).

COMPUTE RecallFWS1 = SUM(RecallFW1_SQ001,RecallFW1_SQ002,RecallFW1_SQ003,RecallFW1_SQ004,RecallFW1_SQ005,RecallFW1_SQ006,RecallFW1_SQ007,RecallFW1_SQ008,RecallFW1_SQ009,RecallFW1_SQ010,RecallFW1_SQ011,RecallFW1_SQ012).
COMPUTE RecallFWS2 = SUM(RecallFW2_SQ001,RecallFW2_SQ002,RecallFW2_SQ003,RecallFW2_SQ004,RecallFW2_SQ005,RecallFW2_SQ006).
COMPUTE RecallFWSum = SUM(RecallFWS1,RecallFWS2).

COMPUTE RecallPRS1 = SUM(RecallPR1_SQ001,RecallPR1_SQ002,RecallPR1_SQ003,RecallPR1_SQ004,RecallPR1_SQ005,RecallPR1_SQ006,RecallPR1_SQ007,RecallPR1_SQ008).
COMPUTE RecallPRS2 = SUM(RecallPR2_SQ001,RecallPR2_SQ002,RecallPR2_SQ003,RecallPR2_SQ004,RecallPR2_SQ005,RecallPR2_SQ006).
COMPUTE RecallPRS3 = SUM(RecallPR3_SQ001,RecallPR3_SQ002,RecallPR3_SQ003,RecallPR3_SQ004,RecallPR3_SQ005,RecallPR3_SQ006).
COMPUTE RecallPR_All = SUM(RecallPRS1,RecallPRS2,RecallPRS3).

RECODE Bedingung ('1'=1) ('2'=1) ('3'=2) (MISSING=SYSMIS) INTO Dummy1.
VARIABLE LABELS  Dummy1 'MDL vs PR'.

RECODE Bedingung ('1'=1) ('2'=2) ('3'=SYSMIS) (MISSING=SYSMIS) INTO Dummy2.
VARIABLE LABELS  Dummy1 'Fading'.


###############################.
#Step 3: Descriptives.

FREQUENCIES VARIABLES=Geschlecht Abschluss
  /ORDER=ANALYSIS.

DESCRIPTIVES VARIABLES=Alter YOE
  /STATISTICS=MEAN STDDEV MIN MAX.


###############################.
#Step 4: Inferential data analysis.
##Differences in demographics and prior knowledge between conditions.
GLM Alter YOE NFC_Summe VorwissenAll BY Bedingung 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /POSTHOC=Bedingung(SIDAK) 
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05) 
  /DESIGN= Bedingung.

GLM VorwissenPR VorwissenMDL BY Bedingung 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /POSTHOC=Bedingung(SIDAK) 
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05) 
  /DESIGN= Bedingung.

##Differences in judgement of learning materials between conditions.
GLM Interesse Anstrengung Muehe Schwierigkeit BY Bedingung
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN= Bedingung.

GLM Interesse Anstrengung Muehe Schwierigkeit BY Dummy1
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ
  /CRITERIA=ALPHA(.05)
  /DESIGN= Dummy1.
