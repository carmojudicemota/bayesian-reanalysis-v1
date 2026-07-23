* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.

*Cronbach alpha for identity as scientist = .96.
RELIABILITY
  /VARIABLES=ScientistIdentity_1_SelfImage ScientistIdentity_2_Community 
    ScientistIdentity_3_Reflection ScientistIdentity_4_ThinkOfSelf ScientistIdentity_5_Belong 
    ScientistIdentity_6_AmScientist
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

*Mean identity as scientist.
COMPUTE IdentityAsScientist_Avg=MEAN(ScientistIdentity_1_SelfImage,ScientistIdentity_2_Community,
    ScientistIdentity_3_Reflection,ScientistIdentity_4_ThinkOfSelf,ScientistIdentity_5_Belong,
    ScientistIdentity_6_AmScientist).
EXECUTE.

*Cronbach alpha for competence = .78.
RELIABILITY
  /VARIABLES=Competence_1_LocateResearch Competence_2_UnderstandResearch 
    Competence_3_ConductResearch Competence_4_InterpretData Competence_5_AnalyzeStrengths 
    Competence_6_AnalyzeWeaknesses
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

*Mean competence.
COMPUTE Competence_Avg=MEAN(Competence_1_LocateResearch,Competence_2_UnderstandResearch,
    Competence_3_ConductResearch,Competence_4_InterpretData,Competence_5_AnalyzeStrengths,
    Competence_7_AnalyzeWeaknesses).
EXECUTE.

*Cronbach alpha for social psych in real world/value of social psych = .73.
RELIABILITY
  /VARIABLES=SocPsychRealWorld1_RealWorldImplications SocPsychRealWorld2_IRecognizeSocPsychInWorld 
    SocPsychRealWorld_3_ICanIdentifySocPsychInRealWorld ValueSocPsych_1_EffectsofPandemic 
    ValueSocPsych_2_CreatingInterventions ValueSocPsych_3_PreventingConsequences
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

*Mean value of social psych in real world.
COMPUTE SocialPsychRealWorld_Avg=MEAN(ValueSocPsych_1_EffectsofPandemic,
    ValueSocPsych_2_CreatingInterventions,ValueSocPsych_3_PreventingConsequences,
    SocPsychRealWorld1_RealWorldImplications,SocPsychRealWorld2_IRecognizeSocPsychInWorld,
    SocPsychRealWorld_3_ICanIdentifySocPsychInRealWorld).
EXECUTE.

*Recoding reverse scored emotional response and engagement items.
RECODE EmotionalResponse_2_Helpless EmotionalResponse_3_IncreasedAnxiety 
    EmotionalResponse_8_DecreasedEngagement (1=5) (2=4) (3=3) (4=2) (5=1) INTO 
    EmotionalResponse_2_Helpless_ReverseScored EmotionalResponse_3_IncreasedAnxiety_ReverseScored 
    EmotionalResponse_8_DecreasedEngagement_ReverseScored.
EXECUTE.

*Cronbach alpha for emotion-related items = .69.
RELIABILITY
  /VARIABLES=EmotionalResponse_1_InControl EmotionalResponse_4_DecreasedAnxiety 
    EmotionalResponse_5_ProductiveWayChannelNegEmo EmotionalResponse_6_ProductiveWayChannelPosEmo 
    EmotionalResponse_2_Helpless_ReverseScored EmotionalResponse_3_IncreasedAnxiety_ReverseScored
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

*Cronbach alpha for emotion items without helpless = .75.
RELIABILITY
  /VARIABLES=EmotionalResponse_1_InControl EmotionalResponse_4_DecreasedAnxiety 
    EmotionalResponse_5_ProductiveWayChannelNegEmo EmotionalResponse_6_ProductiveWayChannelPosEmo 
    EmotionalResponse_3_IncreasedAnxiety_ReverseScored
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

*Cronbach alpha for engagement items = .83.
RELIABILITY
  /VARIABLES=EmotionalResponse_7_IncreasedEngagement EmotionalResponse_9_MoreKnowledgeable 
    EmotionalResponse_8_DecreasedEngagement_ReverseScored
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

*Mean engagement.
COMPUTE Engagement_Avg=MEAN(EmotionalResponse_7_IncreasedEngagement,
    EmotionalResponse_9_MoreKnowledgeable,EmotionalResponse_8_DecreasedEngagement_ReverseScored).
EXECUTE.

*Mean managing emotions response.
COMPUTE ManageEmotions_Avg=MEAN(EmotionalResponse_1_InControl,
    EmotionalResponse_4_DecreasedAnxiety,EmotionalResponse_5_ProductiveWayChannelNegEmo,
    EmotionalResponse_6_ProductiveWayChannelPosEmo,EmotionalResponse_3_IncreasedAnxiety_ReverseScored).
EXECUTE.

*Descriptives.
DESCRIPTIVES VARIABLES=IdentityAsScientist_Avg Competence_Avg Engagement_Avg 
    ManageEmotions_Avg SocialPsychRealWorld_Avg GradDegreePsych OtherGradDegree 
    Community_1_PartOfSocPsychCommunity Community_2_InterestedInSocPsychResearch
  /STATISTICS=MEAN STDDEV RANGE MIN MAX.

*Cronbach alpha for community = .37.
RELIABILITY
  /VARIABLES=Community_1_PartOfSocPsychCommunity Community_2_InterestedInSocPsychResearch
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA.

FREQUENCIES VARIABLES=IdentityAsScientist_Avg Competence_Avg Engagement_Avg 
    SocialPsychRealWorld_Avg ManageEmotions_Avg Community_1_PartOfSocPsychCommunity 
    Community_2_InterestedInSocPsychResearch
  /ORDER=ANALYSIS.
