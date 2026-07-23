* Encoding: UTF-8.
*Authors said A one-sample t test indicated that partici-
pants did not score significantly lower or higher
than the midpoint (3.00) of the scale...Overall, the students expressed a moderately
high level of engagement when conducting re-
search related to the pandemic (M 4.19, SD
0.97; t(11) 4.27, p .001, d 1.23

*As our key result is t(11)=4.27, p=.001, d=1.23

DATASET ACTIVATE DataSet2.
T-TEST
  /TESTVAL=3
  /MISSING=ANALYSIS
  /VARIABLES=Engagement_Avg
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

*Result=reproduced



*Extra result: For applicability of social
psychology, the students strongly endorsed the
ideas that social psychology is applicable and
important for understanding real life events
such as the pandemic (M 4.64, SD 0.35;
t(11) 16.03, p .001, d 4.63

*so, t(11)=16.03, p=.001, d=4.63 means same formula but just the different variable

T-TEST
  /TESTVAL=3
  /MISSING=ANALYSIS
  /VARIABLES=SocialPsychRealWorld_Avg
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).
