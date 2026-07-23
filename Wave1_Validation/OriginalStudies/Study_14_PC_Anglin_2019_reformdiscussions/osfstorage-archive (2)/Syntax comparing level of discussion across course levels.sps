* Encoding: UTF-8.
FREQUENCIES VARIABLES=recoded_course_multiples
  /ORDER=ANALYSIS.


ONEWAY topics BY recoded_course_multiples
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05).


