* Encoding: UTF-8.
**Demographics

FREQUENCIES VARIABLES=gender ethnicity race_1 race_2 race_3 race_4 race_5 race_6 race_6_TEXT
  /ORDER=ANALYSIS.

**Descriptives

DESCRIPTIVES VARIABLES=intro_fabricate intro_plagiarize intro_import_replicate intro_freq_replicate 
    intro_replication_failures intro_replication_debates intro_tone_reform intro_prereg 
    intro_openmaterials intro_opendata intro_selreport intro_phacking intro_HARKing intro_samplesize 
    intro_power intro_effectsize intro_overclaiming intro_generalize intro_WEIRD intro_pubbias 
    intro_polbias intro_theorybias intro_polhomogeneity intro_press2pub intro_authorship 
    intro_uncertainty intro_altexplain intro_conflictevidence
  /STATISTICS=MEAN STDDEV MIN MAX.


DESCRIPTIVES VARIABLES=adv_fabricate adv_plagiarize adv_import_replicate adv_freq_replicate 
    adv_replication_failures adv_replication_debates adv_tone_reform adv_prereg adv_openmaterials 
    adv_opendata adv_selreport adv_phacking adv_HARKing adv_samplesize adv_power adv_effectsize 
    adv_overclaiming adv_generalize adv_WEIRD adv_pubbias adv_polbias adv_theorybias adv_polhomogeneity 
    adv_press2pub adv_authorship adv_uncertainty adv_altexplain adv_conflictevidence
  /STATISTICS=MEAN STDDEV MIN MAX.


DESCRIPTIVES VARIABLES=grad_fabricate grad_plagiarize grad_import_replicate grad_freq_replicate 
    grad_replication_failures grad_replication_debates grad_tone_reform grad_prereg grad_openmaterials 
    grad_opendata grad_selreport grad_phacking grad_HARKing grad_samplesize grad_power grad_effectsize 
    grad_overclaiming grad_generalize grad_WEIRD grad_pubbias grad_polbias grad_theorybias 
    grad_polhomogeneity grad_press2pub grad_authorship grad_uncertainty grad_altexplain 
    grad_conflictevidence
  /STATISTICS=MEAN STDDEV MIN MAX.


RELIABILITY
  /VARIABLES=intro_fabricate intro_plagiarize intro_import_replicate intro_freq_replicate 
    intro_replication_failures intro_replication_debates intro_tone_reform intro_prereg 
    intro_openmaterials intro_opendata intro_selreport intro_phacking intro_HARKing intro_samplesize 
    intro_power intro_effectsize intro_overclaiming intro_generalize intro_WEIRD intro_pubbias 
    intro_polbias intro_theorybias intro_polhomogeneity intro_press2pub intro_authorship 
    intro_uncertainty intro_altexplain intro_conflictevidence
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=adv_fabricate adv_plagiarize adv_import_replicate adv_freq_replicate 
    adv_replication_failures adv_replication_debates adv_tone_reform adv_prereg adv_openmaterials 
    adv_opendata adv_selreport adv_phacking adv_HARKing adv_samplesize adv_power adv_effectsize 
    adv_overclaiming adv_generalize adv_WEIRD adv_pubbias adv_polbias adv_theorybias adv_polhomogeneity 
    adv_press2pub adv_authorship adv_uncertainty adv_altexplain adv_conflictevidence
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=grad_fabricate grad_plagiarize grad_import_replicate grad_freq_replicate 
    grad_replication_failures grad_replication_debates grad_tone_reform grad_prereg grad_openmaterials 
    grad_opendata grad_selreport grad_phacking grad_HARKing grad_samplesize grad_power grad_effectsize 
    grad_overclaiming grad_generalize grad_WEIRD grad_pubbias grad_polbias grad_theorybias 
    grad_polhomogeneity grad_press2pub grad_authorship grad_uncertainty grad_altexplain 
    grad_conflictevidence
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

COMPUTE Intro_topics=Mean(intro_fabricate, intro_plagiarize, intro_import_replicate, 
    intro_freq_replicate, intro_replication_failures, intro_replication_debates, intro_tone_reform, 
    intro_prereg, intro_openmaterials, intro_opendata, intro_selreport, intro_phacking, intro_HARKing, 
    intro_samplesize, intro_power, intro_effectsize, intro_overclaiming, intro_generalize, intro_WEIRD, 
    intro_pubbias, intro_polbias, intro_theorybias, intro_polhomogeneity, intro_press2pub, 
    intro_authorship, intro_uncertainty, intro_altexplain, intro_conflictevidence).
EXECUTE.

COMPUTE Adv_topics=Mean(adv_fabricate, adv_plagiarize, adv_import_replicate, adv_freq_replicate, 
    adv_replication_failures, adv_replication_debates, adv_tone_reform, adv_prereg, adv_openmaterials, 
    adv_opendata, adv_selreport, adv_phacking, adv_HARKing, adv_samplesize, adv_power, adv_effectsize, 
    adv_overclaiming, adv_generalize, adv_WEIRD, adv_pubbias, adv_polbias, adv_theorybias, 
    adv_polhomogeneity, adv_press2pub, adv_authorship, adv_uncertainty, adv_altexplain, 
    adv_conflictevidence).
EXECUTE.


COMPUTE Grad_topics=Mean(grad_fabricate, grad_plagiarize, grad_import_replicate, 
    grad_freq_replicate, grad_replication_failures, grad_replication_debates, grad_tone_reform, 
    grad_prereg, grad_openmaterials, grad_opendata, grad_selreport, grad_phacking, grad_HARKing, 
    grad_samplesize, grad_power, grad_effectsize, grad_overclaiming, grad_generalize, grad_WEIRD, 
    grad_pubbias, grad_polbias, grad_theorybias, grad_polhomogeneity, grad_press2pub, grad_authorship, 
    grad_uncertainty, grad_altexplain, grad_conflictevidence).
EXECUTE.


**Factor analyses

FACTOR
  /VARIABLES intro_fabricate intro_plagiarize intro_import_replicate intro_freq_replicate 
    intro_replication_failures intro_replication_debates intro_tone_reform intro_prereg 
    intro_openmaterials intro_opendata intro_selreport intro_phacking intro_HARKing intro_samplesize 
    intro_power intro_effectsize intro_overclaiming intro_generalize intro_WEIRD intro_pubbias 
    intro_polbias intro_theorybias intro_polhomogeneity intro_press2pub intro_authorship 
    intro_uncertainty intro_altexplain intro_conflictevidence
  /MISSING LISTWISE 
  /ANALYSIS intro_fabricate intro_plagiarize intro_import_replicate intro_freq_replicate 
    intro_replication_failures intro_replication_debates intro_tone_reform intro_prereg 
    intro_openmaterials intro_opendata intro_selreport intro_phacking intro_HARKing intro_samplesize 
    intro_power intro_effectsize intro_overclaiming intro_generalize intro_WEIRD intro_pubbias 
    intro_polbias intro_theorybias intro_polhomogeneity intro_press2pub intro_authorship 
    intro_uncertainty intro_altexplain intro_conflictevidence
  /PRINT INITIAL EXTRACTION ROTATION
  /PLOT EIGEN
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

FACTOR
  /VARIABLES adv_fabricate adv_plagiarize adv_import_replicate adv_freq_replicate 
    adv_replication_failures adv_replication_debates adv_tone_reform adv_prereg adv_openmaterials 
    adv_opendata adv_selreport adv_phacking adv_HARKing adv_samplesize adv_power adv_effectsize 
    adv_overclaiming adv_generalize adv_WEIRD adv_pubbias adv_polbias adv_theorybias adv_polhomogeneity 
    adv_press2pub adv_authorship adv_uncertainty adv_altexplain adv_conflictevidence
  /MISSING LISTWISE 
  /ANALYSIS adv_fabricate adv_plagiarize adv_import_replicate adv_freq_replicate 
    adv_replication_failures adv_replication_debates adv_tone_reform adv_prereg adv_openmaterials 
    adv_opendata adv_selreport adv_phacking adv_HARKing adv_samplesize adv_power adv_effectsize 
    adv_overclaiming adv_generalize adv_WEIRD adv_pubbias adv_polbias adv_theorybias adv_polhomogeneity 
    adv_press2pub adv_authorship adv_uncertainty adv_altexplain adv_conflictevidence
  /PRINT INITIAL EXTRACTION ROTATION
  /PLOT EIGEN
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

FACTOR
  /VARIABLES grad_fabricate grad_plagiarize grad_import_replicate grad_freq_replicate 
    grad_replication_failures grad_replication_debates grad_tone_reform grad_prereg grad_openmaterials 
    grad_opendata grad_selreport grad_phacking grad_HARKing grad_samplesize grad_power grad_effectsize 
    grad_overclaiming grad_generalize grad_WEIRD grad_pubbias grad_polbias grad_theorybias 
    grad_polhomogeneity grad_press2pub grad_authorship grad_uncertainty grad_altexplain 
    grad_conflictevidence
  /MISSING LISTWISE 
  /ANALYSIS grad_fabricate grad_plagiarize grad_import_replicate grad_freq_replicate 
    grad_replication_failures grad_replication_debates grad_tone_reform grad_prereg grad_openmaterials 
    grad_opendata grad_selreport grad_phacking grad_HARKing grad_samplesize grad_power grad_effectsize 
    grad_overclaiming grad_generalize grad_WEIRD grad_pubbias grad_polbias grad_theorybias 
    grad_polhomogeneity grad_press2pub grad_authorship grad_uncertainty grad_altexplain 
    grad_conflictevidence
  /PRINT INITIAL EXTRACTION ROTATION
  /PLOT EIGEN
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

**Subscales for graduate teaching items

RELIABILITY
  /VARIABLES=grad_overclaiming grad_generalize grad_WEIRD grad_polbias grad_theorybias grad_polhomogeneity grad_altexplain grad_conflictevidenc
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

COMPUTE grad_replication=Mean(grad_fabricate, grad_import_replicate, grad_freq_replicate, grad_replication_failures, grad_replication_debates, 
    grad_prereg, grad_openmaterials, grad_opendata, grad_selreport, grad_phacking, grad_HARKing, 
    grad_samplesize, grad_power, grad_effectsize, grad_pubbias, grad_press2pub, grad_authorship).


RELIABILITY
  /VARIABLES=grad_overclaiming grad_generalize grad_WEIRD grad_polbias grad_theorybias grad_polhomogeneity grad_altexplain grad_conflictevidenc
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.


COMPUTE grad_interpretation=Mean(grad_overclaiming, grad_generalize, grad_WEIRD, grad_polbias, grad_theorybias, grad_polhomogeneity, grad_altexplain, grad_conflictevidenc).

**Level of discussion of topics by course type

USE ALL.
COMPUTE filter_$=(intro_course_coded < 4).
VARIABLE LABELS filter_$ 'intro_course_coded < 4 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

ONEWAY Intro_topics BY intro_course_coded
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05).


USE ALL.
COMPUTE filter_$=(adv_course_coded < 3).
VARIABLE LABELS filter_$ 'adv_course_coded < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

T-TEST GROUPS=adv_course_coded(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=Adv_topics
  /CRITERIA=CI(.95).


USE ALL.
COMPUTE filter_$=(grad__course_coded < 3).
VARIABLE LABELS filter_$ 'grad__course_coded < 3 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


T-TEST GROUPS=grad__course_coded(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=Grad_topics grad_replication grad_interpretation
  /CRITERIA=CI(.95).


FILTER OFF.
USE ALL.
EXECUTE.

**Years teaching and level of discussion of topics

DESCRIPTIVES VARIABLES=years_teaching
  /STATISTICS=MEAN STDDEV MIN MAX.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER years_teaching.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER years_teaching.



REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER years_teaching.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER years_teaching.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER years_teaching.

**Academic rank and level of discussion of topics

FREQUENCIES VARIABLES=position_recoded
  /ORDER=ANALYSIS.

ONEWAY Intro_topics BY position_recoded
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05).

ONEWAY Adv_topics BY position_recoded
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05).

**Teaching load and level of discussion

DESCRIPTIVES VARIABLES=teaching_load_coded
  /STATISTICS=MEAN STDDEV MIN MAX.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER teaching_load_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER teaching_load_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER teaching_load_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER teaching_load_coded.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER teaching_load_coded.

**Teaching vs. research focus


DESCRIPTIVES VARIABLES=work_time_1 work_time_2 work_time_3 work_time_4
  /STATISTICS=MEAN STDDEV MIN MAX.

CORRELATIONS
  /VARIABLES=work_time_1 work_time_2
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER work_time_1.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER work_time_1.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER work_time_1.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER work_time_1.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER work_time_1.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER work_time_2.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER work_time_2.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER work_time_2.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER work_time_2.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER work_time_2.


FREQUENCIES VARIABLES=focus
  /ORDER=ANALYSIS.

CORRELATIONS
  /VARIABLES=focus work_time_1 work_time_2
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER focus.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER focus.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER focus.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER focus.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER focus.

**Class size

DESCRIPTIVES VARIABLES=intro_size_coded adv_size_coded grad_size_coded
  /STATISTICS=MEAN STDDEV MIN MAX.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER intro_size_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER adv_size_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER grad_size_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER grad_size_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER grad_size_coded.

**Specialty/area of study in psychology

FREQUENCIES VARIABLES=specialty_single_code
  /ORDER=ANALYSIS.


USE ALL.
COMPUTE filter_$=(specialty_groups = 2 | specialty_groups = 4 | specialty_groups = 5).
VARIABLE LABELS filter_$ 'specialty_groups = 2 | specialty_groups = 4 | specialty_groups = 5 '+
    '(FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


ONEWAY Intro_topics BY specialty_groups
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05).


FILTER OFF.
USE ALL.
EXECUTE.

T-TEST GROUPS=specialty_groups(4 5)
  /MISSING=ANALYSIS
  /VARIABLES=Adv_topics
  /CRITERIA=CI(.95).

FREQUENCIES VARIABLES=socpers_v_other
  /ORDER=ANALYSIS.

T-TEST GROUPS=socpers_v_other(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=Intro_topics Adv_topics Grad_topics grad_replication grad_interpretation
  /CRITERIA=CI(.95).

**Number of student researchers supervised per year

DESCRIPTIVES VARIABLES=RAs_coded undergrad_advisees_coded grad_advisees_coded
  /STATISTICS=MEAN STDDEV MIN MAX.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER RAs_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER undergrad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER grad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER RAs_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER undergrad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER grad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER RAs_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER undergrad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER grad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER RAs_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER undergrad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER grad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER RAs_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER undergrad_advisees_coded.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER grad_advisees_coded.

**Reform descriptives, reliability, and factor analysis

DESCRIPTIVES VARIABLES=reform_fabricate reform_plagarize reform_improve_replicability 
    reform_increase_replication reform_replication_outlets reform_increase_prereg 
    reform_increase_openmaterials reform_increase_opendata reform_encourage_badges 
    reform_require_badges reform_test_reform_policies reform_tone reform_transparency reform_phacking 
    reform_posthoc reform_samplesize reform_power reform_effectsize reform_strength reform_generalize 
    reform_WEIRD reform_pubbias reform_polbias reform_theorybias reform_poldiversity reform_press2pub 
    reform_incentives reform_authorship reform_uncertainty reform_altexplain reform_conflictevidence
  /STATISTICS=MEAN STDDEV MIN MAX.


RELIABILITY
  /VARIABLES=reform_fabricate reform_plagarize reform_improve_replicability 
    reform_increase_replication reform_replication_outlets reform_increase_prereg 
    reform_increase_openmaterials reform_increase_opendata reform_encourage_badges 
    reform_require_badges reform_test_reform_policies reform_tone reform_transparency reform_phacking 
    reform_posthoc reform_samplesize reform_power reform_effectsize reform_strength reform_generalize 
    reform_WEIRD reform_pubbias reform_polbias reform_theorybias reform_poldiversity reform_press2pub 
    reform_incentives reform_authorship reform_uncertainty reform_altexplain reform_conflictevidence
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE
  /SUMMARY=TOTAL.

COMPUTE reform=Mean(reform_fabricate, reform_plagarize, reform_improve_replicability, 
    reform_increase_replication, reform_replication_outlets, reform_increase_prereg, 
    reform_increase_openmaterials, reform_increase_opendata, reform_encourage_badges, 
    reform_require_badges, reform_test_reform_policies, reform_tone, reform_transparency, 
    reform_phacking, reform_posthoc, reform_samplesize, reform_power, reform_effectsize, 
    reform_strength, reform_generalize, reform_WEIRD, reform_pubbias, reform_polbias, 
    reform_theorybias, reform_poldiversity, reform_press2pub, reform_incentives, reform_authorship, 
    reform_uncertainty, reform_altexplain, reform_conflictevidence).
EXECUTE.

FACTOR
  /VARIABLES reform_fabricate reform_plagarize reform_improve_replicability 
    reform_increase_replication reform_replication_outlets reform_increase_prereg 
    reform_increase_openmaterials reform_increase_opendata reform_encourage_badges 
    reform_require_badges reform_test_reform_policies reform_tone reform_transparency reform_phacking 
    reform_posthoc reform_samplesize reform_power reform_effectsize reform_strength reform_generalize 
    reform_WEIRD reform_pubbias reform_polbias reform_theorybias reform_poldiversity reform_press2pub 
    reform_incentives reform_authorship reform_uncertainty reform_altexplain reform_conflictevidence
  /MISSING LISTWISE 
  /ANALYSIS reform_fabricate reform_plagarize reform_improve_replicability 
    reform_increase_replication reform_replication_outlets reform_increase_prereg 
    reform_increase_openmaterials reform_increase_opendata reform_encourage_badges 
    reform_require_badges reform_test_reform_policies reform_tone reform_transparency reform_phacking 
    reform_posthoc reform_samplesize reform_power reform_effectsize reform_strength reform_generalize 
    reform_WEIRD reform_pubbias reform_polbias reform_theorybias reform_poldiversity reform_press2pub 
    reform_incentives reform_authorship reform_uncertainty reform_altexplain reform_conflictevidence
  /PRINT INITIAL EXTRACTION ROTATION
  /PLOT EIGEN
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25)
  /ROTATION VARIMAX
  /METHOD=CORRELATION.

**Length of time teaching and perceived need for reform


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT reform
  /METHOD=ENTER years_teaching.

**Academic rank and perceived need for reform

ONEWAY reform BY position_recoded
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05).

**Teaching load and perceived need for reform

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT reform
  /METHOD=ENTER teaching_load_coded.

**teaching vs. research focus and perceived need for reform

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT reform
  /METHOD=ENTER work_time_1.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT reform
  /METHOD=ENTER work_time_2.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT reform
  /METHOD=ENTER focus.

**Specialty/area of psychology and perceived need for reform


ONEWAY reform BY specialty_groups
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=TUKEY ALPHA(0.05).

T-TEST GROUPS=socpers_v_other(1 2)
  /MISSING=ANALYSIS
  /VARIABLES=reform
  /CRITERIA=CI(.95).

**Perceived need for reform and teaching issues of replication and reform

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intro_topics
  /METHOD=ENTER reform.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Adv_topics
  /METHOD=ENTER reform.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Grad_topics
  /METHOD=ENTER reform.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_replication
  /METHOD=ENTER reform.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT grad_interpretation
  /METHOD=ENTER reform.


