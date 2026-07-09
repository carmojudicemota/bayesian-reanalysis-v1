# Study 5 reproduction notes

Study: Hard, Lovett, & Brady (2019)  
DOI: 10.1037/stl0000136

Target rows:

- `id = 3`: psychology students vs nonpsychology students on senior-year 16-item quiz performance.
- `id = 4`: psychology students vs nonpsychology students on number of additional psychology courses.

The reproduction script does not hide the analysis inside a generic helper. It explicitly:

1. selects psychology students and nonpsychology students from `Speciality`;
2. removes missing values from each outcome;
3. runs `stats::t.test(..., var.equal = TRUE)`;
4. recomputes pooled SD, Cohen's d, mean difference, standard error, total n, and effective n;
5. writes `outputs/reproduced/study_05_recomputed.csv`.

The raw data file is not included in this branch-ready package.
