# Study 13 reproduction notes

Study: Hawkins, Camp, & Schunke (2022/2025)  
DOI: 10.1177/00986283221142016

Target rows:

- `id = 8`: education vs control on subjective knowledge.
- `id = 9`: education vs control on objective knowledge.

The uploaded SPSS syntax reports independent-samples t-tests with `GROUPS=condition(0 1)`. The paper reports Welch/Satterthwaite tests for objective and subjective knowledge.

The reproduction script explicitly:

1. reads the `.sav` file using `haven::read_sav()`;
2. selects education (`condition == 1`) and control (`condition == 0`);
3. removes missing values from each outcome;
4. runs `stats::t.test(..., var.equal = FALSE)`;
5. recomputes Welch t, Satterthwaite df, exact p, group means, group SDs, mean difference, Welch SE, n values, and effective n;
6. computes two effect-size values:
   - `effect_size_value`: `2 * t / sqrt(df)`, because this matches the article's reported d convention;
   - conventional pooled-SD Cohen's d, preserved in the notes.

The raw SPSS `.sav` file is not included in this branch-ready package.
