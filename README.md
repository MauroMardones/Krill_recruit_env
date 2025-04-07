# Krill Recruitment Analysis

This repository contains the analysis of krill recruitment based on environmental variables, including sea ice concentration, sea surface temperature (TSM), and chlorophyll-a concentrations, among others. The analysis uses several models to explore the relationship between recruitment (as measured by the logarithmic proportion, `PROPLOG2`) and environmental predictors.

## Code for Reproducible Analysis

The analysis can be reproduced using the R scripts provided in this repository. To run the analysis, you will need the following R packages:

- `tidyverse`
- `ggplot2`
- `lme4`
- `kableExtra`
- `knitr`

The code for this analysis is available [here](https://mauromardones.github.io/Krill_recruit_env/).

### Steps for Reproducibility

1. Clone this repository to your local machine.
   
   ```bash
   git clone https://github.com/MauroMardones/Krill_recruit_env.git
   ```

2. Install the necessary R packages by running:

   ```r
   install.packages(c("tidyverse", "ggplot2", "lme4", "kableExtra", "knitr"))
   ```

3. Load the data and perform the analysis:

   - Load your data into R.
   - Fit the models using the `glm` function for linear regressions or `lmer` for mixed models.
   - Use the `summary()` function to inspect the results.

4. Use `kable()` from the `knitr` package to generate HTML tables of model summaries.

5. Visualize results with `ggplot2`.

## Results and Interpretation

Model results are provided in the `summary` outputs for each model. These outputs include estimates for each parameter, along with standard errors, t-values, and p-values. For example:

```r
summary(mod2_R)
```

The output from the models helps identify significant predictors of krill recruitment, such as chlorophyll-a concentration (`Chla`), sea surface temperature (`TSM`), and others. Significant relationships are indicated by p-values below the 0.05 threshold.

## Conclusion

This analysis provides insights into the environmental factors influencing krill recruitment. By fitting multiple models, we can examine the individual and combined effects of different predictors, helping to understand the complex dynamics driving krill populations in the studied area.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

You can customize this README according to the structure of your repository and the specific methods or results you want to highlight.