# HCI2000 data analysis: how to run

Run these three files in this order:

1. `Data Preprocessing/preprocessing.ipynb`  (Python notebook, press Run All)
   Cleans and combines the four data sources. Creates the `Data Preprocessing/cleaned/` folder.
2. `Data Analysis/analysis01.ipynb`  (Python notebook, press Run All)
   Descriptive tables and interaction plots.
3. `Data Analysis/analysis02.R`  (plain R script, NOT a notebook)
   Normality checks and the statistical tests. Open it in VS Code or RStudio and run it all,
   or in Terminal: `Rscript "Data Analysis/analysis02.R"`.

Each file has one line near the top, `BASE = "..."` (Python) or `BASE <- "..."` (R), with the path of this folder.
Change it only if the folder moves.

Right now they read the DUMMY data in `Data Preprocessing/dummy_data/`.
For the real data, change the few settings in the "Setup" cell of `preprocessing.ipynb` (folder, `PREFIX`, `MODE`).
Dummy results are rehearsal only, not findings.

R needs the packages afex, lme4 and ARTool. `analysis02.R` installs any that are missing the first time it runs.
No Jupyter R kernel is needed.
