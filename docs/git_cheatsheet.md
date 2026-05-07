# Git Cheat Sheet for AMIS Main Outcome

This repository is only for code, syntax, and non-sensitive documentation.

Do not store data, outputs, manuscripts, or participant-level information in this repository.

------------------------------------------------------------------------

## 1. Before starting work

Always update your local version first:

``` bash
git pull
```

------------------------------------------------------------------------

## 2. Check what has changed

``` bash
git status
```

------------------------------------------------------------------------

## 3. Save your changes to Git

After editing files, run:

``` bash
git add .
git commit -m "Describe what changed"
git push
```

Example:

``` bash
git add .
git commit -m "Add Mplus syntax for internalizing model"
git push
```

------------------------------------------------------------------------

## 4. Standard workflow

Use this every time:

``` bash
git pull
# work on files
git status
git add .
git commit -m "Describe what changed"
git push
```

------------------------------------------------------------------------

## 5. Clone this repository on a new computer

``` bash
git clone https://github.com/a-janitor/main_outcome_amis2.git
```

Then open the `.Rproj` file in RStudio.

------------------------------------------------------------------------

## 6. If Git asks who you are

``` bash
git config --global user.name "Jan Keil"
git config --global user.email "jan.keil1984@googlemail.com"
```

------------------------------------------------------------------------

## 7. Files that belong in Git

Allowed:

``` text
R scripts
Mplus .inp files
README files
workflow documentation
non-sensitive notes
empty templates
```

------------------------------------------------------------------------

## 8. Files that do NOT belong in Git

Do not commit:

``` text
participant-level data
analysis datasets
.RDS files
.rds files
.dat files
.sav files
.csv files with data
Mplus .out files
Mplus savedata files
Excel files with real results
Word manuscript drafts
PDF reports
anything with personal or sensitive information
```

------------------------------------------------------------------------

## 9. If you accidentally see data files in git status

Do not commit them.

Check that `.gitignore` contains these lines:

``` gitignore
*.RDS
*.rds
*.sav
*.csv
*.dat
*.dta
*.xlsx
*.out
*.gh5
*.res
*.fscores
*.docx
*.pdf
.Rhistory
.RData
.Rproj.user/
.DS_Store
Thumbs.db
```

------------------------------------------------------------------------

## 10. Rule of thumb

Before starting:

``` bash
git pull
```

After finishing:

``` bash
git add .
git commit -m "Describe what changed"
git push
```

GitHub = code and syntax only.

Speicherwolke = data, outputs, tables, figures, manuscript, supplement.
