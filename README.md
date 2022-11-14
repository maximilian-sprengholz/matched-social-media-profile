# Matched Social Media Profile

For an experiment in a follow-up wave of a panel survey, individual social media profiles are used as a stimulus to signal similarity or difference to a respondent. We estimate similarity/difference via a matching approach using respondent's information from the prior wave, and dynamically create a fictitious social media profile for matched individuals.

End of most recent run: _16.10.22_

Repo handover (with some housekeeping changes): _14.11.22_

## Project organization

```
.
├── .gitignore
├── README.md
├── data               <- data (input and output)
├── profiles           <- exported profiles
├── results            <- tables/figures
└── src                <- Source code
```

## Usage
Create a project-specific environment via `conda` and run `00_master.R`. The environment specified in `environment.yml` allows to use `R` interactively via the `radian` console, e.g. in VS Code. Replication is possible without the environment, but make sure that the versions of `R` and `r-essentials` correspond to `environment.yml`.

```sh
# create and activate conda environment (initialized as subdirectory ./env)
cd /path/to/qualtrics-input
conda env create --prefix ./env --file environment.yml
conda activate ./env
# run
radian
source("src/00_master.R")
```
