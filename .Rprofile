print("Loading .Rprofile...")

packages <- c("car",
              "e1071",
              "hcci",
              "ICC",
              "lavaan",
              "lme4",
              "lmerTest",
              "nlme",
              "MASS",
              "mirt",
              "psych",
              "reghelper",
              "semTools",
              "simr",
              "stargazer",
              "tidyverse")

new.packages <- packages[!(packages %in% utils::installed.packages()[,"Package"])]

if(length(new.packages)) {
  print(paste(c("Installing the following packages: ", new.packages), collapse=" "))
  utils::install.packages(new.packages, repos='http://cran.us.r-project.org')
}

base::remove(list=c("packages", "new.packages"))
