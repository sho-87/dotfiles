packages <- c("car",
              "e1071",
              "hcci",
              "lavaan",
              "MASS",
              "mirt",
              "psych",
              "semTools",
              "stargazer",
              "tidyverse")

for (p in packages) {
  if(!suppressWarnings(require(p, character.only = TRUE))){
    utils::install.packages(p, repos='http://cran.us.r-project.org')
  }
}

base::remove(list=c("p", "packages"))
