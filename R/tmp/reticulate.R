library(reticulate)
library(reidentify)
library(stringi)
library(tidyverse)

dat = create_dummy_master_data(1000)
dat %>% head

## pandas_profilingにdatを投げたい
reticulate::use_python(
  python = "/usr/bin/python3"
)

repl_python()
