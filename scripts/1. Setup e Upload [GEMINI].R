# === 1. Set-up e Upload === #

#### PACOTES NECESSÁRIOS NO R ####
# Vetor com os nomes dos pacotes
pkgs_R <- c("readxl", "tidyr", "dplyr", "ggplot2", "reticulate", "tibble", "knitr", "viridis", "purrr")

# Instala os pacotes que ainda não estão instalados
installed_pkgs <- pkgs_R %in% rownames(installed.packages())
if (any(installed_pkgs == FALSE)) {
  install.packages(pkgs_R[!installed_pkgs])
}

# Carrega os pacotes
lapply(pkgs_R, library, character.only = TRUE)

#### SETUP PYTHON COM RETICULATE ####
# Nomes dos pacotes Python a serem instalados
pkgs_Py <- c("pandas", "matplotlib", "scikit-learn", "seaborn", "scipy")

# Cria e ativa o ambiente virtual
if (!virtualenv_exists("r_env")) {
  virtualenv_create("r_env")
}
use_virtualenv("r_env", required = TRUE)

# Instala os pacotes Python necessários no ambiente virtual
py_install(pkgs_Py)

# Testa a configuração do Python para garantir que está usando o ambiente correto
py_config()

# Importa o módulo pandas para o R
pd <- import("pandas")

#### UPLOAD E PREPARAÇÃO DOS DADOS ####
# Define o diretório de trabalho
setwd(here::here()) # aponta para o presente projeto do Rstudio

# Carrega as planilhas
df_manualfh <- read_excel("Piloto_Manual_FH.xlsx")
df_geminifh    <- read_excel("Piloto_GEMINI_FH.xlsx")

# Remove linhas que são completamente vazias
df_manualfh <- df_manualfh[rowSums(is.na(df_manualfh)) != ncol(df_manualfh), ]
df_geminifh    <- df_geminifh[rowSums(is.na(df_geminifh)) != ncol(df_geminifh), ]

# Passa os data.frames do R para o ambiente Python
py$df_manualfh <- r_to_py(df_manualfh)
py$df_geminifh    <- r_to_py(df_geminifh)

# Verificação dos objetos no ambiente Python
py_run_string("
import pandas as pd

print('Objetos disponíveis no Python:')
print(dir())
")
