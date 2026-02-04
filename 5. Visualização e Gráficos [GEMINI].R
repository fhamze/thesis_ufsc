# === 5. Visualização e Gráficos === #

#### PREPARAÇÃO E CARGA DOS DADOS ####
# 1. Métricas de Desempenho (Precision, Recall, F1)
metricas_gemini <- bind_rows(
  lapply(analise_r_gemini$metricas_por_conteudo, function(x) {
    data.frame(precision = x$precision, recall = x$recall, f1_score = x$f1_score)
  }), 
  .id = "coluna"
)

# 2. Dados de Erros (FN e FP)
erros_fn_gemini <- bind_rows(
  lapply(analise_r_gemini$metricas_por_conteudo, function(x) {
    if (length(x$erros_fn) > 0) tibble::enframe(x$erros_fn, name = "token", value = "frequencia")
  }), 
  .id = "coluna"
) %>% 
  filter(!is.na(token)) %>%
  mutate(frequencia = as.numeric(frequencia))

erros_fp_gemini <- bind_rows(
  lapply(analise_r_gemini$metricas_por_conteudo, function(x) {
    if (length(x$erros_fp) > 0) tibble::enframe(x$erros_fp, name = "token", value = "frequencia")
  }), 
  .id = "coluna"
) %>% 
  filter(!is.na(token)) %>%
  mutate(frequencia = as.numeric(frequencia))

# 3. Dados de Kappa
kappa_df_gemini <- data.frame(
  coluna = names(analise_r_gemini$kappa_scores), 
  kappa = unlist(analise_r_gemini$kappa_scores)
)

# 4. Dados da Matriz de Confusão
# Transforma em um único dataframe 'long'
matriz_gemini_long <- imap_dfr(analise_r_gemini$matrizes_confusao, ~ {
  # .x é a lista de dicts (linhas), .y é o nome da coluna
  bind_rows(.x) %>% mutate(coluna = .y)
})

#### RENOMEAR VARIÁVEIS PARA OS GRÁFICOS ####
# Define o dicionário de tradução
mapa_nomes <- c(
  "neurogenesis_marker" = "Marcador de Neurogênese",
  "species"             = "Espécie",
  "sex"                 = "Sexo",
  "strain"              = "Linhagem",
  "antidepressant_atd"  = "Antidepressivo",
  "stress_protocol"     = "Protocolo de Estresse",
  "behavioral_test"     = "Teste Comportamental",
  "atd_dose"            = "Dose do ATD",
  "brdu_dose"           = "Dose de BrdU",
  "treatment_duration"  = "Duração do Tratamento",
  "survival"            = "Tempo de Sobrevivência",
  "atd_injections"      = "Injeções de ATD",
  "brdu_injections"     = "Injeções de BrdU"
)

# Aplica a renomeação em todos os dataframes de métricas
if (exists("metricas_gemini")) {
  metricas_gemini <- metricas_gemini %>% mutate(coluna = recode(coluna, !!!mapa_nomes))
}
if (exists("erros_fn_gemini")) {
  erros_fn_gemini <- erros_fn_gemini %>% mutate(coluna = recode(coluna, !!!mapa_nomes))
}
if (exists("erros_fp_gemini")) {
  erros_fp_gemini <- erros_fp_gemini %>% mutate(coluna = recode(coluna, !!!mapa_nomes))
}
if (exists("kappa_df_gemini")) {
  kappa_df_gemini <- kappa_df_gemini %>% mutate(coluna = recode(coluna, !!!mapa_nomes))
}
if (exists("matriz_gemini_long")) {
  matriz_gemini_long <- matriz_gemini_long %>% mutate(coluna = recode(coluna, !!!mapa_nomes))
}

#### ANÁLISE DE DESEMPENHO CATEGÓRICO ####
cat("--- Métricas de Desempenho para Colunas Categóricas (por String/Tokens) ---\n")
if (nrow(metricas_gemini) > 0) print(kable(metricas_gemini))

## Métricas de Precision, Recall, F1-score ##
metricas_gemini_long <- metricas_gemini %>%
  select(coluna, precision, recall, f1_score) %>%
  pivot_longer(cols = -coluna, names_to = "metrica", values_to = "valor")

# Gráfico comparativo
ggplot(metricas_gemini_long, aes(x = reorder(coluna, valor), y = valor, fill = metrica)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = if_else(metrica == "f1_score", as.character(round(valor, 2)), "")), 
    position = position_dodge(width = 0.9),
    hjust = 1.5, 
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  geom_text(
    aes(label = if_else(metrica != "f1_score", as.character(round(valor, 2)), "")),
    position = position_dodge(width = 0.9),
    hjust = 1.5,
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  ylim(0, 1) +
  coord_flip() +
  labs(
    title = "Análise de Desempenho (por Tokens) da Extração Gemini", 
    subtitle = "Comparação entre Precision, Recall e F1-score", 
    x = "Coluna", y = "Desempenho da Extração"
  ) +
  scale_fill_manual(
    name = "Métrica", 
    values = c("precision" = "#9A32CD", "recall" = "#9ACD32", "f1_score" = "#FF8C00"), 
    labels = c("F1-score", "Precision", "Recall")
  ) +
  theme_minimal()

#### ANÁLISE DE ERROS CATEGÓRICOS ####
cat("--- Análise de Erros para Colunas Categóricas (por String/Tokens) ---\n")

# Gráfico de Falsos Negativos (Tokens Perdidos)
erros_fn_coluna_gemini <- erros_fn_gemini %>% 
  group_by(coluna) %>% 
  slice_max(order_by = frequencia, n = 5, with_ties = FALSE) %>% 
  ungroup()

if (nrow(erros_fn_coluna_gemini) > 0) {
  ggplot(erros_fn_coluna_gemini, aes(x = reorder(token, frequencia), y = frequencia, fill = coluna)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = frequencia), 
      hjust = -0.3,
      size = 2.5, 
      color = "black"
    ) +
    coord_flip() + facet_wrap(~ coluna, scales = "free_y") +
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Erros de Tokens (Falsos Negativos)",
      subtitle = "Os 5 tokens mais perdidos pelo Gemini (estavam no Manual)",
      x = "Token Perdido",
      y = "Frequência"
    ) + theme_minimal() +
    scale_fill_viridis_d()
}

# Gráfico de Falsos Positivos (Tokens 'Inventados')
erros_fp_coluna_gemini <- erros_fp_gemini %>% 
  group_by(coluna) %>% 
  slice_max(order_by = frequencia, n = 5, with_ties = FALSE) %>% 
  ungroup()

if (nrow(erros_fp_coluna_gemini) > 0) {
  ggplot(erros_fp_coluna_gemini, aes(x = reorder(token, frequencia), y = frequencia, fill = coluna)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = frequencia), 
      hjust = -0.3,
      size = 2.5, 
      color = "black"
    ) +
    coord_flip() + facet_wrap(~ coluna, scales = "free_y") +
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Erros de Tokens (Falsos Positivos)",
      subtitle = "Os 5 tokens mais 'inventados' pelo Gemini (não estavam no Manual)",
      x = "Token Inventado",
      y = "Frequência"
    ) + theme_minimal() +
    scale_fill_viridis_d(option = "plasma")
}

#### ANÁLISE DE CONCORDÂNCIA SINGLE-LABEL (Kappa e Matriz de Confusão) ####
cat("\n--- Gerando Gráficos de Concordância para Colunas Single-Label ---\n")

## KAPPA DE COHEN ##
if (nrow(kappa_df_gemini) > 0) {
  print(kable(kappa_df_gemini, caption = "Concordância Corrigida para o Acaso"))
} else {
  cat("Nenhum score Kappa calculado.\n")
}

# Gráfico de Kappa
if (nrow(kappa_df_gemini) > 0) {
  ggplot(kappa_df_gemini, aes(x = reorder(coluna, kappa), y = kappa)) +
    geom_col(fill = "#6959CD") +
    geom_text(aes(label = round(kappa, 2)), hjust = 1.2, color = "white", fontface = "bold") +
    coord_flip() +
    labs(title = "Concordância (Kappa de Cohen) entre Manual e Gemini", 
         subtitle = "Concordância para estudos pareados (com 'Best-Fit')",
         x = "Coluna", y = "Kappa Score") + 
    theme_minimal() + ylim(0, 1)
}

## MATRIZ DE CONFUSÃO (com Python) ##
# Passa o mapa de nomes do R para o ambiente Python
py$mapa_nomes_py <- as.list(mapa_nomes)

# Código da matriz de confusão
py_run_string(r"(
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import confusion_matrix
import numpy as np

try:
    df_para_analise = df_alinhado_gemini.copy()
    print(f"Gerando matrizes com base em {len(df_para_analise)} estudos pareados.")
except NameError:
    print("ERRO: O dataframe 'df_alinhado_gemini' não foi encontrado no escopo do Python.")

# Define as colunas 'single-label' para a matriz
colunas_para_matriz = [
    'neurogenesis_marker', 
    'species', 
    'sex', 
    'strain', 
    'antidepressant_atd',
    'atd_dose', 
    'brdu_dose', 
    'treatment_duration', 
    'survival', 
    'atd_injections', 
    'brdu_injections'
]

# Filtra colunas que realmente existem no df_para_analise
colunas_existentes = [
    col for col in colunas_para_matriz 
    if f'manual_{col}' in df_para_analise.columns and f'gemini_{col}' in df_para_analise.columns
]
print(f'Gerando matrizes de confusão para: {colunas_existentes}')

for coluna in colunas_existentes:
    coluna_manual = f'manual_{coluna}'
    coluna_gemini = f'gemini_{coluna}'

    y_true = df_para_analise[coluna_manual].astype(str).replace('nan', 'nr').str.lower().str.strip()
    y_pred = df_para_analise[coluna_gemini].astype(str).replace('nan', 'nr').str.lower().str.strip()

    labels = sorted(list(set(y_true) | set(y_pred)))
    
    # Só gera a matriz se houver rótulos e mais de um rótulo
    if labels and len(labels) > 0:
        cm = confusion_matrix(y_true, y_pred, labels=labels)

        # Gera o gráfico usando Seaborn
        fig_size = max(6, len(labels) * 0.5)
        plt.figure(figsize=(fig_size, fig_size))
        
        ax = sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
                         xticklabels=labels, yticklabels=labels)

        nome__colunas_alterado = mapa_nomes_py.get(coluna, coluna)

        # Lógica para negrito na diagonal
        # (Compara o rótulo X e Y da célula; se forem iguais fica em negrito)
        for text_obj in ax.texts:
            try:
                x, y = text_obj.get_position()
                # Pega o índice do tick com base na posição
                idx_x = int(round(x - 0.5))
                idx_y = int(round(y - 0.5))
                
                # Compara os rótulos
                if ax.get_xticklabels()[idx_x].get_text() == ax.get_yticklabels()[idx_y].get_text():
                   text_obj.set_weight('bold')
            except Exception as e:
                # Ignora erros se a célula estiver fora dos limites, etc.
                continue

        plt.title(f'Matriz de Confusão - {nome__colunas_alterado}', fontsize=16)
        plt.ylabel('Valor Manual (Verdadeiro)', fontsize=12)
        plt.xlabel('Valor Gemini (Previsto)', fontsize=12)
        plt.xticks(rotation=45, ha='right')
        plt.yticks(rotation=0)
        plt.tight_layout()

        # Salva o arquivo
        plt.savefig(f'matriz_confusao_{coluna}.png')
        plt.close()

print('Matrizes de confusão geradas com sucesso.')
)")

#### ANÁLISE DE CONCORDÂNCIA MULTI-LABEL (Concordância e Halteres) ####
cat("\n--- Gerando Gráficos de Concordância para Colunas Multi-Label ---\n")

# Gráfico de Barra e de Halteres
colunas_para_concordancia <- c("stress_protocol", "behavioral_test")
for (coluna_atual in colunas_para_concordancia) {
  
  # Verifica se a coluna existe nos dataframes de R
  if (!coluna_atual %in% names(df_manualfh_tratado) || !coluna_atual %in% names(df_geminifh_tratado)) {
    cat(paste("Pulando coluna", coluna_atual, ": não encontrada nos dataframes de R.\n"))
    next
  }
  
  # Processa contagens do Manual
  manual_counts <- df_manualfh_tratado %>%
    select(!!sym(coluna_atual)) %>%
    separate_rows(!!sym(coluna_atual), sep = ",") %>%
    mutate(!!sym(coluna_atual) := tolower(trimws(!!sym(coluna_atual)))) %>%
    filter(!is.na(!!sym(coluna_atual)) & !!sym(coluna_atual) != "" & !!sym(coluna_atual) != "nr") %>%
    count(!!sym(coluna_atual), name = "manual")
  
  # Processa contagens do Gemini
  gemini_counts <- df_geminifh_tratado %>%
    select(!!sym(coluna_atual)) %>%
    separate_rows(!!sym(coluna_atual), sep = ",") %>%
    mutate(!!sym(coluna_atual) := tolower(trimws(!!sym(coluna_atual)))) %>%
    filter(!is.na(!!sym(coluna_atual)) & !!sym(coluna_atual) != "" & !!sym(coluna_atual) != "nr") %>%
    count(!!sym(coluna_atual), name = "gemini")
  
  # Cria objetos necessários
  concordancia_wide_gemini <- full_join(manual_counts, gemini_counts, by = coluna_atual) %>%
    replace_na(list(manual = 0, gemini = 0)) %>%
    mutate(
      diferenca = manual - gemini,
      posicao_texto_y = pmax(manual, gemini)
    )
  
  concordancia_gemini <- concordancia_wide_gemini %>%
    pivot_longer(
      cols = c(manual, gemini), 
      names_to = "fonte", 
      values_to = "frequencia"
    )
  
  max_freq <- max(concordancia_gemini$frequencia) * 1.15 
  
  # Gráfico de Concordância (Barras)
  grafico_concordancia <- ggplot(concordancia_gemini, aes(x = reorder(!!sym(coluna_atual), -posicao_texto_y), y = frequencia, fill = fonte)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = ifelse(frequencia > 0, frequencia, "")), 
      position = position_dodge(width = 0.9), 
      vjust = -0.5, 
      size = 3, 
      color = "black"
    ) +
  scale_y_continuous(limits = c(0, max_freq)) +
    labs(title = paste("Análise de Concordância para:", mapa_nomes[coluna_atual]), 
         x = "Valor Extraído (sigla)", 
         y = "Frequência") +
    scale_fill_manual(name = "Fonte:", values = c("manual" = "#009ACD", "gemini" = "#00CD66"), labels = c("Gemini", "Manual")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  print(grafico_concordancia)
  
  # Gráfico de Halteres (Dumbbell Plot)
  concordancia_wide_gemini <- full_join(manual_counts, gemini_counts, by = coluna_atual) %>% 
    replace_na(list(manual = 0, gemini = 0)) %>%
  mutate(
    diferenca = manual - gemini,
    posicao_texto_y = pmax(manual, gemini) 
  )
  
  grafico_halteres <- ggplot(concordancia_wide_gemini, aes(x = reorder(!!sym(coluna_atual), -posicao_texto_y))) +
    geom_segment(aes(xend = !!sym(coluna_atual), y = manual, yend = gemini), 
                 color = "gray", linewidth = 1.5, alpha = 0.7) +
    geom_point(aes(y = manual, color = "Manual"), size = 4) +
    geom_point(aes(y = gemini, color = "Gemini"), size = 4) +
    geom_text(
      aes(label = if_else(diferenca == 0, "", as.character(diferenca)),
          y = posicao_texto_y),
      vjust = -1.0,
      size = 3,
      color = "black"
    ) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_color_manual(name = "Fonte:", values = c("Manual" = "#009ACD", "Gemini" = "#00CD66")) +
    labs(
      title = paste("Análise de Concordância para:", mapa_nomes[coluna_atual]),
      subtitle = "Diferença na frequência de valores extraídos (Manual vs. Gemini)",
      x = "Valor Extraído (sigla)",
      y = "Frequência"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  print(grafico_halteres)
}

#### ANÁLISE DE DIVERGÊNCIA ESTRUTURAL ####
## PRÉ BEST-FIT ##
cat("\n--- Gerando Gráfico de Divergência Estrutural ---\n")
# Divergência de Estudos por Artigo
contagem_manual <- df_manualfh_tratado %>% group_by(id) %>% summarise(estudos_manual = n(), .groups = "drop")
contagem_gemini <- df_geminifh_tratado %>% group_by(id) %>% summarise(estudos_gemini = n(), .groups = "drop")
comparacao_artigos_gemini <- full_join(contagem_manual, contagem_gemini, by = "id") %>%
  mutate(
    estudos_manual = replace_na(estudos_manual, 0),
    estudos_gemini = replace_na(estudos_gemini, 0),
    diferenca = estudos_manual - estudos_gemini,
    status = if_else(diferenca == 0, "Igual", "Divergente")
  )

comparacao_artigos_gemini_long <- comparacao_artigos_gemini %>% 
  pivot_longer(
    cols = c(estudos_manual, estudos_gemini), 
    names_to = "metodo", 
    values_to = "n_estudos")

# Gráfico de Barras
ggplot(comparacao_artigos_gemini_long, aes(x = factor(id), y = n_estudos, fill = metodo)) +
  geom_col(aes(alpha = status), position = "dodge", width = 0.9) +
  geom_text(
    aes(label = n_estudos, alpha = status), 
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    color = "black",
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Divergência no Número de Estudos por Artigo",
    subtitle = "Comparação estrutural antes do alinhamento 'Best-Fit'",
    x = "ID do Artigo",
    y = "Número de Estudos",
    fill = "Fonte"
  ) +
  scale_fill_manual(
    values = c("estudos_manual" = "#009ACD", "estudos_gemini" = "#00CD66"),
    labels = c("Gemini", "Manual")
  ) +
  scale_alpha_manual(
    name = "Comparação",
    values = c("Divergente" = 1.0, "Igual" = 0.4),
    labels = c("Divergente", "Igual")
  ) +
  guides(
    alpha = guide_legend(override.aes = list(shape = 15, size = 5))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1))

## PÓS BEST-FIT ##
cat("\n--- Gerando Gráfico de Resultados do Alinhamento ---\n")
# Análise de Resultados do Alinhamento
gemini_status <- df_alinhado_gemini %>%
  mutate(
    id_artigo = coalesce(as.character(manual_id), as.character(gemini_id)),
    status_alinhamento = case_when(
      !is.na(manual_id) & !is.na(gemini_id) ~ "Par (Matched)",
      !is.na(manual_id) &  is.na(gemini_id) ~ "Manual Only (FN)",
      is.na(manual_id) & !is.na(gemini_id) ~ "Gemini Only (FP)",
      TRUE ~ NA_character_))

# Contagem
contagem_status_gemini <- gemini_status %>%
  group_by(id_artigo, status_alinhamento) %>%
  summarise(contagem = n(), .groups = "drop")

# Filtro (extra) para NAs
plot_data_stacked <- contagem_status_gemini %>%
  filter(!is.na(id_artigo), !is.na(status_alinhamento))

# Calcula total por artigo
total_artigo_gemini <- plot_data_stacked %>%
  group_by(id_artigo) %>%
  summarise(total = sum(contagem), .groups = "drop")

# Gráfico de Barras
ggplot(plot_data_stacked, aes(x = factor(id_artigo), y = contagem, fill = status_alinhamento)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = ifelse(contagem > 0, contagem, "")), 
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3
  ) +
  geom_text(
    data = total_artigo_gemini,
    aes(x = factor(id_artigo), y = total, label = total),
    inherit.aes = FALSE,
    vjust = -0.5,
    color = "black",
    size = 3
  ) +
  labs(
    title = "Comparação Estrutural após o Alinhamento 'Best-Fit'",
    subtitle = "Composição de cada artigo (Pares, Falsos Positivos, Falsos Negativos)",
    x = "ID do Artigo",
    y = "Número de Estudos",
    fill = "Status do Alinhamento"
  ) +
  scale_fill_manual(
    name = "Status:",
    values = c(
      "Par (Matched)" = "#9ACD32",
      "Manual Only (FN)" = "#009ACD",
      "Gemini Only (FP)" = "#00CD66"
    ),
    labels = c(
      "Par (Matched)" = "Pares (Manual + Gemini)",
      "Manual Only (FN)" = "Falsos Negativos (Manual)",
      "Gemini Only (FP)" = "Falsos Positivos (Gemini)"
    )
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1))
