# === 3. Alinhamento por "Melhor Correspondência" (Best-Fit) === #

#### BEST-FIT ####
# "Problema de Mapeamento de Entidades" (Entity Resolution)
# Alinhamento por 'Best-fit' para solucionar o problema
py_run_string("
import pandas as pd
import numpy as np
from scipy.optimize import linear_sum_assignment

# --- Definição da Lista de Limpeza ---
# Lista de todos os valores que significam 'Nulo' (exceto 'nr')
list_to_replace = ['', 'na', 'nan', 'NA', 'NaN', None]

# --- Limpezad dos Dataframes de Origem ---
df_manualfh_tratado = df_manualfh_tratado.replace(list_to_replace, np.nan)
df_geminifh_tratado = df_geminifh_tratado.replace(list_to_replace, np.nan)

# --- Definição de Colunas ---
# Todas as colunas (resultados) exceto ID, Study e multi-label (stress, behavioral)
result_columns = [
    'author', 'year', 'neurogenesis_marker', 'species', 'sex', 'strain', 
    'antidepressant_atd', 'atd_dose', 'treatment_duration', 
    'atd_injections', 'brdu_injections', 'brdu_dose', 'survival'
]

# --- Algoritmo de Melhor Correspondência ---
aligned_gemini = []
unique_ids = df_manualfh_tratado['id'].unique()

for current_id in unique_ids:
    
    # Filtra os dataframes para o ID atual
    manual_subset = df_manualfh_tratado[df_manualfh_tratado['id'] == current_id].reset_index(drop=True)
    gemini_subset = df_geminifh_tratado[df_geminifh_tratado['id'] == current_id].reset_index(drop=True)

    if gemini_subset.empty:
        for idx, manual_row in manual_subset.iterrows():
            aligned_gemini.append(pd.concat([
                manual_row.add_prefix('manual_'),
                pd.Series(dtype='object').add_prefix('gemini_')
            ]))
        continue

    # Criar a Matriz de Similaridade (Score)
    # Linhas = Estudos do Manual, Colunas = Estudos do Gemini
    num_manual = len(manual_subset)
    num_gemini = len(gemini_subset)
    similarity_matrix = np.zeros((num_manual, num_gemini))

    for m_idx in range(num_manual):
        for g_idx in range(num_gemini):
            manual_results = manual_subset.loc[m_idx, result_columns]
            gemini_results = gemini_subset.loc[g_idx, result_columns]
            
            # Compara as duas séries e conta as igualdades
            score = (manual_results.eq(gemini_results) | (manual_results.isna() & gemini_results.isna())).sum()
            similarity_matrix[m_idx, g_idx] = score

    # Resolve o Mapeamento
    # O SciPy minimiza o 'custo', então se inverte o score
    # Custo = (Máximo de colunas) - (Similaridade)
    cost_matrix = len(result_columns) - similarity_matrix
    
    # Roda o algoritmo que encontra os melhores pares
    manual_indices, gemini_indices = linear_sum_assignment(cost_matrix)

    # Cria o dataframe alinhado para o ID
    matched_manual_indices = set()
    matched_gemini_indices = set()

    for m_idx, g_idx in zip(manual_indices, gemini_indices):
        # Só mapeia se a similaridade for boa (ex: >0).
        if similarity_matrix[m_idx, g_idx] > 0: 
            manual_row = manual_subset.loc[m_idx].add_prefix('manual_')
            gemini_row = gemini_subset.loc[g_idx].add_prefix('gemini_')
            aligned_gemini.append(pd.concat([manual_row, gemini_row]))
            matched_manual_indices.add(m_idx)
            matched_gemini_indices.add(g_idx)

    # Adiciona linhas que não tiveram par (Falsos Negativos do Gemini)
    for m_idx in range(num_manual):
        if m_idx not in matched_manual_indices:
            aligned_gemini.append(pd.concat([
                manual_subset.loc[m_idx].add_prefix('manual_'),
                pd.Series(dtype='object').add_prefix('gemini_')
            ]))

    # Adiciona linhas que não tiveram par (Falsos Positivos do Gemini)
    for g_idx in range(num_gemini):
        if g_idx not in matched_gemini_indices:
            aligned_gemini.append(pd.concat([
                pd.Series(dtype='object').add_prefix('manual_'),
                gemini_subset.loc[g_idx].add_prefix('gemini_')
            ]))

# --- Cria o DataFrame Final ---
df_alinhado_gemini = pd.DataFrame(aligned_gemini)

# Reordena as colunas para melhor visualização
cols_order = ['manual_id', 'gemini_id', 'manual_study', 'gemini_study']
for col in result_columns:
    cols_order.append(f'manual_{col}')
    cols_order.append(f'gemini_{col}')

multi_label_cols = ['stress_protocol', 'behavioral_test']
for col in multi_label_cols:
    if f'manual_{col}' in df_alinhado_gemini.columns: cols_order.append(f'manual_{col}')
    if f'gemini_{col}' in df_alinhado_gemini.columns: cols_order.append(f'gemini_{col}')

# Filtra colunas que realmente existem no df_alinhado_gemini
final_cols = [c for c in cols_order if c in df_alinhado_gemini.columns]
df_alinhado_gemini = df_alinhado_gemini[final_cols]

# --- Correção para '.0' (linhas novas) ---
cols_to_convert = ['manual_id', 'gemini_id', 'manual_study', 'gemini_study']
for col in cols_to_convert:
    if col in df_alinhado_gemini.columns:
        try:
            # Converte a coluna (que é 'object'/'string') para numérico (float)
            df_alinhado_gemini[col] = pd.to_numeric(df_alinhado_gemini[col], errors='coerce')
            
            # Converte o float (ex: 1.0) para Int64 (ex: 1)
            df_alinhado_gemini[col] = df_alinhado_gemini[col].astype('Int64')
        except Exception as e:
            pass
")

# Traz o dataframe alinhado para o R (renomeado)
df_alinhado_gemini <- py$df_alinhado_gemini

# Correção para .0 (em R)
cols_to_fix_in_r <- c("manual_id", "gemini_id", "manual_study", "gemini_study")
df_alinhado_gemini <- df_alinhado_gemini %>%
  mutate(across(any_of(cols_to_fix_in_r), as.integer))

#### VERIFICAÇÃO DO DATAFRAME ALINHADO ####
## Verificação do "Best-Fit" ##
# Encontra linhas onde o ID do artigo é o mesmo, mas o ID do estudo é diferente
best_fit_gemini <- df_alinhado_gemini %>%
  filter(
    !is.na(manual_id) & !is.na(gemini_id),  # Garante que é um par alinhado
    manual_study != gemini_study            # IDs de estudo são diferentes
  )

print(paste("VERIFICAÇÃO 'Best-Fit': Encontradas", nrow(best_fit_gemini), "linhas onde manual_study != gemini_study, mas foram alinhadas:"))
if (nrow(best_fit_gemini) > 0) {
  print(head(best_fit_gemini %>% select(manual_id, manual_study, gemini_study, manual_author)))
}

## Verificação de Falsos Negativos ##
# Linhas onde o Gemini falhou em extrair um estudo que existia no manual
falsos_negativos_gemini <- df_alinhado_gemini %>%
  filter(
    !is.na(manual_id), # Manual: existe
    is.na(gemini_id)      # Gemini: não existe
  )

print(paste("\nVERIFICAÇÃO 2 (Falsos Negativos): Encontrados", nrow(falsos_negativos_gemini), "estudos presentes no Manual, mas não encontrados pelo Gemini"))
if (nrow(falsos_negativos_gemini) > 0) {
  print(head(falsos_negativos_gemini %>% select(manual_id, manual_study, manual_author)))
}

## Verificação de Falsos Positivos ##
# Linhas onde o Gemini extraiu um estudo que não existia no manual
falsos_positivos_gemini <- df_alinhado_gemini %>%
  filter(
    is.na(manual_id),  # Manual: não existe
    !is.na(gemini_id)     # Gemini: existe
  )

print(paste("\nVERIFICAÇÃO de Falsos Positivos: Encontrados", nrow(falsos_positivos_gemini), "estudos extraídos pelo Gemini, mas sem par no Manual."))
if (nrow(falsos_positivos_gemini) > 0) {
  print(head(falsos_positivos_gemini %>% select(gemini_id, gemini_study, gemini_author)))
}
