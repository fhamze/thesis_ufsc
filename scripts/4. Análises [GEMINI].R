# === 4. Análises Estatísticas === #

#### CÁLCULO DE MÉTRICAS ####
# Single-Label = "O estudo era da classe X, mas foi previsto como classe Y"
# Multi-Label = "Para o estudo, o conjunto de rótulos {A, B} foi previsto como o conjunto {A, C}"
py_run_string(r"(
import pandas as pd
import numpy as np
from collections import Counter
from sklearn.metrics import confusion_matrix, cohen_kappa_score
import re

# --- DEFINIÇÃO DAS COLUNAS PARA CADA TIPO DE ANÁLISE ---
colunas_analise_tokens = [
    'neurogenesis_marker', 
    'species', 
    'sex', 
    'strain', 
    'antidepressant_atd',
    'stress_protocol', 
    'behavioral_test',
    'atd_dose', 
    'brdu_dose', 
    'treatment_duration', 
    'survival', 
    'atd_injections', 
    'brdu_injections'
]

# Colunas single-label para Kappa e Matriz
# stress_protocol e behavioral_test excluídos porque são Multi-Label
colunas_kappa_matriz = [
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

# --- FUNÇÕES DE ANÁLISES DE TOKENS (POR ARTIGO) ---
def comparar_conteudo_artigos(df_true, df_pred, coluna):
    id_artigo = ['id']

    # 1. Agrupar dados 'manual_' por 'manual_id'
    true_agrupado = df_true.groupby(id_artigo)[coluna].apply(
        lambda x: set(tok.strip().lower() for s in x.dropna().astype(str) for tok in s.split(',') if tok.strip())
    ).rename('true_set')
    true_agrupado.index = true_agrupado.index.astype(str)

    # 2. Agrupar dados 'gemini_' por 'gemini_id'
    pred_agrupado = df_pred.groupby(id_artigo)[coluna].apply(
        lambda x: set(tok.strip().lower() for s in x.dropna().astype(str) for tok in s.split(',') if tok.strip())
    ).rename('pred_set')
    pred_agrupado.index = pred_agrupado.index.astype(str)

    # 3. 'Merge' pelo índice ('id' do artigo)
    true_agrupado.index.name = 'id'
    pred_agrupado.index.name = 'id'
    # Concatena e preenche NaNs (para artigos que só existem em um DF)
    df_comparacao = pd.concat([true_agrupado, pred_agrupado], axis=1).fillna('')

    # 4. Calcular métricas
    tp, fp, fn = 0, 0, 0
    erros_fn = Counter() # Erros de Falso Negativo
    erros_fp = Counter() # Erros de Falso Positivo
    
    for index, row in df_comparacao.iterrows():
        t_set = set(row['true_set']) if isinstance(row['true_set'], set) else set()
        p_set = set(row['pred_set']) if isinstance(row['pred_set'], set) else set()
        
        # Limpa tokens 'lixo' que não devem ser contados
        for token_lixo in ['nan', '']:
            t_set.discard(token_lixo)
            p_set.discard(token_lixo)

        tp += len(t_set & p_set)
        fp += len(p_set - t_set) # Falso Positivo: no Gemini, não no Manual
        fn += len(t_set - p_set) # Falso Negativo: no Manual, não no Gemini
        
        for erro in (t_set - p_set): erros_fn[erro] += 1
        for erro in (p_set - t_set): erros_fp[erro] += 1
            
    precision = tp / (tp + fp) if (tp + fp) > 0 else 0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0
    f1 = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0
    
    return {
        'precision': precision, 
        'recall': recall, 
        'f1_score': f1, 
        'erros_fn': dict(erros_fn.most_common(10)),
        'erros_fp': dict(erros_fp.most_common(10))
    }

# --- EXECUÇÃO DE ANÁLISES ---
analises_py_gemini = {'metricas_por_conteudo': {}, 'kappa_scores': {}, 'matrizes_confusao': {}}

# Análise de Tokens (Multi-label, por Artigo)
for coluna in colunas_analise_tokens:
    # Verifica se a coluna existe em AMBOS os dataframes de origem
    if coluna in df_manualfh_tratado.columns and coluna in df_geminifh_tratado.columns:
        try:
            # Chama a função corrigida com os dois dataframes
            resultado = comparar_conteudo_artigos(df_manualfh_tratado, df_geminifh_tratado, coluna)
            analises_py_gemini['metricas_por_conteudo'][coluna] = resultado
        except Exception as e:
            print(f"Erro ao analisar tokens da coluna '{coluna}': {e}")

# Análise de Kappa e Matriz de Confusão (Single-label, por Estudo)
df_analise_gemini = df_alinhado_gemini.dropna(subset=['manual_id', 'gemini_id']).copy()

# Filtra colunas para kappa que existem no df_alinhado_gemini
colunas_kappa_alinhadas = [col for col in colunas_kappa_matriz if f'manual_{col}' in df_analise_gemini.columns]

for coluna in colunas_kappa_alinhadas:
    try:
        coluna_manual = f'manual_{coluna}'
        coluna_gemini = f'gemini_{coluna}'

        y_true = df_analise_gemini[coluna_manual].astype(str).replace('nan', 'nr').str.lower().str.strip()
        y_pred = df_analise_gemini[coluna_gemini].astype(str).replace('nan', 'nr').str.lower().str.strip()
        
        # Calcula Kappa
        kappa_score = cohen_kappa_score(y_true, y_pred)
        if not np.isnan(kappa_score) and len(set(y_true) | set(y_pred)) > 1:
            analises_py_gemini['kappa_scores'][coluna] = kappa_score

        # Prepara dados para a Matriz de Confusão
        labels = sorted(list(set(y_true) | set(y_pred)))
        if labels:
            cm = confusion_matrix(y_true, y_pred, labels=labels)
            matriz_gemini = pd.DataFrame(cm, index=labels, columns=labels)
            
            # Formato "long" (tidy) para facilitar plotagem no R/Python
            matriz_gemini_long = matriz_gemini.reset_index().melt(id_vars='index', var_name='Predicted', value_name='Count')
            matriz_gemini_long = matriz_gemini_long.rename(columns={'index': 'Actual'})
            
            analises_py_gemini['matrizes_confusao'][coluna] = matriz_gemini_long.to_dict('records')
            
    except Exception as e:
        print(f"Erro ao analisar Kappa/Matriz da coluna '{coluna}': {e}")

)")

# Traz as análises de volta para o R
analise_r_gemini <- py$analises_py_gemini
