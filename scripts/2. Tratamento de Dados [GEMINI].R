# === 2. Tratamento de Dados === #

#### TRATAMENTO DE DADOS ####
py_run_string(r"(
import pandas as pd
import re

# --- Funções de Padronização Geral ---
def remover_parenteses(df):
    '''Itera sobre todas as colunas de texto e remove o conteúdo entre parênteses.'''
    for col in df.select_dtypes(include=['object']).columns:
        df[col] = df[col].str.replace(r'\s*\([^)]*\)\s*', '', regex=True).str.strip()
    return df

def padronizar_minusculas(df):
    '''Converte qualquer texto para minúsculas.'''
    df.columns = df.columns.map(str).str.lower().str.replace(' ', '_').str.replace('_(atd)', '_atd')
    for col in df.select_dtypes(include=['object']).columns:
        df[col] = df[col].str.lower().str.strip()
    return df

def limpar_ano(valor):
    '''Converte anos em formato float para string de inteiro (ex: 2020.0 = 2020).'''
    if pd.isna(valor):
        return valor
    s_valor = str(valor).strip().lower().replace(' ', '')
    if s_valor in ['nan', 'na', 'n/a', 'nr']:
        return valor
    try:
        return str(int(float(valor)))
    except (ValueError, TypeError):
        return str(valor).strip().lower()

# --- Funções de Padronização Específica --- #
def padronizar_marker(valor):
    '''Padroniza marcadores de neurogênese (ex: ki67 = ki-67).'''
    if pd.isna(valor):
        return valor
    s_valor = str(valor).strip().lower().replace(' ', '') 
    if s_valor in ['nan', 'na', 'n/a', 'nr']:
         return valor

    # Dicionário de regras específicas
    marker_map = {
        'ki67': 'ki-67'
        }
    return marker_map.get(s_valor, s_valor)

def padronizar_species(valor):
    '''Padroniza os nomes das espécies (ex: rats = rat, mouse = mice).'''
    if pd.isna(valor):
        return valor
    val_limpo = str(valor).lower().strip()
    if val_limpo in ['nan', 'na', 'n/a', 'nr']:
        return valor

    # Dicionário de regras específicas    
    species_map = {
        'rats': 'rat',
        'mouse': 'mice'
    }
    return species_map.get(val_limpo, val_limpo)

def padronizar_sex(valor):
    '''Padroniza o sexo com base em um dicionário.'''
    if pd.isna(valor): return valor
    val_limpo = str(valor).lower().strip()
    if val_limpo in ['nan', 'na', 'n/a', 'nr']:
        return valor
    
    # Dicionário de regras específicas
    sex_map = {
        'male/female': 'both',
        'female/male': 'both'
    }
    return sex_map.get(val_limpo, val_limpo)

def padronizar_strain(valor):
    '''Padroniza os nomes das linhagens (ex: sprague dawley = sprague-dawley).'''
    if pd.isna(valor):
        return valor
    val_limpo = str(valor).strip().lower() 
    if val_limpo in ['nan', 'na', 'n/a', 'nr']:
        return valor
    
    # Dicionário de regras específicas
    strain_map = {
        'wistar kyoto': 'wistar-kyoto',
        'sprague dawley': 'sprague-dawley',
        'sprague–dawley': 'sprague-dawley',
        'camkiv null mice': 'camkiv',
        'camkiv null': 'camkiv',
        'balb/cj': 'balb/c',
        'c57bl/6n': 'c57bl/6',
        'swiss albino': 'swiss',
        '129/svj': '129s1',
        '129s1/svimj': '129s1',
        'lr line': 'ir line',
        'c57/bl6': 'c57bl/6'
    }
    return strain_map.get(val_limpo, val_limpo)

def padronizar_dose(valor):
    '''Padroniza a unidade de dose (ex: 10 = 10 mg/kg).'''
    if pd.isna(valor):
        return valor
    s_valor = str(valor).strip().lower().replace(' ', '')
    if s_valor in ['nan', 'na', 'n/a', 'nr']:
        return valor
    match = re.match(r'^(\d+(?:\.\d+)?)([a-zµ]*(?:/[a-zµ]+)?)', s_valor)
    if not match: return valor
    numero, unidade = match.groups()

    # Se o valor for apenas um número, assume que é mg/kg   
    if unidade in ['', 'mg/kg', 'mgkg']: return f'{numero} mg/kg'
    else: return f'{numero} {unidade}'

def padronizar_tempo(valor):
    '''Padroniza o tempo: converte meses/semanas/dias para 'x day' e horas para 'x hour', EXCETO '24 horas' (= '1 day').'''
    if pd.isna(valor):
        return valor
    v = str(valor).strip().lower() 
    if v in ['nan', 'na', 'n/a', 'nr']:
        return valor

    # Regra para dias pós-natais (ex: 'P3 a P15')
    # Procura pelo padrão P(número) - P(número) ou P(número) a P(número)
    pn_match = re.match(r'p(\d+)\s*(?:to|-)\s*p(\d+)', v)
    if pn_match:
        dia_inicio = int(pn_match.group(1))
        dia_fim = int(pn_match.group(2))
        duracao = dia_fim - dia_inicio
        return f"{duracao} day"
    
    v = re.sub(r'\b(horas?|hours?|h)\b', 'hour', v)
    v = re.sub(r'\b(dias?|days?)\b', 'day', v)
    v = re.sub(r'\b(semanas?|weeks?)\b', 'week', v)
    v = re.sub(r'\b(meses?|months?)\b', 'month', v)

    v = v.replace(' ', '')

    match = re.match(r'(\d+(?:\.\d+)?)\s*([a-z]+)', v)
    if match:
        numero = float(match.group(1))
        unidade = match.group(2)
        if unidade == 'hour':
            if numero == 24:
                return '1 day'
            elif numero == 72:
                return '3 day'
            else:
                return f"{round(numero)} hour"

        elif unidade == 'month':
            dias = numero * 28
            return f"{round(dias)} day"
        elif unidade == 'week':
            dias = numero * 7
            return f"{round(dias)} day"
        elif unidade == 'day':
            return f"{round(numero)} day"

    # Se o valor for apenas um número, assume que são dias
    if v.replace('.', '', 1).isdigit():
        return f"{round(float(v))} day"
    return v

# --- Funções Específicas com Dicionários Robustos ---
def criar_mapa_otimizado(dicionario):
    mapa_otimizado = {}
    for k, v in dicionario.items():
        chave_ordenada = tuple(sorted(k.replace('-', ' ').split()))
        mapa_otimizado[chave_ordenada] = v
    return mapa_otimizado

# Função para CONCATENAR valores (colunas stress e behavioral)
def padronizar_valor_concatenado(valor, mapa_otimizado):
    if pd.isna(valor):
        return valor
    s_valor = str(valor).strip().lower()
    if s_valor in ['nan', 'na', 'n/a', 'nr']:
        return valor
    items = [item.strip() for item in re.split(r'[;,]', s_valor)]
    siglas = set()
    for item in items:
        item_limpo = re.sub(r'[-_]', ' ', item)
        palavras_ordenadas = tuple(sorted(item_limpo.split()))
        sigla = mapa_otimizado.get(palavras_ordenadas, item) 
        siglas.add(sigla)
    return ', '.join(sorted(list(siglas)))

# Função para padronizar valores ÚNICOS (para colunas explodidas)
def padronizar_valor_unico(valor, mapa_otimizado):
    if pd.isna(valor):
        return valor
    val_limpo = str(valor).strip().lower() 
    if val_limpo in ['nan', 'na', 'n/a', 'nr']:
        return valor
    val_limpo = re.sub(r'[-_]', ' ', val_limpo) 
    palavras_ordenadas = tuple(sorted(val_limpo.split()))
    return mapa_otimizado.get(palavras_ordenadas, val_limpo)

# --- Dicionários e Mapas ---
antidepressants_map = {
        'fluoxetine': 'flx', 'imipramine': 'imi', 'amitriptyline': 'ami', 'citalopram': 'cit',
        'desipramine': 'des', 'escitalopram': 'esc', 'paroxetine': 'prx', 'fluvoxamine': 'flv'
    }
antidepressants_map_otimizado = criar_mapa_otimizado(antidepressants_map)

protocolos_map = {
        'chronic unpredictable mild stress': 'cums', 'unpredictable chronic mild stress': 'cums', 
        'ucms': 'cums', 'cums + mcao': 'cums', 'chronic unpredictable stress': 'cus', 
        'chronic mild stress': 'cms', 'water immersion restraint stress': 'wirs', 'lps-induced': 'lps', 
        'morris water maze': 'mwm', 'maternal separation': 'ms', 'maternal deprivation': 'ms', 
        'social stress': 'sir', 'early-life stress': 'els', 'wirs / restraint stress': 'wirs', 
        'lps-induced depression model': 'lps', 'isolation rearing': 'sir', 'lipopolysaccharideinduced': 'lps',
        'rotenone-induced pd model': 'rotenone'
    }
protocolos_map_otimizado = criar_mapa_otimizado(protocolos_map)

testes_map = {
        'forced swim test': 'fst', 'forced swim': 'fst', 'porsolt test': 'fst', 'forced swimming': 'fst',
        'forced swimming test': 'fst', 'tail suspension test': 'tst', 'tail suspension': 'tst', 
        'sucrose preference test': 'spt', 'sucrose preference': 'spt', 'open field test': 'oft', 
        'open field': 'oft', 'locomotor activity': 'oft', 'novelty-suppressed feeding test': 'nsf', 
        'novelty-suppressed feeding': 'nsf', 'novelty-suppressed feeding (nsf)': 'nsf', 
        'contextual fear conditioning': 'fcp', 'fear conditioning paradigm': 'fcp', 
        'social interaction test': 'sit', 'social interaction': 'sit', 'splash test': 'spl',
        'splash': 'spl', 'elevated plus maze': 'epm', 'nest test': 'nest', 'shuttle-box test': 'sbx',
        'shuttle-box': 'sbx', 'morris water maze': 'mwm', 'water maze': 'mwm', 
        'unconditioning test with tone': 'utt', 'grip strength': 'gst', 'beam-walking': 'bwt', 
        'other behavioral tests': 'other', 'fear conditioning': 'fcp', 'shuttle-box escape test': 'sbx', 
        'nest building test': 'nest', 'rotarod': 'rot', 'actimeter': 'act', 'cylinder': 'cyl', 
        'morris water-maze test': 'mwm', 'elevated plus maze test': 'epm', 'forced swim task': 'fst',
        'tail suspension task': 'tst', 'beam-walking test': 'bwt', 'cued fear-conditioning test': 'fcp',
        'cylinder test': 'cyl', 'grip strength test': 'gst', 'open-field behavior': 'oft', 'rotarod test': 'rot',
        'spontaneous locomotor activity': 'oft', 'water maze test': 'mwm', 'novel suppressed feeding test': 'nsf',
        'sucrose intake': 'spt'
    }
testes_map_otimizado = criar_mapa_otimizado(testes_map)

# --- Funções de Aplicação ---
def padronizar_estresse(valor):
    return padronizar_valor_concatenado(valor, protocolos_map_otimizado)

def padronizar_teste(valor):
    return padronizar_valor_concatenado(valor, testes_map_otimizado)

def padronizar_atd(valor):
    return padronizar_valor_unico(valor, antidepressants_map_otimizado)

# --- Função Principal de Tratamento ---
def tratar_dataframe(df):
    df = df.copy()
    df = remover_parenteses(df)
    df = padronizar_minusculas(df)
    
    # "EXPLODIR" LINHAS PARA CRIAR NOVOS ESTUDOS
    # Define as colunas que NÃO devem ser explodidas
    cols_to_exclude = ['id', 'author', 'year', 'study', 'stress_protocol', 'behavioral_test']
    
    # Cria a lista de colunas dinamicamente para explodir 
    cols_to_explode = [col for col in df.columns if col not in cols_to_exclude]

    for col in cols_to_explode:
        if col in df.columns:
            # Garante que a coluna seja string (para lidar com números e outros tipos)
            df[col] = df[col].astype(str).str.split(r'\s*[;,]\s*')
            df = df.explode(col)
            df[col] = df[col].str.strip()

    # PADRONIZAR VALORES (APÓS EXPLODIR)
    colunas_para_tratar = {
        'year': limpar_ano,
        'atd_dose': padronizar_dose,
        'brdu_dose': padronizar_dose,
        'treatment_duration': padronizar_tempo,
        'survival': padronizar_tempo,
        'species': padronizar_species,
        'antidepressant_atd': padronizar_atd,
        'strain': padronizar_strain,
        'neurogenesis_marker': padronizar_marker,
        'sex': padronizar_sex, 
        'stress_protocol': padronizar_estresse,
        'behavioral_test': padronizar_teste
    }
    for coluna, funcao in colunas_para_tratar.items():
        if coluna in df.columns:
            df[coluna] = df[coluna].apply(funcao)
            
    if 'atd_injections' in df.columns:
        df['atd_injections'] = df['atd_injections'].astype(str).str.replace('/day', '', regex=False).str.strip()

    # GERAR ID SEQUENCIAL PARA ESTUDOS
    if 'id' in df.columns:
        df = df.sort_values('id').reset_index(drop=True)
        df['id_estudo'] = df.groupby('id').cumcount() + 1
    
    # LIMPEZA FINAL DO ÍNDICE
    # Garante um índice limpo e sequencial antes de retornar ao R
    df = df.reset_index(drop=True)

    return df
)")

# Executa o tratamento nos dataframes no ambiente Python
py_run_string("
df_manualfh_tratado = tratar_dataframe(df_manualfh)
df_geminifh_tratado = tratar_dataframe(df_geminifh)
")

# Traz os dataframes tratados para o R (renomeados)
df_manualfh_tratado <- py$df_manualfh_tratado
df_geminifh_tratado <- py$df_geminifh_tratado

#### VERIFICAÇÃO DE FUNÇÕES APLICADAS NO PYTHON ####
# Garantem que cada função funciona como esperado
py_run_string(r"(
import unittest

class TestPadronizacaoFuncoes(unittest.TestCase):
    def test_limpar_ano(self):
        self.assertEqual(limpar_ano('2020.0'), '2020')
        self.assertEqual(limpar_ano('1999'), '1999')

    def test_padronizar_dose(self):
        self.assertEqual(padronizar_dose('10'), '10 mg/kg')
        self.assertEqual(padronizar_dose('10mg/kg'), '10 mg/kg')

    def test_padronizar_tempo(self):
        self.assertEqual(padronizar_tempo('4 week'), '28 day')
        self.assertEqual(padronizar_tempo('14 dias'), '14 day')
        self.assertEqual(padronizar_tempo('24 hours'), '1 day')
        self.assertEqual(padronizar_tempo('72 hours'), '3 day')
        self.assertEqual(padronizar_tempo('2 hora'), '2 hour')
        self.assertEqual(padronizar_tempo('6'), '6 day')
        self.assertEqual(padronizar_tempo('p3 to p15'), '12 day')

    def test_padronizar_species(self):
        self.assertEqual(padronizar_species('rats'), 'rat')
        self.assertEqual(padronizar_species('mouse'), 'mice')

    def test_padronizar_strain(self):
        self.assertEqual(padronizar_strain('sprague dawley'), 'sprague-dawley')
        self.assertEqual(padronizar_strain('c57bl/6n'), 'c57bl/6')
        
    def test_padronizar_atd(self):
        self.assertEqual(padronizar_atd('fluoxetine'), 'flx')
        self.assertEqual(padronizar_atd('imipramine'), 'imi')

    def test_padronizar_estresse_concatenado(self):
        self.assertEqual(padronizar_estresse('chronic unpredictable mild stress; lps-induced'), 'cums, lps')

    def test_padronizar_teste_concatenado(self):
        self.assertEqual(padronizar_teste('tail suspension, splash'), 'spl, tst')

# Executa os testes e mostra os resultados
suite = unittest.TestLoader().loadTestsFromTestCase(TestPadronizacaoFuncoes)
unittest.TextTestRunner(verbosity=2).run(suite)
)")

#### VERIFICAÇÃO DOS DADOS TRATADOS ####
# Verificar a concatenação: devem existir valores com ',' nesta coluna
cat("\nValores concatenados em 'behavioral_test':\n")
print(head(unique(df_manualfh_tratado$behavioral_test)))
print(head(unique(df_geminifh_tratado$behavioral_test)))
cat("\nValores concatenados em 'stress_protocol':\n")
print(head(unique(df_manualfh_tratado$stress_protocol)))
print(head(unique(df_geminifh_tratado$stress_protocol)))
cat("\nValores concatenados em 'survival':\n")
print(head(unique(df_manualfh_tratado$survival)))
print(head(unique(df_geminifh_tratado$survival)))
cat("\nValores concatenados em 'brdu_dose':\n")
print(head(unique(df_manualfh_tratado$brdu_dose)))
print(head(unique(df_geminifh_tratado$brdu_dose)))
cat("\nValores concatenados em 'brdu_injections':\n")
print(head(unique(df_manualfh_tratado$brdu_injections)))
print(head(unique(df_geminifh_tratado$brdu_injections)))
cat("\nValores concatenados em 'atd_injections':\n")
print(head(unique(df_manualfh_tratado$atd_injections)))
print(head(unique(df_geminifh_tratado$atd_injections)))
cat("\nValores concatenados em 'treatment_duration':\n")
print(head(unique(df_manualfh_tratado$treatment_duration)))
print(head(unique(df_geminifh_tratado$treatment_duration)))
cat("\nValores concatenados em 'atd_dose':\n")
print(head(unique(df_manualfh_tratado$atd_dose)))
print(head(unique(df_geminifh_tratado$atd_dose)))
cat("\nValores concatenados em 'antidepressant_atd':\n")
print(head(unique(df_manualfh_tratado$antidepressant_atd)))
print(head(unique(df_geminifh_tratado$antidepressant_atd)))
cat("\nValores concatenados em 'strain':\n")
print(head(unique(df_manualfh_tratado$strain)))
print(head(unique(df_geminifh_tratado$strain)))
cat("\nValores concatenados em 'sex':\n")
print(head(unique(df_manualfh_tratado$sex)))
print(head(unique(df_geminifh_tratado$sex)))
cat("\nValores concatenados em 'species':\n")
print(head(unique(df_manualfh_tratado$species)))
print(head(unique(df_geminifh_tratado$species)))
cat("\nValores concatenados em 'neurogenesis_marker':\n")
print(head(unique(df_manualfh_tratado$neurogenesis_marker)))
print(head(unique(df_geminifh_tratado$neurogenesis_marker)))

# Verificar se as novas padronizações foram aplicadas
cat("\nValores únicos em 'species':\n")
print(unique(df_manualfh_tratado$species))
print(unique(df_geminifh_tratado$species))
cat("\nValores únicos em 'antidepressant_atd':\n")
print(unique(df_manualfh_tratado$antidepressant_atd))
print(unique(df_geminifh_tratado$antidepressant_atd))

# Verificar colunas específicas
head(df_geminifh_tratado[, c("atd_injections", "treatment_duration")])
head(df_manualfh_tratado[, c("atd_injections", "treatment_duration")])

# Verificar se há valores “nr”
sapply(df_manualfh_tratado, function(x) sum(tolower(x) == "nr", na.rm = TRUE))
sapply(df_geminifh_tratado, function(x) sum(tolower(x) == "nr", na.rm = TRUE))

# Verificar combinações incomuns
table(df_geminifh_tratado$atd_injections, df_geminifh_tratado$treatment_duration)
table(df_geminifh_tratado$antidepressant_atd, df_geminifh_tratado$atd_dose)

# Verificar estrutura e tipos de dados
str(df_manualfh_tratado)
str(df_geminifh_tratado)

# Verificar consistência entre planilhas
cat("\nConsistência para a coluna 'stress_protocol':\n")
setdiff(unique(df_manualfh_tratado$stress_protocol), unique(df_geminifh_tratado$stress_protocol))
setdiff(unique(df_geminifh_tratado$stress_protocol), unique(df_manualfh_tratado$stress_protocol))
