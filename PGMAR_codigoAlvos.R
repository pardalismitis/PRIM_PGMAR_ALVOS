# ============================================================
# PIPELINE DE EXTRAÇÃO E NORMALIZAÇÃO DE ALVOS DE CONSERVAÇÃO
# ============================================================
#
# Este script realiza a leitura, estruturação e normalização
# de listas de alvos de conservação extraídas de arquivos CSV
# derivados de PDFs institucionais.
#
# O objetivo é transformar um texto originalmente não estruturado
# (com múltiplas UPs e diferentes componentes misturados)
# em um dataframe estruturado em formato longo, contendo:
#
#   UP | Tipo | Conteudo
#
# Onde:
#   UP        → Unidade de Planejamento
#   Tipo      → componente (Fauna, Flora, Ambientes Singulares, Serviços Ecossistêmicos)
#   Conteudo  → alvo individual
#
# O pipeline realiza:
#
# 1) leitura do arquivo bruto
# 2) reconstrução de blocos de texto por UP
# 3) separação dos componentes temáticos
# 4) transformação para formato longo
# 5) limpeza e normalização textual
# 6) aplicação de correções conhecidas
# 7) exportação do resultado final
#
# ============================================================


# ============================================================
# 0) Carregamento de bibliotecas
# ============================================================

# readr  → leitura e escrita eficiente de arquivos
# dplyr  → manipulação de dataframes
# stringr → manipulação de strings com regex
# tidyr  → transformação estrutural de tabelas

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# ============================================================
# 1) Leitura do arquivo bruto
# ============================================================

# O arquivo de entrada possui apenas uma coluna contendo
# linhas de texto extraídas do PDF.
alvos <- read_csv("PGMAR_entradaAlvos.csv", col_names = FALSE, show_col_types = FALSE)

# Padroniza o nome da coluna para facilitar manipulações posteriores
colnames(alvos) <- "coluna"

alvos <- alvos %>%
  mutate(
    coluna = coluna %>%
      str_replace_all(
        "Marinho",
        "marinho"
      )
  )
# ============================================================
# 2) Reconstrução do texto original
# ============================================================

# Em alguns casos, durante a extração do PDF,
# múltiplas Unidades de Planejamento (UPs) acabam
# sendo fragmentadas ou combinadas em linhas diferentes.
#
# Para permitir uma extração robusta baseada em padrões,
# todo o texto é primeiro colapsado em uma única string.

texto_unico <- alvos$coluna %>%
  str_squish() %>%        # remove espaços duplicados
  str_c(collapse = " ")   # concatena todas as linhas

# ============================================================
# 3) Identificação dos blocos de UP
# ============================================================

# A expressão regular abaixo identifica blocos que seguem o padrão:
#
#    número_da_UP + texto associado
#
# O bloco termina quando aparece:
#
#    outro número isolado indicando nova UP
#
# Estrutura do padrão:
#
# (\\b\\d+\\b)      → captura o número da UP
# \\s+              → espaço
# (.*?)             → captura o texto associado
# (?= ...)          → lookahead que detecta o início do próximo bloco

padrao <- "(\\b\\d+\\b)\\s+(marinho|costeiro)\\s+(Fauna:.*?)(?=\\s+\\d+\\s+(marinho|costeiro)\\s+Fauna:|$)"
#padrao <- "(\\b\\d+\\b)\\s+(.*?)(?=\\s+\\d+\\s+(Fauna:|$)|$)"

matches <- str_match_all(texto_unico, padrao)[[1]]

# Constrói um dataframe contendo:
#
# ID    → número da UP
# Ambiente → marinho/costeiro 
# texto → bloco de texto correspondente

resultado <- tibble(
  ID = matches[,2],
  Ambiente = matches[,3],
  texto = str_squish(matches[,4])
) %>%
  filter(str_detect(texto, "Fauna:")) %>%
  distinct()

# ============================================================
# 4) Extração dos componentes temáticos
# ============================================================

# Cada bloco de texto contém subseções com rótulos como:
#
# Fauna:
# Flora:
# Ambientes Singulares:
# Serviços Ecossistêmicos:
#
# Esta etapa padroniza os rótulos e extrai o conteúdo de cada um.

resultado_blocos <- resultado %>%
  
# ----------------------------------------------------------
# 4.1 Padronização preventiva de rótulos
# ----------------------------------------------------------
  mutate(
    texto = texto %>%
      str_replace_all(
        "Formicivora acutirostris Formicivora paludicola,,",
        "Formicivora acutirostris, Formicivora paludicola,"
    ) %>%
    str_replace_all(
      "_",
      " "
    ) %>%
    str_replace_all(
      " subsp ",
      " subsp. "
    ) %>%
    str_replace_all(
      " var ",
      " var. "
    ) %>%
    
      # Normaliza variações de "Ambientes"
  #    str_replace_all(
   #     regex("Fitofisionomia:|Ambientes(?!:)", ignore_case = TRUE),
    #    "Ambientes:"
     # ) %>%
      # Padroniza uso de hífens
  #    str_replace_all("\\s*-\\s*", "-") #\\s* = qualquer quantidade de espaço (inclusive zero)
  #) %>%
  
  #mutate(
   # texto = texto %>%
      # Garante que "Flora" sempre possua dois pontos, sempre que for uma palavra isolada
#      str_replace_all(
 #       regex("\\bFlora\\b(?!:)", ignore_case = TRUE),
  #      "Flora:"
   #   ) %>%
      str_replace_all("\\s*-\\s*", "-")
)%>%
# ----------------------------------------------------------
# 4.2 Extração dos blocos por componente
# ----------------------------------------------------------
mutate(
  Fauna = str_extract(texto, "Fauna:.*?(?=Flora:|Ambientes Singulares?:|Serviços Ecossistêmicos:|$)"),
  Flora = str_extract(texto, "Flora:.*?(?=Fauna:|Ambientes Singulares?:|Serviços Ecossistêmicos:|$)"),
  Ambientes_Singulares = str_extract(texto, "Ambientes Singulares?:.*?(?=Fauna:|Flora:|Serviços Ecossistêmicos:|$)"),
  Servicos = str_extract(texto, "Serviços Ecossistêmicos:.*?(?=Fauna:|Flora:|Ambientes Singulares?:|$)")
) %>%
  
# ----------------------------------------------------------
# 4.3 Remoção dos rótulos
# ----------------------------------------------------------

mutate(
  Fauna = str_remove(Fauna, "^Fauna:\\s*"),
  Flora = str_remove(Flora, "^Flora:\\s*"),
  Ambientes_Singulares = str_remove(Ambientes_Singulares, "^Ambientes Singulares?:\\s*"),
  Servicos = str_remove(Servicos, "^Serviços Ecossistêmicos:\\s*")
)

# ============================================================
# 5) Transformação para formato longo
# ============================================================

# Converte a estrutura de colunas em formato longo.
#
# Exemplo:
#
# ID | Fauna | Flora
#
# torna-se:
#
# ID | Ambiente | Tipo | Conteudo

alvos_long <- resultado_blocos %>%
  select(ID, Ambiente, Fauna, Flora, Ambientes_Singulares, Servicos) %>%
  pivot_longer(
    cols = -c(ID, Ambiente),
    names_to = "Tipo",
    values_to = "Conteudo"
  ) %>%
  # Remove registros vazios
  filter(!is.na(Conteudo)) %>%
  
# ----------------------------------------------------------
# 5.1 Separação de múltiplos alvos
# ----------------------------------------------------------

# Muitos registros possuem múltiplos alvos separados por vírgula

  separate_rows(Conteudo, sep = ",") %>%
  
# ----------------------------------------------------------
# 5.2 Limpeza textual
# ----------------------------------------------------------

  mutate(
    Conteudo = str_trim(Conteudo),
    # remove ponto e vírgula no final
    Conteudo = str_remove(Conteudo, ";+$"),
    # remove pontos finais duplicados
    Conteudo = str_remove(Conteudo, "\\.+$")  # remove ponto(s) apenas no final
    #\\. → ponto literal
    #+ → um ou mais
    #$ → apenas no final
  ) %>%
  filter(Conteudo != "")


# ============================================================
# 6) Correções manuais conhecidas
# ============================================================

# Algumas inconsistências presentes no material original
# exigem correções específicas.
#
# Estas correções incluem:
#
# • erros de digitação
# • problemas de encoding
# • nomes científicos quebrados
# • rótulos institucionais incompletos

correcoes <- tribble(
  ~Conteudo_antigo, ~Conteudo_novo,
  "Halichondria Halichondria cebimarensis", "Halichondria cebimarensis",
  "Latrunculia Biannulata_janeirensis", "Latrunculia janeirensis",
  "Mikania hastato cordata", "Mikania hastato-cordata",
  "Acanthosyris paulo alvinii", "Acanthosyris paulo-alvinii",
  "Cryptanthus burle marxii", "Cryptanthus burle-marxii",
  "Erythroxylum petrae caballi", "Erythroxylum petrae-caballi",
  "Padraria de fanerógamas", "Pradaria de fanerógamas (seagrass)",
  "Important Bird Areas", "Important Bird Areas (IBAS)",
  "Buchenavia parvifolia subsp. Rabelloana", "Buchenavia parvifolia subsp. rabelloana"
  )

# Aplica as correções ao dataframe principal

alvos_long <- alvos_long %>%
  left_join(correcoes, by = c("Conteudo" = "Conteudo_antigo")) %>%
  mutate(Conteudo = coalesce(Conteudo_novo, Conteudo)) %>%
  select(-Conteudo_novo) #%>%
  #rename(UP = ID)

# ============================================================
# 7) Checagens exploratórias
# ============================================================

# Extração para checagem de alvos por Grupo
# Fauna
alvos_fauna <- alvos_long |>
  filter(Tipo == "Fauna") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_fauna

# Flora
alvos_flora <- alvos_long |>
  filter(Tipo == "Flora") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_flora

alvos_flora[str_count(alvos_flora, "\\S+") > 2]

# Ambientes Singulares
alvos_ambientes <- alvos_long |>
  filter(Tipo == "Ambientes_Singulares") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_ambientes

# Ambientes
alvos_servicos <- alvos_long |>
  filter(Tipo == "Servicos") |>
  pull(Conteudo) |>
  unique() |>
  sort()
alvos_servicos

# Exibe todos os tipos identificados

sort(unique(alvos_long$Tipo))

# Exibe todos os alvos únicos

sort(unique(alvos_long$Conteudo))

# Estrutura do dataframe

str(alvos_long)

# Nome das colunas

names(alvos_long)

# ============================================================
# 8) Exportação do resultado final
# ============================================================

# O arquivo final contém:
#
# UP | Tipo | Conteudo
#
# Cada linha representa um alvo individual associado
# a uma Unidade de Planejamento e a um componente temático.
alvos_Integracao <- alvos_long %>%
  select(Tipo,
         Conteudo
         ) %>%
  distinct(Conteudo, .keep_all = TRUE) %>%
  rename(
    COMPONENTE = Tipo,
    ALVO = Conteudo
  )

write_csv(alvos_Integracao, "alvos_Processados.csv")
write_csv(alvos_long, "PGMAR_alvosProcessado.csv")

