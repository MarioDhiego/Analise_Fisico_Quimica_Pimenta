# 1. Instalação e carregamento dos pacotes necessários

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(flextable)
library(gtsummary)
library(agricolae)






# 2. Importação dos dados
# Certifique-se de que o arquivo está no diretório de trabalho correto
df <- read_excel("Banco_Fabiana.xlsx")

# 3. Limpeza e Estruturação
df_limpo <- df %>%

# Corrige "abril" para "Abril" usando a primeira letra maiúscula
mutate(Meses = str_to_title(Meses)) %>%
  
# Transforma em fatores (variáveis categóricas) e define a ordem cronológica
mutate(
    Meses = factor(Meses, levels = c("Fevereiro", "Abril", "Maio", "Junho")),
    Pimenta = factor(Pimenta)
  ) %>%
  
# Renomeia a coluna para facilitar a digitação no R
rename(Acidez = `% acidez`)

# Visualizar a estrutura final
glimpse(df_limpo)


# 4. Cálculo das Estatísticas Descritivas
resumo_acidez <- df_limpo %>%
  group_by(Meses, Pimenta) %>%
  summarise(
    Media = round(mean(Acidez, na.rm = TRUE), 3),
    Desvio_Padrao = round(sd(Acidez, na.rm = TRUE), 3),
    N = n(),
    .groups = 'drop'
  )

# Imprime o resultado no console (o resultado será igual à tabela que apresentei antes)
print(resumo_acidez)


# 5. Criação do Gráfico Boxplot
grafico_boxplot <- ggplot(df_limpo, aes(x = Meses, y = Acidez, fill = Pimenta)) +
  geom_boxplot() +
  scale_fill_viridis_d() + # Aplica uma paleta de cores amigável e científica
  labs(
    title = "Acidez da Pimenta-do-Reino",
    #x = "Mês de Acompanhamento",
    y = "Acidez (%)",
    fill = "Tratamento"
  ) +
  theme_minimal() + # Deixa o gráfico com fundo limpo (sem caixa cinza)
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

# Exibe o gráfico
print(grafico_boxplot)



# 6. Modelo de Análise de Variância (ANOVA Two-Way)
modelo_anova <- aov(Acidez ~ Pimenta * Meses, data = df_limpo)

# 7. Tabela de Resultados
summary(modelo_anova)



# 3. Importar e padronizar os dados
df <- read_excel("Banco_Fabiana.xlsx")

df_limpo <- df %>%
  # Padroniza a escrita dos meses e define a ordem cronológica
  mutate(Meses = str_to_title(Meses)) %>%
  mutate(Meses = factor(Meses, levels = c("Fevereiro", "Abril", "Maio", "Junho"))) %>%
  # Renomeia a coluna para simplificar o código
  rename(Acidez = `% acidez`)

# 4. Gerar a tabela técnica interativa
tabela_tecnica <- df_limpo %>%
  tbl_continuous(
    variable = Acidez,              # A variável numérica que será resumida nas células
    by = Meses,                     # Variável categórica que formará as colunas
    include = Pimenta,              # Variável categórica que formará as linhas
    statistic = ~ "{mean} ± {sd}",  # Formato científico desejado: Média ± Desvio Padrão
    digits = ~ 2                    # Define 2 casas decimais
  ) %>%
  # 5. Ajustes estéticos finais
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_caption("**Tabela 1. Evolução da Acidez (%) (Média ± Desvio Padrão) ao longo dos Meses.**") %>%
  bold_labels()

# Exibe a tabela no painel Viewer do RStudio
tabela_tecnica






# 2. Importar e organizar os dados
df <- read_excel("Banco_Fabiana.xlsx")

df_limpo <- df %>%
  mutate(Meses = str_to_title(Meses)) %>%
  mutate(
    Meses = factor(Meses, levels = c("Fevereiro", "Abril", "Maio", "Junho")),
    Pimenta = factor(Pimenta)
  ) %>%
  rename(Acidez = `% acidez`)

# 3. Criar o gráfico híbrido com cores claras
grafico_hibrido <- ggplot(df_limpo, aes(x = Meses, y = Acidez, fill = Pimenta)) +
  
  # Camada 1: Violin Plot (densidade). alpha = 0.4 deixa a cor bem clara/transparente
  geom_violin(alpha = 0.4, trim = FALSE, position = position_dodge(0.8), color = "gray50") +
  
  # Camada 2: Boxplot fininho por cima do violino (width = 0.15)
  geom_boxplot(width = 0.15, alpha = 0.7, outlier.shape = NA, 
               position = position_dodge(0.8), color = "black") +
  
  # Camada 3: Jitter (pontos reais). dodge.width alinha com os violinos/boxplots
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
              alpha = 0.8, size = 2, shape = 21, color = "black") +
  
  # Paleta de cores pastéis/claras (Set2 é excelente para publicações científicas)
  scale_fill_brewer(palette = "Set2") +
  
  # Estética, títulos e temas
  labs(
    title = "Distribuição da Acidez da Pimenta-do-Reino",
   # subtitle = "Análise por tratamento ao longo dos meses (Violin + Boxplot + Raw Data)",
    x = "Meses de Acompanhamento",
    y = "Acidez (%)",
    fill = "Tratamento"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical",
    panel.grid.major.x = element_blank() # Remove as linhas de grade verticais para ficar mais limpo
  )

# Exibir o gráfico no RStudio
print(grafico_hibrido)






# 2. Importar os dados
df <- read_excel("Banco_Fabiana.xlsx")

# 3. Limpar e FILTRAR apenas "In Natura"
df_in_natura <- df %>%
  mutate(Meses = str_to_title(Meses)) %>%
  mutate(Meses = factor(Meses, levels = c("Fevereiro", "Abril", "Maio", "Junho"))) %>%
  rename(Acidez = `% acidez`) %>%
  filter(Pimenta == "In Natura") # <--- O SEGREDO ESTÁ AQUI

# 4. Criar o gráfico híbrido com cores claras
grafico_in_natura <- ggplot(df_in_natura, aes(x = Meses, y = Acidez, fill = Meses)) +
  
  # Camada 1: Violin Plot (densidade clara)
  geom_violin(alpha = 0.4, trim = FALSE, color = "gray50") +
  
  # Camada 2: Boxplot fininho (width = 0.15)
  geom_boxplot(width = 0.15, alpha = 0.7, outlier.shape = NA, color = "black") +
  
  # Camada 3: Jitter (pontos reais centralizados)
  geom_jitter(width = 0.09, 
              alpha = 0.8, 
              size = 3, 
              shape = 21, 
              color = "black") +
  
  # Paleta de cores pastéis (Tons de azul/verde claros)
  scale_fill_brewer(palette = "Pastel1") +
  
  # Estética e títulos
  labs(
    title = "Distribuição da Acidez da Pimenta-do-Reino (Grupo Controle)",
    #subtitle = "Evolução natural (In Natura) sem aplicação de recobrimento",
    #x = "Meses de Acompanhamento",
    y = "Acidez (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "none", # Remove a legenda, pois o eixo X já explica os meses
    panel.grid.major.x = element_blank()
  )

# Exibir o gráfico no RStudio
print(grafico_in_natura)




# 3. Criar a tabela de resumo para as linhas
df_resumo <- df_limpo %>%
  group_by(Meses, Pimenta) %>%
  summarise(
    Media = mean(Acidez, na.rm = TRUE),
    SD = sd(Acidez, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Definir o deslocamento (dodge)
pd <- position_dodge(width = 0.4)

# 5. Criar o Gráfico de Tendência (com legenda interna)
grafico_tendencia <- ggplot() +
  
  # Camadas de dados
  geom_jitter(
    data = df_limpo, 
    aes(x = Meses, y = Acidez, group = Pimenta),
    position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.4),
    color = "gray60", alpha = 0.5, size = 2
  ) +
  geom_errorbar(
    data = df_resumo, 
    aes(x = Meses, ymin = Media - SD, ymax = Media + SD, group = Pimenta, color = Pimenta),
    position = pd, width = 0.2, size = 0.8
  ) +
  geom_line(
    data = df_resumo, 
    aes(x = Meses, y = Media, group = Pimenta, color = Pimenta),
    position = pd, size = 1.2
  ) +
  geom_point(
    data = df_resumo, 
    aes(x = Meses, y = Media, fill = Pimenta, group = Pimenta),
    shape = 21, size = 4, color = "black", stroke = 1, position = pd
  ) +
  
  # Cores e Títulos
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Parâmetro Físico-Químico",
   # x = "Meses de Acompanhamento",
    y = "Acidez (%)",
    fill = "Tratamento",
    color = "Tratamento"
  ) +
  
  # Tema e Posicionamento da Legenda
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    
    # --- O SEGREDO DA LEGENDA INTERNA ESTÁ AQUI ---
    legend.position = c(0.85, 0.12), # Coordenadas X e Y relativas (0 a 1)
    legend.background = element_rect(fill = "white", color = "black", size = 0.3), # Caixa branca com borda
    legend.title = element_text(face = "bold") # Título da legenda em negrito
  )

# Exibir o gráfico no RStudio
print(grafico_tendencia)





# 2. Importação e limpeza
df <- read_excel("Banco_Fabiana.xlsx")
df_limpo <- df %>%
  mutate(Meses = str_to_title(Meses),
         Meses = factor(Meses, levels = c("Fevereiro", "Abril", "Maio", "Junho")),
         Pimenta = factor(Pimenta)) %>%
  rename(Acidez = `% acidez`)

# 3. Análise de Variância (ANOVA Two-Way com Interação)
modelo_anova <- aov(Acidez ~ Pimenta * Meses, data = df_limpo)

print("--- RESULTADO DA ANOVA ---")
summary(modelo_anova)

# 4. Teste de Tukey para as Interações
# Dica de Especialista: Criamos uma coluna combinando Tratamento e Mês 
# para que o R gere as letras comparando exatamente o cruzamento de ambos
df_limpo$Trat_Mes <- interaction(df_limpo$Pimenta, df_limpo$Meses, sep = " em ")
modelo_interacao <- aov(Acidez ~ Trat_Mes, data = df_limpo)

tukey_resultado <- HSD.test(modelo_interacao, "Trat_Mes", group = TRUE)

print("--- GRUPOS DE SIGNIFICÂNCIA (TUKEY) ---")
print(tukey_resultado$groups)



df_limpo <- df %>%
  mutate(Meses = str_to_title(Meses)) %>%
  mutate(Meses = factor(Meses, levels = c("Fevereiro", "Abril", "Maio", "Junho")),
         Pimenta = factor(Pimenta)) %>%
  rename(Acidez = `% acidez`)

# 3. Ajustar o Modelo Linear (Base da ANOVA Fatorial)
# Usamos lm() no lugar de aov() porque o gtsummary lê modelos lineares de forma mais fluida
modelo_lm <- lm(Acidez ~ Pimenta * Meses, data = df_limpo)

# 4. Gerar a Tabela da ANOVA com gtsummary
tabela_anova <- tbl_regression(modelo_lm) %>%
  
  # Adiciona o p-valor global (Teste F da ANOVA) para os tratamentos, meses e interação
  add_global_p(keep = TRUE) %>% 
  
  # Deixa os p-valores significativos (< 0.05) em negrito automaticamente
  bold_p() %>% 
  
  # Estética: rótulos em negrito e título da tabela
  bold_labels() %>%
  modify_caption("**Tabela 2. Modelo Linear e Análise de Variância para a Evolução da Acidez**")

# Exibe a tabela no painel Viewer do RStudio (pronta para copiar e colar)
tabela_anova


#------------------------------------------------------------------------------#
# Supondo que você importou seus dados como 'df_umidade'

df_umidade <- read_excel("Banco_Fabiana.xlsx", 
                         sheet = "Banco_Umidade")



# 3. Limpeza e padronização
df_umidade_limpo <- df_umidade %>%
  # Limpa espaços em branco e arruma a grafia de Março
  mutate(Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
         Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
         Pimenta = factor(str_trim(Pimenta))) %>%
  # Renomeia a coluna para facilitar o código
  rename(Umidade = `% umidade`)

# 4. Gerar a tabela interativa com gtsummary
tabela_gtsummary_umidade <- df_umidade_limpo %>%
  tbl_continuous(
    variable = Umidade,             # A variável numérica que vai preencher as células
    by = Meses,                     # Variável que formará as colunas
    include = Pimenta,              # Variável que formará as linhas
    statistic = ~ "{mean} ± {sd}",  # Formato científico: Média ± Desvio Padrão
    digits = ~ 2                    # 2 casas decimais
  ) %>%
  # 5. Estética final
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_caption("**Tabela 3. Evolução da Umidade (%) (Média ± Desvio Padrão) ao longo dos meses.**") %>%
  bold_labels()

# Exibe a tabela no painel "Viewer" do RStudio
tabela_gtsummary_umidade



# 4. Criar a tabela de resumo para as linhas (Média e SD)
df_resumo_umidade <- df_umidade_limpo %>%
  group_by(Meses, Pimenta) %>%
  summarise(
    Media = mean(Umidade, na.rm = TRUE),
    SD = sd(Umidade, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Definir o deslocamento (dodge) para as linhas não se sobreporem
pd <- position_dodge(width = 0.4)

# 6. Criar o Gráfico de Tendência (com legenda interna)
grafico_tendencia_umidade <- ggplot() +
  
  # CAMADA 1: Pontos de dados brutos (transparência ao fundo)
  geom_jitter(
    data = df_umidade_limpo, 
    aes(x = Meses, y = Umidade, group = Pimenta),
    position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.4),
    color = "gray60", alpha = 0.5, size = 2
  ) +
  
  # CAMADA 2: Barras de erro (Média ± SD)
  geom_errorbar(
    data = df_resumo_umidade, 
    aes(x = Meses, ymin = Media - SD, ymax = Media + SD, group = Pimenta, color = Pimenta),
    position = pd, width = 0.2, size = 0.8
  ) +
  
  # CAMADA 3: Linhas conectando as médias
  geom_line(
    data = df_resumo_umidade, 
    aes(x = Meses, y = Media, group = Pimenta, color = Pimenta),
    position = pd, size = 1.2
  ) +
  
  # CAMADA 4: Pontos das médias (Grandes e com borda preta)
  geom_point(
    data = df_resumo_umidade, 
    aes(x = Meses, y = Media, fill = Pimenta, group = Pimenta),
    shape = 21, size = 4, color = "black", stroke = 1, position = pd
  ) +
  
  # Cores contrastantes ("Dark2")
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  # Textos e Títulos
  labs(
    title = "Parâmetros Físico_Químico",
   # x = "Meses de Acompanhamento",
    y = "Umidade (%)",
    fill = "Tratamento",
    color = "Tratamento"
  ) +
  
  # Tema limpo e Posicionamento da Legenda Interna
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    
    # Colocando a legenda no canto superior direito (onde há mais espaço neste gráfico)
    legend.position = c(0.85, 0.90), 
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.title = element_text(face = "bold")
  )

# Exibir o gráfico no RStudio
print(grafico_tendencia_umidade)



#------------------------------------------------------------------------------#



df_limpo <- df_umidade %>%
  mutate(Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
         Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
         Pimenta = factor(str_trim(Pimenta))) %>%
  rename(Umidade = `% umidade`)

# 3. Definir os Limites de Controle baseados na Pimenta In Natura em Fevereiro
baseline <- df_limpo %>%
  filter(Pimenta == "In Natura", Meses == "Fevereiro")

media_controle <- mean(baseline$Umidade, na.rm = TRUE) # 10.44%
sd_controle <- sd(baseline$Umidade, na.rm = TRUE)      # 0.07%

# Calculando Limites (+/- 3 Desvios Padrões é a regra clássica de SPC)
LSC <- media_controle + (3 * sd_controle) # Limite Superior
LIC <- media_controle - (3 * sd_controle) # Limite Inferior

# 4. Tabela de médias gerais para plotar as linhas de cada tratamento
df_medias <- df_limpo %>%
  group_by(Meses, Pimenta) %>%
  summarise(Media = mean(Umidade, na.rm = TRUE), .groups = "drop")

# 5. Plotar o Gráfico de Controle
grafico_controle <- ggplot(df_medias, aes(x = Meses, y = Media, color = Pimenta, group = Pimenta)) +
  
  # Zona de Controle (Faixa cinza de aceitação)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = LIC, ymax = LSC, 
           fill = "gray80", alpha = 0.4) +
  
  # Linhas e limites de controle
  geom_hline(yintercept = media_controle, linetype = "dashed", color = "black", size = 0.8) +
  geom_hline(yintercept = LSC, linetype = "dotted", color = "red", size = 1) +
  geom_hline(yintercept = LIC, linetype = "dotted", color = "red", size = 1) +
  
  # Linhas e pontos da evolução da pimenta
  geom_line(size = 1.2) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
  
  scale_color_brewer(palette = "Set1") +
  
  # Textos e Títulos
  labs(
    title = "Gráfico de Controle de Estabilidade - Umidade (%)",
    subtitle = "Limites baseados na qualidade inicial In Natura (Fevereiro ± 3 SD)",
    x = "Mês de Armazenamento",
    y = "Média de Umidade (%)",
    color = "Tratamento"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

print(grafico_controle)




#-------------------------------------------------------------------------------#
# Grafico de Controle In Natura


# 1. Preparar os dados
df_in_natura <- df_umidade %>%
  mutate(Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
         Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
         Pimenta = str_trim(Pimenta)) %>%
  rename(Umidade = `% umidade`) %>%
  filter(Pimenta == "In Natura")

# 3. Calcular Limites Baseados em Fevereiro
baseline <- df_in_natura %>% filter(Meses == "Fevereiro")
media_controle <- mean(baseline$Umidade, na.rm = TRUE)
sd_controle <- sd(baseline$Umidade, na.rm = TRUE)

LSC <- media_controle + (3 * sd_controle) # Limite Superior
LIC <- media_controle - (3 * sd_controle) # Limite Inferior

# 4. Calcular a Evolução (Média e Desvio Padrão para a faixa azul)
df_medias <- df_in_natura %>%
  group_by(Meses) %>%
  summarise(
    Media = mean(Umidade, na.rm = TRUE),
    SD = sd(Umidade, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Construir o Gráfico de Controle
grafico_controle_in_natura <- ggplot(df_medias, aes(x = Meses, y = Media, group = 1)) +
  
  # Zona de Estabilidade (Faixa cinza ao fundo)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = LIC, ymax = LSC, 
           fill = "gray70", alpha = 0.3) +
  
  # A FAIXA AZUL: O nome aqui precisa bater com o scale_fill_manual
  geom_ribbon(aes(ymin = Media - SD, ymax = Media + SD, fill = "Zona de Estabilidade"), alpha = 0.2) +
  
  # LINHAS DE CONTROLE: Os nomes aqui precisam bater com o scale_color_manual
  geom_hline(aes(yintercept = media_controle, color = "Média"), linetype = "dashed", size = 0.8) +
  geom_hline(aes(yintercept = LSC, color = "LSC (+3 SD: 10.6%)"), linetype = "dotted", size = 1) +
  geom_hline(aes(yintercept = LIC, color = "LIC (-3 SD: 10.2%)"), linetype = "dotted", size = 1) +
  
  # Linha e pontos principais
  geom_line(color = "blue", size = 1.2) +
  geom_point(size = 4, shape = 21, fill = "blue", color = "white", stroke = 1.2) +
  
  # Configuração manual das cores e nomes da Legenda
  scale_color_manual(name = "Legendas", 
                     values = c("Média" = "darkgreen", 
                                "LSC (+3 SD: 10.6%)" = "red", 
                                "LIC (-3 SD: 10.2%)" = "red")) +
  
  scale_fill_manual(name = "Dispersão", 
                    values = c("Zona de Estabilidade" = "blue")) +
  
  # Textos e Títulos
  labs(
    title = "Gráfico de Controle de Estabilidade - Pimenta In Natura",
    subtitle = "Evolução da umidade com intervalo de dispersão amostral",
    y = "Umidade (%)"
  ) +
  
  # Tema limpo e posicionamento da legenda
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    
    # Coloca a legenda no canto inferior esquerdo (dentro do gráfico)
    # Dica: como os nomes ficaram mais longos, talvez você precise ajustar o 0.25 para não cobrir a linha
    legend.position = c(0.22, 0.25), 
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.margin = margin(5, 5, 5, 5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  )

# Exibir o gráfico no RStudio
print(grafico_controle_in_natura)



# ANOVA Umidade
# 3. Limpeza e adequação dos dados
df_umidade_limpo <- df_umidade %>%
  # Limpa espaços em branco e corrige a grafia de "Marco" para "Março"
  mutate(Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março")) %>%
  # Transforma em fatores com a ordem cronológica correta
  mutate(Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
         Pimenta = factor(str_trim(Pimenta))) %>%
  # Renomeia a coluna para facilitar as fórmulas
  rename(Umidade = `% umidade`)

# 4. Ajuste do Modelo de Análise de Variância Fatorial (Two-Way ANOVA)
modelo_umidade <- aov(Umidade ~ Pimenta * Meses, data = df_umidade_limpo)

print("--- RESULTADO DA ANOVA (UMIDADE) ---")
summary(modelo_umidade)

# 5. Criação da coluna de interação para o Tukey
# Dica: Cria uma nova coluna juntando Tratamento e Mês para gerar as letras comparativas
df_umidade_limpo$Trat_Mes <- interaction(df_umidade_limpo$Pimenta, df_umidade_limpo$Meses, sep = " em ")
modelo_interacao_um <- aov(Umidade ~ Trat_Mes, data = df_umidade_limpo)

# 6. Teste de Tukey para agrupar as médias com letras (a, b, c...)
tukey_umidade <- HSD.test(modelo_interacao_um, "Trat_Mes", group = TRUE)

print("--- GRUPOS DE SIGNIFICÂNCIA (TUKEY) ---")
print(tukey_umidade$groups)






















