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






df_cinzas <- read_excel("Banco_Fabiana.xlsx", sheet = "Banco_Cinzas")

df_cinzas_limpo <- df_cinzas %>%
  mutate(Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
         Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
         Pimenta = factor(str_trim(Pimenta))) %>%
  rename(Cinzas = `% Cinzas`)

tabela_cinzas <- df_cinzas_limpo %>%
  tbl_continuous(
    variable = Cinzas,
    by = Meses,
    include = Pimenta,
    statistic = ~ "{mean} ± {sd}",
    digits = ~ 2
  ) %>%
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_caption("**Tabela 4. Evolução do teor de Cinzas (%) (Média ± SD) ao longo do armazenamento.**") %>%
  bold_labels()

tabela_cinzas



# Usa o mesmo df_cinzas_limpo gerado no script acima
modelo_cinzas <- aov(Cinzas ~ Pimenta * Meses, data = df_cinzas_limpo)

print("--- ANOVA DAS CINZAS ---")
summary(modelo_cinzas)

# Interação
df_cinzas_limpo$Trat_Mes <- interaction(df_cinzas_limpo$Pimenta, df_cinzas_limpo$Meses, sep = " em ")
modelo_int_cinzas <- aov(Cinzas ~ Trat_Mes, data = df_cinzas_limpo)

tukey_cinzas <- HSD.test(modelo_int_cinzas, "Trat_Mes", group = TRUE)
print(tukey_cinzas$groups)





# 4. Criar a tabela de resumo para as linhas (Média e SD)
df_resumo_cinzas <- df_cinzas_limpo %>%
  group_by(Meses, Pimenta) %>%
  summarise(
    Media = mean(Cinzas, na.rm = TRUE),
    SD = sd(Cinzas, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Definir o deslocamento (dodge) para as barras de erro não se sobreporem
pd <- position_dodge(width = 0.4)

# 6. Criar o Gráfico de Tendência
grafico_tendencia_cinzas <- ggplot() +
  
  geom_jitter(data = df_cinzas_limpo, aes(x = Meses, y = Cinzas, group = Pimenta),
              position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.4),
              color = "gray60", alpha = 0.5, size = 2) +
  
  geom_errorbar(data = df_resumo_cinzas, aes(x = Meses, ymin = Media - SD, ymax = Media + SD, group = Pimenta, color = Pimenta),
                position = pd, width = 0.2, size = 0.8) +
  
  geom_line(data = df_resumo_cinzas, aes(x = Meses, y = Media, group = Pimenta, color = Pimenta),
            position = pd, size = 1.2) +
  
  geom_point(data = df_resumo_cinzas, aes(x = Meses, y = Media, fill = Pimenta, group = Pimenta),
             shape = 21, size = 4, color = "black", stroke = 1, position = pd) +
  
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  # Forçando o eixo Y a ter espaço de sobra na parte de baixo para a legenda não cobrir nada
  scale_y_continuous(limits = c(3.5, 5.5)) +
  
  labs(
    title = "Tendência do Teor de Cinzas ao Longo do Armazenamento",
    x = "Meses de Armazenamento",
    y = "Cinzas (%)",
    fill = "Tratamento",
    color = "Tratamento"
  ) +
  
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    
    # Legenda agora fica no canto inferior esquerdo, na vertical, elegante e compacta
    legend.position = c(0.20, 0.22), 
    legend.background = element_rect(fill = alpha("white", 0.9), color = "black", size = 0.3),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    legend.direction = "vertical",
    legend.key.height = unit(0.8, "cm") # Dá um respiro entre as linhas da legenda
  )

# Exibir o gráfico no RStudio
print(grafico_tendencia_cinzas)




















#------------------------------------------------------------------------------#
# Atributo : Proteínas


# 2. Importar e limpar
df_prot <- read_excel("Banco_Fabiana.xlsx", sheet = "Banco_Proteinas") %>%
  mutate(Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
         Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
         Pimenta = factor(str_trim(Pimenta))) %>%
  rename(Proteina = `% Proteína`)

# 3. Gerar a tabela com gtsummary
tabela_prot <- df_prot %>%
  tbl_continuous(
    variable = Proteina,
    by = Meses,
    include = Pimenta,
    statistic = ~ "{mean} ± {sd}",
    digits = ~ 2
  ) %>%
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_caption("**Tabela 6. Evolução do teor de Proteínas (%) (Média ± SD) ao longo do armazenamento.**") %>%
  bold_labels()

# Exibir tabela
tabela_prot




# 4. Criar a tabela de resumo para as linhas (Média e SD)
df_resumo_prot <- df_prot %>%
  group_by(Meses, Pimenta) %>%
  summarise(
    Media = mean(Proteina, na.rm = TRUE),
    SD = sd(Proteina, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Definir o deslocamento (dodge) para as linhas não se sobreporem
pd <- position_dodge(width = 0.4)

# 6. Criar o Gráfico de Tendência (com legenda interna)
grafico_tendencia_prot <- ggplot() +
  
  # CAMADA 1: Pontos de dados brutos (transparência ao fundo)
  geom_jitter(
    data = df_prot, 
    aes(x = Meses, y = Proteina, group = Pimenta),
    position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.4),
    color = "gray60", alpha = 0.5, size = 2
  ) +
  
  # CAMADA 2: Barras de erro (Média ± SD)
  geom_errorbar(
    data = df_resumo_prot, 
    aes(x = Meses, ymin = Media - SD, ymax = Media + SD, group = Pimenta, color = Pimenta),
    position = pd, width = 0.2, size = 0.8
  ) +
  
  # CAMADA 3: Linhas conectando as médias
  geom_line(
    data = df_resumo_prot, 
    aes(x = Meses, y = Media, group = Pimenta, color = Pimenta),
    position = pd, size = 1.2
  ) +
  
  # CAMADA 4: Pontos das médias (Grandes e com borda preta)
  geom_point(
    data = df_resumo_prot, 
    aes(x = Meses, y = Media, fill = Pimenta, group = Pimenta),
    shape = 21, size = 4, color = "black", stroke = 1, position = pd
  ) +
  
  # Cores contrastantes ("Dark2")
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  # Ajuste do eixo Y para criar um "céu" vazio no topo para a legenda
  scale_y_continuous(limits = c(8, 14.5)) + 
  
  # Textos e Títulos
  labs(
    title = "Parâmetros Físico-Químico",
    # x = "Meses de Acompanhamento",
    y = "Proteína (%)",
    fill = "Tratamento",
    color = "Tratamento"
  ) +
  
  # Tema limpo e Posicionamento da Legenda Interna
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    
    # Colocando a legenda no canto superior direito. 
    # Usei 0.70 no X para o texto longo não vazar do gráfico
    legend.position = c(0.85, 0.14), 
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.title = element_text(face = "bold")
  )

# Exibir o gráfico no RStudio
print(grafico_tendencia_prot)










# 1. Definir a Linha Base e Limites (Fevereiro - In Natura)
base_prot <- df_prot %>% filter(Pimenta == "In Natura", Meses == "Fevereiro")
media_base <- mean(base_prot$Proteina, na.rm = TRUE) # 10.69
sd_base <- sd(base_prot$Proteina, na.rm = TRUE)      # 0.21
LSC <- media_base + (3 * sd_base)                    # 11.32
LIC <- media_base - (3 * sd_base)                    # 10.06

# 2. Resumo de Médias de Todos os Tratamentos
df_medias_todas <- df_prot %>%
  group_by(Meses, Pimenta) %>%
  summarise(Media = mean(Proteina, na.rm=TRUE), SD = sd(Proteina, na.rm=TRUE), .groups="drop")

# 3. GRÁFICO 1: Apenas In Natura
graf_controle_innatura <- ggplot(df_medias_todas %>% filter(Pimenta == "In Natura"), aes(x = Meses, y = Media, group = 1)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = LIC, ymax = LSC, fill = "gray70", alpha = 0.3) +
  geom_ribbon(aes(ymin = Media - SD, ymax = Media + SD, fill = "Dispersão Amostral"), alpha = 0.2) +
  geom_hline(aes(yintercept = media_base, color = "Média Referência"), linetype = "dashed", size = 0.8) +
  geom_hline(aes(yintercept = LSC, color = "LSC (+3 SD)"), linetype = "dotted", size = 1) +
  geom_hline(aes(yintercept = LIC, color = "LIC (-3 SD)"), linetype = "dotted", size = 1) +
  geom_line(color = "blue", size = 1.2) + geom_point(size = 4, shape = 21, fill = "blue", color = "white", stroke = 1.2) +
  scale_color_manual(name = "Limites", values = c("Média Referência" = "darkgreen", "LSC (+3 SD)" = "red", "LIC (-3 SD)" = "red")) +
  scale_fill_manual(name = "", values = c("Dispersão Amostral" = "blue")) +
  labs(title = "Controle de Estabilidade - Proteína (In Natura)", x = "Mês", y = "Proteína (%)") +
  theme_bw() + theme(legend.position = "bottom")

# 4. GRÁFICO 2: Todos os Tratamentos na Zona de Controle
graf_controle_todos <- ggplot(df_medias_todas, aes(x = Meses, y = Media, group = Pimenta, color = Pimenta)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = LIC, ymax = LSC, fill = "gray70", alpha = 0.3) +
  geom_hline(yintercept = media_base, linetype = "dashed", color = "darkgreen", size = 0.8) +
  geom_hline(yintercept = LSC, linetype = "dotted", color = "red", size = 1) +
  geom_hline(yintercept = LIC, linetype = "dotted", color = "red", size = 1) +
  geom_line(size = 1.2) + geom_point(aes(fill = Pimenta), shape = 21, size = 3, color = "black") +
  scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") +
  labs(title = "Controle de Estabilidade Comparativo - Proteína", x = "Mês", y = "Proteína (%)") +
  theme_bw() + theme(legend.position = "bottom", legend.direction = "vertical")

print(graf_controle_innatura)
print(graf_controle_todos)




# 1. ANOVA Fatorial
modelo_prot <- aov(Proteina ~ Pimenta * Meses, data = df_prot)
print("--- ANOVA ---")
summary(modelo_prot)

# 2. Tukey para a Interação
df_prot$Trat_Mes <- interaction(df_prot$Pimenta, df_prot$Meses, sep = " em ")
tukey_prot <- HSD.test(aov(Proteina ~ Trat_Mes, data = df_prot), "Trat_Mes", group = TRUE)
print("--- TESTE DE TUKEY ---")
print(tukey_prot$groups)




# Grafico 2 Controle estabilidade

# 1. Preparar os dados (Usando o df_prot que já importamos antes)
df_in_natura_prot <- df_prot %>%
  filter(Pimenta == "In Natura")

# 2. Calcular Limites Baseados em Fevereiro (Proteína)
baseline_prot <- df_in_natura_prot %>% filter(Meses == "Fevereiro")
media_controle_prot <- mean(baseline_prot$Proteina, na.rm = TRUE)
sd_controle_prot <- sd(baseline_prot$Proteina, na.rm = TRUE)

LSC_prot <- media_controle_prot + (3 * sd_controle_prot) # Limite Superior
LIC_prot <- media_controle_prot - (3 * sd_controle_prot) # Limite Inferior

# 3. Calcular a Evolução (Média e Desvio Padrão para a faixa azul)
df_medias_prot <- df_in_natura_prot %>%
  group_by(Meses) %>%
  summarise(
    Media = mean(Proteina, na.rm = TRUE),
    SD = sd(Proteina, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Construir o Gráfico de Controle
grafico_controle_in_natura_prot <- ggplot(df_medias_prot, aes(x = Meses, y = Media, group = 1)) +
  
  # Zona de Estabilidade (Faixa cinza ao fundo calculada com 3 SD)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = LIC_prot, ymax = LSC_prot, 
           fill = "gray70", alpha = 0.3) +
  
  # A FAIXA AZUL: O nome aqui precisa bater com o scale_fill_manual
  geom_ribbon(aes(ymin = Media - SD, ymax = Media + SD, fill = "Zona de Estabilidade"), alpha = 0.2) +
  
  # LINHAS DE CONTROLE: Atualizadas com os valores de Proteína
  geom_hline(aes(yintercept = media_controle_prot, color = "Média"), linetype = "dashed", size = 0.8) +
  geom_hline(aes(yintercept = LSC_prot, color = "LSC (+3 SD: 11.3%)"), linetype = "dotted", size = 1) +
  geom_hline(aes(yintercept = LIC_prot, color = "LIC (-3 SD: 10.1%)"), linetype = "dotted", size = 1) +
  
  # Linha e pontos principais
  geom_line(color = "blue", size = 1.2) +
  geom_point(size = 4, shape = 21, fill = "blue", color = "white", stroke = 1.2) +
  
  # Configuração manual das cores e nomes da Legenda
  scale_color_manual(name = "Legendas", 
                     values = c("Média" = "darkgreen", 
                                "LSC (+3 SD: 11.3%)" = "red", 
                                "LIC (-3 SD: 10.1%)" = "red")) +
  
  scale_fill_manual(name = "Dispersão", 
                    values = c("Zona de Estabilidade" = "blue")) +
  
  # Textos e Títulos
  labs(
    title = "Gráfico de Controle de Estabilidade - Proteína (In Natura)",
    subtitle = "Evolução da proteína com intervalo de dispersão amostral",
    y = "Proteína (%)",
  ) +
  
  # Tema limpo e posicionamento da legenda INTERNA
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    
    # Coloca a legenda no canto inferior esquerdo (dentro do gráfico) exatamente como o modelo
    legend.position = c(0.13, 0.80), 
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.margin = margin(5, 5, 5, 5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  )

# Exibir o gráfico no RStudio
print(grafico_controle_in_natura_prot)




#------------------------------------------------------------------------------#
# MODELOS MULTIVARIADOS

# 1. Carregar Pacotes Essenciais
library(FactoMineR)
library(factoextra)

# 2. Função rápida para importar e padronizar os nomes de Pimenta e Meses
limpar_base <- function(aba, var_resposta) {
  df <- read_excel("Banco_Fabiana.xlsx", sheet = aba)
  
  # Padronizar nomes
  df <- df %>%
    mutate(Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
           Pimenta = str_trim(Pimenta))
  
  # Calcular a média por Tratamento e Mês
  df_media <- df %>%
    group_by(Pimenta, Meses) %>%
    summarise(Media_Var = mean(!!sym(var_resposta), na.rm = TRUE), .groups = "drop")
  
  # Renomear a coluna da média para o nome real da variável
  colnames(df_media)[3] <- aba 
  return(df_media)
}

# 3. Importar as 4 planilhas e extrair as médias
df_acidez <- limpar_base("Banco_Acidez", "% acidez")
df_umidade <- limpar_base("Banco_Umidade", "% umidade")
df_cinzas <- limpar_base("Banco_Cinzas", "% Cinzas")
df_proteina <- limpar_base("Banco_Proteinas", "% Proteína")

# 4. Fundir (Merge) todas as planilhas em um único Banco Multivariado
df_multivariado <- df_acidez %>%
  left_join(df_umidade, by = c("Pimenta", "Meses")) %>%
  left_join(df_cinzas, by = c("Pimenta", "Meses")) %>%
  left_join(df_proteina, by = c("Pimenta", "Meses"))

# Renomear colunas para ficar elegante no gráfico
colnames(df_multivariado) <- c("Tratamento", "Mes", "Acidez", "Umidade", "Cinzas", "Proteina")

# Criar uma coluna combinada "Tratamento_Mes" para ser o rótulo dos pontos no gráfico
df_multivariado <- df_multivariado %>%
  mutate(Rotulo = paste(Tratamento, Mes, sep = " - ")) %>%
  as.data.frame()

# Transformar o rótulo no "nome da linha" (rownames) para o pacote de PCA reconhecer
rownames(df_multivariado) <- df_multivariado$Rotulo

# 5. Executar a PCA (Apenas nas colunas 3 a 6, que são os números)
res.pca <- PCA(df_multivariado[, 3:6], scale.unit = TRUE, graph = FALSE)

# 6. Gerar o Gráfico Biplot (Indivíduos + Variáveis)
grafico_pca <- fviz_pca_biplot(res.pca,
                               
                               # Configuração das Setas (Variáveis Físico-Químicas)
                               col.var = "red",          # Cor das setas
                               arrowsize = 0.8,
                               labelsize = 5,
                               repel = TRUE,             # Evita que os textos se sobreponham
                               
                               # Configuração dos Pontos (Tratamentos)
                               col.ind = df_multivariado$Tratamento, # Colore os pontos pelo Tratamento
                               palette = "Dark2",                    # Paleta de cores contrastantes
                               addEllipses = TRUE,                   # Desenha elipses ao redor dos grupos
                               ellipse.level = 0.95,
                               pointsize = 3,
                               
                               # Textos
                               title = "Análise de Componentes Principais (PCA) - Perfil de Qualidade",
                               legend.title = "Tratamento") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")

# Exibir o gráfico
print(grafico_pca)



# 6. Gerar o Gráfico PCA (Versão Clean)
grafico_pca_limpo <- fviz_pca_biplot(res.pca,
                                     
                                     # --- CONFIGURAÇÃO DOS INDIVÍDUOS (As amostras) ---
                                     geom.ind = "point",        # O SEGREDO 1: Manda desenhar SÓ o ponto, removendo os textos poluidos
                                     col.ind = df_multivariado$Tratamento,
                                     fill.ind = df_multivariado$Tratamento,
                                     palette = "Dark2",
                                     pointsize = 4,             # Aumenta a bolinha para compensar a falta de texto
                                     pointshape = 21,           # Bolinha com borda
                                     
                                     addEllipses = TRUE,        
                                     ellipse.level = 0.95,
                                     ellipse.alpha = 0.15,      # O SEGREDO 2: Deixa as elipses bem clarinhas/transparentes
                                     
                                     # --- CONFIGURAÇÃO DAS VARIÁVEIS (As setas vermelhas) ---
                                     label = "var",             # O SEGREDO 3: Manda colocar texto APENAS nas setas (Umidade, etc)
                                     col.var = "red",          
                                     arrowsize = 0.8,
                                     labelsize = 5,
                                     repel = TRUE,              # Afasta o nome da variável da ponta da seta
                                     
                                     # --- TEXTOS E TEMAS ---
                                     title = "Análise de Componentes Principais (PCA) - Perfil de Qualidade",
                                     legend.title = "Tratamento"
) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  # Força a legenda a ter 2 colunas para ficar elegante na base
  guides(color = guide_legend(ncol = 2), fill = guide_legend(ncol = 2))

# Exibir o gráfico limpo
print(grafico_pca_limpo)










