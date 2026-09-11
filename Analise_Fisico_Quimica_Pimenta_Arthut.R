# 1. Instalação e carregamento dos pacotes necessários

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(flextable)
library(gtsummary)
library(agricolae)
library(emmeans)
library(multcompView)
library(multcomp)


# ==============================================================================
# 1. IMPORTAÇÃO E LIMPEZA
# ==============================================================================
df_umidade <- read_excel("Banco_Arthur.xlsx", sheet = "Banco_Umidade") %>%
  mutate(
    Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
    Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
    Pimenta = as.factor(str_trim(Pimenta))
  ) %>%
  rename(Umidade = `% umidade`)

# ==============================================================================
# 2. TABELA RESUMO (gtsummary) - Tratamentos nas Linhas e Meses nas Colunas
# ==============================================================================
tabela_gtsummary <- df_umidade %>%
  select(Pimenta, Meses, Umidade) %>%
  # tbl_continuous cruza duas variáveis categóricas preenchendo com uma numérica
  tbl_continuous(
    variable = Umidade,        # Variável numérica das células
    include = Pimenta,         # Variável das LINHAS
    by = Meses,                # Variável das COLUNAS
    statistic = ~ "{mean} \u00B1 {sd}" # Formato Média ± Desvio Padrão
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Meses de Armazenamento**") %>%
  bold_labels()

# Exibe a tabela no Viewer do RStudio
print(tabela_gtsummary)



# ==============================================================================
# 3. GRÁFICO DE TENDÊNCIA TEMPORAL
# ==============================================================================
df_resumo <- df_umidade %>%
  group_by(Pimenta, Meses) %>%
  summarise(Media = mean(Umidade, na.rm = TRUE),
            SD = sd(Umidade, na.rm = TRUE), .groups = "drop")

grafico_tendencia_umidade <- ggplot(df_resumo, aes(x = Meses, y = Media, group = Pimenta, color = Pimenta)) +
  geom_line(size = 1.2) +
  geom_point(aes(fill = Pimenta), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2, size = 0.8) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Parâmetros Físico-Químicos",
       y = "Umidade (%)",
       fill = "Tratamento",
       color = "Tratamento") + # <-- CORREÇÃO: Faltava este sinal de "+" aqui!
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    
    # --- AJUSTE: Legenda no canto INFERIOR ESQUERDO ---
    # O valor 0.22 centraliza na esquerda, e 0.22 mantém perto da base
    legend.position = c(0.15, 0.15), 
    
    legend.background = element_rect(fill = alpha("white", 0.9), color = "black", size = 0.3),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    legend.direction = "vertical",
    legend.key.height = unit(0.8, "cm") 
  )
print(grafico_tendencia_umidade)

# ==============================================================================
# 4. GRÁFICO DE CONTROLE DE ESTABILIDADE (Foco: In Natura)
# ==============================================================================
# Filtrar apenas In Natura e calcular Limites (Média Global ± 3 Desvios Padrões)
df_controle <- df_umidade %>% filter(Pimenta == "In Natura")
media_global <- mean(df_controle$Umidade, na.rm = TRUE)
sd_global <- sd(df_controle$Umidade, na.rm = TRUE)
limite_sup <- media_global + (3 * sd_global)
limite_inf <- media_global - (3 * sd_global)

grafico_controle_umidade <- ggplot(df_controle, aes(x = Meses, y = Umidade)) +
  geom_hline(yintercept = media_global, color = "blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = limite_sup, color = "red", linetype = "solid", size = 1) +
  geom_hline(yintercept = limite_inf, color = "red", linetype = "solid", size = 1) +
  geom_jitter(width = 0.1, size = 3, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "blue", size = 1.2) +
  labs(title = "Controle de Estabilidade Hídrica - In Natura",
       subtitle = "Limites de Controle: \u00B1 3 SD",
       y = "Umidade (%)", x = "Tempo") +
  theme_classic() +
  annotate("text", x = 5.2, y = limite_sup + 0.1, label = "LSC", color = "red", fontface = "bold") +
  annotate("text", x = 5.2, y = limite_inf - 0.1, label = "LIC", color = "red", fontface = "bold")

print(grafico_controle_umidade)
# ==============================================================================


# ==============================================================================
# 5. ANOVA E TESTE DE COMPARAÇÃO DE MÉDIAS (TUKEY)
# ==============================================================================
# 1. Garantir que todos os pacotes necessários estão ativos na memória


# 2. Rodando o modelo de Análise de Variância (Fatorial 3x5)
modelo_umidade <- aov(Umidade ~ Pimenta * Meses, data = df_umidade)

cat("\n--- TABELA DA ANOVA ---\n")
print(summary(modelo_umidade))

# 3. Rodar o Teste de Tukey dentro de cada Mês
medias_tukey <- emmeans(modelo_umidade, ~ Pimenta | Meses)

# 4. Extrair as letras com a função cld() agora ativada
letras_tukey <- cld(medias_tukey, Letters = letters, adjust = "tukey") %>%
  as.data.frame() %>% 
  mutate(.group = str_trim(.group))

cat("\n--- RESULTADO DO TESTE DE TUKEY (Letras de Significância) ---\n")
print(letras_tukey %>% select(Meses, Pimenta, emmean, .group))
#-------------------------------------------------------------------------------#





# ==============================================================================
# 1. IMPORTAÇÃO E LIMPEZA
# ==============================================================================
df_cinzas <- read_excel("Banco_Arthur.xlsx", 
                        sheet = "Banco_Cinzas") %>%
  mutate(
    Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
    Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
    Pimenta = as.factor(str_trim(Pimenta))
  ) %>%
  rename(Cinzas = `% Cinzas`) # Ajustando para o nome exato da planilha

# ==============================================================================
# 2. TABELA RESUMO (gtsummary) - Cruzada
# ==============================================================================
tabela_gtsummary_cinzas <- df_cinzas %>%
  dplyr::select(Pimenta, Meses, Cinzas) %>%
  tbl_continuous(
    variable = Cinzas,        
    include = Pimenta,         
    by = Meses,                
    statistic = ~ "{mean} \u00B1 {sd}" 
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Meses de Armazenamento**") %>%
  bold_labels()
print(tabela_gtsummary_cinzas)

# ==============================================================================
# 3. GRÁFICO DE TENDÊNCIA TEMPORAL
# ==============================================================================
df_resumo_cinzas <- df_cinzas %>%
  group_by(Pimenta, Meses) %>%
  summarise(Media = mean(Cinzas, na.rm = TRUE),
            SD = sd(Cinzas, na.rm = TRUE), .groups = "drop")

grafico_tendencia_cinzas <- ggplot(df_resumo_cinzas, aes(x = Meses, y = Media, group = Pimenta, color = Pimenta)) +
  geom_line(size = 1.2) +
  geom_point(aes(fill = Pimenta), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2, size = 0.8) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Parâmetros Físico-Químicos",
       y = "Cinzas (%)",
       fill = "Tratamento",
       color = "Tratamento") + 
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85, 0.10), 
    legend.background = element_rect(fill = alpha("white", 0.9), color = "black", size = 0.3),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )
print(grafico_tendencia_cinzas)

# ==============================================================================
# 4. GRÁFICO DE CONTROLE DE ESTABILIDADE (Foco: In Natura)
# ==============================================================================
df_controle_cinzas <- df_cinzas %>% filter(Pimenta == "In Natura")
media_global_c <- mean(df_controle_cinzas$Cinzas, na.rm = TRUE)
sd_global_c <- sd(df_controle_cinzas$Cinzas, na.rm = TRUE)
limite_sup_c <- media_global_c + (3 * sd_global_c)
limite_inf_c <- media_global_c - (3 * sd_global_c)

grafico_controle_cinzas <- ggplot(df_controle_cinzas, aes(x = Meses, y = Cinzas)) +
  geom_hline(yintercept = media_global_c, color = "blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = limite_sup_c, color = "red", linetype = "solid", size = 1) +
  geom_hline(yintercept = limite_inf_c, color = "red", linetype = "solid", size = 1) +
  geom_jitter(width = 0.1, size = 3, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "blue", size = 1.2) +
  labs(title = "Controle de Estabilidade Mineral - In Natura",
       subtitle = "Limites de Controle: \u00B1 3 SD",
       y = "Cinzas (%)", x = "Tempo") +
  theme_classic() +
  annotate("text", x = 5.2, y = limite_sup_c + 0.05, label = "LSC", color = "red", fontface = "bold") +
  annotate("text", x = 5.2, y = limite_inf_c - 0.05, label = "LIC", color = "red", fontface = "bold")

print(grafico_controle_cinzas)

# ==============================================================================
# 5. ANOVA E TESTE DE COMPARAÇÃO DE MÉDIAS (TUKEY)
# ==============================================================================
modelo_cinzas <- aov(Cinzas ~ Pimenta * Meses, data = df_cinzas)

cat("\n--- TABELA DA ANOVA (Cinzas) ---\n")
print(summary(modelo_cinzas))


medias_tukey_c <- emmeans(modelo_cinzas, ~ Pimenta | Meses)
letras_tukey_c <- cld(medias_tukey_c, Letters = letters, adjust = "tukey") %>%
  as.data.frame() %>% 
  mutate(.group = str_trim(.group))

cat("\n--- RESULTADO DO TESTE DE TUKEY (Cinzas) ---\n")
print(letras_tukey_c %>% select(Meses, Pimenta, emmean, .group))







# ==============================================================================
# 1. IMPORTAÇÃO E LIMPEZA
# ==============================================================================
df_acidez <- read_excel("Banco_Arthur.xlsx", 
                        sheet = "Banco_Acidez") %>%
  mutate(
    Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
    # ATENÇÃO: Março foi retirado pois não existe na planilha
    Meses = factor(Meses, levels = c("Fevereiro", "Abril", "Maio", "Junho")),
    Pimenta = as.factor(str_trim(Pimenta))
  ) %>%
  rename(Acidez = `% Acidez`)

# ==============================================================================
# 2. TABELA RESUMO (gtsummary) - Cruzada
# ==============================================================================
tabela_gtsummary_acidez <- df_acidez %>%
  dplyr::select(Pimenta, Meses, Acidez) %>%
  tbl_continuous(
    variable = Acidez,        
    include = Pimenta,         
    by = Meses,                
    statistic = ~ "{mean} \u00B1 {sd}" 
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Meses de Armazenamento**") %>%
  bold_labels()
print(tabela_gtsummary_acidez)

# ==============================================================================
# 3. GRÁFICO DE TENDÊNCIA TEMPORAL
# ==============================================================================
df_resumo_acidez <- df_acidez %>%
  group_by(Pimenta, Meses) %>%
  summarise(Media = mean(Acidez, na.rm = TRUE),
            SD = sd(Acidez, na.rm = TRUE), .groups = "drop")

grafico_tendencia_acidez <- ggplot(df_resumo_acidez, aes(x = Meses, y = Media, group = Pimenta, color = Pimenta)) +
  geom_line(size = 1.2) +
  geom_point(aes(fill = Pimenta), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2, size = 0.8) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Parâmetros Físico-Químicos",
       y = "Acidez (%)",
       fill = "Tratamento", color = "Tratamento") + 
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85, 0.10), # Subi a legenda para o topo esquerdo para não cobrir as linhas
    legend.background = element_rect(fill = alpha("white", 0.9), color = "black", size = 0.3),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )
print(grafico_tendencia_acidez)

# ==============================================================================
# 4. GRÁFICO DE CONTROLE DE ESTABILIDADE (Foco: In Natura)
# ==============================================================================
df_controle_acidez <- df_acidez %>% filter(Pimenta == "In Natura")
media_global_a <- mean(df_controle_acidez$Acidez, na.rm = TRUE)
sd_global_a <- sd(df_controle_acidez$Acidez, na.rm = TRUE)
limite_sup_a <- media_global_a + (3 * sd_global_a)
limite_inf_a <- media_global_a - (3 * sd_global_a)

grafico_controle_acidez <- ggplot(df_controle_acidez, aes(x = Meses, y = Acidez)) +
  geom_hline(yintercept = media_global_a, color = "blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = limite_sup_a, color = "red", linetype = "solid", size = 1) +
  geom_hline(yintercept = limite_inf_a, color = "red", linetype = "solid", size = 1) +
  geom_jitter(width = 0.1, size = 3, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "blue", size = 1.2) +
  labs(title = "Controle de Acidez - In Natura",
       subtitle = "Limites de Controle: \u00B1 3 SD",
       y = "Acidez (%)", x = "Tempo") +
  theme_classic() +
  annotate("text", x = 4.2, y = limite_sup_a + 0.01, label = "LSC", color = "red", fontface = "bold") +
  annotate("text", x = 4.2, y = limite_inf_a - 0.01, label = "LIC", color = "red", fontface = "bold")

print(grafico_controle_acidez)

# ==============================================================================
# 5. ANOVA E TESTE DE COMPARAÇÃO DE MÉDIAS (TUKEY)
# ==============================================================================
modelo_acidez <- aov(Acidez ~ Pimenta * Meses, data = df_acidez)

cat("\n--- TABELA DA ANOVA (Acidez) ---\n")
print(summary(modelo_acidez))

medias_tukey_a <- emmeans(modelo_acidez, ~ Pimenta | Meses)
letras_tukey_a <- cld(medias_tukey_a, Letters = letters, adjust = "tukey") %>%
  as.data.frame() %>% 
  mutate(.group = str_trim(.group))

cat("\n--- RESULTADO DO TESTE DE TUKEY (Acidez) ---\n")
print(letras_tukey_a %>% dplyr::select(Meses, Pimenta, emmean, .group))









# ==============================================================================
# 1. IMPORTAÇÃO E LIMPEZA
# ==============================================================================
df_proteinas <- read_excel("Banco_Arthur.xlsx", 
                           sheet = "Banco_Proteinas") %>%
  mutate(
    Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
    Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
    Pimenta = as.factor(str_trim(Pimenta))
  ) %>%
  rename(Proteinas = `% Proteína`)

# ==============================================================================
# 2. TABELA RESUMO (gtsummary) - Cruzada
# ==============================================================================
tabela_gtsummary_proteinas <- df_proteinas %>%
  dplyr::select(Pimenta, Meses, Proteinas) %>%
  tbl_continuous(
    variable = Proteinas,        
    include = Pimenta,         
    by = Meses,                
    statistic = ~ "{mean} \u00B1 {sd}" 
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Meses de Armazenamento**") %>%
  bold_labels()
print(tabela_gtsummary_proteinas)

# ==============================================================================
# 3. GRÁFICO DE TENDÊNCIA TEMPORAL
# ==============================================================================
df_resumo_prot <- df_proteinas %>%
  group_by(Pimenta, Meses) %>%
  summarise(Media = mean(Proteinas, na.rm = TRUE),
            SD = sd(Proteinas, na.rm = TRUE), .groups = "drop")

grafico_tendencia_prot <- ggplot(df_resumo_prot, aes(x = Meses, y = Media, group = Pimenta, color = Pimenta)) +
  geom_line(size = 1.2) +
  geom_point(aes(fill = Pimenta), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2, size = 0.8) +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Parâmetros Físico-Químicos",
       y = "Proteínas (%)",
       fill = "Tratamento", color = "Tratamento") + 
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = c(0.85, 0.12), # Canto inferior esquerdo
    legend.background = element_rect(fill = alpha("white", 0.9), color = "black", size = 0.3),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )
print(grafico_tendencia_prot)

# ==============================================================================
# 4. GRÁFICO DE CONTROLE DE ESTABILIDADE (Foco: In Natura)
# ==============================================================================
df_controle_prot <- df_proteinas %>% filter(Pimenta == "In Natura")
media_global_p <- mean(df_controle_prot$Proteinas, na.rm = TRUE)
sd_global_p <- sd(df_controle_prot$Proteinas, na.rm = TRUE)
limite_sup_p <- media_global_p + (3 * sd_global_p)
limite_inf_p <- media_global_p - (3 * sd_global_p)

grafico_controle_prot <- ggplot(df_controle_prot, aes(x = Meses, y = Proteinas)) +
  geom_hline(yintercept = media_global_p, color = "blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = limite_sup_p, color = "red", linetype = "solid", size = 1) +
  geom_hline(yintercept = limite_inf_p, color = "red", linetype = "solid", size = 1) +
  geom_jitter(width = 0.1, size = 3, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "blue", size = 1.2) +
  labs(title = "Controle de Estabilidade Proteica - In Natura",
       subtitle = "Limites de Controle: \u00B1 3 SD",
       y = "Proteínas (%)", x = "Tempo") +
  theme_classic() +
  annotate("text", x = 5.2, y = limite_sup_p + 0.2, label = "LSC", color = "red", fontface = "bold") +
  annotate("text", x = 5.2, y = limite_inf_p - 0.2, label = "LIC", color = "red", fontface = "bold")

print(grafico_controle_prot)

# ==============================================================================
# 5. ANOVA E TESTE DE COMPARAÇÃO DE MÉDIAS (TUKEY)
# ==============================================================================
modelo_prot <- aov(Proteinas ~ Pimenta * Meses, data = df_proteinas)

cat("\n--- TABELA DA ANOVA (Proteínas) ---\n")
print(summary(modelo_prot))

medias_tukey_p <- emmeans(modelo_prot, ~ Pimenta | Meses)
letras_tukey_p <- cld(medias_tukey_p, Letters = letters, adjust = "tukey") %>%
  as.data.frame() %>% 
  mutate(.group = str_trim(.group))

cat("\n--- RESULTADO DO TESTE DE TUKEY (Proteínas) ---\n")
print(letras_tukey_p %>% dplyr::select(Meses, Pimenta, emmean, .group))




# ==============================================================================
# 1. IMPORTAÇÃO E LIMPEZA
# ==============================================================================
df_gordura <- read_excel("Banco_Arthur.xlsx", 
                         sheet = "Banco_Gordura") %>%
  mutate(
    Meses = str_replace(str_to_title(str_trim(Meses)), "Marco", "Março"),
    Meses = factor(Meses, levels = c("Fevereiro", "Março", "Abril", "Maio", "Junho")),
    Pimenta = as.factor(str_trim(Pimenta))
  ) %>%
  rename(Gordura = `% GORDURA`)

# ==============================================================================
# 2. TABELA RESUMO (gtsummary) - Cruzada
# ==============================================================================
tabela_gtsummary_gordura <- df_gordura %>%
  dplyr::select(Pimenta, Meses, Gordura) %>%
  tbl_continuous(
    variable = Gordura,        
    include = Pimenta,         
    by = Meses,                
    statistic = ~ "{mean} \u00B1 {sd}" 
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  modify_header(label = "**Tratamento (Pimenta)**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Meses de Armazenamento**") %>%
  bold_labels()
print(tabela_gtsummary_gordura)

# ==============================================================================
# 3. GRÁFICO DE TENDÊNCIA TEMPORAL
# ==============================================================================
df_resumo_gord <- df_gordura %>%
  group_by(Pimenta, Meses) %>%
  summarise(Media = mean(Gordura, na.rm = TRUE),
            SD = sd(Gordura, na.rm = TRUE), .groups = "drop")

grafico_tendencia_gord <- ggplot(df_resumo_gord, aes(x = Meses, y = Media, group = Pimenta, color = Pimenta)) +
  geom_line(size = 1.2) +
  geom_point(aes(fill = Pimenta), shape = 21, size = 4, color = "black", stroke = 1) +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2, size = 0.8) +
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Parâmetros Físico-Químicos",
       y = "Lipídios (%)",
       fill = "Tratamento", color = "Tratamento") + 
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = c(0.12, 0.85), # Posicionado no alto à esquerda
    legend.background = element_rect(fill = alpha("white", 0.9), color = "black", size = 0.3),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )

print(grafico_tendencia_gord)

# ==============================================================================
# 4. GRÁFICO DE CONTROLE DE ESTABILIDADE (Foco: In Natura)
# ==============================================================================
df_controle_gord <- df_gordura %>% filter(Pimenta == "In Natura")
media_global_g <- mean(df_controle_gord$Gordura, na.rm = TRUE)
sd_global_g <- sd(df_controle_gord$Gordura, na.rm = TRUE)
limite_sup_g <- media_global_g + (3 * sd_global_g)
limite_inf_g <- media_global_g - (3 * sd_global_g)
# Ajustar LIC para não ser negativo (gordura não pode ser menor que zero)
limite_inf_g <- ifelse(limite_inf_g < 0, 0, limite_inf_g)

grafico_controle_gord <- ggplot(df_controle_gord, aes(x = Meses, y = Gordura)) +
  geom_hline(yintercept = media_global_g, color = "blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept = limite_sup_g, color = "red", linetype = "solid", size = 1) +
  geom_hline(yintercept = limite_inf_g, color = "red", linetype = "solid", size = 1) +
  geom_jitter(width = 0.1, size = 3, color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "line", group = 1, color = "blue", size = 1.2) +
  labs(title = "Controle de Estabilidade Lipídica - In Natura",
       subtitle = "Limites de Controle: \u00B1 3 SD",
       y = "Gordura (%)", x = "Tempo") +
  theme_classic() +
  annotate("text", x = 5.2, y = limite_sup_g + 0.1, label = "LSC", color = "red", fontface = "bold") +
  annotate("text", x = 5.2, y = limite_inf_g + 0.1, label = "LIC", color = "red", fontface = "bold")

print(grafico_controle_gord)

# ==============================================================================
# 5. ANOVA E TESTE DE COMPARAÇÃO DE MÉDIAS (TUKEY)
# ==============================================================================
modelo_gord <- aov(Gordura ~ Pimenta * Meses, data = df_gordura)

cat("\n--- TABELA DA ANOVA (Gordura) ---\n")
print(summary(modelo_gord))

medias_tukey_g <- emmeans(modelo_gord, ~ Pimenta | Meses)
letras_tukey_g <- cld(medias_tukey_g, Letters = letters, adjust = "tukey") %>%
  as.data.frame() %>% 
  mutate(.group = str_trim(.group))

cat("\n--- RESULTADO DO TESTE DE TUKEY (Gordura) ---\n")
print(letras_tukey_g %>% dplyr::select(Meses, Pimenta, emmean, .group))


















