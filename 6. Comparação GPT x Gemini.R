# === COMPARAÇÃO GPT X GEMINI === #

#### PREPARAÇÃO DOS DADOS ####
# Adiciona a identificação do modelo e une os dataframes
comparativo <- bind_rows(
  metricas_gemini %>% mutate(Modelo = "GEMINI"),
  metricas_gpt %>% mutate(Modelo = "GPT")
)

#### ESTILIZAÇÃO ####
comparativo <- comparativo %>%
  mutate(coluna = recode(coluna, !!!mapa_nomes))

# Transforma para formato longo (Tidy) para o ggplot
comparativo_long <- comparativo %>%
  pivot_longer(
    cols = c(precision, recall, f1_score), 
    names_to = "Metrica", 
    values_to = "Score"
  ) %>%
  mutate(Metrica = recode(Metrica, 
                          "precision" = "Precisão", 
                          "recall" = "Revocação", 
                          "f1_score" = "F1-Score"))

#### GRÁFICO COMPARATIVO ####
ggplot(comparativo_long, aes(x = reorder(coluna, Score), y = Score, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  geom_text(
    aes(label = round(Score, 2)),
    position = position_dodge(width = 0.9),
    hjust = 1.2,
    color = "white",
    size = 3,
    fontface = "bold"
  ) +
  coord_flip() +
  facet_wrap(~Metrica) +
  scale_fill_manual(values = c("GEMINI" = "#00CD66", "GPT" = "#CD2990")) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
  labs(
    # title = "Desempenho de Extração: GEMINI vs GPT",
    # subtitle = "Comparação direta baseada em métricas de Precision, Recall e F1",
    x = "Variável",
    y = "Pontuação (0-1)",
    fill = "Modelo de IA:"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 11, color = "black"),
    axis.text.y = element_text(size = 8)
  )
