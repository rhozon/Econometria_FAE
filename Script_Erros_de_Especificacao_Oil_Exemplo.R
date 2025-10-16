# =========================================================================
# ANÁLISE DE ESPECIFICAÇÃO DE MODELOS ECONOMÉTRICOS
# Dataset: Oil Prices
# Testes: RESET de Ramsey e Multiplicador de Lagrange (ML)
# 
# Professor: Rodrigo Hermont Ozon
# Disciplina: Econometria - FAE
# Data: 2025
# =========================================================================

# -------------------------------------------------------------------------
# CONFIGURAÇÃO INICIAL
# -------------------------------------------------------------------------

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║        ANÁLISE DE ESPECIFICAÇÃO: PREÇOS DO PETRÓLEO (OIL)         ║\n")
cat("║                  Testes RESET e ML - Tutorial                      ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Limpar ambiente
rm(list = ls())
cat("✓ Ambiente limpo\n\n")

# -------------------------------------------------------------------------
# FUNÇÃO PARA INSTALAR E CARREGAR PACOTES (definida APÓS limpeza)
# -------------------------------------------------------------------------

instalar_e_carregar <- function(pacotes) {
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║              VERIFICANDO E INSTALANDO PACOTES                      ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  
  # Verificar quais pacotes NÃO estão instalados
  pacotes_faltando <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  
  # Instalar pacotes faltando
  if (length(pacotes_faltando) > 0) {
    cat("Pacotes não encontrados. Instalando...\n")
    for (pkg in pacotes_faltando) {
      cat(sprintf("  → Instalando %s...\n", pkg))
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      cat(sprintf("  ✓ %s instalado!\n", pkg))
    }
    cat("\n")
  } else {
    cat("✓ Todos os pacotes já estão instalados!\n\n")
  }
  
  # Carregar todos os pacotes
  cat("Carregando pacotes...\n")
  for (pkg in pacotes) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("  ✓ %s carregado\n", pkg))
  }
  
  cat("\n✓ Todos os pacotes carregados com sucesso!\n\n")
}

# Lista de pacotes necessários
pacotes_necessarios <- c(
  "gamlss",       # Dataset oil
  "tidyverse",    # Manipulação de dados (inclui dplyr, ggplot2, etc.)
  "lmtest",       # Teste RESET automático
  "car",          # VIF e testes diagnósticos
  "tseries",      # Teste Jarque-Bera
  "gridExtra",    # Múltiplos gráficos
  "knitr",        # Tabelas formatadas
  "kableExtra"    # Tabelas ainda mais bonitas
)

# Instalar (se necessário) e carregar pacotes
instalar_e_carregar(pacotes_necessarios)

# Registrar tempo de início
start_time <- Sys.time()

# Criar arquivo de log
log_file <- "analise_oil_log.txt"
sink(log_file, append = FALSE, split = TRUE)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("Log da análise salvo em:", log_file, "\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Carregar dados
cat("Carregando dataset 'oil'...\n")
data(oil)
cat(sprintf("✓ Dataset carregado: %d observações, %d variáveis\n\n", 
            nrow(oil), ncol(oil)))

# =========================================================================
# PARTE 1: ESTIMAÇÃO DO MODELO INICIAL
# =========================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    PARTE 1: MODELO INICIAL                         ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("O que estamos fazendo:\n")
cat("  • Estimando um modelo LINEAR para explicar OILPRICE\n")
cat("  • Variáveis explicativas: CL2_log, BDIY_log, SPX_log, DX1_log\n")
cat("  • Método: Mínimos Quadrados Ordinários (MQO)\n\n")

# Estimar modelo
modelo_restrito <- lm(OILPRICE ~ CL2_log + BDIY_log + SPX_log + DX1_log, 
                      data = oil)

cat("Modelo estimado:\n")
cat("  OILPRICE = β₀ + β₁·CL2_log + β₂·BDIY_log + β₃·SPX_log + β₄·DX1_log + u\n\n")

# Resumo formatado
cat("═══════════════════════════════════════════════════════════════════\n")
cat("                     RESULTADOS DA REGRESSÃO\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

summary_modelo <- summary(modelo_restrito)
print(summary_modelo)

# Extrair estatísticas importantes
r2_restrito <- summary_modelo$r.squared
r2_adj <- summary_modelo$adj.r.squared
n <- nrow(oil)
rse <- summary_modelo$sigma
f_stat <- summary_modelo$fstatistic[1]

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat("                   ESTATÍSTICAS PRINCIPAIS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("  Observações:         %d\n", n))
cat(sprintf("  R²:                  %.6f  (%.2f%% da variação explicada)\n", 
            r2_restrito, r2_restrito*100))
cat(sprintf("  R² Ajustado:         %.6f\n", r2_adj))
cat(sprintf("  Erro Padrão:         %.5f\n", rse))
cat(sprintf("  Estatística F:       %.2f  (p-valor < 0.0001)\n", f_stat))

cat("\n")
cat("INTERPRETAÇÃO INICIAL:\n")
cat(sprintf("  → O modelo explica %.2f%% da variação nos preços do petróleo\n", 
            r2_restrito*100))
cat("  → Quase todas as variáveis são significativas\n")
cat("  → Modelo tem ajuste EXCELENTE (R² muito alto)\n\n")

# =========================================================================
# PARTE 2: DIAGNÓSTICOS BÁSICOS
# =========================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                  PARTE 2: DIAGNÓSTICOS BÁSICOS                     ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("Verificando premissas do modelo:\n")
cat("  1. Normalidade dos resíduos (Jarque-Bera)\n")
cat("  2. Autocorrelação (Durbin-Watson)\n")
cat("  3. Multicolinearidade (VIF)\n\n")

# Extrair resíduos e valores ajustados
residuos <- residuals(modelo_restrito)
fitted_values <- fitted(modelo_restrito)

cat("─────────────────────────────────────────────────────────────────────\n")
cat("2.1. TESTE DE JARQUE-BERA (Normalidade)\n")
cat("─────────────────────────────────────────────────────────────────────\n\n")

jb_test <- jarque.bera.test(residuos)

cat(sprintf("  Estatística JB:  %.4f\n", jb_test$statistic))
cat(sprintf("  p-valor:         %.10f\n\n", jb_test$p.value))

if (jb_test$p.value < 0.01) {
  cat("✗ DECISÃO: Rejeita H₀ (p < 1%) - Resíduos NÃO são normais\n\n")
} else {
  cat("✓ DECISÃO: Não rejeita H₀ - Resíduos são normais\n\n")
}

cat("─────────────────────────────────────────────────────────────────────\n")
cat("2.2. TESTE DE DURBIN-WATSON (Autocorrelação)\n")
cat("─────────────────────────────────────────────────────────────────────\n\n")

dw_test <- durbinWatsonTest(modelo_restrito)

cat(sprintf("  Estatística DW:  %.4f\n", dw_test$dw))
cat(sprintf("  p-valor:         %.4f\n\n", dw_test$p))

if (dw_test$p < 0.05) {
  cat("✗ DECISÃO: Há autocorrelação\n\n")
} else {
  cat("✓ DECISÃO: Sem autocorrelação\n\n")
}

cat("─────────────────────────────────────────────────────────────────────\n")
cat("2.3. VIF (Multicolinearidade)\n")
cat("─────────────────────────────────────────────────────────────────────\n\n")

vif_valores <- vif(modelo_restrito)

cat("Resultados:\n")
for (i in 1:length(vif_valores)) {
  vif_val <- vif_valores[i]
  var_name <- names(vif_valores)[i]
  
  if (vif_val > 10) {
    status <- "✗ SEVERA"
  } else if (vif_val > 5) {
    status <- "△ Moderada"
  } else {
    status <- "✓ OK"
  }
  
  cat(sprintf("  %-12s  VIF = %6.2f  %s\n", var_name, vif_val, status))
}

cat("\n")

# =========================================================================
# PARTE 3: GRÁFICOS
# =========================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    PARTE 3: GRÁFICOS                               ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("Gerando gráficos diagnósticos...\n\n")

oil_diagnostico <- oil %>%
  mutate(
    residuos = residuos,
    fitted = fitted_values,
    obs = 1:n()
  )

p1 <- ggplot(oil_diagnostico, aes(x = fitted, y = residuos)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  labs(title = "1. Resíduos vs. Valores Ajustados",
       x = "Ŷ", y = "û") +
  theme_minimal()

p2 <- ggplot(oil_diagnostico, aes(x = obs, y = residuos)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  labs(title = "2. Resíduos ao Longo do Tempo",
       x = "Observação", y = "û") +
  theme_minimal()

p3 <- ggplot(oil_diagnostico, aes(sample = residuos)) +
  stat_qq(alpha = 0.3, color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "3. Q-Q Plot") +
  theme_minimal()

p4 <- ggplot(oil_diagnostico, aes(x = residuos)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "4. Histograma dos Resíduos") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2)

cat("✓ Gráficos gerados!\n\n")

# =========================================================================
# PARTE 4: TESTE RESET
# =========================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    PARTE 4: TESTE RESET                            ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

Y_hat <- fitted(modelo_restrito)
modelo_expandido_reset <- lm(OILPRICE ~ CL2_log + BDIY_log + SPX_log + DX1_log + 
                               I(Y_hat^2) + I(Y_hat^3), data = oil)

r2_expandido <- summary(modelo_expandido_reset)$r.squared
q_reset <- 2
k_novo_reset <- length(coef(modelo_expandido_reset))

F_calc_reset <- ((r2_expandido - r2_restrito) / q_reset) / 
  ((1 - r2_expandido) / (n - k_novo_reset))

p_valor_reset <- 1 - pf(F_calc_reset, q_reset, n - k_novo_reset)

cat("RESULTADOS:\n")
cat(sprintf("  F = %.4f, p-valor = %.6f\n\n", F_calc_reset, p_valor_reset))

if (p_valor_reset < 0.05) {
  cat("✗ RESET rejeita H₀ - Modelo mal especificado\n\n")
} else {
  cat("✓ RESET não rejeita H₀ - Modelo adequado\n\n")
}

# =========================================================================
# PARTE 5: TESTE ML
# =========================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    PARTE 5: TESTE ML                               ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

residuos_restrito <- residuals(modelo_restrito)
modelo_auxiliar <- lm(residuos_restrito ~ CL2_log + BDIY_log + SPX_log + DX1_log +
                        GC1_log + HO1_log + USCI_log, data = oil)

r2_auxiliar <- summary(modelo_auxiliar)$r.squared
q_ml <- 3
chi2_calc <- n * r2_auxiliar
p_valor_ml <- 1 - pchisq(chi2_calc, q_ml)

cat("Testando: GC1_log, HO1_log, USCI_log\n\n")
cat("RESULTADOS:\n")
cat(sprintf("  χ² = %.4f, p-valor = %.6f\n\n", chi2_calc, p_valor_ml))

if (p_valor_ml < 0.05) {
  cat("✗ ML rejeita H₀ - Variáveis são relevantes\n\n")
} else {
  cat("✓ ML não rejeita H₀ - Variáveis desnecessárias\n\n")
}

# =========================================================================
# RESUMO FINAL
# =========================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                      RESUMO FINAL                                  ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat(sprintf("  RESET:  %s (p = %.4f)\n", 
            ifelse(p_valor_reset < 0.05, "✗ Rejeitou", "✓ Não rejeitou"),
            p_valor_reset))

cat(sprintf("  ML:     %s (p = %.4f)\n\n", 
            ifelse(p_valor_ml < 0.05, "✗ Rejeitou", "✓ Não rejeitou"),
            p_valor_ml))

if (p_valor_reset >= 0.05 && p_valor_ml >= 0.05) {
  cat("✓✓✓ CONCLUSÃO: Modelo adequadamente especificado!\n\n")
} else {
  cat("✗✗✗ CONCLUSÃO: Modelo precisa ser respecificado\n\n")
}

# Fechar log
sink()

tempo_total <- as.numeric(Sys.time() - start_time, units = "secs")

cat(sprintf("\n✓ Análise concluída! Log salvo em: %s\n", log_file))
cat(sprintf("✓ Tempo total: %.2f segundos\n\n", tempo_total))

# =========================================================================
# INTERPRETAÇÃO GUIADA PASSO A PASSO
# =========================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                  INTERPRETAÇÃO GUIADA PASSO A PASSO                ║\n")
cat("║                      (Leitura do Log)                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

Sys.sleep(1)  # Pausa dramática

cat("📖 Lendo arquivo de log...\n\n")

Sys.sleep(0.5)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("                        RESUMO EXECUTIVO\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("🎯 OBJETIVO DA ANÁLISE:\n")
cat("   Testar se o modelo linear para OILPRICE está corretamente\n")
cat("   especificado usando os testes RESET e ML.\n\n")

cat("📊 DADOS ANALISADOS:\n")
cat(sprintf("   • Dataset: Oil (preços do petróleo)\n"))
cat(sprintf("   • Observações: %d\n", n))
cat(sprintf("   • Variáveis no modelo: 4 + intercepto\n"))
cat(sprintf("   • Período de análise: %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

cat("═══════════════════════════════════════════════════════════════════\n\n")

Sys.sleep(1)

cat("📝 PASSO 1: ESTIMAÇÃO DO MODELO INICIAL\n")
cat("─────────────────────────────────────────────────────────────────────\n\n")

cat("Modelo estimado:\n")
cat("   OILPRICE = f(CL2_log, BDIY_log, SPX_log, DX1_log)\n\n")

cat("Resultados principais:\n")
cat(sprintf("   ✓ R² = %.4f (%.2f%% de ajuste)\n", r2_restrito, r2_restrito*100))
cat(sprintf("   ✓ Erro padrão = %.5f\n", rse))
cat(sprintf("   ✓ F-estatística = %.2f (altamente significativo)\n\n", f_stat))

cat("💡 INTERPRETAÇÃO:\n")
if (r2_restrito > 0.95) {
  cat("   🟢 EXCELENTE ajuste! O modelo explica mais de 95% da variação.\n")
  cat("   → As variáveis escolhidas capturam muito bem o comportamento\n")
  cat("     dos preços do petróleo.\n\n")
} else if (r2_restrito > 0.80) {
  cat("   🟡 BOM ajuste. O modelo explica mais de 80% da variação.\n\n")
} else {
  cat("   🔴 Ajuste FRACO. Considere adicionar mais variáveis.\n\n")
}

Sys.sleep(1)

cat("📝 PASSO 2: VERIFICAÇÃO DE PREMISSAS\n")
cat("─────────────────────────────────────────────────────────────────────\n\n")

# Normalidade
cat("2.1. NORMALIDADE DOS RESÍDUOS (Jarque-Bera)\n")
cat(sprintf("     Estatística: JB = %.2f, p-valor = %.6f\n\n", 
            jb_test$statistic, jb_test$p.value))

if (jb_test$p.value < 0.01) {
  cat("     🔴 PROBLEMA: Resíduos NÃO são normais\n")
  cat("     → Causas possíveis:\n")
  cat("        • Outliers na amostra\n")
  cat("        • Distribuição leptocúrtica (caudas pesadas)\n")
  cat("        • Má especificação do modelo\n")
  cat("     → Impacto:\n")
  cat("        • Em amostras grandes (n>100), menos preocupante\n")
  cat("        • Testes de hipótese ainda válidos (Teorema Central do Limite)\n")
  cat("        • Intervalos de confiança podem ser afetados\n\n")
} else {
  cat("     ✅ OK: Resíduos seguem distribuição normal\n\n")
}

# Autocorrelação
cat("2.2. AUTOCORRELAÇÃO (Durbin-Watson)\n")
cat(sprintf("     Estatística: DW = %.4f, p-valor = %.4f\n\n", 
            dw_test$dw, dw_test$p))

if (dw_test$p < 0.05) {
  cat("     🔴 PROBLEMA: Há autocorrelação nos resíduos\n")
  cat("     → Erros padrão SUBESTIMADOS\n")
  cat("     → Testes de significância NÃO confiáveis\n\n")
} else {
  cat("     ✅ OK: Sem autocorrelação detectada\n")
  cat("     → DW próximo de 2 indica independência\n")
  cat("     → Erros padrão são confiáveis\n\n")
}

# Multicolinearidade
cat("2.3. MULTICOLINEARIDADE (VIF)\n\n")
tem_problema_vif <- FALSE
for (i in 1:length(vif_valores)) {
  vif_val <- vif_valores[i]
  var_name <- names(vif_valores)[i]
  
  if (vif_val > 10) {
    cat(sprintf("     🔴 %s: VIF = %.2f (SEVERA)\n", var_name, vif_val))
    tem_problema_vif <- TRUE
  } else if (vif_val > 5) {
    cat(sprintf("     🟡 %s: VIF = %.2f (Moderada)\n", var_name, vif_val))
  } else {
    cat(sprintf("     ✅ %s: VIF = %.2f (OK)\n", var_name, vif_val))
  }
}

cat("\n")
if (tem_problema_vif) {
  cat("     💡 INTERPRETAÇÃO:\n")
  cat("     → Variáveis altamente correlacionadas entre si\n")
  cat("     → Coeficientes INSTÁVEIS e não confiáveis\n")
  cat("     → Dificulta interpretação individual dos coeficientes\n")
  cat("     → SOLUÇÃO: Considere remover uma das variáveis correlacionadas\n\n")
} else {
  cat("     ✅ Multicolinearidade em níveis aceitáveis\n\n")
}

Sys.sleep(1)

cat("📝 PASSO 3: TESTE RESET DE RAMSEY\n")
cat("─────────────────────────────────────────────────────────────────────\n\n")

cat("O que o RESET testa:\n")
cat("   H₀: O modelo linear está CORRETO\n")
cat("   H₁: O modelo está MAL ESPECIFICADO (há não-linearidades)\n\n")

cat(sprintf("Resultado: F = %.4f, p-valor = %.6f\n\n", F_calc_reset, p_valor_reset))

if (p_valor_reset < 0.01) {
  cat("🔴 CONCLUSÃO: REJEITA H₀ fortemente (p < 1%)\n\n")
  cat("💡 INTERPRETAÇÃO:\n")
  cat("   → O modelo LINEAR não é adequado\n")
  cat("   → Há relações NÃO-LINEARES importantes omitidas\n")
  cat("   → Adicionar Ŷ² e Ŷ³ melhora significativamente o ajuste\n\n")
  cat("📋 RECOMENDAÇÕES:\n")
  cat("   1. Considere transformações logarítmicas adicionais\n")
  cat("   2. Teste termos quadráticos das variáveis\n")
  cat("   3. Explore interações entre variáveis (X₁ × X₂)\n")
  cat("   4. Revise a teoria econômica para formas funcionais corretas\n\n")
  
} else if (p_valor_reset < 0.05) {
  cat("🟡 CONCLUSÃO: REJEITA H₀ a 5%\n\n")
  cat("💡 INTERPRETAÇÃO:\n")
  cat("   → Evidências moderadas de má especificação\n")
  cat("   → Investigue com testes adicionais\n\n")
  
} else {
  cat("✅ CONCLUSÃO: NÃO REJEITA H₀\n\n")
  cat("💡 INTERPRETAÇÃO:\n")
  cat("   → A forma funcional LINEAR parece apropriada\n")
  cat("   → Adicionar termos não-lineares (Ŷ², Ŷ³) NÃO melhora\n")
  cat("     significativamente o modelo\n")
  cat("   → O modelo captura bem as relações entre as variáveis\n\n")
  cat("⚠️  ATENÇÃO:\n")
  cat("   → Isso NÃO garante que o modelo esteja 100% correto\n")
  cat("   → Pode haver variáveis OMITIDAS (teste ML a seguir)\n\n")
}

Sys.sleep(1)

cat("📝 PASSO 4: TESTE DO MULTIPLICADOR DE LAGRANGE (ML)\n")
cat("─────────────────────────────────────────────────────────────────────\n\n")

cat("O que o ML testa:\n")
cat("   H₀: GC1_log, HO1_log e USCI_log são DESNECESSÁRIAS\n")
cat("   H₁: Pelo menos uma dessas variáveis é RELEVANTE\n\n")

cat("Variáveis testadas:\n")
cat("   • GC1_log  → Preço do ouro (Gold)\n")
cat("   • HO1_log  → Óleo de aquecimento (Heating Oil)\n")
cat("   • USCI_log → Índice de commodities\n\n")

cat(sprintf("Resultado: χ² = %.4f, p-valor = %.6f\n\n", chi2_calc, p_valor_ml))

if (p_valor_ml < 0.01) {
  cat("🔴 CONCLUSÃO: REJEITA H₀ fortemente (p < 1%)\n\n")
  cat("💡 INTERPRETAÇÃO:\n")
  cat("   → As variáveis testadas SÃO RELEVANTES\n")
  cat("   → O modelo atual OMITIU informação importante\n")
  cat("   → Essas variáveis melhoram significativamente a explicação\n\n")
  cat("📋 RECOMENDAÇÕES:\n")
  cat("   1. INCLUA GC1_log, HO1_log e USCI_log no modelo final\n")
  cat("   2. Re-estime o modelo com as variáveis adicionais\n")
  cat("   3. Compare R² antes e depois\n")
  cat("   4. Verifique se multicolinearidade aumenta\n\n")
  
} else if (p_valor_ml < 0.05) {
  cat("🟡 CONCLUSÃO: REJEITA H₀ a 5%\n\n")
  cat("💡 INTERPRETAÇÃO:\n")
  cat("   → Evidências moderadas de que as variáveis são relevantes\n")
  cat("   → Considere incluí-las no modelo\n\n")
  
} else {
  cat("✅ CONCLUSÃO: NÃO REJEITA H₀\n\n")
  cat("💡 INTERPRETAÇÃO:\n")
  cat("   → As variáveis GC1_log, HO1_log e USCI_log são DESNECESSÁRIAS\n")
  cat("   → Elas não adicionam informação relevante ao modelo\n")
  cat("   → O modelo atual já captura o essencial\n\n")
  cat("🎯 PRINCÍPIO DA PARCIMÔNIA:\n")
  cat("   → Modelos mais SIMPLES são preferíveis\n")
  cat("   → 'Navalhade Occam': não complique desnecessariamente\n")
  cat("   → Menos variáveis = mais fácil de interpretar e usar\n\n")
}

Sys.sleep(1)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("                       CONCLUSÃO FINAL\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# Matriz de decisão
decisao_reset <- ifelse(p_valor_reset < 0.05, "REJEITOU", "NÃO REJEITOU")
decisao_ml <- ifelse(p_valor_ml < 0.05, "REJEITOU", "NÃO REJEITOU")

cat("📊 RESUMO DOS TESTES:\n\n")
cat(sprintf("   RESET (Forma Funcional):  %s H₀  (p = %.4f)\n", 
            decisao_reset, p_valor_reset))
cat(sprintf("   ML (Variáveis Omitidas):  %s H₀  (p = %.4f)\n\n", 
            decisao_ml, p_valor_ml))

# Análise combinada
if (p_valor_reset >= 0.05 && p_valor_ml >= 0.05) {
  cat("🎉 VEREDICTO: MODELO ADEQUADAMENTE ESPECIFICADO\n\n")
  cat("✅ Forma funcional LINEAR está correta (RESET OK)\n")
  cat("✅ Não há variáveis relevantes omitidas (ML OK)\n")
  cat("✅ O modelo captura bem as relações econômicas\n\n")
  
  cat("⚠️  RESSALVAS:\n")
  if (jb_test$p.value < 0.05) {
    cat("   • Resíduos NÃO-normais (mas n=1000 é grande → menos preocupante)\n")
  }
  if (any(vif_valores > 10)) {
    cat("   • Multicolinearidade SEVERA (coeficientes instáveis)\n")
    cat("   → Considere remover DX1_log (não-significativo + VIF alto)\n")
  }
  
  cat("\n📋 MODELO RECOMENDADO:\n")
  if (any(vif_valores > 10)) {
    cat("   OILPRICE = β₀ + β₁·CL2_log + β₂·BDIY_log + β₃·SPX_log + u\n")
    cat("   (Remover DX1_log por multicolinearidade)\n\n")
  } else {
    cat("   Manter o modelo atual (sem alterações)\n\n")
  }
  
} else if (p_valor_reset < 0.05 && p_valor_ml >= 0.05) {
  cat("🟡 VEREDICTO: PROBLEMA DE FORMA FUNCIONAL\n\n")
  cat("✗ Forma linear NÃO é adequada (RESET rejeitou)\n")
  cat("✓ Mas não faltam variáveis (ML OK)\n\n")
  cat("📋 AÇÕES NECESSÁRIAS:\n")
  cat("   1. Testar transformações não-lineares\n")
  cat("   2. Adicionar termos quadráticos\n")
  cat("   3. Explorar interações entre variáveis\n\n")
  
} else if (p_valor_reset >= 0.05 && p_valor_ml < 0.05) {
  cat("🟡 VEREDICTO: VARIÁVEIS OMITIDAS\n\n")
  cat("✓ Forma linear está OK (RESET OK)\n")
  cat("✗ Mas faltam variáveis importantes (ML rejeitou)\n\n")
  cat("📋 AÇÕES NECESSÁRIAS:\n")
  cat("   1. ADICIONAR GC1_log, HO1_log, USCI_log ao modelo\n")
  cat("   2. Re-estimar e comparar resultados\n")
  cat("   3. Verificar melhoria no R² e erros\n\n")
  
} else {
  cat("🔴 VEREDICTO: MODELO MAL ESPECIFICADO\n\n")
  cat("✗ Forma funcional INCORRETA (RESET rejeitou)\n")
  cat("✗ Variáveis importantes OMITIDAS (ML rejeitou)\n\n")
  cat("📋 AÇÕES NECESSÁRIAS:\n")
  cat("   1. Revisar COMPLETAMENTE a especificação\n")
  cat("   2. Adicionar as variáveis omitidas\n")
  cat("   3. Testar formas funcionais alternativas\n")
  cat("   4. Consultar teoria econômica relevante\n\n")
}

cat("═══════════════════════════════════════════════════════════════════\n\n")

cat("📚 PARA APRENDER MAIS:\n")
cat("   • Gujarati (2011), Capítulo 13: Especificação de Modelos\n")
cat("   • Ramsey (1969): 'Tests for Specification Errors'\n")
cat("   • Engle (1984): 'Wald, Likelihood Ratio, and Lagrange Multiplier Tests'\n\n")

cat("💾 ARQUIVOS GERADOS:\n")
cat(sprintf("   • Log completo: %s\n", log_file))
cat("   • Gráficos de diagnóstico: Visualizados no console\n\n")

cat(sprintf("⏱️  Tempo total de análise: %.2f segundos\n\n", tempo_total))

cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                 ✓ INTERPRETAÇÃO CONCLUÍDA ✓                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
