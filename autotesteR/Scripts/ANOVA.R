anova <- function(..., titulo = "ANOVA/Tukey HSD", x = "Eixo x", y = "Eixo y", ajuda = FALSE) {
  
  if (ajuda || length(list(...)) == 0) {
    cat("
Função anova()

Descrição:
  Realiza análise de variância (ANOVA) entre dois ou mais grupos, seguida de teste post-hoc de Tukey HSD.
  Exibe o resultado gráfico com letras de significância.

Uso:
  anova(grupo1, grupo2, ..., titulo = \"Título\", x = \"Eixo x\", y = \"Eixo y\")

Argumentos:
  grupo1, grupo2, ... Vetores numéricos representando os grupos
  titulo              Título do gráfico
  x, y                Rótulos dos eixos
  ajuda               Exibe esta mensagem de ajuda

Exemplo:
  g1 <- rnorm(15, mean = 10)
  g2 <- rnorm(15, mean = 12)
  g3 <- rnorm(15, mean = 16)
  anova(g1, g2, g3, x = \"Tratamento\", y = \"Resposta\")
")
    return(invisible(NULL))
  }
  
  # Pacotes necessários
  pacotes_necessarios <- c("ggplot2", "multcompView", "agricolae")
  for (pkg in pacotes_necessarios) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "não está instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }
  library(ggplot2)
  library(multcompView)
  
  # Agrupamento dos dados
  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)
  
  if (length(grupos) < 2) stop("Forneça pelo menos dois grupos.")
  
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)
  
  # ANOVA
  modelo <- aov(valor ~ grupo, data = dados)
  p_anova <- as.numeric(summary(modelo)[[1]][["Pr(>F)"]][1])
  p_label <- if (p_anova < 0.001) {
    "ANOVA: p < 0.001"
  } else {
    paste0("ANOVA: p = ", signif(p_anova, 3))
  }
  
  # Tukey HSD
  tukey_res <- TukeyHSD(modelo)
  tukey_df <- as.data.frame(tukey_res[[1]])
  pares_signif <- subset(tukey_df, `p adj` < 0.05)
  
  # Mensagem com pares significativos
  if (nrow(pares_signif) == 0) {
    message("Nenhuma comparação post-hoc significativa (p < 0.05).")
  } else {
    nomes_pares <- rownames(pares_signif)
    pvals <- pares_signif$`p adj`
    msg <- paste0("(", gsub("-", " vs ", nomes_pares), ", p = ", signif(pvals, 3), ")")
    message("Pares significativos (Tukey HSD):\n", paste(msg, collapse = "\n"))
  }
  
  # Letras de significância
  letras <- multcompLetters4(modelo, tukey_res)
  letras_df <- data.frame(grupo = names(letras$grupo$Letters),
                          letra = letras$grupo$Letters)
  
  # Posição das letras no gráfico
  maximos <- aggregate(valor ~ grupo, data = dados, max)
  letras_df <- merge(maximos, letras_df, by = "grupo")
  letras_df$valor <- letras_df$valor + 1  # desloca para cima
  
  # Gráfico
  g <- ggplot(dados, aes(x = grupo, y = valor, fill = grupo)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    geom_text(data = letras_df, aes(x = grupo, y = valor, label = letra),
              size = 4, vjust = 0) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = titulo,
      subtitle = p_label,
      x = x,
      y = y
    )
  
  print(g)
  invisible(modelo)
}


