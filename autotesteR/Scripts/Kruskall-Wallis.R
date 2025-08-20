kruskal.wallis <- function(..., titulo = "Kruskal-Wallis + Dunn", 
                           x = "Grupo", y = "Valor", ajuda = FALSE) {
 
  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)
  
   if (ajuda || length(grupos) < 2) {
    cat("
Função kruskal.wallis()

Descrição:
  Realiza o Teste de Kruskal-Wallis (uma ANOVA não paramétrica) com pós-teste de Dunn.
  Indicado para comparar mais de dois grupos independentes quando os dados **não seguem distribuição normal**.

Quando usar:
  - Comparação de 3 ou mais grupos independentes;
  - Os dados são **ordinais** ou **numéricos não normais**;
  - É a alternativa à ANOVA quando a suposição de normalidade ou homogeneidade de variâncias é violada.

Assunções:
  - Observações independentes;
  - Variáveis dependentes contínuas ou ordinais;
  - As distribuições dos grupos têm a mesma forma (a diferença está na mediana).

Pós-teste:
  - Dunn com ajuste de Bonferroni é aplicado automaticamente para comparações múltiplas.

Limitações:
  - Menos potência que ANOVA quando as suposições são atendidas;
  - Detecta diferenças na **distribuição geral**, não apenas na média/mediana.

Exemplo:
  grupo1 <- c(2, 3, 4)
  grupo2 <- c(5, 6, 7)
  grupo3 <- c(8, 9, 10)
  kruskal.wallis(grupo1, grupo2, grupo3)

")
    return(invisible(NULL))
  }
  
  
  pacotes <- c("ggplot2", "FSA", "dplyr", "rcompanion", "multcompView")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "não está instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }
  
  library(ggplot2)
  library(FSA)
  library(dplyr)
  library(rcompanion)
  library(multcompView)
  
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)
  
  kruskal <- kruskal.test(valor ~ grupo, data = dados)
  p_kruskal <- kruskal$p.value
  p_label <- if (p_kruskal < 0.001) {
    "Kruskal-Wallis: p < 0.001"
  } else {
    paste0("Kruskal-Wallis: p = ", signif(p_kruskal, 3))
  }
  
  dunn <- FSA::dunnTest(valor ~ grupo, data = dados, method = "bonferroni")
  dunn_df <- dunn$res
  
  pares_signif <- subset(dunn_df, P.adj < 0.05)
  if (nrow(pares_signif) == 0) {
    message("Nenhuma comparação pós-hoc significativa (p < 0.05).")
  } else {
    nomes_pares <- pares_signif$Comparison
    pvals <- pares_signif$P.adj
    msg <- paste0("(", nomes_pares, ", p = ", signif(pvals, 3), ")")
    message("Pares significativos (Dunn, Bonferroni):\n", paste(msg, collapse = "\n"))
  }
  
  comparacoes <- setNames(dunn_df$P.adj, gsub(" ", "", dunn_df$Comparison))
  letras <- multcompView::multcompLetters(comparacoes)$Letters
  letras_df <- data.frame(grupo = names(letras), letra = letras)
  
  maximos <- aggregate(valor ~ grupo, data = dados, max)
  letras_df <- merge(maximos, letras_df, by = "grupo")
  letras_df$valor <- letras_df$valor + 0.05 * max(letras_df$valor, na.rm = TRUE)
  
  g <- ggplot(dados, aes(x = grupo, y = valor, fill = grupo)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    geom_text(data = letras_df, aes(x = grupo, y = valor, label = letra),
              size = 4, vjust = 0) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = titulo, subtitle = p_label, x = x, y = y)
  
  print(g)
  invisible(kruskal)
}




