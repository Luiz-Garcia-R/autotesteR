correlacao <- function(x, y, metodo = "auto", ajuda = FALSE, verbose = TRUE, plot_normalidade = FALSE) {
  
  if (ajuda || missing(x) || missing(y)) {
    cat("
Função correlacao()

Descrição:
  Testa a correlação entre duas variáveis numéricas, escolhendo automaticamente entre Pearson e Spearman,
  ou permitindo forçar o método com o argumento 'metodo'. Pode também exibir testes gráficos de normalidade.

Uso:
  correlacao(x, y, metodo = 'auto', plot_normalidade = TRUE)

Argumentos:
  x, y             Vetores numéricos de mesma dimensão
  metodo           'auto', 'pearson' ou 'spearman'
  verbose          Se TRUE, exibe qual método foi usado e o motivo
  ajuda            Se TRUE, exibe esta mensagem com exemplo
  plot_normalidade Se TRUE, exibe QQ-plots para avaliar a normalidade

Exemplo:
  set.seed(123)
  x <- rnorm(30)
  y <- x + rnorm(30, 0, 1)
  correlacao(x, y, plot_normalidade = TRUE)
")
    return(invisible(NULL))
  }
  
  if (!is.numeric(x) || !is.numeric(y)) stop("Ambos os vetores devem ser numéricos.")
  if (length(x) != length(y)) stop("Os vetores devem ter o mesmo comprimento.")
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('O pacote "ggplot2" é necessário. Instale-o com: install.packages("ggplot2")')
  }
  
  library(ggplot2)
  
  # Captura nomes legíveis dos vetores
  nome_x <- sub(".*\\$", "", deparse(substitute(x)))
  nome_y <- sub(".*\\$", "", deparse(substitute(y)))
  
  dados <- data.frame(x = x, y = y)
  
  # Diagnóstico de normalidade e empates
  shapiro_x <- shapiro.test(x)
  shapiro_y <- shapiro.test(y)
  normal_x <- shapiro_x$p.value > 0.05
  normal_y <- shapiro_y$p.value > 0.05
  possui_empates <- any(duplicated(x)) || any(duplicated(y))
  
  # Mostrar QQ-plots se solicitado
  if (plot_normalidade) {
    par(mfrow = c(1, 2))
    qqnorm(x, main = paste("QQ-Plot de", nome_x), xlab = "Quantis Teóricos", ylab = "Quantis Amostrais"); qqline(x, col = "red")
    qqnorm(y, main = paste("QQ-Plot de", nome_y), xlab = "Quantis Teóricos", ylab = "Quantis Amostrais"); qqline(y, col = "red")
    par(mfrow = c(1, 1))
  }
  
  # Determinar método
  metodo_usado <- metodo
  if (metodo == "auto") {
    if (normal_x && normal_y && !possui_empates) {
      metodo_usado <- "pearson"
    } else {
      metodo_usado <- "spearman"
    }
    
    if (verbose) {
      cat(sprintf("Método escolhido: %s\n", metodo_usado))
      if (!normal_x || !normal_y) {
        cat("- Dados não normalmente distribuídos (Shapiro-Wilk):\n")
        cat(sprintf("  p (x) = %.4f | p (y) = %.4f\n", shapiro_x$p.value, shapiro_y$p.value))
      }
      if (possui_empates) cat("- Foram encontrados empates nos dados\n")
    }
  }
  
  # Correlação
  teste <- cor.test(x, y, method = metodo_usado)
  r <- round(teste$estimate, 3)
  p <- teste$p.value
  p_texto <- if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))
  
  interprete <- if (abs(r) >= 0.9) {
    "correlação muito forte"
  } else if (abs(r) >= 0.7) {
    "correlação forte"
  } else if (abs(r) >= 0.5) {
    "correlação moderada"
  } else if (abs(r) >= 0.3) {
    "correlação fraca"
  } else {
    "correlação muito fraca ou inexistente"
  }
  
  # Gráfico
  g <- ggplot(dados, aes(x = x, y = y)) +
    geom_point(alpha = 0.6, size = 2.5, color = "steelblue") +
    {
      if (metodo_usado == "pearson") {
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")
      } else {
        geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dotted")
      }
    } +
    theme_minimal() +
    labs(
      title = paste("Correlação de", tools::toTitleCase(metodo_usado)),
      subtitle = paste(
        if (metodo_usado == "pearson") "r =" else "ρ =",
        r, "|", p_texto, "|", interprete
      ),
      x = nome_x,
      y = nome_y
    )
  
  print(g)
  invisible(teste)
}

