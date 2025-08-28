#' Teste de Correlacao (Pearson ou Spearman)
#'
#' Realiza teste de correlacao entre duas variaveis numericas, escolhendo automaticamente
#' entre o metodo Pearson ou Spearman com base na normalidade dos dados e na presenca de empates.
#' Opcionalmente exibe graficos de diagnostico e grafico da correlacao com linha de tendencia.
#'
#' @param x Vetor numerico.
#' @param y Vetor numerico.
#' @param metodo Metodo de correlacao: "auto" (padrao), "pearson" ou "spearman".
#' @param ajuda Se TRUE, exibe explicacao detalhada da funcao.
#' @param verbose Se TRUE, imprime mensagens sobre o metodo escolhido e testes de normalidade.
#' @param plot_normalidade Se TRUE, gera QQ-plots para avaliacao da normalidade dos dados.
#'
#' @return Objeto de classe \code{htest} com os resultados do teste de correlacao invisivelmente.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30)
#' y <- x + rnorm(30)
#' teste.correlacao(x, y, plot_normalidade = TRUE)
#'
#' @export


teste.correlacao <- function(x, y, metodo = "auto", ajuda = FALSE, verbose = TRUE, plot_normalidade = FALSE) {

  # Mensagem de ajuda
  if (ajuda || missing(x) || missing(y)) {
    cat("
Funcao teste.correlacao()

Descricao:
  Testa a correlacao entre duas variaveis numericas, escolhendo automaticamente entre Pearson e Spearman.
  Pode exibir testes graficos de normalidade (QQ-plot).

Uso:
  teste.correlacao(x, y, metodo = 'auto', plot_normalidade = TRUE)

Argumentos:
  x, y             Vetores numericos de mesma dimensao
  metodo           'auto', 'pearson' ou 'spearman'
  verbose          Se TRUE, exibe qual metodo foi usado e o motivo
  ajuda            Se TRUE, exibe esta mensagem com exemplo
  plot_normalidade Se TRUE, exibe QQ-plots

Exemplo:
  set.seed(123)
  x <- rnorm(30)
  y <- x + rnorm(30, 0, 1)
  teste.correlacao(x, y, plot_normalidade = TRUE)
")
    return(invisible(NULL))
  }

  # Verificacoes
  if (!is.numeric(x) || !is.numeric(y)) stop("Ambos os vetores devem ser numericos.")
  if (length(x) != length(y)) stop("Os vetores devem ter o mesmo comprimento.")

  # Nome das variaveis para o grafico
  nome_x <- deparse(substitute(x))
  nome_y <- deparse(substitute(y))

  dados <- data.frame(x = x, y = y)
  possui_empates <- anyDuplicated(x) > 0 || anyDuplicated(y) > 0

  # Teste de normalidade
  shapiro_x <- stats::shapiro.test(x)
  shapiro_y <- stats::shapiro.test(y)
  normal_x <- shapiro_x$p.value > 0.05
  normal_y <- shapiro_y$p.value > 0.05

  # Plot de normalidade
  if (plot_normalidade) {
    op <- graphics::par(mfrow = c(1, 2))
    stats::qqnorm(x, main = paste("QQ-Plot de", nome_x)); stats::qqline(x, col = "red")
    stats::qqnorm(y, main = paste("QQ-Plot de", nome_y)); stats::qqline(y, col = "red")
    graphics::par(op)
  }

  # Escolha do metodo
  metodo_usado <- if (metodo == "auto") {
    if (normal_x && normal_y && !possui_empates) "pearson" else "spearman"
  } else {
    metodo
  }

  if (verbose && metodo == "auto") {
    cat(sprintf("Metodo escolhido: %s\n", metodo_usado))
    if (!normal_x || !normal_y) {
      cat("- Dados nao normalmente distribuidos:\n")
      cat(sprintf("  p (x) = %.4f | p (y) = %.4f\n", shapiro_x$p.value, shapiro_y$p.value))
    }
    if (possui_empates) cat("- Foram encontrados empates nos dados\n")
  }

  # Teste estatistico
  teste <- stats::cor.test(x, y, method = metodo_usado)
  r <- round(teste$estimate, 3)
  p <- teste$p.value
  p_texto <- if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))

  interprete <- cut(abs(r),
                    breaks = c(-Inf, 0.3, 0.5, 0.7, 0.9, Inf),
                    labels = c("muito fraca ou inexistente", "fraca", "moderada", "forte", "muito forte"),
                    right = FALSE
  )

  # Grafico com ggplot2
  g <- ggplot2::ggplot(dados, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.6, size = 2.5, color = "steelblue") +
    (if (metodo_usado == "pearson") {
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")
    } else {
      ggplot2::geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dotted")
    }) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Correlacao de", tools::toTitleCase(metodo_usado)),
      subtitle = paste(ifelse(metodo_usado == "pearson", "r =", "? ="), r, "|", p_texto, "|", interprete),
      x = nome_x,
      y = nome_y
    )

  print(g)
  invisible(teste)
}


