#' Teste de correlacao (Pearson, Spearman ou Kendall)
#'
#' Realiza teste de correlacao entre duas variaveis numericas, escolhendo automaticamente
#' entre Pearson, Spearman ou Kendall com base na normalidade, empates e outliers.
#' Opcionalmente exibe graficos de diagnostico e grafico da correlacao com linha de tendencia.
#'
#' @param x Vetor numerico
#' @param y Vetor numerico
#' @param metodo Metodo de correlacao: "auto" (padrao), "pearson", "spearman" ou "kendall"
#' @param ajuda Se TRUE, exibe explicacao detalhada da funcao
#' @param verbose Se TRUE, imprime mensagens sobre o metodo escolhido e testes de normalidade
#' @param plot_normalidade Se TRUE, gera QQ-plots para avaliacao da normalidade dos dados
#' @importFrom stats shapiro.test cor.test qqnorm qqline quantile
#'
#' @return Objeto de classe htest invisivel com o resultado do teste de correlacao
#'
#' @examples
#' x <- rnorm(30)
#' y <- x + rnorm(30)
#' teste.correlacao(x, y, plot_normalidade = TRUE)
#'
#' @export
teste.correlacao <- function(x, y, metodo = "auto", ajuda = FALSE,
                             verbose = TRUE, plot_normalidade = FALSE) {

  # Mensagem de ajuda
  if (ajuda || missing(x) || missing(y)) {
    if (verbose) {
      message("
Funcao teste.correlacao()

Descricao:
  Testa a correlacao entre duas variaveis numericas, escolhendo automaticamente entre Pearson, Spearman e Kendall.
  Pode exibir testes graficos de normalidade (QQ-plot).

Uso:
  teste.correlacao(x, y, metodo = 'auto', plot_normalidade = TRUE)

Argumentos:
  x, y             Vetores numericos de mesma dimensao
  metodo           'auto', 'pearson', 'spearman' ou 'kendall'
  verbose          Se TRUE, exibe qual metodo foi usado e o motivo
  ajuda            Se TRUE, exibe esta mensagem com exemplo
  plot_normalidade Se TRUE, exibe QQ-plots

Exemplo:
  x <- rnorm(30)
  y <- x + rnorm(30)
  teste.correlacao(x, y, plot_normalidade = TRUE)
")
    }
    return(invisible(NULL))
  }

  # Validacoes
  if (!is.numeric(x) || !is.numeric(y)) stop("Ambos os vetores devem ser numericos.")
  if (length(x) != length(y)) stop("Os vetores devem ter o mesmo comprimento.")

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
    oldpar <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(oldpar), add = TRUE)
    stats::qqnorm(x, main = paste("QQ-Plot de", nome_x)); stats::qqline(x, col = "red")
    stats::qqnorm(y, main = paste("QQ-Plot de", nome_y)); stats::qqline(y, col = "red")
  }

  # Funcao para detectar outliers (IQR)
  detect_outliers <- function(v) {
    q1 <- stats::quantile(v, 0.25)
    q3 <- stats::quantile(v, 0.75)
    iqr <- q3 - q1
    which(v < (q1 - 1.5 * iqr) | v > (q3 + 1.5 * iqr))
  }
  out_total <- max(length(detect_outliers(x)) / length(x),
                   length(detect_outliers(y)) / length(y))

  # Escolha do metodo
  metodo_usado <- if (metodo == "auto") {
    if (out_total > 0.05) {
      "kendall"
    } else if (normal_x && normal_y && !possui_empates) {
      "pearson"
    } else {
      "spearman"
    }
  } else {
    metodo
  }

  if (verbose && metodo == "auto") {
    message(sprintf("Metodo escolhido: %s", metodo_usado))
    if (!normal_x || !normal_y) {
      message("- Dados nao normalmente distribuidos:")
      message(sprintf("  p(x) = %.4f | p(y) = %.4f", shapiro_x$p.value, shapiro_y$p.value))
    }
    if (possui_empates) message("- Foram encontrados empates nos dados")
    if (out_total > 0.05) message(sprintf("- Presenca de outliers detectada: %.1f%%", out_total*100))
  }

  # Teste estatistico
  teste <- stats::cor.test(x, y, method = metodo_usado)
  coef_val <- round(unname(teste$estimate), 3)
  p <- teste$p.value
  p_texto <- if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))

  # Interpretacao da correlacao
  interprete <- cut(abs(coef_val),
                    breaks = c(-Inf, 0.3, 0.5, 0.7, 0.9, Inf),
                    labels = c("muito fraca ou inexistente", "fraca", "moderada", "forte", "muito forte"),
                    right = FALSE)

  simbolo <- ifelse(metodo_usado == "kendall", "tau", "r")
  subtitulo <- paste0(simbolo, " = ", coef_val, " | ", p_texto, " | ", interprete)

  # Grafico
  g <- ggplot2::ggplot(dados, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.6, size = 2.5, color = "steelblue") +
    if (metodo_usado == "pearson") {
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")
    } else {
      ggplot2::geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dotted")
    } +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = paste("Correlacao de", tools::toTitleCase(metodo_usado)),
      subtitle = subtitulo,
      x = nome_x,
      y = nome_y
    )

  print(g)
  invisible(teste)
}
