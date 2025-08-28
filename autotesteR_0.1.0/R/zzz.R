.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    crayon::green("autotesteR "), "carregado com sucesso!\n",
    "----------------------------------------------------------\n",
    "Um pacote desenvolvido para facilitar análises estatísticas\n",
    "com uma linguagem acessível, diagnósticos automáticos e gráficos claros.\n",
    "\n",
    "Digite ", crayon::green("?autotesteR"), " para ver uma introdução geral\n",
    "Digite o nome da fórmula sem parâmetros (", crayon::bold("ex. teste.t()"), ") para ver uma breve descrição do teste\n",
    "Site oficial e atualizações: https://github.com/Luiz-Garcia-R/autotesteR.git\n",
    "----------------------------------------------------------\n"
  )
}
