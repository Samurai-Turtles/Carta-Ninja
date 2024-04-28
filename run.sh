#!/bin/bash

# Script de execução das versões do Carta Ninja.

# Exibe a mensagem de seleção da versão
print_selection_msg() {
    clear
    echo "===== Carta Ninja Runner ====="
    echo "Selecione a versão do jogo a ser executada:"
    echo "(1) Haskell (requer Cabal instalado)"
    echo "(2) Prolog  (requer SWI-Prolog instalado)"
}

# Executa a versão Haskell do jogo
run_haskell_version() {
    cd haskell/ || return
    cabal run
    cd ../
}

# Executa a versão Prolog do jogo
run_prolog_version() {
    cd prolog/ || return
    swi-prolog.swipl app/main.pl
    cd ../
}

# Rotina principal, executa a lógica de escolha da versão
main() {
    print_selection_msg
    echo -n ">> "
    read -r version

    if [[ version -eq 1 ]]; then
        run_haskell_version
    elif [[ version -eq 2 ]]; then
        run_prolog_version
    else
        echo "Opção indisponível"
    fi
}

main
