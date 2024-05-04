#!/bin/bash

# Script de execução das versões do Carta Ninja.

BANNER="
 ██████╗ █████╗ ██████╗ ████████╗ █████╗     ███╗   ██╗██╗███╗   ██╗     ██╗ █████╗ 
██╔════╝██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗    ████╗  ██║██║████╗  ██║     ██║██╔══██╗
██║     ███████║██████╔╝   ██║   ███████║    ██╔██╗ ██║██║██╔██╗ ██║     ██║███████║
██║     ██╔══██║██╔══██╗   ██║   ██╔══██║    ██║╚██╗██║██║██║╚██╗██║██   ██║██╔══██║
╚██████╗██║  ██║██║  ██║   ██║   ██║  ██║    ██║ ╚████║██║██║ ╚████║╚█████╔╝██║  ██║
 ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝    ╚═╝  ╚═══╝╚═╝╚═╝  ╚═══╝ ╚════╝ ╚═╝  ╚═╝
                                                                                    
██████╗ ██╗   ██╗███╗   ██╗███╗   ██╗███████╗██████╗                                
██╔══██╗██║   ██║████╗  ██║████╗  ██║██╔════╝██╔══██╗                               
██████╔╝██║   ██║██╔██╗ ██║██╔██╗ ██║█████╗  ██████╔╝                               
██╔══██╗██║   ██║██║╚██╗██║██║╚██╗██║██╔══╝  ██╔══██╗                               
██║  ██║╚██████╔╝██║ ╚████║██║ ╚████║███████╗██║  ██║                               
╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝                               
"

print_colored() {
    echo -en "$1"
    echo -n "$2"
    echo -en "\e[0m"
}

# Exibe a mensagem de seleção da versão
print_selection_msg() {
    clear
    print_colored "\e[33m" "$BANNER"
    print_colored "\e[96m" "Selecione a versão do jogo a ser executada:"
    echo ""
    echo "  [1] Haskell (requer Cabal instalado)"
    echo "  [2] Prolog  (requer SWI-Prolog instalado)"
    echo "  [Q] Encerrar programa"
}

# Executa a versão Haskell do jogo
run_haskell_version() {
    if ! command -v cabal > /dev/null 2>&1; then
        echo "[ERRO] Comando \"cabal\" não encontrado!"
        exit 1
    fi

    cd haskell/
    cabal run
    cd ../
}

# Executa a versão Prolog do jogo
run_prolog_version() {
    cd prolog/
    
    if command -v swipl > /dev/null 2>&1; then
        swipl main.pl # Instalação via APT ou alias
    elif command -v swi-prolog.swipl > /dev/null 2>&1; then
        swi-prolog.swipl main.pl # Instalação via Snap
    else
        echo "[ERRO] Comandos Prolog não encontrados!"
        cd ../
        exit 2
    fi

    cd ../
}

# Rotina principal, executa a lógica de escolha da versão
main() {
    print_selection_msg
    print_colored "\e[92m" ">> "
    read -r version

    if [[ version -eq 1 ]]; then
        run_haskell_version
    elif [[ version -eq 2 ]]; then
        run_prolog_version
    elif [[ "${version^^}" == "Q" ]]; then
        print_colored "\e[92m" "Obrigado por jogar CartaNinja :)"
        echo ""
        exit 0
    else
        print_colored "\e[31m" "Opção indisponível"
        echo ""
        exit 3
    fi
}

main
