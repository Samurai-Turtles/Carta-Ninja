{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Essas extensões de linguagem estão aqui para que o linter
-- não "reclame" do estilo sintático escolhido.
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

module ReadData where

-- TODO lembrar de mudar os imports para que importem
-- somente as funções necessárias.
import Data.Aeson
import System.IO
import GHC.Generics
import System.IO.Unsafe
import System.Directory
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

-- | Este tipo representa o estado geral do jogo. Ele inclui
-- a representação atual da tela e o ranking de pontuações.
data GeneralState = GeneralState {
    -- | A informação sobre qual tela mostrar, dependendo
    -- do estado do jogo.
    screen :: String,
    -- | Uma lista de representações textuais do ranking.
    -- As maiores pontuações são exibidas primeiro.
    ranking :: [String]
} deriving (Generic, Show)

-- | Este tipo representa o estado da campanha atual. Ele inclui
-- informações sobre a pontuação geral do jogador, sua quantidade
-- de vidas e o nível atual em que o jogador se encontra.
data CampaignState = CampaignState {
    -- | A pontuação geral do jogador. Ela é o somatório 
    -- das pontuações obtidas em cada partida.
    pontosGerais :: Int,

    -- | A quantidade atual de vidas do jogador. Quando o número
    -- de vidas atinge 0, ele perde a campanha.
    vidas :: Int,

    -- | O nível atual do jogador. Conforme o jogador avança
    -- o nível, a dificuldade do jogo aumenta.
    nivel :: Int
} deriving (Generic, Show)

-- | Este tipo representa o estado atual de gameplay. Ele é utilizado
-- em batalhas e contém informações pertinentes a elas.
data GameplayState = GameplayState {
    -- | A pontuação do jogador na batalha atual. Quando o
    -- jogador ganha numa comparação entre cartas, essa
    -- pontuação é incrementada.
    h_score :: Int,

    -- | A quantidade de vitórias consecutivas que o jogador acumulou
    -- na batalha atual. Quando esse número atinge 3, o jogador
    -- ganha automaticamente a batalha.
    h_vitorias :: Int,

    -- | Uma lista de Bools que representa se o jogador ganhou ou não com
    -- o elemento correspondente à posição. Se o jogador ganhar uma
    -- comparação com cada elemento, ele ganha automaticamente a batalha.
    h_vitoriasElementos :: [Bool],

    -- | Uma lista de Strings que representa o deck do jogador. Os
    -- primeiros 5 elementos representam a sua mão.
    h_deck :: [String],


    -- | A pontuação do bot na batalha atual. Quando o
    -- bot ganha numa comparação entre cartas, essa
    -- pontuação é incrementada.
    c_score :: Int,

    -- | A quantidade de vitórias consecutivas que o bot acumulou
    -- na batalha atual. Quando esse número atinge 3, o bot
    -- ganha automaticamente a batalha.
    c_vitorias :: Int,

    -- | Uma lista de Bools que representa se o bot ganhou ou não com
    -- o elemento correspondente à posição. Se o bot ganhar uma
    -- comparação com cada elemento, ele ganha automaticamente a batalha.
    c_vitoriasElementos :: [Bool],

    -- | Uma lista de Strings que representa o deck do bot. Os
    -- primeiros 5 elementos representam a sua mão.    
    c_deck :: [String],


    -- | A rodada atual em que a batalha se encontra. Ela é incrementada
    -- a cada turno. Quando esse número atinge 10, o competidor com a
    -- maior pontuação ganha a batalha. Caso ambos o jogador e o bot tenham
    -- a mesma pontuação, a batalha resulta em um empate.
    rodada :: Int
} deriving (Generic, Show)

instance FromJSON GeneralState
instance FromJSON GameplayState
instance FromJSON CampaignState


-- | Esta função representa o estado geral padrão. Ela pode ser
-- utilizada para denotar um erro quando o arquivo JSON correpsondente
-- não pode ser encontrado no sistema de arquivos.
standardGeneral :: GeneralState
standardGeneral = GeneralState "" []

-- | Esta função representa o estado padrão da campanha. Ela pode ser
-- utilizada para denotar um erro quando o arquivo JSON correpsondente
-- não pode ser encontrado no sistema de arquivos.
standardCampaign :: CampaignState
standardCampaign = CampaignState (-7) (-7) (-7)

-- | Esta função representa o estado padrão do gameplay. Ela pode ser
-- utilizada para denotar um erro quando o arquivo JSON correpsondente
-- não pode ser encontrado no sistema de arquivos.
standardGameplay :: GameplayState
standardGameplay = GameplayState (-7) (-7) [] [] (-7) (-7) [] [] (-1)

-- | Esta função lista os caminhos dos arquivos JSON correspondentes
-- aos dados do estado geral, do estado da campanha e do estado de
-- gameplay, respectivamente.
toPath :: [FilePath]
toPath = ["app/core/GameState/GeneralState.json",
        "app/core/GameState/CampaignState.json",
        "app/core/GameState/GameplayState.json"]

-- | Esta função pega os dados do estado geral do arquivo JSON
-- correspondente. Caso o arquivo esteja vazio, o estado geral
-- padrão é retornado.
getGeneralData :: GeneralState
getGeneralData = do
    let file = unsafePerformIO ( B.readFile (toPath !! 0))
    let decodeData = decode file :: Maybe GeneralState
    case decodeData of
        Nothing -> standardGeneral
        Just out -> out

-- | Esta função pega os dados do estado da campanha do arquivo JSON
-- correspondente. Caso o arquivo esteja vazio, o estado padrão da
-- campanha é retornado.
getCampaignData :: CampaignState
getCampaignData = do
    let file = unsafePerformIO ( B.readFile (toPath !! 1))
    let decodeData = decode file :: Maybe CampaignState
    case decodeData of
        Nothing -> standardCampaign
        Just out -> out

-- | Esta função pega os dados do estado de gameplay do arquivo JSON
-- correspondente. Caso o arquivo esteja vazio, o estado padrão de 
-- gameplay é retornado.
getGameplayData :: GameplayState
getGameplayData = do
    let file = unsafePerformIO ( B.readFile (toPath !! 2))
    let decodeData = decode file :: Maybe GameplayState
    case decodeData of
        Nothing -> standardGameplay
        Just out -> out
