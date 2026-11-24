{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- Back-end: Servant
module Main where

import Lucid
import qualified Network.HTTP.Media as Media
import Data.Proxy
import Data.List (find, nub)
import Data.String (fromString)
import Network.Wai.Handler.Warp (run)
import Servant
import Text.Printf (printf)
import Web.FormUrlEncoded (Form, parseAll)

data Curso = Curso
    { cursoId :: Int
    , titulo :: String
    , descricao :: String
    , precoCentavos :: Int
    , disponivel :: Bool
    }

cursos :: [Curso]
cursos =
    [ Curso 1 "LASER, LED e Fonoaudiologia"
        "Fundamentos do LASER e LED para fonoaudiólogos" 51000 True
    , Curso 2 "Curso Multidisciplinar e Terapias Integrativas"
        "Fundamentos do LASER e LED para diversas áreas de atuação" 84000 True
    , Curso 3 "Método LBA"
        "Aula completa da técnica Light Brain Access" 134900 False
    ]

data HTML

newtype Pagina = Pagina (Html ())

instance Accept HTML where
    contentType _ = ("text" Media.// "html") Media./: ("charset", "utf-8")

instance MimeRender HTML Pagina where
    mimeRender _ (Pagina html) = renderBS html

-- Mude a linha de baixo para cada página
type API = QueryParams "curso" Int :> Get '[HTML] Pagina
    :<|> "cursos" :> Capture "id" Int :> Get '[HTML] Pagina
    :<|> "carrinho" :> QueryParams "curso" Int :> Get '[HTML] Pagina
    :<|> "pedido" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] Pagina
    :<|> "hello" :> Get '[JSON] String
    :<|> "static" :> Raw

-- Mude as duas linhas de baixo para cada página
servidor :: Server API
servidor =
    mostrarCatalogo
        :<|> detalhesCurso
        :<|> mostrarCarrinho
        :<|> confirmarPedido
        :<|> pure "Hello, world!"
        :<|> serveDirectoryFileServer "public"

mostrarCatalogo :: [Int] -> Handler Pagina
mostrarCatalogo idsMarcados = pure (Pagina (paginaCatalogo idsMarcados))

buscarCurso :: Int -> Maybe Curso
buscarCurso idCurso = find (\curso -> cursoId curso == idCurso) cursos

detalhesCurso :: Int -> Handler Pagina
detalhesCurso idCurso =
    case buscarCurso idCurso of
        Just curso -> pure (Pagina (paginaDetalhes curso))
        Nothing -> throwError err404

mostrarCarrinho :: [Int] -> Handler Pagina
mostrarCarrinho [] = pure (Pagina (paginaCarrinho []))
mostrarCarrinho ids = do
    selecionados <- validarCarrinho ids
    pure (Pagina (paginaCarrinho selecionados))
    
confirmarPedido :: Form -> Handler Pagina
confirmarPedido formulario =
    case parseAll "curso" formulario of
        Left _ -> throwError (err400 { errBody = "ID de curso invalido." })
        Right ids -> do
            selecionados <- validarCarrinho ids
            pure (Pagina (paginaConfirmacao selecionados))

validarCarrinho :: [Int] -> Handler [Curso]
validarCarrinho [] =
    throwError (err400 { errBody = "Selecione pelo menos um curso." })
validarCarrinho ids =
    case traverse buscarCurso (nub ids) of
        Nothing -> throwError (err400 { errBody = "Curso inexistente." })
        Just selecionados
            | all disponivel selecionados -> pure selecionados
            | otherwise ->
                throwError (err400 { errBody = "Ha curso indisponivel na selecao." })

paginaCatalogo :: [Int] -> Html ()
paginaCatalogo idsMarcados = paginaBase "Cursos de LASERterapia" $ do
    h1_ "Cursos de LASERterapia"
    form_ [method_ "get", action_ "/carrinho"] $ do
        div_ [class_ "grade-cursos"] $ mapM_ (exibirCurso idsMarcados) cursos
        button_ [type_ "submit"] "Ver carrinho"

exibirCurso :: [Int] -> Curso -> Html ()
exibirCurso idsMarcados curso = article_ $ do
    h2_ $ a_ [href_ (fromString ("/cursos/" <> show (cursoId curso)))] $
        toHtml (titulo curso)
    dadosCurso curso
    if disponivel curso
        then p_ $ label_ $ do
            input_ $
                [ type_ "checkbox"
                , name_ "curso"
                , value_ (fromString (show (cursoId curso)))
                ] <> [checked_ | cursoId curso `elem` idsMarcados]
            " Selecionar curso"
        else pure ()

paginaDetalhes :: Curso -> Html ()
paginaDetalhes curso = paginaBase (titulo curso) $ do
    h1_ (toHtml (titulo curso))
    dadosCurso curso
    p_ $ a_ [href_ "/"] "Voltar ao catálogo"

dadosCurso :: Curso -> Html ()
dadosCurso curso = do
    p_ (toHtml (descricao curso))
    p_ (toHtml (formatarPreco (precoCentavos curso)))
    p_ (if disponivel curso then "Disponível" else "Indisponível")

paginaBase :: String -> Html () -> Html ()
paginaBase tituloPagina conteudo = do
    doctype_
    html_ [lang_ "pt-BR"] $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            title_ (toHtml tituloPagina)
            link_ [rel_ "stylesheet", href_ "/static/loja.css"]
        body_ $ main_ [class_ "pagina"] conteudo

paginaCarrinho :: [Curso] -> Html ()
paginaCarrinho [] = paginaBase "Carrinho" $ do
    h1_ "Carrinho"
    p_ "Seu carrinho está vazio."
    p_ $ a_ [href_ "/"] "Voltar ao catálogo"

paginaCarrinho selecionados = paginaBase "Carrinho" $ do
    h1_ "Carrinho"
    resumoPedido selecionados
    form_ [method_ "post", action_ "/pedido"] $ do
        mapM_ campoCurso selecionados
        button_ [type_ "submit"] "Confirmar pedido simulado"
    form_ [method_ "get", action_ "/"] $ do
        mapM_ campoCurso selecionados
        p_ $ button_ [type_ "submit"] "Editar seleção"

campoCurso :: Curso -> Html ()
campoCurso curso =
    input_
        [ type_ "hidden"
        , name_ "curso"
        , value_ (fromString (show (cursoId curso)))
        ]

paginaConfirmacao :: [Curso] -> Html ()
paginaConfirmacao selecionados = paginaBase "Pedido simulado" $ do
    h1_ "Pedido simulado confirmado"
    resumoPedido selecionados
    p_ $ a_ [href_ "/"] "Voltar ao catálogo"

resumoPedido :: [Curso] -> Html ()
resumoPedido selecionados = do
    ul_ $ mapM_ exibirItem selecionados
    p_ $ strong_ $ toHtml ("Total: " <> formatarPreco total)
  where
    total = sum (map precoCentavos selecionados)

    exibirItem :: Curso -> Html ()
    exibirItem curso =
        li_ $ toHtml (titulo curso <> " — " <> formatarPreco (precoCentavos curso))

formatarPreco :: Int -> String
formatarPreco valor =
    let (reais, centavos) = valor `divMod` 100
    in printf "R$ %d,%02d" reais centavos

-- Não alterar
app :: Application
app = serve (Proxy :: Proxy API) servidor

-- cabal repl
-- cabal update
main :: IO ()
main = do
    print "Serving on port 8080"
    run 8080 app
