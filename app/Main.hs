{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- Back-end: Servant
module Main where

import Lucid
import Lucid.Base (makeAttribute)
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
    , capa :: String
    , descricao :: String
    , precoCentavos :: Int
    , disponivel :: Bool
    , categoria :: String
    , cargaHoraria :: Int
    , ementa :: Html ()
    }

cursos :: [Curso]
cursos =
    [ Curso
        { cursoId = 1
        , titulo = "LASER, LED e Fonoaudiologia"
        , capa = "laser-fono.jpg"        
        , descricao = "Fundamentos do LASER e LED para fonoaudiólogos"
        , precoCentavos = 51000
        , disponivel = True
        , categoria = "Fonoaudiologia"
        , cargaHoraria = 16
        , ementa = ementaFono
        }
    , Curso
        { cursoId = 2
        , titulo = "Curso Multidisciplinar"
        , capa = "multidisciplinar.jpg"
        , descricao = "Fundamentos do LASER e LED para diversas áreas de atuação"
        , precoCentavos = 84000
        , disponivel = False
        , categoria = "Multidisciplinar"
        , cargaHoraria = 16
        , ementa = ementaMulti
        }
    , Curso
        { cursoId = 3
        , titulo = "Método LBA"
        , capa = "metodo-lba.jpg"
        , descricao = "Aula completa da técnica Light Brain Access"
        , precoCentavos = 134900
        , disponivel = True
        , categoria = "Multidisciplinar"
        , cargaHoraria = 9
        , ementa = ementaLBA
        }
    ]

secaoEmenta :: String -> Html () -> Html ()
secaoEmenta tituloSecao conteudo = section_ $ do
    h3_ (toHtml tituloSecao)
    conteudo

topicoEmenta :: String -> Html ()
topicoEmenta topico = li_ (toHtml topico)

listaNumerada :: [String] -> Html ()
listaNumerada topicos = ol_ (mapM_ topicoEmenta topicos)

listaComMarcadores :: [String] -> Html ()
listaComMarcadores topicos = ul_ (mapM_ topicoEmenta topicos)

ementaFono :: Html ()
ementaFono = do
    secaoEmenta "LASERTERAPIA" $
        listaNumerada
            [ "Histórico e definição do LASER"
            , "Aspectos legais e éticos da LASERterapia na Fonoaudiologia"
            , "Princípios da LASERterapia, normas de segurança e ergonomia"
            , "Tipos de LASER/LED"
            , "Princípios Físicos e Biológicos do LASER"
            , "Técnicas de aplicação do LASER"
            , "Efeitos da fotobiomodulação"
            , "Dosimetria"
            , "Desinfecção com terapia fotodinâmica (PDT)"
            , "Indicações e contraindicações"
            , "ILIB transdérmico modificado (aplicabilidade do laser sistêmico na fonoaudiologia)"
            ]

    secaoEmenta "APLICABILIDADE NA FONOAUDIOLOGIA" $
        listaNumerada
            [ "Disfagia"
            , "Fononcologia"
            , "Paralisia Facial"
            , "Pós Cirurgia Ortognática"
            , "Voz"
            , "Amamentação"
            , "Ronco e Apnéia do Sono"
            , "Sialorréia/Xerostomia"
            , "Estética Facial"
            , "Nefralgia"
            , "Parestesia"
            , "DTMs"
            , "Aplicabilidade na Linguagem (novos achados)"
            , "Aplicabilidade na Audiologia (Reabilitação Labiríntica e Zumbido)"
            , "Aplicabilidade na Fonoaudiologia Hospitalar (novos paradigmas)"
            , "Queimados"
            , "Fissura Labio Palatina"
            ]

ementaMulti :: Html ()
ementaMulti = do
    secaoEmenta "LASER e Motricidade Oral" $
        listaComMarcadores
            [ "Motricidade Orofacial;"
            , "Disfagia;"
            , "Alterações Anatomofuncionais"
            ]

    secaoEmenta "LASERterapia no tratamento de feridas" $
        listaComMarcadores
            [ "Anatomia da pele"
            , "Tipos de feridas"
            , "Coberturas e correlatos para tratamento de feridas"
            , "Relato de Casos"
            ]

    secaoEmenta "Amamentação, Pós Parto e Fotobiomodulação" $
        listaComMarcadores
            [ "Leite Humano: Aspectos Nutricionais e Imunológicos"
            , "Psicofisiologia da Amamentação"
            , "Anatomia e tipos de mamas"
            , "Principais intercorrências mamárias e seus Tratamentos (Ingurgitamento Mamário, Mastite, Ducto Obstruído, Candidíase Mamária, entre outros...)"
            , "Discussão de Casos e Fotobiomodulação"
            , "Pós Parto"
            ]

    secaoEmenta "A importância da Saúde Integrativa nos tratamentos das doenças bucais" $
        ul_ $ do
            li_ $ do
                "As principais inflamações e infecções orais:"
                listaNumerada
                    [ "Cáries"
                    , "Gengivite"
                    , "Periodontite"
                    , "Aftas"
                    , "Herpes"
                    , "Mucosite"
                    , "Doença mão pé e boca"
                    , "Candidíase"
                    , "Câncer"
                    ]
            topicoEmenta "Causas"
            topicoEmenta "Tratamentos: como as terapias integrativas tem ajudado a otimizar os resultados"

ementaLBA :: Html ()
ementaLBA = do
    p_ "O Método LBA - Light Brain Access® foi baseado nos estudos de Michael Hamblin (Harvard) onde é utilizado a luz LASER de baixa potência (terapêutico) para atingirmos a Fotobiomodulação, isto é, homeostase do tecido neurológico através da luz, buscando proporcionar o reequilíbrio das funções cerebrais. A técnica não é invasiva, podendo ser utilizada por todos os profissionais de saúde que trabalham com demandas neurológicas e/ou psiquiátricas."

    p_ "Bases da Fotobiomodulação para Fototerapia Transcraniana"
    p_ "Neuroanatomia e Neurofisiologia e suas patologias"
    p_ "Direcionadores Clínicos do ILIB transdermico para o Light Brain Access"
    p_ "Direcionadores Clínicos do Light Brain Access para as seguintes patologias:"

    listaComMarcadores
        [ "Pares Cranianos"
        , "Esclerose Múltipla"
        , "Esclerose Lateral Amiotrófica"
        , "Alzheimer"
        , "Parkinson"
        , "Disartria"
        , "Apraxia"
        , "TDAH"
        , "Paralisia Cerebral"
        , "Dislexia"
        , "Afasia"
        , "Transtorno do Espectro Autista"
        , "Depressão"
        , "Ansiedade"
        , "Bipolaridade"
        , "Insônia"
        , "Zumbido"
        ]

    p_ "Publico Alvo: Profissionais da Saúde habilitados a fazer Fotobiomodulação/LASERterapia"
    p_ $ strong_ $ toHtml
        ("OBS.: Necessário comprovar, posterior a inscrição, habilitação em Curso de Fotobiomodulação/LASERterapia" :: String)

data HTML

newtype Pagina = Pagina (Html ())

instance Accept HTML where
    contentType _ = ("text" Media.// "html") Media./: ("charset", "utf-8")

instance MimeRender HTML Pagina where
    mimeRender _ (Pagina html) = renderBS html

-- Mude a linha de baixo para cada página
type API = QueryParams "curso" Int :> Get '[HTML] Pagina
    :<|> "cursos" :> Capture "id" Int :> QueryParams "curso" Int :> Get '[HTML] Pagina
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

detalhesCurso :: Int -> [Int] -> Handler Pagina
detalhesCurso idCurso idsMarcados =
    case buscarCurso idCurso of
        Just curso -> pure (Pagina (paginaDetalhes curso idsMarcados))
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
    header_ [class_ "catalogo-topo"] $ do
        h1_ "Cursos de LASERterapia"
        form_ [method_ "get", action_ "/carrinho"] $ do
            mapM_ campoCurso selecionados
            button_ [type_ "submit"] $
                toHtml ("Ver carrinho (" <> show (length selecionados) <> ")")

    div_ [class_ "grade-cursos"] $ mapM_ (exibirCurso selecionados) cursos
  where
    selecionados = cursosSelecionados idsMarcados

exibirCurso :: [Curso] -> Curso -> Html ()
exibirCurso selecionados curso = article_ $ do
    form_ [method_ "get", action_ (fromString (caminhoDetalhes curso))] $ do
        mapM_ campoCurso selecionados
        h2_ $ button_ [type_ "submit", class_ "link-acao cartao-link"] $ do
            span_ (toHtml (titulo curso))
            img_
                [ class_ "capa-curso"
                , src_ (fromString ("/static/capas/" <> capa curso))
                , alt_ ""
                ]

    dadosCurso curso

    div_ [class_ "acoes-cartao"] $
        if disponivel curso
            then formularioAlternarCurso "/" curso selecionados
            else button_ [type_ "button", disabled_ "disabled"] "Indisponível"

caminhoDetalhes :: Curso -> String
caminhoDetalhes curso = "/cursos/" <> show (cursoId curso)

cursosSelecionados :: [Int] -> [Curso]
cursosSelecionados idsMarcados =
    filter (\curso -> disponivel curso && cursoId curso `elem` idsMarcados) cursos

formularioAlternarCurso :: String -> Curso -> [Curso] -> Html ()
formularioAlternarCurso destino curso selecionados =
    form_ [method_ "get", action_ (fromString destino)] $ do
        mapM_ campoCurso novaSelecao
        button_
            ([type_ "submit"] <> [class_ "botao-remover" | cursoNoCarrinho])
            (toHtml rotuloCarrinho)
  where
    mesmoCurso item = cursoId item == cursoId curso
    cursoNoCarrinho = any mesmoCurso selecionados

    novaSelecao
        | cursoNoCarrinho = filter (not . mesmoCurso) selecionados
        | otherwise = selecionados <> [curso]

    rotuloCarrinho :: String
    rotuloCarrinho
        | cursoNoCarrinho = "Remover do carrinho"
        | otherwise = "Adicionar ao carrinho"

paginaDetalhes :: Curso -> [Int] -> Html ()
paginaDetalhes curso idsMarcados = paginaBase (titulo curso) $ do
    header_ [class_ "cabecalho-curso"] $ do
        div_ $ do
            h1_ (toHtml (titulo curso))
            p_ (toHtml (descricao curso))
            p_ $ do
                strong_ "Categoria: "
                toHtml (categoria curso)
            p_ $ do
                strong_ "Carga horária: "
                toHtml (show (cargaHoraria curso) <> " h")
            p_ $ do
                strong_ "Valor: "
                toHtml (formatarPreco (precoCentavos curso))

            div_ [class_ "acoes-curso"] $ do
                if disponivel curso
                    then formularioAlternarCurso (caminhoDetalhes curso) curso selecionados
                    else button_ [type_ "button", disabled_ "disabled"] "Indisponível"

                form_ [method_ "get", action_ "/carrinho"] $ do
                    mapM_ campoCurso selecionados
                    button_ [type_ "submit", class_ "botao-secundario"] $
                        toHtml ("Ver carrinho (" <> show (length selecionados) <> ")")

        img_
            [ class_ "capa-curso capa-detalhes"
            , src_ (fromString ("/static/capas/" <> capa curso))
            , alt_ (fromString ("Capa do curso " <> titulo curso))
            ]

    section_ [class_ "ementa"] $ do
        h2_ "Conteúdo do curso"
        ementa curso

    form_ [method_ "get", action_ "/"] $ do
        mapM_ campoCurso selecionados
        button_ [type_ "submit", class_ "botao-secundario"] "Voltar ao catálogo"
  where
    selecionados = cursosSelecionados idsMarcados

dadosCurso :: Curso -> Html ()
dadosCurso curso = div_ [class_ "dados-cartao"] $ do
    p_ (toHtml (categoria curso <> " · " <> show (cargaHoraria curso) <> " h"))
    p_ (toHtml (descricao curso))
    p_ [class_ "preco-cartao"] (toHtml (formatarPreco (precoCentavos curso)))

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

paginaCarrinho [] = paginaBase "Carrinho" $ do
    h1_ "Carrinho"

    div_ [class_ "carrinho-vazio"] $ do
        p_ "Seu carrinho está vazio."
        form_ [method_ "get", action_ "/"] $
            button_ [type_ "submit"] "Explorar cursos"

paginaCarrinho selecionados = paginaBase "Carrinho" $ do
    h1_ "Carrinho"
    ul_ [class_ "lista-carrinho"] $
        mapM_ (linhaCarrinho selecionados) selecionados
    totalPedido selecionados

    div_ [class_ "acoes-carrinho"] $ do
        form_ [method_ "post", action_ "/pedido"] $ do
            mapM_ campoCurso selecionados
            button_ [type_ "submit", class_ "botao-confirmar"] "Confirmar o pedido"

        form_ [method_ "get", action_ "/"] $ do
            mapM_ campoCurso selecionados
            button_ [type_ "submit", class_ "botao-remover"] "Voltar ao catálogo"

linhaCarrinho :: [Curso] -> Curso -> Html ()
linhaCarrinho selecionados curso = li_ [class_ "linha-carrinho"] $ do
    form_ [method_ "get", action_ "/carrinho", class_ "carrinho-remocao"] $ do
        mapM_ campoCurso restantes
        button_
            [ type_ "submit"
            , class_ "remover-item"
            , makeAttribute "aria-label"
                (fromString ("Remover " <> titulo curso <> " do carrinho"))
            ] "×"

    dadosLinhaCarrinho curso

  where
    restantes = filter ((/= cursoId curso) . cursoId) selecionados

dadosLinhaCarrinho :: Curso -> Html ()
dadosLinhaCarrinho curso = do
    img_
        [ class_ "carrinho-capa"
        , src_ (fromString ("/static/capas/" <> capa curso))
        , alt_ ""
        ]
    strong_ [class_ "carrinho-titulo"] (toHtml (titulo curso))
    span_ [class_ "carrinho-preco"] $
        toHtml (formatarPreco (precoCentavos curso))

campoCurso :: Curso -> Html ()
campoCurso curso =
    input_
        [ type_ "hidden"
        , name_ "curso"
        , value_ (fromString (show (cursoId curso)))
        ]

paginaConfirmacao :: [Curso] -> Html ()
paginaConfirmacao selecionados = paginaBase "Pedido confirmado" $ do
    h1_ [class_ "titulo-confirmacao"] $ do
        span_
            [ class_ "icone-confirmacao"
            , makeAttribute "aria-hidden" "true"
            ] "✓"
        span_ "Pedido confirmado"

    resumoPedido selecionados

    form_ [method_ "get", action_ "/", class_ "acoes-carrinho"] $
        button_ [type_ "submit", class_ "botao-secundario"] "Voltar ao catálogo"

resumoPedido :: [Curso] -> Html ()
resumoPedido selecionados = do
    ul_ [class_ "lista-carrinho"] $ mapM_ exibirItem selecionados
    totalPedido selecionados
  where
    exibirItem :: Curso -> Html ()
    exibirItem curso =
        li_ [class_ "linha-carrinho linha-confirmada"] (dadosLinhaCarrinho curso)

totalPedido :: [Curso] -> Html ()
totalPedido selecionados =
    p_ [class_ "carrinho-total"] $ strong_ $
        toHtml ("Total: " <> formatarPreco total)
  where
    total = sum (map precoCentavos selecionados)

formatarPreco :: Int -> String
formatarPreco valor =
    let (reais, centavos) = abs (toInteger valor) `divMod` 100
        sinal = if valor < 0 then "-" else ""
    in "R$ " <> sinal <> formatarMilhares reais <> printf ",%02d" centavos

formatarMilhares :: Integer -> String
formatarMilhares valor
    | valor < 1000 = show valor
    | otherwise =
        let (milhares, resto) = valor `divMod` 1000
        in formatarMilhares milhares <> printf ".%03d" resto

-- Não alterar
app :: Application
app = serve (Proxy :: Proxy API) servidor

-- cabal repl
-- cabal update
main :: IO ()
main = do
    print "Serving on port 8080"
    run 8080 app
