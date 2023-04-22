import Ficha1
import Data.Bool (Bool (True))

--ex1 

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (H h1 m1,H h2 m2) = horaValida h1 && horaValida h2 && horaDepois h1 h2

viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [(H h1 m1, H h2 m2)] = etapaBemConstruida [(H h1 m1, H h2 m2)]
viagemBemConstruida ((H h1 m1, H h2 m2):(H h3 m3, H h4 m4):t) = etapaBemConstruida (H h1 m1, H h2 m2) && viagemBemConstruida t

partidaeChegada :: Viagem -> (Hora,Hora)
partidaeChegada [] = error "viagem não pode ser vazia"
partidaeChegada v = (hi,hf)
    where (hi,_) = head v 
          (_,hf) = last v 

-- ex2

type Poligonal = [Ponto]
poligonal_ex = [Cartesiano 0 0, Cartesiano 2 0, Cartesiano 2 2, Cartesiano 0 2]

comprimento :: Poligonal -> Double
comprimento (p1:p2:t) = dist p1 p2 + comprimento (p2:t)
comprimento _ = 0

triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:t) = Triangulo p1 p2 p3 : triangula (p1:p3:t)
triangula _ = []

-- ex3

data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email Integer
                deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

agenda_ex = [("Edgar" , [Tlm 93772214, Email "edinho17@gmail.com"])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome Email [] = [(nome, [Email email])]
acrescEmail nome email ((nome_c,contactos):t)
    | nome == nome_c = ((nome_c, Email email : contactos):t)
    | otherwise = (nome_c, contactos) : acrescEmail nome email t

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((nome_c, contactos):t)
    | nome == nome_c = Just (filtrarEmails contactos)
    | otherwise = verEmails nome t

--------------- função auxiliar -----------------
filtrarEmails :: [Contacto] -> [String]
filtrarEmails [] = []
filtrarEmails (Email e : t) = e : filtarEmails t
filtrarEmails (_:t) = filtrarEmails t
-------------------------------------------------

--ex4

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

anterior :: Data -> Data -> Bool
anterior (D dia1 mes1 ano1) (D dia2 mes2 ano2) = ano1 < ano2 || (ano1 == ano2 && mes1 < mes2) || (ano1 == ano2 && mes1 == mes2 && dia1 < dia2)

ordena :: TabDN -> TabDN
ordena [] = []
ordena (h:t) = inserirData h (ordena t)

--------------- função auxiliar -----------------
inserirDN :: (Nome,Data) -> TabDN -> TabDN
inserirDN (nome,data') [] = [(nome,data')]
inserirDN (nome,data') ((nomeT,dataT):t)
    | anterior dataT data' = (nomeT,dataT) : inserir DN (nome,data') t
    | otherwise = (nome,data') : (nomeT,dataT) : t
-------------------------------------------------