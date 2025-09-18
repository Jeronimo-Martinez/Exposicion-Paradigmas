-- Se define el tipo de dato cliente 
data Cliente = Cliente
  { idCliente :: Int       -- Identificador
  , cuenta    :: String    -- Número de cuenta
  , saldo     :: Float     -- Saldo actual
  } deriving (Show, Eq)

--Se crea una lista inicial de clientes para probar las funciones 
clientesIniciales :: [Cliente]
clientesIniciales = [
    Cliente 1 "Ana" 1000.0,
    Cliente 2 "Luis" 500.0,
    Cliente 3 "Maria" 200.0
    ]