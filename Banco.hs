-- Se define el tipo de dato cliente 
data Cliente = Cliente
  { idCliente :: Int       -- Identificador
  , cuenta    :: String    -- Número de cuenta
  , saldo     :: Float     -- Saldo actual
  } deriving (Show, Eq)

--Se crea una lista inicial de clientes para probar las funciones 
clientesIniciales :: [Cliente]


-- Se crea la función retirar
retirar :: []