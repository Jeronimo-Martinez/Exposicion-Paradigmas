
data Cliente = Cliente
  { idCliente :: Int       -- Identificador
  , cuenta    :: String    -- Número de cuenta
  , saldo     :: Float     -- Saldo actual
  } deriving (Show, Eq)