-- Se define el tipo de dato cliente 
data Cliente = Cliente
  { idCliente :: Int       -- Identificador
  , cuenta    :: String    -- Número de cuenta
  , saldo     :: Float     -- Saldo actual
  } deriving (Show, Eq)

--Se crea una lista inicial de clientes para probar las funciones 
clientesIniciales :: [Cliente]
clientesIniciales =
    [ Cliente 1 "001" 1000.0
    , Cliente 2 "002"  500.0
    , Cliente 3 "003"  200.0
    ]

--Esta es la función depositar
depositar :: String -> Float -> [Cliente] -> [Cliente]       
depositar numCuenta monto clientes = -- Recibe una cuenta, el monto y la lista de clientes 
  map (\cliente_a_buscar -> if cuenta cliente_a_buscar== numCuenta --La función superior map, la cual transforma la lista aplicando una función a cada elemento si cumple la condición, sin modificar la lista original porque todo es inmutable. 
              then cliente_a_buscar { saldo = saldo cliente_a_buscar + monto } -- Si se encuentra se hace una nueva versión del cliente pero con el saldo actualizado 
              else cliente_a_buscar) clientes  -- Si no lo encuentra devuelve la lista tal y como estaba