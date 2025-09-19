-- Se define el tipo de dato cliente 
data Cliente = Cliente
  { nombre :: String       -- Nombre
  , cuenta    :: String    -- Número de cuenta
  , saldo     :: Float     -- Saldo actual
  } deriving (Show, Eq)


clientesIniciales =
    [ Cliente "Juan" "001" 1000.0
    , Cliente "Ivana" "002"  500.0
    , Cliente "Carlos" "003"  200.0
    ]

--Esta es la función depositar
depositar :: String -> Float -> [Cliente] -> [Cliente]       
depositar numCuenta monto clientes = -- Recibe una cuenta, el monto y la lista de clientes 
  map (\cliente_a_buscar -> if cuenta cliente_a_buscar== numCuenta --La función superior map, la cual transforma la lista aplicando una función a cada elemento si cumple la condición, sin modificar la lista original porque todo es inmutable. 
              then cliente_a_buscar { saldo = saldo cliente_a_buscar + monto } -- Si se encuentra se hace una nueva versión del cliente pero con el saldo actualizado 
              else cliente_a_buscar) clientes  -- Si no lo encuentra devuelve la lista tal y como estaba


-- Se crea la función retirar
retirar :: String -> Float -> [Cliente] -> [Cliente]

-- se ponen los nombres de los parámetros y se usa la función map
retirar numCuenta montoRetiro clientes = map intentarRetiro clientes  -- Uso de map: Se recorre la lista de clientes aplicando la función auxiliar intentarRetiro a cada cliente
  where
    intentarRetiro cliente
      -- Este es el primer caso: Si el monto a retirar es negativo o cero

      | montoRetiro <= 0 = cliente    -- Si el monto es negativo o cero, retorna el cliente

      -- Este es el segundo caso: Si se encuentra al cliente y tiene saldo suficiente

      | numCuenta == cuenta cliente && saldo cliente >= montoRetiro = cliente {saldo = saldo cliente - montoRetiro}  -- Si se cumple el segundo caso, se copia el cliente pero con el dato saldo cambiado y se retorna

      -- En cualquier otro caso ...

      | otherwise = cliente  -- Se retorna el cliente