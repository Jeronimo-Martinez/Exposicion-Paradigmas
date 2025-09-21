main :: IO()
main = menu

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


-- Esta es la función depositar
-- La función rebice una cuenta (String), un monto a depositar(Float)y una lista de clientes (Cliente) y devuelve una lista de clientes actualizada

depositar :: String -> Float -> [Cliente] -> [Cliente]

-- Nombre de la función y parámetros  
depositar numCuenta monto clientes = 

  -- La función map, aplica la función actaulizarSaldo a cada cliente de la lista de clientes
  map actualizarSaldo clientes

  -- Se utiliza where para definir la función local actualizarSaldo que solo existe en la función depositar
  where

    -- la función auxiliar recibe como parámetro un cliente y devuelve ese mismo cliente o con el saldo aumentado, según el cado
    actualizarSaldo cliente
    
      -- Si la cuenta coincide, se crea un nuevo cliente con los datos del encontrado pero con el saldo aumentado
      | cuenta cliente == numCuenta = cliente { saldo = saldo cliente + monto }

      -- Si no la encuentra, se devuelve el cliente sin cambios 
      | otherwise                   = cliente


-- Se declara el tipo de datos que recibe y devuelve la función retirar
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

transferencia :: String -> String -> Float -> [Cliente] -> [Cliente] 

transferencia numCuentaEnvia numCuentaRec monto clientes 

 | monto <= 0 = clientes  -- Si el monto es negativo o cero, se retorna la lista sin cambios 

 | not(saldoSuficiente numCuentaEnvia monto clientes) = clientes --Si el saldo del remitente no es suficiente , se retorna la lista sin cambios
   
   -- En cualquier otro caso (el monto no es cero y hay suficiente saldo ... 
   
 | otherwise  = depositar numCuentaRec monto (retirar numCuentaEnvia monto clientes) -- se llama a las funciones depositar y retirar para debitar y consignar el monto que tambien crean la nueva lista modificada y la retornan 

  where -- definicion de saldoSuficiente
    saldoSuficiente buscarRem monto clientes = 
      -- case evalua el resultado del filtrado 
      case filter (\remitente -> cuenta remitente == buscarRem) clientes of  -- la funcion filter filtra la lista segun el numero de cuenta y monto 
        (remitente:_)  -> saldo remitente >= monto -- si filter retorna un cliente retorna True (comparacion =>)
        [] -> False -- si el filtrado retorna una lista vacia , se retorna False


-- Definimos la función consultarSaldo
consultarSaldo :: String -> [Cliente] -> String
consultarSaldo cuentaBuscar listaClientes
  -- Caso 1: si la lista de clientes está vacía
  | null listaClientes = "No hay clientes registrados"
  -- Continuación de la función consultarSaldo
  -- Caso 2: si la lista no está vacía, se busca la cuenta
  | otherwise =
      case filter (\cliente -> cuenta cliente == cuentaBuscar) listaClientes of -- la funcion filter busca en la lista de clientes los que tengan el mismo número de cuenta que cuentaBuscar
        (clienteEncontrado : _) -> -- Si filter devuelve al menos un cliente mostramos un mensaje con su saldo
          "El saldo de la cuenta " ++ cuentaBuscar ++ " es: " ++ show (saldo clienteEncontrado)
        [] -> -- Si filter devuelve una lista vacía significa que no existe la cuenta buscada
          "La cuenta " ++ cuentaBuscar ++ " no existe."

menu :: IO()
menu = do 
  putStrLn""
  putStrLn"=== Menu Banco ==="
  putStrLn""
  putStrLn"1. Depositar"
  putStrLn"2. Retirar"
  putStrLn"3. Hacer una tranferencia"
  putStrLn"4. Consultar Saldo"
  putStrLn"5. Salir"
  putStrLn""
  putStrLn"Elija una opción: "
  opc <- getLine
  
  case opc of 
    "1"  -> do 
      putStrLn""
      putStrLn"Ingrese el numero de cuenta: "
      numCuenta <- getLine 
      putStrLn""
      putStrLn"Ingrese el monto a depositar: "
      input <- getLine
      putStrLn""
      let monto = read input :: Float
      let nuevosClientes = depositar numCuenta monto clientesIniciales

      if nuevosClientes == clientesIniciales
        then do
          putStrLn "Monto invalido... Presione Enter para continuar"
          _ <- getLine
          return()
        else do
          putStrLn "Lista de clientes actualizada:"
          print nuevosClientes
          _ <- getLine
          return()
      menu 
        
    "2"  -> do 
      putStrLn""
      putStrLn"Ingrese el numero de cuenta: "
      numCuenta <- getLine 
      putStrLn"Ingrese el monto a retirar: "
      putStrLn""
      input <- getLine
      let monto = read input :: Float
      let nuevosClientes = retirar numCuenta monto clientesIniciales

      if nuevosClientes == clientesIniciales
        then do 
          putStrLn "Saldo insuficiente o monto invalido... Presione Enter para continuar"
          _ <- getLine
          return()
        else do
          putStrLn "Retiro exitoso... Presione Enter para continuar"
          _ <- getLine
          return()
      putStrLn "Lista de clientes actualizada:"
      print nuevosClientes
      menu
    
    "3" -> do 
      putStrLn""
      putStrLn"Ingrese el numero de cuenta del remitente:"
      numCuentaEnvia <- getLine
      putStrLn""
      putStrLn"Ingrese el monto:"
      input <- getLine
      putStrLn""
      let monto = read input :: Float
      putStrLn"Ingrese el numero de cuenta del destinaraio: "
      numCuentaRec <- getLine
      putStrLn""
      let nuevosClientes = transferencia numCuentaEnvia numCuentaRec monto clientesIniciales
      if nuevosClientes == clientesIniciales
        then do
          putStrLn "Saldo insuficiente o monto invalido... Presione Enter para continuar"
          _ <- getLine
          return()
        else do
          putStrLn "Transferencia exitosa... Presione Enter para continuar"
          _ <- getLine
          return()
      putStrLn "Lista de clientes actualizada:"
      print nuevosClientes
      menu

    "4" -> do
      putStrLn"" 
      putStrLn"Ingrese el numero de cuenta: "
      numCuenta <- getLine 
      putStrLn""
      let saldo = consultarSaldo numCuenta clientesIniciales
      print saldo
      putStrLn"Presione Enter para continuar"
      _ <- getLine
      menu

    "5" -> do 
      putStrLn "Programa finalizado. Presiona Enter para salir..."
      _ <- getLine   -- Espera hasta que el usuario presione Enter
      return ()
    
    _ -> do
      putStrLn""
      putStrLn"Opcion no valida. Intente de nuevo"
      putStrLn"Presione Enter para continuar"
      _ <- getLine
      menu    
