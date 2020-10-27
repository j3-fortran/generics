class Stringer t where
  string :: t -> String

stringify :: Stringer t => [t] -> String
stringify (only:[]) = string only
stringify (first:rest) = (string first) ++ (stringify rest)

data MyT = MyT

instance Stringer MyT where
  string _ = "X"

main :: IO ()
main = do
  let v = [MyT, MyT, MyT]
  putStrLn (stringify v)
