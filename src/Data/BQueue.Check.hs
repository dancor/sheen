import Data.BQueue

main = do
  let q = singleton 4 "hi"
  mapM_ print . take 6 $ iterate (append "oh") q
