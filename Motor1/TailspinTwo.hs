module TailspinTwo where

import Engine;
import Draw;

tailspinTwo :: IO ()
tailspinTwo = runEngine settings [] initState render update
    where settings = EngineSettings "MOTOR" (500, 500) True "knobs.txt" darkGray
          darkGray = rgbaColor 0.2 0.2 0.2 1.0

type GameState = [(Double, Double)]
type KnobType = [(String, Double)]

initState :: GameState
initState = zip (repeat 0.0) [0.0, 0.02 .. 0.9]

render :: KnobType -> GameState -> IO ()
render _ state = sequence_ (arm state)

arm :: GameState -> [IO ()]
arm ((v,r):(v2,r2):xs) = line (cos v * r, sin v * r) (cos v2 * r2, sin v2 * r2) : arm ((v2,r2) : xs)
arm _ = return $ return ()

update :: KnobType -> GameState -> Double -> GameState
update _ state dt = map moveDot state
    where moveDot (v,r) = (v + r * dt * 10, r)
