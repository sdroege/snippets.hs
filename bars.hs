import Control.Applicative
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

num :: Int
num = 100

probabilities :: Int -> [(Int, Double)]
probabilities n = map (\v -> (v, probability v)) [1..n]
    where
        probability v = (v' / n') * product (map (\i -> 1 - (fromIntegral i) / n') [1..(v-1)])
            where
                v' = fromIntegral v
                n' = fromIntegral n

titles :: [String]
titles = ["Probability"]
values :: [(Int, [Double])]
values = map (\(v, p_v) -> (v, [p_v])) (probabilities num)

mean :: Double
mean = sum $ map (\(v, p_v) -> fromIntegral v * p_v) (probabilities num)

main :: IO ()
main = toFile def "probabilities.svg" $ do
    layout_title .= "Probabilities: " ++ "n=" ++ show num ++ ", mean=" ++ show mean
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map (show . fst) values)
    plot $ plotBars <$> bars titles (addIndexes (map snd values))

