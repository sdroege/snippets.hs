import Control.Applicative
import Control.Monad.State
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
    values <- map read . lines <$> readFile "hist.data" :: IO [Double]

    let p = snd $ flip runState defaultPlotHist $ do
        plot_hist_values .= values
        plot_hist_bins   .= 12

    toFile def "hist.svg" $ do
        layout_title .= "Histogram (mean=" ++ show (mean values) ++ ")"
        layout_title_style . font_size .= 10
        layout_plots .= [histToPlot p]

