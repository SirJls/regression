{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- |
-- Module      :  Regression.Main
-- Copyright   :  Joris Sparreboom 2019
-- License     :  BSD3
--
-- Maintainer  :  jlssparreboom@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- The 'Regression exercise entry'
--

module Regression.Main
   ( regression
   )
where

import           System.FilePath
import           Control.Applicative
import           Options.Applicative
import           System.IO
import           System.Environment
import           GA
import           Data.List
import           Data.Csv                       ( (.:)
                                                , FromNamedRecord
                                                , parseNamedRecord
                                                , decodeByName
                                                )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Vector                   as V
import           Text.Printf
import qualified Numeric.Matrix                as M
import           Statistics.Distribution
import           Statistics.Distribution.FDistribution
import           Statistics.Distribution.StudentT
import           Numeric                        (showFFloat)
import           Numeric.Decimal

-------------------------------------------------------------------------------
--                      CSV Operations                                       --
-------------------------------------------------------------------------------

data CustomerData = CustomerData {
    male                 :: !Double
  , female               :: !Double
  , home                 :: !Double
  , apt                  :: !Double
  , pregnancyTest        :: !Double
  , birthControl         :: !Double
  , feminineHygiene      :: !Double
  , folicAcid            :: !Double
  , prenatalVitamins     :: !Double
  , prenatalYoga         :: !Double
  , bodyPillow           :: !Double
  , gingerAle            :: !Double
  , seaBands             :: !Double
  , stoppedBuyingCiggies :: !Double
  , cigarettes           :: !Double
  , smokingCessation     :: !Double
  , stoppedBuyingWine    :: !Double
  , wine                 :: !Double
  , maternityClothes     :: !Double
  , pregnant             :: !Double
} deriving (Show)

data Customer = Customer {
     customerData :: !CustomerData
   , predicition  :: !Double
   , squaredError :: !Double
   , intercept    :: !Double
} deriving (Show)

instance FromNamedRecord CustomerData where
   parseNamedRecord r =
      CustomerData
         <$> r
         .:  "Male"
         <*> r
         .:  "Female"
         <*> r
         .:  "Home"
         <*> r
         .:  "Apt"
         <*> r
         .:  "Pregnancy Test"
         <*> r
         .:  "Birth Control"
         <*> r
         .:  "Feminine Hygiene"
         <*> r
         .:  "Folic Acid"
         <*> r
         .:  "Prenatal Vitamins"
         <*> r
         .:  "Prenatal Yoga"
         <*> r
         .:  "Body Pillow"
         <*> r
         .:  "Ginger Ale"
         <*> r
         .:  "Sea Bands"
         <*> r
         .:  "Stopped buying ciggies"
         <*> r
         .:  "Cigarettes"
         <*> r
         .:  "Smoking Cessation"
         <*> r
         .:  "Stopped buying wine"
         <*> r
         .:  "Wine"
         <*> r
         .:  "Maternity Clothes"
         <*> r
         .:  "PREGNANT"

decodeCustomerData :: BL.ByteString -> Either String (V.Vector CustomerData)
decodeCustomerData = fmap snd . decodeByName

parseCsv :: String -> IO (V.Vector CustomerData)
parseCsv dataSet = do
   csvData <- BL.readFile dataSet
   case decodeCustomerData csvData of
      Left  _ -> error "Failed to parse the given data set"
      Right v -> return v

-------------------------------------------------------------------------------
--                      CLI operations                                       --
-------------------------------------------------------------------------------

usage :: IO ()
usage =
   putStr
      . unlines
      $ concat ["Usage: ", "Regression", "[OPTION]"]
      : "Options:"
      : " --version        Print the version number"
      : " --help           Print this message"
      : []

version :: IO ()
version = putStr "0.1.0.0"

-------------------------------------------------------------------------------
--                      Shared                                               --
-------------------------------------------------------------------------------

initialSolution :: GA.Solution
initialSolution = Solution { GA.bestIndividual  = Nothing
                           , GA.population      = []
                           , GA.populationCount = 0
                           }

toMatrix cl = sscpMatrix'
 where
  m          = M.fromList cl
  sscpMatrix = M.transpose m * m
  sscpMatrix' =
     maybe (error "Matrix could not be constructed") id $ M.inv sscpMatrix

-- Necessary in order to avoid rounding error
-- E.G. 0.1 + 0.2 /= 0.3
-- With decimal 0.1 + 0.2 == 0.3
toDecimal !n = realToFrac n :: BasicDecimal

toDouble n = realToFrac n :: Double

fs = [ male . customerData
     , female . customerData
     , home . customerData
     , apt . customerData
     , pregnancyTest . customerData
     , birthControl . customerData
     , feminineHygiene . customerData
     , folicAcid . customerData
     , prenatalVitamins . customerData
     , prenatalYoga . customerData
     , bodyPillow . customerData
     , gingerAle . customerData
     , seaBands . customerData
     , stoppedBuyingCiggies . customerData
     , cigarettes . customerData
     , smokingCessation . customerData
     , stoppedBuyingWine . customerData
     , wine . customerData
     , maternityClothes . customerData
     , intercept
     -- , pregnant . customerData
     ]

points :: Customer -> [Double]
points c = [ f c | f <- fs ] -- <> [intercept c]

predict :: [Customer] -> [Double] -> [Customer]
predict cs betas = [ c { predicition = sumProduct c betas } | c <- cs ]

-------------------------------------------------------------------------------
--                      Linear Model                                         --
-------------------------------------------------------------------------------

config :: GA.Config
config = GA.Config { GA.generations    = 1500
                   , GA.constraint     = Just (Constraint (-0.5) 0.501)
                   , GA.crossoverRate  = 0.4
                   , GA.elitism        = True
                   , GA.mutationRate   = 0.1
                   , GA.populationSize = 30
                   , GA.chromLength    = 20
                   , GA.problemDesc    = Nothing
                   , GA.problemType    = MIN
                   }

objectiveFunction cs betas = cs `sse` betas

sumProduct :: Customer -> [Double] -> Double
sumProduct c betas = sum [ a * b | (a, b) <- zip (points c) betas ]
   -- let zipped = zip (points c) betas
   -- in  foldl' (\acc (i, ii) -> acc + (i * ii)) 0.0 zipped

squareError :: [Customer] -> [Customer]
squareError cs =
   [ c { squaredError = (pregnant (customerData c) - predicition c) ** 2.0 }
   | c <- cs
   ]

sse :: [Customer] -> [Double] -> Double
sse cs betas = sum . map squaredError . squareError . (`predict` betas) $ cs

devsq :: [Double] -> Double
devsq xs =
   let xbar = sum xs / fromIntegral (length xs)
   in  foldl' (\acc x -> (x - xbar) ^ 2 + acc) 0.0 xs

tss :: [Customer] -> Double
tss cs = devsq $ pregnant . customerData <$> cs

ess :: Double -> Double -> Double
ess tss sse = tss - sse

r2 :: Double -> Double -> Double
r2 ess tss = ess / tss

prediciationStandardError :: GA.Config -> Double -> Int -> Double
prediciationStandardError c sse obsCount =
   sqrt $ sse / fromIntegral (degreesOfFreedom c obsCount)

degreesOfFreedom :: GA.Config -> Int -> Int
degreesOfFreedom c obsCount = obsCount - GA.chromLength c

fStatistics :: GA.Config -> Double -> Double -> Int -> Double
fStatistics c sse ess obsCount =
   let rval = fromIntegral (degreesOfFreedom c obsCount)
          / fromIntegral (GA.chromLength c - 1)
   in  (ess / sse) * rval

fTestPValue :: Double -> Double -> Double -> Double
fTestPValue fs mc df = complCumulative (fDistributionReal (mc - 1) df) fs

tTestPValue :: Double -> Double -> Double
tTestPValue tv df = let ot = complCumulative (studentT tv) df in ot * 2

prec :: [Customer] -> BasicDecimal -> BasicDecimal
prec cl v =
   let nom v = fromIntegral $ length $ filter
          (\c -> toDecimal (predicition c) >= v && (toDecimal . pregnant . customerData) c == 1)
          cl
       denom = fromIntegral $ length $ filter (\c -> toDecimal (predicition c) >= v) cl
   in  nom v / denom

specifity :: [Customer] -> BasicDecimal -> BasicDecimal
specifity cl v =
   let nom v = fromIntegral $ length $ filter
          (\c -> toDecimal (predicition c) < v && (toDecimal . pregnant . customerData) c == 0)
          cl
       denom = fromIntegral $ length $ filter
          (\c -> (toDecimal . pregnant . customerData) c == 0)
          cl
   in  nom v / denom

sensitivity :: [Customer] -> BasicDecimal -> BasicDecimal
sensitivity cl v =
   let nom v = fromIntegral $ length $ filter
          (\c -> toDecimal (predicition c) >= v && (toDecimal . pregnant . customerData) c == 1)
          cl
       denom = fromIntegral $ length $ filter
          (\c -> (toDecimal . pregnant . customerData) c == 1)
          cl
   in  nom v / denom


-------------------------------------------------------------------------------
--                      Logistic Model                                       --
-------------------------------------------------------------------------------

logisticConfig :: GA.Config
logisticConfig = GA.Config { GA.generations    = 2500
                           , GA.constraint     = Just (Constraint (-5) (5.01))
                           , GA.crossoverRate  = 0.8
                           , GA.elitism        = True
                           , GA.mutationRate   = 0.2
                           , GA.populationSize = 30
                           , GA.chromLength    = 20
                           , GA.problemDesc    = Nothing
                           , GA.problemType    = MAX
                           }

logisticObjectiveFunction cs betas = cs `logisticSSE` betas

logisticPredict :: [Customer] -> [Double] -> [Customer]
logisticPredict cs betas = [ c { predicition = logisticFunc $ sumProduct c betas } | c <- cs ]

logisticFunc :: Double -> Double
logisticFunc x = exp x / (1 + exp x)

likelihood :: Double -> Double -> Double
likelihood y p = y * log p + (1 - y) * log (1 - p)

logisticSSE :: [Customer] -> [Double] -> Double
logisticSSE cs betas = let cs' = logisticPredict cs betas
                       in sum . map (\c -> likelihood ((pregnant . customerData) c) (predicition c)) $ cs'

points' :: Customer -> [Double]
points' c = init [ f c | f <- fs ]

logisticLinearComb :: [Customer] -> [Double] -> Double -> [Double]
logisticLinearComb cs betas intcpt = [ (foldl' (\acc (i, ii) -> acc + (i * ii)) 0.0 $ zip (points' c) betas) + intcpt | c <- cs ]
-- let zipped = zip (points' c) betas
--                               in  foldl' (\acc (i, ii) -> acc + (i * ii)) 0.0 zipped

logisticPrec :: [Customer] -> [BasicDecimal] -> BasicDecimal -> BasicDecimal
logisticPrec cl ps v =
   let nom v = fromIntegral $ length $ filter
          (\(p, c) -> p >= v && (toDecimal . pregnant . customerData) c == 1) $ zip ps cl
       denom = fromIntegral $ length $ filter ((>=) v) ps
   in  nom v / denom

logisticSpecifity :: [Customer] -> [BasicDecimal] -> BasicDecimal -> BasicDecimal
logisticSpecifity cl ps v =
   let nom v = fromIntegral $ length $ filter
          (\(p, c) -> p < v && (toDecimal . pregnant . customerData) c == 0) $ zip ps cl
       denom = fromIntegral $ length $ filter
          (\c -> (toDecimal . pregnant . customerData) c == 0) cl
   in  nom v / denom

logisticSensitivity :: [Customer] -> [BasicDecimal] -> BasicDecimal -> BasicDecimal
logisticSensitivity cl ps v =
   let nom v = fromIntegral $ length $ filter
          (\(p, c) -> p >= v && (toDecimal . pregnant . customerData) c == 1) $ zip ps cl
       denom = fromIntegral $ length $ filter
          (\c -> (toDecimal . pregnant . customerData) c == 1) cl
   in  nom v / denom

-------------------------------------------------------------------------------
--                      Rounding / Printing / Writing                        --
-------------------------------------------------------------------------------

data Prefix = COMMENT | COMMENT_BAR | EMPTY deriving (Show, Enum)

fprs :: (Floating a, PrintfArg a) => [a] -> String
fprs = foldr (\s acc -> printf "%.2f" s <> " " <> acc) ""

fprsn :: (Floating a, PrintfArg a) => [a] -> Prefix -> String
fprsn l c = case c of
   COMMENT     -> foldr (\s acc -> "# " <> printf "%.2f" s <> "\n" <> acc) "" l
   COMMENT_BAR -> foldr (\s acc -> "# | " <> printf "%.2f" s <> "\n" <> acc) "" l
   _           -> foldr (\s acc -> printf "%.2f" s <> "\n" <> acc) "" l

fprsn' :: [BasicDecimal] -> Prefix -> String
fprsn' l c = case c of
   COMMENT     -> foldr (\s acc -> "# " <> show' s <> "\n" <> acc) "" l
   COMMENT_BAR -> foldr (\s acc -> "# | " <> show' s <> "\n" <> acc) "" l
   _           -> foldr (\s acc -> show' s <> "\n" <> acc) "" l
   where
      show' n = showFFloat (Just 2) n ""

fprsn2 :: (Floating a, PrintfArg a) => [(a, a)] -> String
fprsn2 = foldr
   (\(a, b) acc -> printf "%.2f" a <> ", " <> printf "%.2f" b <> "\n" <> acc)
   ""

fprsn2' :: [(BasicDecimal, BasicDecimal)] -> String
fprsn2' = foldr (\(a, b) acc -> show' a <> ", " <> show' b <> "\n" <> acc) ""
   where
      show' n = showFFloat (Just 2) n ""

fpr :: (Floating a, PrintfArg a) => a -> Prefix -> String
fpr s p = case p of
   COMMENT -> "# " <> printf "%.2f" s
   _       -> "" <> printf "%.2f" s

linearTrainingSet file sse tss ess r2 pse obsCount mc df fs fpv cofs cse ts = do
   handle <- openFile (file <> ".dat") WriteMode
   hPutStr handle (set <> per)
   hClose handle
 where
  set = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | TrainingSet                                                   \n"
  per = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | Linear model:                                                 \n"               
        <> "# | SSE: " <> fpr sse EMPTY <>                                   "\n"
        <> "# | TSS: " <> fpr tss EMPTY <>                                   "\n" 
        <> "# | ESS: " <> fpr ess EMPTY <>                                   "\n"
        <> "# | R^2: " <> fpr r2 EMPTY  <>                                   "\n" 
        <> "# | PSE: " <> fpr pse EMPTY <>                                   "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | Observation Count      : " <> show obsCount <>               "\n"
        <> "# | Model Coefficient Count: " <> fpr mc EMPTY  <>               "\n"
        <> "# | Degrees of Freedom     : " <> fpr df EMPTY  <>               "\n"
        <> "# | F-Statistics           : " <> fpr fs EMPTY  <>               "\n"
        <> "# | F-Test P Value         : " <> show fpv      <>               "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | Model Coefficients:                                           \n"
        <>     fprsn cofs COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Coefficient Std Error:                                        \n"
        <>     fprsn cse COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | T-Statistics:                                                 \n"
        <>     fprsn ts COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | T-Test P Value:                                               \n"
        <>     fprsn ((df `tTestPValue`) <$> ts) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"

linearTestSet file minP maxP cuts precisions specifities sensitivities falsePositiveR = do
   handle <- openFile (file <> ".dat") AppendMode
   hPutStr handle (set <> per <> cont)
   hClose handle
 where
  set = "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | TestSet                                                       \n"
  per = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | Linear Performance:                                           \n"               
        <> "# | Min Prediction: " <> minP' <>                                "\n"
        <> "# | Max Prediction: " <> maxP' <>                                "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | Probability Cutoff for Pregnant Classification: " <>         "\n"
        <>      fprsn' cuts COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Precision: "                                      <>         "\n"
        <>      fprsn' precisions COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Specificity / True Negative Rate: "               <>         "\n"
        <>      fprsn' specifities COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Sensitivity / True Positive Rate: "               <>         "\n"
        <>      fprsn' sensitivities COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Specificity / Fale Positive Rate: "               <>         "\n"
        <>      fprsn' falsePositiveR COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
  minP' = show minP 
  maxP' = show maxP
  cont = fprsn2' (zip falsePositiveR sensitivities)

logisticTestSet file minP maxP cuts precisions specifities sensitivities falsePositiveR sse = do
   handle <- openFile (file <> ".dat") WriteMode
   hPutStr handle (set <> per <> cont)
   hClose handle
 where
  set = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | TestSet                                                       \n"
  per = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | Logistic Performance:                                         \n"               
        <> "# | Min Prediction: " <> minP' <>                                "\n"
        <> "# | Max Prediction: " <> maxP' <>                                "\n"
        <> "# | SSE: "            <> sse'  <>                                "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | Probability Cutoff for Pregnant Classification: " <>         "\n"
        <>      fprsn' cuts COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Precision: "                                      <>         "\n"
        <>      fprsn' precisions COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Specificity / True Negative Rate: "               <>         "\n"
        <>      fprsn' specifities COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Sensitivity / True Positive Rate: "               <>         "\n"
        <>      fprsn' sensitivities COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Specificity / Fale Positive Rate: "               <>         "\n"
        <>      fprsn' falsePositiveR COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
  minP' = show minP 
  maxP' = show maxP
  sse' = fpr sse EMPTY
  cont = fprsn2' (zip falsePositiveR sensitivities)

createPlotScript :: FilePath -> IO ()
createPlotScript filename = writeFile (filename <> ".plot") content
 where
  content =
     ""
        <> "set object 1 rectangle from screen 0,0 to screen 1,1 fillcolor rgb 'white' behind\n"
        <> "set key fixed left top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid\n"
        <> "set style increment default\n"
        <> "set title 'ROC Curve'\n"
        <> "set xlabel 'False Positive Rate'\n"
        <> "set ylabel 'True Positive Rate'\n"
        <> "set xtics 0,.1,1\n"
        <> "set ytics 0,.1,1\n"
        <> "set xrange [0:1]\n"
        <> "set yrange [0:1]\n"
        <> "plot '" <> (filename <> ".dat") <> "' using 1:2 with linespoint linecolor rgb 'red' lw 4"

data Input = ProgramInput
      {
        generate         :: Bool
      , trainingSetPath  :: String
      , testSetPath      :: String
      , outputScriptName :: String
      }
      | Version

generateFlag :: Parser Input
generateFlag =
   ProgramInput
      <$> switch
             (long "generate" <> short 'g' <> help "Generate the GNUplot script")
      <*> strArgument (metavar "TRAININGSET")
      <*> strArgument (metavar "TESTSET")
      <*> strArgument (metavar "FILENAME")

versionFlag = flag'
   Version
   (long "version" <> short 'v' <> help "Version number of the program")

regression :: IO ()
regression = do
   opts <- execParser opts
   case opts of
      Version -> version
      ProgramInput _ tap tep scn -> launch tap tep scn
 where
  opts = info
     (helper <*> versionFlag <|> generateFlag)
     (fullDesc <> progDesc "Generate a (GNUplot) datasets & scripts that produces a ROC curve for a linear and logistic model" <> header
        "Regression - An example of the ROC cruve script generation"
     )
  launch tap tep filename = do
     putStrLn ("==> Parsing " <> tap)
     cds <- parseCsv tap 
     let customerList = (\cd -> Customer cd 0.0 0.0 1.0) <$> V.toList cds
     let cl           = points <$> customerList
     let sscpMatrix   = toMatrix cl
     putStrLn "==> Optimizing a linear regression model, by Minizing the SSE..."
     (bi, _) <- GA.runGA config initialSolution
        $ GA.train (objectiveFunction customerList)
     let obsCount = length cl
     let tss'     = tss customerList
     let sse'     = GA.objective bi
     let ess'     = ess tss' sse'
     let r2'      = r2 ess' tss'
     let pse = prediciationStandardError config sse' obsCount
     let cse = V.imap (\i as -> let f x = pse * sqrt x in f $ (V.!) as i)
            $ V.fromList (V.fromList <$> M.toList sscpMatrix)
     let cofs = GA.chromosome bi
     let cse' = V.toList cse
     let ts   = (\(x, y) -> abs (x / y)) <$> zip cofs cse'
     let mc   = fromIntegral $ GA.chromLength config
     let df   = fromIntegral $ degreesOfFreedom config obsCount
     let fs   = fStatistics config sse' ess' obsCount
     let fpv  = fTestPValue fs mc df
     putStrLn ("Cooficients found: " <> fprs cofs)

     putStrLn ("==> Parsing " <> tep)
     cds' <- parseCsv tep 
     let customerList'  = (\cd -> Customer cd 0.0 0.0 1.0) <$> V.toList cds'
     let customerList'' = predict customerList (GA.chromosome bi)
     let predlist       = toDecimal . predicition <$> customerList''
     let minP           = minimum predlist
     let maxP           = maximum predlist
     let cuts           = [minP, (minP + 0.05) .. maxP]
     let precisions     = prec customerList'' <$> cuts
     let specifities    = specifity customerList'' <$> cuts
     let sensitivities  = sensitivity customerList'' <$> cuts
     let falsePositiveR = ((-) 1.0) <$> specifities
     
     putStrLn $ "==> Creating "  <> (linearRegressionFile <> ".dat")  <> " dataset"
     putStrLn "==> Creating report for the linear regression model"
     linearTrainingSet linearRegressionFile sse' tss' ess' r2' pse obsCount mc df fs fpv cofs cse' ts
     linearTestSet linearRegressionFile minP maxP cuts precisions specifities sensitivities falsePositiveR
     putStrLn "==> Done!"
     putStrLn $ "==> Creating " <> (linearRegressionFile <> ".plot") <> " script"
     createPlotScript linearRegressionFile
     putStrLn "==> Done!"

     putStrLn "==> Optimizing a logistic regression model by Maximizing the joint probability..."
     (bi', _) <- GA.runGA logisticConfig initialSolution
        $ GA.train (logisticObjectiveFunction customerList)

     let cofs'                  = GA.chromosome bi'
     let linearCombination      = (`sumProduct` cofs') <$> customerList
     let predictionLink         = toDecimal . logisticFunc <$> linearCombination 
     let (cofs'', intcpt)       = splitAt (length cofs' - 1) cofs'
     let logRegComb             = logisticLinearComb customerList cofs'' (head intcpt)
     let probs                  = toDecimal . logisticFunc <$> logRegComb
     let logisticMinP           = minimum predictionLink         
     let logisticMaxP           = maximum predictionLink
     let logisticMinP'          = toDecimal . round $ logisticMinP
     let logisticMaxP'          = toDecimal . round $ logisticMaxP
     let logisticCuts           = [logisticMinP', (logisticMinP' + 0.05) .. logisticMaxP']
     let logisticPrecisions     = logisticPrec customerList probs <$> logisticCuts
     let logisticSpecifities    = logisticSpecifity customerList probs <$> logisticCuts
     let logisticSensitivities  = logisticSensitivity customerList probs <$> logisticCuts
     let logisticFalsePositiveR = ((-) 1.0) <$> logisticSpecifities
     putStrLn ("Cooficients found: " <> fprs cofs')

     putStrLn $ "==> Creating "  <> (linearRegressionFile <> ".dat")  <> " dataset"
     putStrLn "==> Creating report for the logistic regression model"
     logisticTestSet logisticRegressionFile logisticMinP logisticMaxP logisticCuts logisticPrecisions logisticSpecifities logisticSensitivities logisticFalsePositiveR (objective bi')
     putStrLn "==> Done!"
     putStrLn $ "==> Creating " <> (logisticRegressionFile <> ".plot") <> " script"
     createPlotScript logisticRegressionFile
     putStrLn "==> Done!"

     putStrLn "INFO: Please make sure GNUplot is installed and is in your $PATH!"
     putStrLn "INFO: To produce the ROC curve(s) run: "
     putStrLn $ "INFO: `gnuplot -persist " <> (linearRegressionFile   <> ".plot`") <> " for the linear model."
     putStrLn $ "INFO: `gnuplot -persist " <> (logisticRegressionFile <> ".plot`") <> " for the logistic regression model."
      where
       linearRegressionFile   = filename <> "_linear_model"
       logisticRegressionFile = filename <> "_logistic_regression_model"
