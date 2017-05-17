import Language.ISOZ.SecSolver (secSolve)
import Test.Hspec
import Data.List
import System.Process 
import System.Exit
--import System.IO (hPutStrLn, stderr, stdout, stdin, hGetContents, hPutStr, IOMode(..), openFile, hClose)

isContained :: String -> [String] -> Bool
isContained _ [] = False 
isContained p (x:xs) = if p `isInfixOf` x 
                        then True
                        else isContained p xs

isAllContained :: [String] -> [String] -> Bool
isAllContained [] _ = True
isAllContained _ [] = False 
isAllContained (px:pxs) s = if isContained px s
                        then True
                        else isAllContained pxs s
main :: IO ()
main = hspec $ do
 
  describe "Solve toolkits" $ do
    it "check prelude section is solved" $ do
      r <- (secSolve "specs/ISOZ/toolkits/prelude.tex") 
      case r of
            Left err    -> err `shouldBe` "Error to find prelude.tex" 
            Right lst   -> lst `shouldSatisfy` (isContained "prelude.tex")

    it "check set toolkit is solved" $ do
      r <- (secSolve "specs/ISOZ/toolkits/set_toolkit.tex") 
      case r of
            Left err    -> err `shouldBe` "Error to find prelude.tex" 
            Right lst   -> lst `shouldSatisfy` (isAllContained ["prelude.tex", "set_toolkit.tex"])

    it "check sequence toolkit is solved" $ do
      r <- (secSolve "specs/ISOZ/toolkits/sequence_toolkit.tex") 
      case r of
            Left err    -> err `shouldBe` "Error to find prelude.tex" 
            Right lst   -> lst `shouldSatisfy` (isAllContained ["prelude.tex", "set_toolkit.tex", "relation_toolkit.tex", "number_toolkit.tex", "function_toolkit", "sequence_toolkit.tex"])

  describe "\n======= Parser specifications in LaTeX ======" $ do
    it "parse prelude.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["specs/ISOZ/toolkits/prelude.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse sequence_toolkit.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["specs/ISOZ/toolkits/sequence_toolkit.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/specs/ISOZ/from_czt/basicsm.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/from_czt/basicsm.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/specs/ISOZ/from_czt/chunqing.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/from_czt/chunqing.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldSatisfy` (== ExitSuccess)
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/specs/ISOZ/from_czt/buffer.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/from_czt/buffer.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/specs/ISOZ/from_czt/birthdaybook.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/from_czt/birthdaybook.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/test_specs/parser/theta.tex to verify theta expression with stroke" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/test_specs/parser/theta.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/test_specs/parser/decl.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/test_specs/parser/decl.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/test_specs/parser/cross.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/test_specs/parser/cross.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/test_specs/parser/mapsto.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/test_specs/parser/mapsto.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse res/test_specs/parser/next_stroke.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/test_specs/parser/next_stroke.tex", "-s 0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse Steam boiler Z specification res/specs/ISOZ/cases/SteamBoiler_z.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/cases/SteamBoiler_z.tex", "-s 0x0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse ESEL Specification Z specification res/specs/ISOZ/cases/ESEL/ESELSpec_z.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/cases/ESEL/ESELSpec_z.tex", "-s 0x0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse ESEL System one Z specification res/specs/ISOZ/cases/ESEL/ESELSystem1_z.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/cases/ESEL/ESELSystem1_z.tex", "-s 0x0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse ESEL System two Z specification res/specs/ISOZ/cases/ESEL/ESELSystem2_z.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/cases/ESEL/ESELSystem2_z.tex", "-s 0x0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse Reactive buffer specification res/specs/ISOZ/cases/buffer/BufferSpec_z.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/cases/buffer/BufferSpec_z.tex", "-s 0x0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."

    it "parse Reactive buffer specification res/specs/ISOZ/cases/buffer/DisBufferSpec_z.tex" $ do
        (exitcode, out, err) <- readProcessWithExitCode "dist/build/ParseCircus/ParseCircus" ["res/specs/ISOZ/cases/buffer/DisBufferSpec_z.tex", "-s 0x0"] [] 
        if exitcode /= ExitSuccess 
        then do
            putStrLn ("stdout: " ++ out)
            putStrLn $ "stderr: " ++ err 
            exitcode `shouldBe` ExitSuccess
        else do
            exitcode `shouldBe` ExitSuccess
            out `shouldContain` "Parse and typecheck successfully."
