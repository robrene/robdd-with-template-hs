{-# LANGUAGE ScopedTypeVariables #-}

module Data.Meta
    ( Meta (..)
    , compileMeta
    ) where

import Control.Exception (bracket)
import Data.Typeable as Typ
import System.Directory
import System.FilePath
import System.IO
import System.Plugins

newtype Meta a = Meta { unMeta :: String }

compileMeta :: forall a. Typeable a => [String] -> Meta a -> IO (Either String a)
compileMeta imports meta = do
  let contents = \pth -> unlines $
          [ "module " ++ (takeBaseName pth) ++ " where"
          , ""
          ] ++ (map ((++) "import ") imports) ++
          [ ""
          , "fn :: " ++ (show $ Typ.typeOf (undefined :: a))
          , "fn = " ++ unMeta meta
          ]
      mkTmpFile = do
        (pth, hnd) <- openTempFile "." "RuntimeModule.hs"
        hPutStr hnd (contents pth)
        --putStrLn (contents pth)
        hFlush hnd
        hClose hnd
        return pth
  bracket mkTmpFile removeFile $ \pth -> do
    makeRes <- make pth ["-O2", "-ddump-simpl"]
    let o_pth = case makeRes of
                  MakeSuccess _ fp -> fp
                  MakeFailure errs -> error $ show errs
    --print $ "loading from: " ++ o_pth
    loadRes <- load o_pth [] [] "fn"
    --print "loaded"
    case loadRes of
      LoadFailure err         -> return $ Left (show err)
      LoadSuccess _ (fn :: a) -> return $ Right fn
