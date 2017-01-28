import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (string)

import Maps

data Opts = Opts
	{ mapFile :: String
	, extractPartition :: Maybe String
	, mergePartition :: Maybe String
	, partitionFile :: Maybe String
--	, imageFile :: Maybe String
	}

optsP :: Parser Opts
optsP = Opts
	<$> strOption
		( long "map"
		<> short 'm'
		<> metavar "FILE"
		<> helpDoc (Just $ string $ unlines [
			"Map of the binary. The default format is the linux dmesg:",
			"0x000000000000-0x000000040000 : \"u-boot\"",
			"0x000000040000-0x000000050000 : \"u-boot-env\"",
			"0x000000050000-0x000000180000 : \"kernel1\"",
			"0x000000180000-0x000000f50000 : \"rootfs\"",
			"0x000000000000-0x000001000000 : \"flash\""])
		)
	<*> optional (option str
		( long "extract"
		<> short 'e'
		<> metavar "PARTITION"
		<> help "Partition to extract"
		))
	<*> optional (option str
		( long "merge"
		<> short 'm'
		<> metavar "PARTITION"
		<> help "Partition to merge"
		))
	<*> optional (option str
		( long "partfile"
		<> short 'f'
		<> metavar "FILE"
		<> help "File to read the partition from/write partition to"
		))
{-
	<*> optional (option str
		( long "image"
		<> short 'i'
		<> metavar "FILE"
		<> help "File to read the image from"
		))
-}

binsm :: Opts -> IO ()
binsm opts = do
	mapp <- Prelude.readFile $ mapFile opts
	let dmap = dmesg mapp
	print dmap
	when (extractPartition opts /= Nothing) $ do
		let part = head $ filter (\p -> name p == (fromJust $ extractPartition opts)) dmap
		img <- BL.getContents
		BL.putStr $ BL.drop (start part) $ BL.take (end part) img
		--BL.concat[take (start part) img, 

main = execParser (info (helper <*> optsP) mempty) >>= binsm
