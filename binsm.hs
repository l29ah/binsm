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

getpart pnam dmap = fromMaybe (error $ "Can't find partition \"" ++ pnam ++ "\"") $ listToMaybe $ filter (\p -> name p == pnam) dmap

binsm :: Opts -> IO ()
binsm opts = do
	mapp <- Prelude.readFile $ mapFile opts
	let dmap = dmesg mapp
	when (and [extractPartition opts /= Nothing, mergePartition opts /= Nothing]) $ error "cannot extract and merge at the same time"
	img <- BL.getContents
	when (extractPartition opts /= Nothing) $ do
		let part = getpart (fromJust $ extractPartition opts) dmap
		maybe BL.putStr BL.writeFile (partitionFile opts) $ BL.drop (start part) $ BL.take (end part) img
	when (mergePartition opts /= Nothing) $ do
		let part = getpart (fromJust $ mergePartition opts) dmap
		partcont <- BL.readFile (fromJust $ partitionFile opts)
		let filelen = BL.length partcont
		let partlen = end part - start part
		when (filelen /= partlen) $ error "partition input file doesn't fit the partition"
		BL.putStr $ BL.concat [BL.take (start part) img, partcont, BL.drop (end part) img]

main = execParser (info (helper <*> optsP) mempty) >>= binsm
