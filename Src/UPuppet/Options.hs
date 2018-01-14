{------------------------------------------------------------------------------
    uPuppet: Command Line Options
------------------------------------------------------------------------------}

module UPuppet.Options
	( Opts , Format(..), Verbosity(..)
	, parseOptions, format, verbosity, outputPath, srcPath, strictVariables
        , nodeName, mainClass, stepLimit
	) where

import UPuppet.Version

import Data.Maybe(fromMaybe)
import Text.Read
import System.IO (hPutStrLn, hPutStr, stderr)
import System.Console.GetOpt(getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo)
import System.FilePath.Posix (takeDirectory, addExtension, dropExtension, takeFileName, (</>))
import System.Exit (exitWith, ExitCode(..), exitSuccess)

{------------------------------------------------------------------------------
    options
------------------------------------------------------------------------------}

-- the output format
data Format = CATALOG_FORMAT | AST_FORMAT | JSON_FORMAT

-- verbosity
data Verbosity = Normal | Verbose deriving(Show,Eq,Ord)

-- the (validated) data from the command line options
data Opts = Opts
	{ srcPath :: String
	, format :: Format
	, outputArg :: String
	, verbosity :: Verbosity
	, nodeName :: String
  	, mainClass :: Maybe String
	, stepLimit :: Maybe Int
	, showVersion :: Bool
    , strictVariables :: Bool
	}

-- the default options
defaults = Opts
	{ srcPath = ""
	, format = CATALOG_FORMAT
	, outputArg = ""
	, verbosity = Normal
	, nodeName = "default"
  	, mainClass = Nothing
	, stepLimit = Nothing
	, showVersion = False
    , strictVariables = False
	}

-- the command line flags
data OptionFlag = Output String | StepLimit String | Format String | StrictVariables |
	 MainClass String | NodeName String | VerboseOpt | VersionOpt deriving(Show,Eq)

options :: [OptDescr OptionFlag]
options =
	[ Option ['f'] ["format"] 	 	(ReqArg Format "catalog|ast|json")
		"output format"
	, Option ['m'] ["main"]       	(OptArg (MainClass . fromMaybe "main") "MAIN")
		"'main' class to include"
	, Option ['n'] ["nodename"]	 	(ReqArg NodeName "NAME")
		"node name"
	, Option ['o'] ["output"]	 	(ReqArg Output "DIR")
		"directory for output"
	, Option ['s'] ["steps"]	 	(ReqArg StepLimit "N")
		"max number of evaluation steps"
	, Option ['v'] ["verbose"]	 	(NoArg VerboseOpt)
		"verbose"
	, Option ['V'] ["version"]	 	(NoArg VersionOpt)
		"display version"
    , Option [] ["strict_variables"](NoArg StrictVariables)
        "strict variables"
	]

parseOptions :: [String] -> IO (Opts, [String])
parseOptions argv = if null argv
	then
		do hPutStrLn stderr uPuppetUsageInfo >> exitSuccess
	else
		case getOpt RequireOrder options argv of
			(opts,fs,[]) -> do
				case (extractOptions opts) of
					(Left e) -> do hPutStrLn stderr e >> exitWith (ExitFailure 1)
					(Right exopts) -> do
						hPutStr stderr $ if (showVersion exopts)
							then "uPuppet: "++versionString++"\n"
							else ""
						return (exopts,fs)
			(_,_,errs) -> do usage >> exitWith (ExitFailure 1)
				where usage = hPutStrLn stderr (concat errs ++uPuppetUsageInfo)
	where uPuppetUsageInfo = usageInfo "\nUsage: uPuppet [options] file .." options

extractOptions :: [OptionFlag] -> Either String Opts
extractOptions [] = Right defaults
extractOptions (f:fs) = do
	opts <- extractOptions fs
	case f of
		VerboseOpt -> return $ opts { verbosity = Verbose }
		(NodeName name) -> return $ opts { nodeName = name }
		(MainClass name) -> return $ opts { mainClass = Just name }
		(Output path) -> return $ opts { outputArg = path }
		(StepLimit n) -> stepLimitOption opts n
		(Format fmt) -> formatOption opts fmt
		VersionOpt -> return $ opts { showVersion = True }
		StrictVariables -> return $ opts { strictVariables = True }

stepLimitOption :: Opts -> String -> Either String Opts
stepLimitOption opts s = 
	case (readEither s) of
		Left _ -> Left $ "invalid step count: " ++ s
		Right n -> if (n<0) then Left $ "invalid step count: \"" ++ s ++ "\""
			else Right $ opts { stepLimit = Just n }

-- parse and validate the format options
formatOption :: Opts -> String -> Either String Opts
formatOption opts fmt = case fmt of
	"catalog" -> Right $ opts { format = CATALOG_FORMAT }
	"ast" -> Right $ opts { format = AST_FORMAT }
	"json" -> Right $ opts { format = JSON_FORMAT }
	otherwise -> Left ( "invalid format: \"" ++ fmt ++ "\"" )

{------------------------------------------------------------------------------
    output file 
------------------------------------------------------------------------------}

-- the default path is the same directory as the source
-- if the output arg is absolute, it is used as the directory for the output
-- if it is relative, it is interpreted relative to the source
-- "-" is interpreted as stdout
-- the extension depends on the format

outputPath :: Opts -> String
outputPath opts = if ((outputArg opts) == "-")
		then "-"
		else ((takeDirectory path) </> (outputArg opts) </>
				(addExtension (dropExtension (takeFileName path)) ext))
		where 
			path = srcPath opts
			ext = case (format opts) of
				CATALOG_FORMAT -> ".catalog"
				AST_FORMAT -> ".ast"
				JSON_FORMAT -> ".json"
