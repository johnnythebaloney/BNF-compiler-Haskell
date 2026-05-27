module Assignment (bnfParser, hklCodeGen, validatator, ADT(..), Rule(..), getTime, getParserNames) where

import Instances (Parser (..), ParseResult (..), ParseError (..))
import Parser
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Data.Char (isAlpha, isDigit, toUpper)
import Data.List (intercalate, nub, nubBy)
import Control.Applicative (many, some, (<|>))


-- ============================================================================
-- DATA TYPES
-- ============================================================================

-- The main ADT for the grammar
-- Grammar is a list of rules
data ADT = Grammar [Rule] deriving (Show)

-- A grammar rule: name, parameters, and production
data Rule = Rule String [String] Production deriving (Show, Eq)

-- A production is a list of alternatives
data Production = Alts [BNFAlternative] deriving (Show, Eq)

-- An alternative is a sequence of components
data BNFAlternative = BNFAlternative [Component] deriving (Show, Eq)

-- A component can be a nonterminal, terminal, or macro
data ComponentBase =
    -- Nonterminal with name and arguments
    CNonTerminal String [String]
    -- Terminal string
    | CTerminal String
    -- Macro string
    | CMacro String
    deriving (Show, Eq)

-- Modifiers for components (e.g. optional, zero or more)
data Modifier =
    -- No modifier
    NoMod
    -- Optional
    | OptionalM
    -- Zero or more
    | ZeroOrMoreM
    -- One or more
    | OneOrMoreM
    deriving (Show, Eq)

-- A component with prefix, base, and modifier
data Component = Component
    { -- Is this a token prefix?
      preTok :: Bool
    , -- The base component
      bse :: ComponentBase
    , -- The modifier
      modif :: Modifier
    } deriving (Show, Eq)

-- ============================================================================
-- PART A: BNF PARSING
-- ============================================================================

-- The main parser for the whole BNF grammar
-- Parse spaces, then many rules, then end of file
bnfParser :: Parser ADT
bnfParser = Grammar <$> (spaces *> many (rParse <* spaces) <* eof)

-- Build a Rule from a name/params tuple and a production
rBuilder :: (String, [String]) -> Production -> Rule
rBuilder (name, params) prod = Rule name params prod

-- Parse a rule (LHS and RHS)
rParse :: Parser Rule
rParse = rBuilder <$> lhsRuleParse <*> rhsRuleParse

-- Parse the left-hand side of a rule
lhsRuleParse :: Parser (String, [String])
lhsRuleParse = lhsNontermDef <* assignParser

-- Parse the right-hand side of a rule
rhsRuleParse :: Parser Production
rhsRuleParse = productionParser

-- Parse a nonterminal definition in angle brackets
lhsNontermDef :: Parser (String, [String])
lhsNontermDef = angleBrackParse nameWithParamsParse

-- Parse a name and its parameters
nameWithParamsParse :: Parser (String, [String])
nameWithParamsParse = (,) <$> identParse <*> argsParse

-- Parse a list of arguments in parentheses, or nothing
argsParse :: Parser [String]
argsParse = parentParse (commSep identParse) <|> pure []

-- Predicate for identifier start (alpha)
identStrt :: Char -> Bool
identStrt = isAlpha

-- Predicate for identifier character (alpha, digit, or '_')
identChar :: Char -> Bool
identChar c = charCond c

-- Parse an identifier (starts with alpha, then many valid chars)
identParse :: Parser String
identParse = (:) <$> satisfiedStart <*> many satisfiedChar

--Parse the assignment op "::=" with the optional surrounding space 
assignParser :: Parser String
assignParser = inSpacer0 *> stringerCond <* inSpacer0

--Parse the first char of the identifier (it has to be alpha)
satisfiedStart :: Parser Char
satisfiedStart = satisfy identStrt

--Parse the rest of the ident (x) rest aka the rest part (you got the idea)
satisfiedChar :: Parser Char
satisfiedChar = satisfy identChar

--The conditions for the valid identifier characters
charCond :: Char -> Bool
charCond c = isAlpha c || isDigit c || c == '_'

--To basically just parse the string ::= 
stringerCond :: Parser String
stringerCond = string "::="

--To parse something within the angle bracket aka <...>
angleBrackParse :: Parser a -> Parser a
angleBrackParse p = is '<' *> p <* is '>'

--Parse something inside the parenthesis with the optional spaces
parentParse :: Parser a -> Parser a
parentParse p = is '(' *> inSpacer0 *> p <* inSpacer0 <* is ')'

--Parse 0 or more occurrences of a parser
manSkipper :: Parser a -> Parser [a]
manSkipper p = many p

--Parses 1 or more occurrences of a parser
someSkipper :: Parser a -> Parser [a]
someSkipper p = some p

--The Conditions for whitespace characters (Additional Function :'\t')
conds :: Char -> Bool
conds = (\c -> c == ' ' || c == '\t' || c == '\f' || c == '\v')

--Parse ZERO or more whitespace chars
inSpacer0 :: Parser String
inSpacer0 = manSkipper (satisfy conds)

--Parse 1 or more whitespace chars
inSpacer1 :: Parser String
inSpacer1 = someSkipper (satisfy conds)

-- Parse a comma separator with optional spaces arnd it
comSepParser :: Parser Char
comSepParser = inSpacer0 *> is ',' <* inSpacer0

--Seperate a parser using comma 
commSep :: Parser a -> Parser [a]
commSep p = sepBy1 p comSepParser

--parse one or more item that has been seperated by seperator (Shamelessly stolen from labs and applied (copyrighted by Monash Universityyyy))
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

--Parse 0 or more item that has been seperated by seperator
sepBy :: Parser a -> Parser sep -> Parser [a]    
sepBy p sep = sepBy1 p sep <|> pure []  

--Parse a pipe with optional spaces where | is the seperator
piperParser :: Parser Char
piperParser = inSpacer0 *> is '|' <* inSpacer0

--Parse one or more item that has been seperated by pipe
sepPipe :: Parser c -> Parser [c]
sepPipe p = sepBy1 p piperParser

--Parse a production (yes its been seperated by "|")
productionParser :: Parser Production
productionParser = Alts <$> sepPipe parseAlternative

--Parse a single Alternative (yes its a sequence of components)
parseAlternative :: Parser BNFAlternative
parseAlternative = BNFAlternative <$> compoSeqParser

--Parser of a sequence of components (need at least 1)
compoSeqParser :: Parser [Component]
compoSeqParser = (:) <$> component <*> many nextCompoParser

--Parse the next component in the sequence (after the designated separator)
nextCompoParser :: Parser Component
nextCompoParser = compoSeparator *> component

--Parse the separator between components
compoSeparator :: Parser ()
compoSeparator = () <$ (inSpacer1 <* neinNewLine)

--Parse a single components
component :: Parser Component
component = buildCompo <$> tokPrefixParse <*> baseCompoParse <*> modSuffParse

--Parse whether the component has the "tok" prefix or not
tokPrefixParse :: Parser Bool
tokPrefixParse = tokkerParse <|> pure False

--Parse the base component (NonTermnial, Terminal, Macro)
baseCompoParse :: Parser ComponentBase
baseCompoParse = nTParse <|> tParse <|> mcoParse

--Parse the modifier according to the type of modifier
modSuffParse :: Parser Modifier
modSuffParse = opParse <|> zOMParse <|> oOMParse <|> pure NoMod

--Optional Modifier Parser
opParse :: Parser Modifier
opParse = OptionalM <$ is '?'

--Zero or More Modifier Parser
zOMParse :: Parser Modifier
zOMParse = ZeroOrMoreM <$ is '*'

--One or More Modifier Parser
oOMParse :: Parser Modifier
oOMParse = OneOrMoreM <$ is '+'

--Parse a NonTerminal component
nTParse :: Parser ComponentBase
nTParse = nTermBuildr <$> nonterminalRef

--Parse a Terminal component
tParse :: Parser ComponentBase
tParse = CTerminal <$> quoteOString

--Parse a Macro component   
mcoParse :: Parser ComponentBase
mcoParse = CMacro <$> brackMco

--Build a NonTerminal component from its name and args
nTermBuildr :: (String, [String]) -> ComponentBase
nTermBuildr (name, args) = CNonTerminal name args

-- Build a Component from its prefix flag, base, and modifier
buildCompo :: Bool -> ComponentBase -> Modifier -> Component
buildCompo flagOTok baseOComp modOComp = Component 
    { preTok = flagOTok
    , bse = baseOComp
    , modif = modOComp
    }

-- Parse a "tok" prefix
tokkerParse :: Parser Bool
tokkerParse = True <$ (string "tok" <* inSpacer1)

-------------------------------------------------------------------------------------------
-- Parse a "nonterminal" reference with optional arguments
nonterminalRef :: Parser (String, [String])
nonterminalRef = angleBrackParse nameAndArgsParse

-- Parse a name and its arguments
nameAndArgsParse :: Parser (String, [String])
nameAndArgsParse = (,) <$> identParse <*> argListParse

-- Parse a list of arguments in parentheses, or nothing
argListParse :: Parser [String]
argListParse = parentParse (commSep brackMco) <|> pure []

-- Parse a quoted string (terminal)
quoteOString :: Parser String
quoteOString = parseQuoted '"'

-- Parse a bracketed macro component
brackMco :: Parser String
brackMco = bracketedParse '[' ']'

-- PART F EXTENSION: Escape Character Support

-- Parse a quoted string with escape character support
parseQuoted :: Char -> Parser String
parseQuoted quote = is quote *> many charInQuote <* is quote

-- Parse a character in a quoted string
charInQuote :: Parser Char
charInQuote = escChar <|> regChar

-- Parse a regular character (not quote or backslash)
regChar :: Parser Char
regChar = satisfy (\c -> c /= '"' && c /= '\'' && c /= '\\')

-- Parse an escape character sequence
escChar :: Parser Char
escChar = is '\\' *> escCodeToChar

-- Parse the escape code and return the corresponding character
escCodeToChar :: Parser Char
escCodeToChar = 
    newLine <|> tab <|> carriageReturn <|> 
    backslash <|> doubleQuote <|> singleQuote
    where
        newLine = is 'n' *> pure '\n'
        tab = is 't' *> pure '\t' --Extension chars (Just Tab, yes i am quite a lazy person)
        carriageReturn = is 'r' *> pure '\r'
        backslash = is '\\' *> pure '\\'
        doubleQuote = is '"' *> pure '"'
        singleQuote = is '\'' *> pure '\''

-- Parse a bracketed macro component
bracketedParse :: Char -> Char -> Parser String
bracketedParse open close = is open *> some (satisfy (/= close)) <* is close

--Not Newline Parser
neinNewLine :: Parser ()
neinNewLine = Parser $ \input -> case input of
    ('\n':_) -> Error (UnexpectedChar '\n')
    _ -> Result input ()

-- ============================================================================
-- PART B: CODE GENERATION
-- ============================================================================

rsrvEsc :: String -> String
rsrvEsc s | s `elem` hskllKeyWords = s ++ "'"
                 | otherwise = s

hskllKeyWords :: [String]
hskllKeyWords = ["data", "type", "newtype", "class", "instance", "where", 
                   "let", "in", "case", "of", "if", "then", "else", "do",
                   "module", "import", "qualified", "as", "deriving"]

--Haskell code Generator from the ADT
hklCodeGen :: ADT -> String
hklCodeGen (Grammar rules) = fullOutBuildr (validRuleFltr rules)

--Builder of the full output Haskell code from the list of rules
fullOutBuildr :: [Rule] -> String
fullOutBuildr rs = dataTypeSel rs ++ secParse rs ++ "\n"

--Haskell code Generator for the data types
dataTypeSel :: [Rule] -> String
dataTypeSel rs = concatMap (dataTypeGen rs) rs

--Haskell code Generator for the parsers
secParse :: [Rule] -> String
secParse rs = intercalate "\n\n" [genParser rs r | r <- rs]

--Haskell code Generator for a single data type
dataTypeGen :: [Rule] -> Rule -> String
dataTypeGen _ (Rule name params (Alts alts)) = typeDecChooser name params alts

--Haskell code Generator chooser for data type based on alternatives
typeDecChooser :: String -> [String] -> [BNFAlternative] -> String
typeDecChooser name params [alt]
    | singleFielder alt = newTypeGen (capitalizor name) params alt
typeDecChooser name params alts = genDat (capitalizor name) params alts

--Haskell code Generator for data type with multiple alternatives
genDat :: String -> [String] -> [BNFAlternative] -> String
genDat name params alts = case alts of
    [] -> ""
    (firstAlt:restAlts) -> dataDecBuilder name params firstAlt restAlts

--Haskell code Generator for data type declaration
dataDecBuilder :: String -> [String] -> BNFAlternative -> [BNFAlternative] -> String
dataDecBuilder name params first rest = 
    "data " ++ typeHeadGettr name params ++ " = " ++ firstConsGettr name params first ++
    addiConsGettr name params (typeHeadGettr name params) rest ++ "    deriving Show\n\n"

--Haskell code Generator for type header
typeHeadGettr :: String -> [String] -> String
typeHeadGettr = headerTypeBuilder

--Haskell code Generator for first constructor line
firstConsGettr :: String -> [String] -> BNFAlternative -> String
firstConsGettr name params f = consGen name params 1 f ++ "\n"

--Haskell code Generator for additional constructor lines
addiConsGettr :: String -> [String] -> String -> [BNFAlternative] -> String
addiConsGettr name params typeHead rest = concatMap (builtAltLine name params typeHead) (zip [2..] rest)

--Haskell code Generator for additional constructor line
builtAltLine :: String -> [String] -> String -> (Int, BNFAlternative) -> String
builtAltLine name params typeHead (i, alt) = 
    replicate indentAmount ' ' ++ "| " ++ altLine1 name params i alt
  where
    indentAmount = length ("data " ++ typeHead) + 1

--Haskell code Generator for alternative line
altLine1 :: String -> [String] -> Int -> BNFAlternative -> String
altLine1 name params i alt = consGen name params i alt ++ "\n"

---Haskell code Generator for type header builder
headerTypeBuilder :: String -> [String] -> String
headerTypeBuilder name params 
    | nullParams params = name 
    | otherwise = finType name params

--Haskell code Generator for component type builder (NULL params)
nullParams :: [String] -> Bool
nullParams ps = null ps

--Haskell code Generator for finishing type with parameters
finType :: String -> [String] -> String
finType n ps = n ++ " " ++ unwords ps

--Haskell code Generator for  type header
buildTypeHeader :: String -> [String] -> String
buildTypeHeader = headerTypeBuilder

--Check if the alternative has a single field only
singleFielder :: BNFAlternative -> Bool
singleFielder (BNFAlternative [_]) = True
singleFielder _ = False

--Haskell code Generator for newtype declaration
newTypeGen :: String -> [String] -> BNFAlternative -> String
newTypeGen nme par (BNFAlternative [com]) = newTypeDecBuilder nme par com
newTypeGen _ _ _ = ""

--Haskell code Generator for newtype declaration
newTypeDecBuilder :: String -> [String] -> Component -> String
newTypeDecBuilder nme par com =
    "newtype " ++ headType nme par ++ " = " ++ nme ++ " " ++ fieldWrappedType par com ++ "\n" ++
    "    deriving Show\n\n"

--Haskell code Generator for type header
headType :: String -> [String] -> String
headType nme par = buildTypeHeader nme par

--Haskell code Generator for field type getter
fieldTypeGetter :: [String] -> Component -> String
fieldTypeGetter par com = compoTypeWParams par com

--Haskell code Generator for field wrapped type
fieldWrappedType :: [String] -> Component -> String
fieldWrappedType par com = wrapFieldIfNeeded (fieldTypeGetter par com)

--wrapping the field if its needed else no wrapping
wrapFieldIfNeeded :: String -> String
wrapFieldIfNeeded t | alreadyWrapped t = t | wrapperNeeded t = "(" ++ t ++ ")" | otherwise = t

--the stuff is already wrapper so no wrappage again (need to pay)
alreadyWrapped :: String -> Bool
alreadyWrapped t = take 1 t == "(" && last t == ')'

--check if the wrapper is needed
wrapperNeeded :: String -> Bool
wrapperNeeded t = any (== ' ') t

--Haskell code Generator for constructor generation
consGen :: String -> [String] -> Int -> BNFAlternative -> String
consGen nme par consNo (BNFAlternative comps) = nameOCons nme consNo ++ fieldsOCons par comps

--Haskell code Generator for constructor name
nameOCons :: String -> Int -> String
nameOCons nme consNo = capitalizor nme ++ show consNo

--Haskell code Generator for capitalizing the first letter of a string
fieldsOCons :: [String] -> [Component] -> String
fieldsOCons par comps = concatMap (fieldBuilder par) comps

--Haskell code Generator for field builder
fieldBuilder :: [String] -> Component -> String
fieldBuilder par com = " " ++ compoTypeWParams par com

--Haskell code Generator for component type with parameters
compoTypeWParams :: [String] -> Component -> String
compoTypeWParams params (Component _ baseComp modifierComp) = modAppToType modifierComp (baseToType params baseComp)

--Haskell code Generator for base component to type conversion
baseToType :: [String] -> ComponentBase -> String
baseToType params (CNonTerminal n args) = ntTypeBuilder params n args
baseToType _ (CTerminal _) = "String"
baseToType params (CMacro m) = mcCvtr params m

--Haskell code Generator for nonterminal type builder
ntTypeBuilder :: [String] -> String -> [String] -> String
ntTypeBuilder _ n [] = capitalizor n
ntTypeBuilder params n args = "(" ++ capitalizor n ++ " " ++ unwords [mcCvtr params a | a <- args] ++ ")"

--Modifier application to type conversion
modAppToType :: Modifier -> String -> String
modAppToType NoMod t = t
modAppToType OptionalM t = "(Maybe " ++ t ++ ")"
modAppToType ZeroOrMoreM t = "[" ++ t ++ "]"
modAppToType OneOrMoreM t = "[" ++ t ++ "]"

--Macro converter from macro to Haskell type
mcCvtr :: [String] -> String -> String
mcCvtr params mco = case mco of
    "int" -> "Int"
    "alpha" -> "String"
    "newline" -> "Char"
    _ -> if mco `elem` params then mco else mco

--Parser generator for a single rule
genParser :: [Rule] -> Rule -> String
genParser allRules (Rule name params (Alts alts)) = 
    case alts of
        [] -> ""
        (firstAlt:restAlts) -> comParseBuildr allRules name params firstAlt restAlts

--Parser generator for complete parser builder
comParseBuildr :: [Rule] -> String -> [String] -> BNFAlternative -> [BNFAlternative] -> String
comParseBuildr allRules nme par first rest = 
    generateParserSignature nme par (typeNme nme) ++ defStrtBuildr nme par ++ compleParserDefBuildr allRules nme par first rest

--Type name generator (capitalized)
typeNme :: String -> String
typeNme nme = capitalizor nme

--Builder for complete parser definition
compleParserDefBuildr :: [Rule] -> String -> [String] -> BNFAlternative -> [BNFAlternative] -> String
compleParserDefBuildr allRules nme par first rest = 
    altBuildr allRules nme par 1 first ++ 
    concatMap (altLineBuildr allRules nme par) (zip [2..] rest)

--Alternative builder for parser
altBuildr :: [Rule] -> String -> [String] -> Int -> BNFAlternative -> String
altBuildr allRules nme _ constructorNum alt = genAltParser allRules (typeNme nme) constructorNum alt

--Alternative line builder
altLineBuildr :: [Rule] -> String -> [String] -> (Int, BNFAlternative) -> String
altLineBuildr allRules nme par (i, alt) = 
    "\n" ++ replicate (indentWidthCalc nme par) ' ' ++ "<|> " ++ altBuildr allRules nme par i alt

--Calculate the indentation width for parser alternatives
indentWidthCalc :: String -> [String] -> Int
indentWidthCalc nme par = length (parserDefNameBuildr nme par) + 1

--Generate the parser signature
generateParserSignature :: String -> [String] -> String -> String
generateParserSignature nme par nmeOType 
    | null par = rsrvEsc nme ++ " :: Parser " ++ nmeOType ++ "\n"
    | otherwise = rsrvEsc nme ++ " :: " ++ chainParmBuildr par nmeOType

--Chain parameter builder for parser signature
chainParmBuildr :: [String] -> String -> String
chainParmBuildr par nmeOType =  concatMap (\p -> "Parser " ++ p ++ " -> ") par ++ "Parser (" ++ nmeOType ++ " " ++ unwords par ++ ")\n"

--Definition start builder for parser
defStrtBuildr :: String -> [String] -> String
defStrtBuildr nme par = parserDefNameBuildr nme par ++ " = "

--Parser definition name builder
parserDefNameBuildr :: String -> [String] -> String
parserDefNameBuildr nme par
    | null par = rsrvEsc nme
    | otherwise = rsrvEsc nme ++ " " ++ unwords par

--Generate alternative parser
genAltParser :: [Rule] -> String -> Int -> BNFAlternative -> String
genAltParser allRules name constructorNum (BNFAlternative comps) =
    case comps of
        [] -> ""
        [comp] -> singleCompoParserBuildr allRules name constructorNum comp
        (firstComp:restComps) -> multiCompoParserBuildr allRules name constructorNum firstComp restComps

--Parser for single component alternative
singleCompoParserBuildr :: [Rule] -> String -> Int -> Component -> String
singleCompoParserBuildr allRules nme consNo com = 
    consNmeBuildr allRules nme consNo ++ " <$> " ++ componentParse com

--Parser for multiple component alternative
multiCompoParserBuildr :: [Rule] -> String -> Int -> Component -> [Component] -> String
multiCompoParserBuildr allRules nme consNo first rest = 
    consNmeBuildr allRules nme consNo ++ " <$> " ++ componentParse first ++ 
    concatMap (\c -> " <*> " ++ componentParse c) rest

--Constructor name builder for parser
consNmeBuildr :: [Rule] -> String -> Int -> String
consNmeBuildr allRules nme consNo = nme ++ consSuff allRules nme consNo

--Constructor suffix generator for parser
consSuff :: [Rule] -> String -> Int -> String
consSuff allRules nme consNo = if newtypeRuleChckr allRules nme then "" else show consNo

--Check if a rule is a newtype rule
newtypeRuleChckr :: [Rule] -> String -> Bool
newtypeRuleChckr rules name = any newTypeRuleMtchChckr (matchingRules rules name)

--Get all rules matching a given name
matchingRules :: [Rule] -> String -> [Rule]
matchingRules rules name = [r | r@(Rule rname _ _) <- rules, capitalizor rname == name]

--Check if a rule matches the newtype rule criteria
newTypeRuleMtchChckr :: Rule -> Bool
newTypeRuleMtchChckr (Rule _ _ (Alts [alt])) = singleFielder alt
newTypeRuleMtchChckr _ = False

--Generate parser for a single component
componentParse:: Component -> String
componentParse (Component tokFlag baseComp modifierComp) = 
    wrapWithMods modifierComp (rndrBseParser tokFlag baseComp)

--Wrap the parser with modifiers
wrapWithMods :: Modifier -> String -> String
wrapWithMods NoMod x = x
wrapWithMods OptionalM x = "(optional " ++ x ++ ")"
wrapWithMods ZeroOrMoreM x = "(many " ++ x ++ ")"
wrapWithMods OneOrMoreM x = "(some " ++ x ++ ")"

-- Render the base component parser
rndrBseParser :: Bool -> ComponentBase -> String
rndrBseParser tokFlag (CTerminal str) = rndrTermParser tokFlag str
rndrBseParser tokFlag (CNonTerminal n args) = rndrNonTermParser tokFlag n args
rndrBseParser tokFlag (CMacro s) = rndrMcoParser tokFlag s

escForHskl :: String -> String
escForHskl = concatMap escapeChar
  where
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar '\'' = "\\'"
    escapeChar c    = [c]

--- Render terminal parser
rndrTermParser :: Bool -> String -> String
rndrTermParser tokFlag str
    | tokFlag = "(stringTok \"" ++ escForHskl str ++ "\")"
    | otherwise = "(string \"" ++ escForHskl str ++ "\")"

-- Render nonterminal parser
rndrNonTermParser :: Bool -> String -> [String] -> String
rndrNonTermParser tokFlag n args
    | null args = rndrSimpleNonterminal tokFlag n
    | otherwise = rndrParameterizedNonterminal tokFlag n args

--- Render simple nonterminal parser
--- Render simple nonterminal parser
rndrSimpleNonterminal :: Bool -> String -> String
rndrSimpleNonterminal tokFlag n
    | tokFlag = "(tok " ++ rsrvEsc n ++ ")"
    | otherwise = rsrvEsc n

--- Render parameterized nonterminal parser
rndrParameterizedNonterminal :: Bool -> String -> [String] -> String
rndrParameterizedNonterminal tokFlag n args
    | tokFlag = "(tok (" ++ baseParser ++ "))"
    | otherwise = "(" ++ baseParser ++ ")"
  where
    baseParser = rsrvEsc n ++ " " ++ unwords [mcoArgParser a | a <- args]

--- Render macro parser
rndrMcoParser :: Bool -> String -> String
rndrMcoParser tokFlag s
    | tokFlag = "(tok " ++ mcoParser s ++ ")"
    | otherwise = mcoParser s

-- Macro argument parser
mcoArgParser :: String -> String
mcoArgParser s = case s of
    "int" -> "int"
    "alpha" -> "(some alpha)"
    "newline" -> "(is '\\n')"
    _ -> s

-- Macro parser
mcoParser :: String -> String
mcoParser s = case s of
    "int" -> "int"
    "alpha" -> "(some alpha)"
    "newline" -> "(is '\\n')"
    _ -> s

-- Capitalize the first letter of a string (Capitalize the char a -> A)
capitalizor :: String -> String
capitalizor [] = []
capitalizor (z:zs) = toUpper z : zs

-- ============================================================================
-- PART E: VALIDATION
-- ============================================================================
-- Filter valid rules by removing those with issues
validRuleFltr :: [Rule] -> [Rule]
validRuleFltr rules = filterIter (dupliRem rules)

-- Iteratively filter rules with issues until none remain
filterIter :: [Rule] -> [Rule]
filterIter ruleset  
    | null toRemove = ruleset 
    | otherwise = filterIter (filter (\(Rule n _ _) -> not (elem n toRemove)) ruleset)
  where
    toRemove = ruleToRemGettr ruleset

-- Get the list of rule names to remove based on issues
ruleToRemGettr :: [Rule] -> [String]
ruleToRemGettr ruleset = nub (leftRec ++ undefinedDetected)
  where
    leftRec = getLeftRecRules ruleset
    undefinedDetected = getUndefRules ruleset

----------------------------------------------------------------------------
-- Get left recursion rule names
getLeftRecRules :: [Rule] -> [String]
getLeftRecRules rules = [drop (length "Left recursion in: ") w | w <- getLeftRecWarnings rules]

-- Get left recursion warnings
getLeftRecWarnings :: [Rule] -> [String]
getLeftRecWarnings rules = lRecurFindr rules

-- Get undefined rules
getUndefRules :: [Rule] -> [String]
getUndefRules rules = [name | r@(Rule name _ _) <- rulesAfterLfRec, any (\nt -> not (elem nt definedNTs)) (usedNTsInRule r)]
  where
    rulesAfterLfRec = filter (\(Rule ruleName _ _) -> not (elem ruleName leftRec)) rules
    leftRec = getLeftRecRules rules
    definedNTs = [name | Rule name _ _ <- rulesAfterLfRec]

-- Main validator function that returns a list of warnings
validatator :: ADT -> [String]
validatator (Grammar rules) = getAllWarnings rules

-- Get all warnings from the rules
getAllWarnings :: [Rule] -> [String]
getAllWarnings rules = dupWarnings ++ undefWarnings ++ leftRecWarnings
  where
    dupWarnings = getDupWarnings rules
    undefWarnings = getUndefWarnings rules
    leftRecWarnings = getLeftRecWarningsForValidation rules

--  Get duplicate warnings
getDupWarnings :: [Rule] -> [String]
getDupWarnings rules = findDups rules

-- Get undefined warnings
getUndefWarnings :: [Rule] -> [String]
getUndefWarnings rules = undefinedFindr (dupliRem rules)

-- Get left recursion warnings for validation
getLeftRecWarningsForValidation :: [Rule] -> [String]
getLeftRecWarningsForValidation rules = lRecurFindr (dupliRem rules)

-- Find duplicate rule names
findDups :: [Rule] -> [String]
findDups rules = ["Duplicate rule: " ++ name | name <- getDupsNames rules]

--  Get duplicate rule names
getDupsNames :: [Rule] -> [String]
getDupsNames rules = [n | (n, cnt) <- getNameCounts rules, cnt > 1]

-- Get counts of rule names
getNameCounts :: [Rule] -> [(String, Int)]
getNameCounts rules = [(n, countOccurrences n names) | n <- nub names]
  where
    names = getRuleNames rules

-- Get rule names
getRuleNames :: [Rule] -> [String]
getRuleNames rules = [name | Rule name _ _ <- rules]

-- Count occurrences of a name in a list
countOccurrences :: String -> [String] -> Int
countOccurrences x xs = length (filter (== x) xs)

-- Remove duplicate rules based on name
dupliRem :: [Rule] -> [Rule]
dupliRem rules = nubBy nameRuleComp rules

-- Compare rules by name for duplication removal
nameRuleComp :: Rule -> Rule -> Bool
nameRuleComp (Rule n1 _ _) (Rule n2 _ _) = n1 == n2

-- Find undefined nonterminals
undefinedFindr :: [Rule] -> [String]
undefinedFindr rules = ["Undefined nonterminal: " ++ name | name <- getUndefinedNTs rules]

-- Get undefined nonterminal names
getUndefinedNTs :: [Rule] -> [String]
getUndefinedNTs rules = nub [nt | nt <- getUsedNTs rules, not (elem nt (getDefinedNTs rules))]

-- Get defined nonterminals
getDefinedNTs :: [Rule] -> [String]
getDefinedNTs rules = getRuleNames rules

-- Get used nonterminals
getUsedNTs :: [Rule] -> [String]
getUsedNTs rules = concatMap usedNTsInRule rules

-- Get used nonterminals in a rule
usedNTsInRule :: Rule -> [String]
usedNTsInRule (Rule _ _ (Alts alts)) = concatMap usedNTsInAlt alts

-- Get used nonterminals in an alternative
usedNTsInAlt :: BNFAlternative -> [String]
usedNTsInAlt (BNFAlternative comps) = concatMap usedNTsInComp comps

-- Get used nonterminals in a component
usedNTsInComp :: Component -> [String]
usedNTsInComp (Component _ (CNonTerminal name _) _) = [name]
usedNTsInComp _ = []

-- Find left recursion
lRecurFindr :: [Rule] -> [String]
lRecurFindr rules = ["Left recursion in: " ++ name | name <- getLeftRecRuleNames rules]

-- Get left recursive rule names
getLeftRecRuleNames :: [Rule] -> [String]
getLeftRecRuleNames rules = [name | (name, isRec) <- getRulesWithRecFlag rules, isRec]

-- Get rules with left recursion flag
getRulesWithRecFlag :: [Rule] -> [(String, Bool)]
getRulesWithRecFlag rules = [(name, selfReachChckr name rules) | name <- getAllRuleNames rules]

-- Get all rule names
getAllRuleNames :: [Rule] -> [String]
getAllRuleNames rules = getRuleNames rules

-- Check if a rule is left recursive
selfReachChckr :: String -> [Rule] -> Bool
selfReachChckr name rules = reachChckr name name [] (buildRuleGraph rules)

-- Build a rule graph mapping rule names to their first nonterminals
buildRuleGraph :: [Rule] -> [(String, [String])]
buildRuleGraph rules = [(name, getFirstNTs alts) | Rule name _ (Alts alts) <- rules]

-- Get first nonterminals from alternatives
getFirstNTs :: [BNFAlternative] -> [String]
getFirstNTs alts = nub (concatMap getFirstNTInAlt alts)

-- Get first nonterminal in an alternative
getFirstNTInAlt :: BNFAlternative -> [String]
getFirstNTInAlt (BNFAlternative []) = []
getFirstNTInAlt (BNFAlternative (Component _ (CNonTerminal nt _) m : _)) = modifierAllowsLeftRec m nt
getFirstNTInAlt (BNFAlternative (_ : _)) = []

-- Determine if a modifier allows left recursion
modifierAllowsLeftRec :: Modifier -> String -> [String]
modifierAllowsLeftRec NoMod nt = [nt]
modifierAllowsLeftRec OneOrMoreM nt = [nt]
modifierAllowsLeftRec _ _ = []

-- Reachability checker for left recursion
reachChckr :: String -> String -> [String] -> [(String, [String])] -> Bool
reachChckr target current visited graph
    | isCircularPath target current visited = True
    | isAlreadyVisited current visited = False
    | otherwise = checkNeighs target current visited graph

-- Check if the current path is circular
isCircularPath :: String -> String -> [String] -> Bool
isCircularPath target current visited = current == target && not (null visited)

-- Check if the current node has already been visited
isAlreadyVisited :: String -> [String] -> Bool
isAlreadyVisited current visited = elem current visited

-- Check neighbors for reachability
checkNeighs :: String -> String -> [String] -> [(String, [String])] -> Bool
checkNeighs target current visited graph = any checkNextReachable (getNeighs current graph)
  where
    newVisited = current : visited
    checkNextReachable next = reachChckr target next newVisited graph

-- Get neighbors of the current node from the graph
getNeighs :: String -> [(String, [String])] -> [String]
getNeighs current graph = case lookup current graph of
    Just neighbors -> neighbors
    Nothing -> []

-- ============================================================================
-- PART G: ADDITIONAL UTILITIES
-- ============================================================================
-- Get current time formatted as a string
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

-- Get parser names from the ADT
getParserNames :: ADT -> [String]
getParserNames (Grammar rules) = [name | Rule name _ _ <- rules]

