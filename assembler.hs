{-#Language FlexibleContexts#-}
import System.IO
import Control.Monad
import Data.Monoid ((<>))
--import Data.Word8
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Char (ord,toUpper)
import Data.Map
import Data.List.Split (chunksOf,splitOn)
import Numeric (showHex,showIntAtBase,readHex)
import Data.Char (intToDigit)

data Prog = Prog SymbolTab LiteralTab OPCodeTab deriving (Show,Eq)

data SymbolTab = SymbolTab [Symbol] deriving (Show,Eq)

instance Monoid SymbolTab where
  mempty = SymbolTab []
  mappend (SymbolTab s1) (SymbolTab s2) = SymbolTab (s1++s2)

data Symbol = Symbol { lnNo :: Int
                     , symbol :: String
                     , symtype :: Type
                     , value :: Maybe [Int]
                     , address :: Maybe String
                     , hexVal :: Maybe String
                     , symsize :: Maybe Int
                     , section :: Section
                     , flagDef :: Bool
                     } deriving (Show,Eq)


data Type = DD | DB | DW | RESD | RESB | LABEL deriving (Show,Eq)

data Section = DATA | BSS | TEXT | EXTERN deriving (Show,Eq)

data LiteralTab = LiteralTab [Literal] deriving (Show,Eq)
  
data Literal = Literal { lineNo :: Int
                       , literal :: Imm
                       , hexValue :: Maybe String
                       } deriving (Show,Eq)

data OPCodeTab = OPCodeTab [Instruction] deriving (Show,Eq)


data Instruction = Ins { lNumber :: Int
                       , addr :: Maybe String
                       , instructSize :: Int
                       , opCode :: Maybe String
                       , instruct :: Maybe Inst
--                       , flag :: Map String Bool
                       } deriving (Show,Eq)

--data FLAG = AF | OF | SF | 

data Inst = MOV InsType | ADD InsType | SUB InsType | MOVSB | SCASB | CMPSB | LODSB | REP Inst | REPE Inst | REPNE Inst | STD | CLD deriving (Show,Eq)

data Macro = Mc { mName :: String
                , mArg :: Int
                , mBody :: String
                } deriving (Show,Eq)

data InsType = RR8 Reg8 Reg8
             | RR16 Reg16 Reg16
             | RR32 Reg32 Reg32
             | RM8 Reg8 Mem8
             | RM16 Reg16 Mem16
             | RM32 Reg32 Mem32
             | MR8 Mem8 Reg8
             | MR16 Mem16 Reg16
             | MR32 Mem32 Reg32
             | ARM8 Reg8 Mem8
             | ARM16 Reg16 Mem16
             | ARM32 Reg32 Mem32
             | AMR8 Mem8 Reg8
             | AMR16 Mem16 Reg16
             | AMR32 Mem32 Reg32
             | RIM8 Reg8 Imm
             | RIM16 Reg16 Imm
             | RIM32 Reg32 Imm
             deriving (Show,Eq)

data Reg = R32 Reg32 | R16 Reg16 | R8 Reg8 deriving (Show,Eq)
data ForArgParse = M Mem | R Reg | I Imm deriving (Show,Eq)

data Reg32 = EAX | EBX | ECX | EDX | ESP | EBP | ESI | EDI deriving (Show,Eq)
data Reg16 = AX | CX | DX | BX | SP | BP | SI | DI deriving (Show,Eq)
data Reg8 = AL | CL | DL | BL | AH | CH | DH | BH deriving (Show,Eq)
data Mem = M8 Mem8 | M16 Mem16 | M32 Mem32   deriving (Eq,Show)
data Mem8 = Byte String  deriving (Eq,Show)
data Mem16 = Word String deriving (Eq,Show)
data Mem32 = Dword String | MIR32 Reg32 deriving (Eq,Show)
data Imm = Imi Int | Imc Char | Ima String deriving (Show,Eq)


strParser :: Parser a -> Parser a
strParser p = between (Token.symbol lexer "\"") (Token.symbol lexer "\"") p

unitSizeOf DD = 4
unitSizeOf DB = 1
unitSizeOf DW = 2
unitSizeOf RESD = 4
unitSizeOf RESB = 1

languageDef =
  emptyDef { Token.commentLine     = ";;"
           , Token.identStart        = letter
           , Token.identLetter       = alphaNum <|> oneOf "_"
           , Token.reservedNames   = ["section" , ".data" , ".text" , ".bss" ,"extern","ebx","ecx","edx","edi","esi","ebp","esp","ax","bx","cx","dx","al","ah","bl","bh","cl","ch","dh","dl","$","$$","dword","byte","word"]
           , Token.reservedOpNames = ["mov","add","mul","sub","dd","db","dw","equ","resd","resb"]
           , Token.caseSensitive   = True
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
ws = Token.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws


----------------------------------Symbol Table--------------------

-------------------------(.data Symbol Table)--------------------

symType = (lexeme (reservedOp "dd") *> (return DD))
           <|> (lexeme (reservedOp "db") *> (return DB))
           <|> (lexeme (reservedOp "dw") *> (return DW))
  
parsedataSymTab :: Parser SymbolTab
parsedataSymTab = parsedata <|> (return (SymbolTab []))

parsedata = do
  lexeme (many digit)
  lexeme $ string "section"
  lexeme $ string ".data" *> option ' ' (char '\n')
  ent <- many (symEntries <|> otherThing)
  let ent1 = Prelude.map (\(Just x)-> x) $ Prelude.filter (/=Nothing) ent
  return $ SymbolTab ent1
  
symEntries :: Parser (Maybe Symbol)
symEntries = do
  lno <- lexeme (many digit)
  iden <- identifier
  identype <- symType
  vconcat <- dataSectionDataParser --many1 ((\x->[read x]::[Int]) <$> (many1 digit <* oneOf [',','\n']) <|> (Prelude.map ord) <$> ((strParser (m-- any (alphaNum <|>)))))))))))))))
  --let vconcat = concat values
  return $ Just (Symbol (read lno::Int) iden identype (pure vconcat) Nothing Nothing (pure $ (length vconcat)*(unitSizeOf identype)) DATA True)

dataSectionDataParser = concat <$> (sepBy1 (ws >> dataSectionArgParser <* ws) (char ','))
  where
    dataSectionArgParser = (\x->[read x]::[Int]) <$> (many1 digit) <|> (Prelude.map ord) <$> ((strParser (many (noneOf "\""))))
-------------------------(.bss Symbol Table)--------------------
  
symType1 = (lexeme (reservedOp "resd") *> (return RESD))
           <|> (lexeme (reservedOp "resb") *> (return RESB))
 
  
parsebssSymTab :: Parser SymbolTab
parsebssSymTab = parsebss <|> (return (SymbolTab []))

parsebss = do
  lno <-   lexeme (many digit)
  lexeme $ string "section"
  lexeme $ string ".bss" *> option ' ' (char '\n')
  ent <- many (try symEntries1 <|> otherThing)
  let ent1 = Prelude.map (\(Just x)-> x) $ Prelude.filter (/=Nothing) ent
  return $ SymbolTab ent1
  
symEntries1 :: Parser (Maybe Symbol)
symEntries1 = do
  lno <-  lexeme (many digit)
  iden <- identifier
  identype <- symType1
  size <- (many1 digit) <* option ' ' newline
  return $ Just (Symbol (read lno::Int) iden identype Nothing Nothing Nothing (pure $ (read size::Int)*(unitSizeOf identype)) BSS True)
  
-------------------------(.text Symbol Table)--------------------

  
parsetextSymTab :: Parser SymbolTab
parsetextSymTab = parsetext <|> (return (SymbolTab []))

parsetext = do
  lno <-  lexeme (many digit)
  lexeme $ string "section"
  lexeme $ string ".text" *> option ' ' (char '\n')
  ent <- many symEntries2
  let ent1 = Prelude.map (\(Just x)-> x) $ Prelude.filter (/=Nothing) ent
  return $ SymbolTab ent1
  
symEntries2 :: Parser (Maybe Symbol)
symEntries2 = try (globExternSym) <|> try (colonSym)  <|> otherThing

parseLabel :: Parser Section
parseLabel = (string "extern") *> return EXTERN
             <|> (string "global") *> return TEXT

globExternSym :: Parser (Maybe Symbol)
globExternSym = do
  lno <-  lexeme (many digit)
  label <- lexeme (parseLabel)
  iden <- lexeme (identifier)
  option ' ' newline
  return $ Just (Symbol (read lno::Int) iden LABEL Nothing Nothing Nothing Nothing label False)

colonSym :: Parser (Maybe Symbol)
colonSym = do
  lno <-  lexeme (many digit)
  l <- lexeme identifier
  char ':'
  option ' ' (char '\n')
  return $ Just (Symbol (read lno::Int) l LABEL Nothing Nothing Nothing Nothing TEXT True)

otherThing :: Parser (Maybe Symbol)
otherThing = do
  kk <- manyTill anyChar (eol) -- many (noneOf "\n")
  option ' ' newline
  return Nothing

eol :: Parser ()
eol = do oneOf "\n\r"
         return ()

--------------------------------------------------------------------------------------
------------------------------------Literal Table--------------------------------------------------

litTabParser :: Parser LiteralTab
litTabParser = litTabParser' <|> (return (LiteralTab []))

litTabParser' :: Parser LiteralTab
litTabParser' = do
  lexeme $ (many digit)
  lexeme $ string "section"
  lexeme $ string ".text" *> option ' ' (char '\n')
  ent <- many (try checkLiteral <|> ((manyTill anyChar (eol)) *> return Nothing))
  let ent1 = Prelude.map (\(Just x)-> x) $ Prelude.filter (/=Nothing) ent
  return $ LiteralTab ent1 
  
checkLiteral :: Parser (Maybe Literal)
checkLiteral = do
  lno <- lexeme (many digit)
  many1 (noneOf ",\n")
  char ','
  val <- try (lexeme intParser) <|> try (lexeme charParser) <|> ((lexeme identifier) >>= (\x-> return $ I $ Ima x))
  option ' ' newline
  let val' = case val of
               (I i) -> i
      valhex = case val' of
                 (Imi i) -> Just $ showHex i ""
                 (Imc c) -> Just $ showHex (ord c) ""
                 _ -> Nothing
  return $ Just (Literal (read lno::Int) val' valhex)
  

---------------------------------------opcode Table ---------------------------------
parseOpCodeTab :: Parser OPCodeTab
parseOpCodeTab = parseOPCode <|> (return (OPCodeTab []))

parseOPCode :: Parser OPCodeTab
parseOPCode = do
  lexeme $ (many digit)
  lexeme $ string "section"
  lexeme $ string ".text" *> option ' ' (char '\n')
  ent <- many (try opCodes <|> noOPcode )
  return $ OPCodeTab ent

noOPcode = do
  lineno <- lexeme (many1 digit)
  kk <- manyTill anyChar (eol) -- many (noneOf "\n")
  option ' ' newline
  return (Ins (read lineno::Int) Nothing 0 Nothing Nothing)
  
opCodes :: Parser Instruction
opCodes = do
  lineno <- lexeme (many1 digit)
  inst <- lexeme instCheck
  return $ (Ins (read lineno::Int) Nothing 0 Nothing (pure inst))

instCheck :: Parser Inst
instCheck = try movParser <|> (lexeme (string "movsb") *> option ' ' newline) *> return MOVSB <|> (lexeme (string "cmpsb") *> option ' ' newline) *> return CMPSB <|> (lexeme (string "scasb") *> option ' ' newline) *> return SCASB <|> (lexeme (string "lodsb") *> option ' ' newline) *> return LODSB <|> try (repParse) <|>  repParse1

repParse :: Parser Inst
repParse = do
  m <-try ((try (lexeme (string "repe") <|> lexeme (string "repz"))*> return REPE) <|> ((try (lexeme (string "repne")) <|> lexeme (string "repnz")) *> return REPNE))
  sub <- (lexeme (string "cmpsb") *> return CMPSB) <|> (lexeme (string "scasb") *> return SCASB) 
  return $ (m sub)

repParse1 :: Parser Inst
repParse1 = do
  m <-(lexeme (string "rep") *> return REP) -- <|> (lexeme (string "repne") *> return REPNE)
  sub <- (lexeme (string "movsb") *> return MOVSB) <|> (lexeme (string "lodsb") *> return LODSB) 
  return $ (m sub)                                                                                                                                                                                                                                            
movParser ::  Parser Inst
movParser = do
  lexeme (string "mov")
  insType <- (instParser)
  return $ MOV insType


instParser :: Parser InsType
instParser = do
  r1 <- (lexeme argParse)
  Token.comma lexer
  r2 <- (lexeme argParse)
  case (r1,r2) of
    (R r,I i) -> case r of
                   (R8 reg) -> return $ RIM8 reg i
                   (R16 reg) -> return $ RIM16 reg i
                   (R32 reg) -> return $ RIM32 reg i
    (R r,M m) -> case (r,m) of
                   (R8 r8,M8 st) -> (return $ RM8 r8 st)
                   (R16 AX,M16 st) -> return $ ARM16 AX st
                   (R16 r16,M16 st) -> return $ RM16 r16 st
                   (R32 EAX,M32 st) -> return $ RM32 EAX st
                   (R32 r32,M32 st) -> return $ RM32 r32 st
                   _               -> fail "Invalid Operation"
    (M m,R r) -> case (m,r) of
                   (M8 m,R8 r8) -> (return $ MR8 m r8)
                   (M16 m,R16 AX) -> return $ AMR16 m AX
                   (M16 m,R16 r16) -> return $ MR16 m r16
                   (M32 m,R32 EAX) -> return $ MR32 m EAX
                   (M32 m,R32 r32) -> return $ MR32 m r32 
                   _               -> fail "Invalid Operation"
    (R r1,R r2) -> case (r1,r2) of
                     (R32 e1,R32 e2) -> return $ RR32 e1 e2
                     (R16 e1,R16 e2) -> return $ RR16 e1 e2
                     (R8  e1,R8  e2) -> return $ RR8 e1 e2
                     _               -> fail "Invalid Operation"
    _-> fail "Invalid Operation"         

argParse :: Parser ForArgParse
argParse = try (regParser) 
           <|> try (memParser)
           <|> try (immParser)
           <|> ((identifier) >>= (\x-> return $ I $ Ima x))
  where
    immParser = (try intParser) <|> (try charParser)
    memParser = try ((\x-> M $ M8 $ Byte x) <$> memParser' "byte") <|> try ((\x-> M $ M16 $ Word x) <$> memParser' "word") <|> try ((\x-> M $ M32 $ Dword x) <$> memParser' "dword") <|> try ((\(R (R32 r))-> M $ M32 $ MIR32 r ) <$> (brackets $ regParser))
    memParser' x = reserved x >> (brackets $ Token.identifier lexer)

charParser = (\x-> I (Imc x)) <$> (apostrophe alphaNum)
intParser = (\x-> I (Imi (read (show x)::Int))) <$> (Token.integer lexer)
    
mapStringParser = Data.Map.mapWithKey (\k a-> reserved k >> return (R a)) regTypeTup
regParser = Data.Map.foldl' (<|>) ((!) mapStringParser "sp") mapStringParser

regTypeTup :: Map String Reg
regTypeTup = fromList [("ah", R8  AH),
                        ("al", R8  AL),
                        ("bh", R8  BH),
                        ("bl", R8  BL),
                        ("ch", R8  CH),
                        ("cl", R8  CL),
                        ("dh", R8  DH),
                        ("dl", R8  DL),
                        ("ax", R16 AX),
                        ("bx", R16 BX),
                        ("cx", R16 CX),
                        ("dx", R16 DX),
                        ("sp", R16 SP),
                        ("bp", R16 BP),
                        ("di", R16 DI),
                        ("si", R16 SI),
                        ("eax",R32 EAX),
                        ("ebx",R32 EBX),
                        ("ecx",R32 ECX),
                        ("edx",R32 EDX),
                        ("esp",R32 ESP),
                        ("ebp",R32 EBP),
                        ("edi",R32 EDI),
                        ("esi",R32 EDI)]

apostrophe p = (char '\'') *> p <* (char '\'')
brackets p = (char '[') *> p <* (char ']')
         
----------------------------------Ark Code ----------------------

getSource :: String -> Map Int String
getSource str = fromList $ zip [1..] (lines str)

getSections :: Map Int String -> SectionTokens
getSections lineSource = ST { sData = toMaybeString $ getSectionWithNo 0,
                              sText = toMaybeString $ getSectionWithNo 1,
                              sBss  = toMaybeString $ getSectionWithNo 2
                            }
  where
    sectionIndexes = keys $ Data.Map.filter (\x-> elem (words x) sections) lineSource
    sectionRanges = Prelude.zipWith (\x y-> (x,y-1)) sectionIndexes (myTail (sectionIndexes ++ [Data.Map.size lineSource + 1]))
    getSectionWithNo z = Prelude.filter (\(x,y)-> ((words $ lineSource Data.Map.! x)) == sections!!z) sectionRanges
    toMaybeString xs = case xs of
                         [] -> Nothing
                         x  -> Just $ unlines $ uncurry (\i j-> elems $ Data.Map.filterWithKey (\k a-> k>=i && k<=j) lineSource) (head x)


myTail :: [a] -> [a]
myTail xs = case xs of
              [] -> []
              x  -> tail xs
data SectionTokens = ST { sText :: Maybe String,
                          sData :: Maybe String,
                          sBss  :: Maybe String
                        } deriving (Eq,Show)

sections :: [[String]]
sections = [["section", ".data"],["section", ".text"],["section", ".bss"]]
----------------------------------------------------

myhexNByte:: Int ->Int -> String
myhexNByte n s= let h = showHex s ""
               in (replicate ((n*2)-(length h)) '0') ++ h

byteFor :: Type -> Int
byteFor DD = 4
byteFor DB = 1
byteFor DW = 2
byteFor RESD = 4
byteFor RESB = 1

calculAdd :: SymbolTab -> SymbolTab
calculAdd (SymbolTab es) = SymbolTab (calculAdd' es 0)

calculAdd' [] _ = []
calculAdd' (e:es) s = (e {address = Just (myhexNByte 4 s),hexVal = (concat <$> (((<$>) (myhexNByte (byteFor $ symtype e))) <$> (value e)))}):(calculAdd' es (s+ (maybe 0 id (symsize e))))





--------------------------------------------------------------------------

movArgOpCode687 :: InsType -> String
movArgOpCode687 inst = case inst of
                         RR8 x y  -> "88"
                         RR16 x y -> "6689"
                         RR32 x y -> "89"
                         RM8 x y  -> if x==AL then "A0" else "8A"
                         RM16 x y -> if x==AX then "66A1" else "668B"
                         RM32 x y -> case y of
                                       MIR32 r -> "89"
                                       _ -> if x==EAX then "A1" else "8B"
                         MR8 x y  -> if y==AL then "A2" else "88"
                         MR16 x y -> if y==AX then "66A3" else "6689"
                         MR32 x y -> case x of
                                       MIR32 r -> "89"
                                       _ -> if y==EAX then "A3" else "89"
                         RIM8 r i   -> addHex "B0" (b2Hex ("0" ++ (getStringFromRegBin (R8 r))))
                         RIM16 r i  -> "66" ++ addHex "B8" (b2Hex ("0" ++ (getStringFromRegBin (R16 r))))
                         RIM32 r i ->  addHex "B8" (b2Hex ("0" ++ (getStringFromRegBin (R32 r))))



movArgOpCode40 :: InsType -> String
movArgOpCode40 arg2 = case arg2 of
                        RR8 x y -> b2Hex $ "11" ++ getStringFromRegBin (R8 y) ++ getStringFromRegBin (R8 x)
                        RR16 x y -> b2Hex $ "11" ++ getStringFromRegBin (R16 y) ++ getStringFromRegBin (R16 x)
                        RR32 x y -> b2Hex $ "11" ++ getStringFromRegBin (R32 y) ++ getStringFromRegBin (R32 x)
                        RM8 x y -> b2Hex $ if x==AL then "" else "00" ++ getStringFromRegBin (R8 x) ++ "101"
                        RM16 r m -> b2Hex $ if r==AX then "" else "00" ++ getStringFromRegBin (R16 r) ++ "101"
                        RM32 x y ->  b2Hex $ case y of
                                               MIR32 r -> "00" ++ getStringFromRegBin (R32 x) ++ getStringFromRegBin (R32 r)
                                               _ -> if x==EAX then "" else "00" ++ getStringFromRegBin (R32 x) ++ "101"
                        MR8 x y -> b2Hex $ if y==AL then "" else "00" ++ getStringFromRegBin (R8 y) ++ "101"
                        MR16 m r -> b2Hex $ if r==AX then "" else "00" ++ getStringFromRegBin (R16 r) ++ "101"
                        MR32 x y -> b2Hex $ case x of
                                              MIR32 r -> "00" ++ getStringFromRegBin (R32 y) ++ getStringFromRegBin (R32 r)
                                              _ ->  if y==EAX then "" else "00" ++ getStringFromRegBin (R32 y) ++ "101"
                        _ -> ""
                        

addHex :: String -> String -> String
addHex h1 h2 = Prelude.map toUpper $ showHex intVal ""
  where
    getInt x = (\[t]-> fst t) x
    intVal = getInt (readHex h1) + getInt (readHex h2)

                     
toHex :: String -> Char
toHex "0000" = '0'
toHex "0001" = '1'
toHex "0010" = '2'
toHex "0011" = '3'
toHex "0100" = '4'
toHex "0101" = '5'
toHex "0110" = '6'
toHex "0111" = '7'
toHex "1000" = '8'
toHex "1001" = '9'
toHex "1010" = 'A'
toHex "1011" = 'B'
toHex "1100" = 'C'
toHex "1101" = 'D'
toHex "1110" = 'E'
toHex "1111" = 'F'

toBin :: Char -> String
toBin '0' = "0000"
toBin '1' = "0001"
toBin '2' = "0010"
toBin '3' = "0011"
toBin '4' = "0100"
toBin '5' = "0101"
toBin '6' = "0110"
toBin '7' = "0111"
toBin '8' = "1000"
toBin '9' = "1001"
toBin 'A' = "1010"
toBin 'B' = "1011"
toBin 'C' = "1100"
toBin 'D' = "1101"
toBin 'E' = "1110"
toBin 'F' = "1111"

h2Bin :: String -> String
h2Bin str = concat $ Prelude.map toBin $ str

b2Hex :: String -> String
b2Hex str = Prelude.map toHex $ chunksOf 4 str

getStringFromRegBin :: Reg -> String
getStringFromRegBin reg = (\[t]-> snd t) $ Prelude.filter (\(x,y)-> x==reg) regBin

regBin :: [(Reg,String)]
regBin = [(R32 EAX,"000")
         ,(R8 AL,"000")
         ,(R16 AX,"000")
         ,(R8 CL,"001")
         ,(R16 CX,"001")
         ,(R32 ECX,"001")
         ,(R8 DL,"010")
         ,(R16 DX,"010")
         ,(R32 EDX,"010")
         ,(R8 BL,"011")
         ,(R16 BX,"011")
         ,(R32 EBX,"011")
         ,(R8 AH,"100")
         ,(R16 SP,"100")
         ,(R32 ESP,"100")
         ,(R8 CH,"101")
         ,(R16 BP,"101")
         ,(R32 EBP,"101")
         ,(R8 DH,"110")
         ,(R16 SI,"110")
         ,(R32 ESI,"110")
         ,(R8 BH,"111")
         ,(R16 DI,"111")
         ,(R32 EDI,"111")
         ]


opCode40 :: Inst -> String
opCode40 ins = case ins of
                 MOVSB -> "A4"
                 SCASB -> "AE"
                 CMPSB -> "A6"
                 LODSB -> "AC"
                 REP s -> "F3"++(opCode40 s)
                 REPE s -> "F3"++(opCode40 s)
                 REPNE s -> "F2" ++ (opCode40 s)
                 MOV arg2 -> (movArgOpCode687 arg2) ++ (movArgOpCode40 arg2)
                 _ -> ""

generateOPcode :: OPCodeTab -> SymbolTab -> OPCodeTab
generateOPcode (OPCodeTab es) ss = OPCodeTab (generateOPcode' es ss)

generateOPcode' es ss = (\x -> if (instruct x) == Nothing then x else (let (Just p) = instruct x
                                                                           k = (opCode40 p) ++ (fun p ss)
                                                                           s = length (Prelude.filter (\x-> x `notElem` "[]") k)
                                                                       in x {opCode = Just k, instructSize = (div s 2)})) <$> es

fun :: Inst -> SymbolTab -> String
fun (MOV inst) ss = case inst of
                 RM8 x (Byte s) -> lookUpSym s ss
                 RM16 x (Word s) -> lookUpSym s ss
                 RM32 x (Dword s) -> lookUpSym s ss
                 MR8 (Byte s) x -> lookUpSym s ss
                 MR16 (Word s) x -> lookUpSym s ss
                 MR32 (Dword s) x -> lookUpSym s ss
                 RIM8 r k -> case k of
                               Imi n -> toUpper <$> myhexNByte 1 n
                               Imc c -> toUpper <$> myhexNByte 1 (ord c)
                               Ima s -> let k = lookUpSym s ss
                                        in "["++drop ((length k)- 3) k
                 RIM16 r k -> case k of
                               Imi n -> toUpper <$> myhexNByte 2 n
                               Imc c -> toUpper <$> myhexNByte 2 (ord c)
                               Ima s -> let k = lookUpSym s ss
                                        in "["++(drop ((length k)- 5) k)
                 RIM32 r k -> case k of
                               Imi n -> toUpper <$> myhexNByte 4 n
                               Imc c -> toUpper <$> myhexNByte 4 (ord c)
                               Ima s -> let k = lookUpSym s ss
                                        in ("["++(drop ((length k)- 9) k))
                 _ -> ""

fun _ _ = ""
--lookUpSym :: String -> [Symbol] -> String
lookUpSym s (SymbolTab ss) = let k = (Prelude.filter (\x-> (symbol x)==s) ss)
                                 p = "["++ (maybe "" id (address $ head k)) ++"]"
                 in if (k /=[]) then p else ""
                                                
-------------------------------------------------------------------------------------

main1 :: String -> IO ()
main1 file = do
  content <- readFile file
  let  filecont = getSections $ getSource content
       macros = extractMacros content
       text1 = replaceMacro macros (maybe "" id (sText filecont))
       textsec = unlines $ zipWith (\x y-> (show x)++" "++y) [1..] (lines text1)
       datasec = unlines $ zipWith (\x y-> (show x)++" "++y) [1..] (lines (maybe "" id (sData filecont)))
       bsssec = unlines $ zipWith (\x y-> (show x)++" "++y) [1..] (lines (maybe "" id (sBss filecont)))
       symdata = calculAdd $ either (\x-> SymbolTab []) id (parse parsedataSymTab "" datasec)
       symbss = calculAdd $ either (\x-> SymbolTab []) id (parse parsebssSymTab "" bsssec)
       symtext = either (\x-> SymbolTab []) id (parse parsetextSymTab "" textsec)
       literaltab = either (\x-> LiteralTab []) id (parse litTabParser "" textsec)
       opcodeT = either (\x-> OPCodeTab []) id (parse parseOpCodeTab "" textsec)
       fullSymTab = symdata <> symbss <> symtext
       fullOpcodeTab = calculAddT (generateOPcode opcodeT fullSymTab)
  -- print $ opcodeT
  -- print $ macros
  -- print $ fullOpcodeTab     
  putStrLn $ printData datasec symdata
  putStrLn $ printBss bsssec symbss
  putStrLn $ printText textsec fullOpcodeTab




takeoutbyLine n (SymbolTab sd) = let p = (Prelude.filter (\x-> (lnNo x)==n) sd)
                                 in case p of
                                      [] -> replicate 40 ' '
                                      e -> let k = head e
                                           in (toUpper <$> (removeMaybe (address k))++" " ++(removeMaybe (hexVal k)))
printData :: String -> SymbolTab -> String
printData dd sd = unlines $ (\(n,y)-> (takeoutbyLine (read n::Int) sd) ++ y) <$> ((break (==' ')) <$> (lines dd))

removeMaybe  = maybe " " id
removeMaybe' = maybe 0 id

takeoutbyLine' n (SymbolTab sd) = let p = (Prelude.filter (\x-> (lnNo x)==n) sd)
                                 in case p of
                                      [] -> replicate 40 ' '
                                      e -> let k= head e
                                           in (toUpper <$> (removeMaybe (address k))++" < res" ++(myhexNByte 4 (removeMaybe' (symsize k)))++">")

printBss dd sd = unlines $ (\(n,y)-> (takeoutbyLine' (read n::Int) sd) ++ y) <$> ((break (==' ')) <$> (lines dd))

takeoutbyLine'' n (OPCodeTab sd) = let p = (Prelude.filter (\x-> (lNumber x)==n) sd)
                                 in case p of
                                      [] -> replicate 40 ' '
                                      e -> let k= head e
                                           in (toUpper <$> (removeMaybe (addr k))++" " ++(removeMaybe (opCode k))++"")
                                              
printText dd sd = unlines $ (\(n,y)-> (takeoutbyLine'' (read n::Int) sd) ++ y) <$> ((break (==' ')) <$> (lines dd))



calculAddT :: OPCodeTab -> OPCodeTab
calculAddT (OPCodeTab es) = OPCodeTab (calculAddT' es 0)

calculAddT' [] _ = []
calculAddT' (e:es) s = (e {addr = Just (myhexNByte 4 s)}):(calculAddT' es (s+ (instructSize e)))



-----------------------------------------------------------------------------

extractMacros :: String -> [Macro]
extractMacros cont = extractMacros' $ takeWhile (\x -> head (words x) /= "section") $ Prelude.filter (\x-> (words x) /= []) (lines cont)

extractMacros':: [String] -> [Macro]
extractMacros' [] = []
extractMacros' cont | (tillEndMacro $ skipTillMacro cont) == [] = (extractMacros' $ myTail $ dropWhile (\x-> head (words x) /= "%endmacro") cont) 
                    | otherwise =  (makeMacro (tillEndMacro $ skipTillMacro cont))
                      :(extractMacros' $ tail $ dropWhile (\x-> head (words x) /= "%endmacro") cont) 
 where
   tillEndMacro s = takeWhile (\x-> head (words x) /= "%endmacro") s
   skipTillMacro s = dropWhile (\x -> head (words x) /= "%macro") s 

makeMacro :: [String] -> Macro
makeMacro mm = Mc { mName = (words (head mm))!!1,
                    mArg = read ((words (head mm))!!2) :: Int,
                    mBody = unlines $ tail mm
                  }

replaceMacro :: [Macro] -> String -> String
replaceMacro [] s = s
replaceMacro mm st = unlines $ (\x -> checkAndChange mm x ) <$> (lines st)

checkAndChange :: [Macro] -> String -> String
checkAndChange mm ln1 | length ln > 1 = case Prelude.filter (\x -> (head ln) == (mName x) && (length args)== mArg x) mm of
                                         [] -> if elem (head ln) instructs1 then ln1 else error "Instruction not defined !! \n"
                                         e -> init $ Prelude.foldl (\z (x,y)-> subst x y z) (mBody $ head e) (zip args perList)
                     | length ln == 1 = case Prelude.filter (\x -> (head ln) == (mName x) && (mArg x)==0) mm of
                                          [] -> if elem (head ln) instructs0 then ln1 else error "Instruction not defined !! \n"
                                          e -> init $ mBody $ head e
                     | otherwise = ln1
  where
    ln = words ln1
    perList = (Prelude.map (\x-> "%"++show x) [1..])
    args = (splitOn "," (ln!!1))
    instructs1 = ["mov","add","sub","rep","repe","repne"]
    instructs0 = ["movsb","scacb","lodsb","cmpsb","std","cld"]

subst :: String -> String -> String -> String
subst _ _ [] = []
subst newsub (o:ld) (e:st) | e==o = case checkRestStr ld st of
                                      True -> (newsub ++ (subst newsub (o:ld) $ drop (length ld) st))
                                      False -> e:(subst newsub (o:ld) st)
                           | otherwise = e:(subst newsub (o:ld) st)

checkRestStr :: String -> String -> Bool
checkRestStr [] [] = True
checkRestStr _ [] = False
checkRestStr [] _ = True
checkRestStr (l:ld) (s:st) | l==s = checkRestStr ld st
                           | otherwise = False
