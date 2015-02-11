{-# LANGUAGE ViewPatterns #-}

module Bytecodegen where
import ClassFormat
import qualified Text.Show.Pretty as Pr
import Data.Bits
import Data.Word
import Data.List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS

cpToBytecode :: CP_Info -> [Word8]
cpToBytecode c = case tag_cp c of
  TagClass              -> [0x07] ++ (intToTwoBytes $ index_cp c)               
  TagFieldRef           -> [0x09] ++ (intToTwoBytes $ index_name_cp c) ++ (intToTwoBytes $ index_nameandtype_cp c)  
  TagMethodRef          -> [0x0a] ++ (intToTwoBytes $ index_name_cp c) ++ (intToTwoBytes $ index_nameandtype_cp c)  
  TagInterfaceMethodRef -> [0x0b] ++ (intToTwoBytes $ index_name_cp c) ++ (intToTwoBytes $ index_nameandtype_cp c)
  TagString             -> [0x08] ++ (intToTwoBytes $ index_cp c)
  TagInteger            -> [0x03] ++ (intToFourBytes $ numi_cp c)                      
  TagFloat              -> [0x04] ++ (intToTwoBytes $ numf_cp c)                
  TagLong               -> [0x05] ++ (intToFourBytes $ numi_l1_cp c)    ++ (intToFourBytes $ numi_l2_cp c)                    
  TagDouble             -> [0x06] ++ (intToFourBytes $ numi_d1_cp c)    ++ (intToFourBytes $ numi_d2_cp c)            
  TagNameAndType        -> [0x0c] ++ (intToTwoBytes $ index_name_cp c) ++ (intToTwoBytes $ index_descr_cp c)                  
  TagUtf8               -> [0x01] ++ (intToTwoBytes $ tam_cp c)        ++ (strToInts $ cad_cp c)    
  
fieldInfoToBytecode :: Field_Info -> [Word8]
fieldInfoToBytecode f = 
  (accessFlagsToBytecode $ af_fi f) ++
  (intToTwoBytes $ index_name_fi f) ++
  (intToTwoBytes $ index_descr_fi f) ++
  (intToTwoBytes $ tam_fi f) ++
  (joinA $ map attributeInfoToBytecode $ array_attr_fi f)

methodInfoToBytecode :: Method_Info -> [Word8]
methodInfoToBytecode m = 
  (accessFlagsToBytecode $ af_mi m) ++
  (intToTwoBytes $ index_name_mi m) ++
  (intToTwoBytes $ index_descr_mi m) ++
  (intToTwoBytes $ tam_mi m) ++
  (joinA $ map attributeInfoToBytecode $ array_attr_mi m)

sp = stripPrefix
codeToBytecode :: Code -> [Word8]
codeToBytecode (sp "aload " -> Just i)          = [25] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "aload_" -> Just i)          = [42 +(read i)]
codeToBytecode (sp "getfield " -> Just i)       = [180] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "goto " -> Just i)           = [167] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "iadd " -> Just i)           = [96]  ++ (intToTwoBytes $ read i)
codeToBytecode "iconst_m1"                      = [2]
codeToBytecode (sp "iconst_" -> Just i)         = [3 + (read i)]
codeToBytecode (sp "if_icmpeq " -> Just i)      = [159] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "if_icmpne " -> Just i)      = [160] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "if_icmplt " -> Just i)      = [161] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "if_icmpge " -> Just i)      = [162] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "if_icmpgt " -> Just i)      = [163] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "if_icmple " -> Just i)      = [164] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "ifeq " -> Just i)           = [153] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "ifne " -> Just i)           = [154] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "iflt " -> Just i)           = [155] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "ifge " -> Just i)           = [156] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "ifgt " -> Just i)           = [157] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "ifle " -> Just i)           = [158] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "ifnonnull " -> Just i)      = [199] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "ifnull " -> Just i)         = [198] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "iload " -> Just i)          = [21]  ++ (intToTwoBytes $ read i)
codeToBytecode (sp "iload_" -> Just i)          = [26 +(read i)]
codeToBytecode (sp "invokespecial " -> Just i)  = [183] ++ (intToTwoBytes $ read i)
codeToBytecode "ireturn"                        = [172]
codeToBytecode (sp "istore " -> Just i)         = [54] ++ (intToTwoBytes $ read i)
codeToBytecode (sp "istore_" -> Just i)         = [59 +(read i)]
codeToBytecode (sp "ldc " -> Just i)            = [18]  ++ (intToTwoBytes $ read i)
codeToBytecode (sp "new " -> Just i)            = [187] ++ (intToTwoBytes $ read i)
codeToBytecode "nop"                            = [0]
codeToBytecode (sp "putfield " -> Just i)       = [181] ++ (intToTwoBytes $ read i)
codeToBytecode "return"                         = [177]
codeToBytecode "aconst_null"                    = [1]
codeToBytecode "PHCL"                           = []


attributeInfoToBytecode :: Attribute_Info -> [Word8]
attributeInfoToBytecode a@(AttributeGeneric           {}) = []         
attributeInfoToBytecode a@(AttributeConstantValue     {}) = []               
attributeInfoToBytecode a@(AttributeCode              {}) = 
  (intToTwoBytes $ index_name_attr a) ++ 
  (intToTwoBytes $ tam_len_attr a) ++
  (intToTwoBytes $ len_stack_attr a) ++
  (intToTwoBytes $  len_local_attr a) ++
  (intToTwoBytes $  tam_code_attr a) ++ 
  (joinA $ map codeToBytecode $ array_code_attr a) ++ 
  (intToTwoBytes $ tam_ex_attr a) ++
  (joinA $ map attributeInfoToBytecode $ array_attr_attr a)    
attributeInfoToBytecode a@(AttributeExceptions        {}) = []           
attributeInfoToBytecode a@(AttributeInnerClasses      {}) = []           
attributeInfoToBytecode a@(AttributeSynthetic         {}) = []           
attributeInfoToBytecode a@(AttributeSourceFile        {}) = []           
attributeInfoToBytecode a@(AttributeLineNumberTable   {}) = []                 
attributeInfoToBytecode a@(AttributeLocalVariableTable{}) = []                   
attributeInfoToBytecode a@(AttributeDeprecated        {}) = []           
                           
                  
strToInts :: String -> [Word8]
strToInts = map (fromIntegral . fromEnum) 

accessFlagsToBytecode :: AccessFlags -> [Word8]
accessFlagsToBytecode (AccessFlags a) = intToTwoBytes $ foldl (\a b -> a .|. b) 0 a

joinA :: [[a]] -> [a]
joinA = foldl (\a b -> a ++ b) []

intsToBytes :: [Int] -> [Word8]
intsToBytes i = joinA $ map (wordToTwoBytes . fromIntegral) i

intToTwoBytes :: Int -> [Word8]
intToTwoBytes = wordToTwoBytes . fromIntegral

intToFourBytes s = (wordToTwoBytes $ fromIntegral $ s `div` (2^16)) ++ (wordToTwoBytes $ fromIntegral $ s `mod` (2^16)) 

wordToTwoBytes :: Word16 -> [Word8]
wordToTwoBytes s = [fromIntegral $ s `div` (2^8), fromIntegral $ s `mod` (2^8)]

bytecodegen :: ClassFile -> [Word8]
bytecodegen c = 
  (intsToBytes $ [ 0xCAFE, 0xBABE, 
    numMinVer $ minver c, numMaxVer $ maxver c,
    count_cp c
  ]) ++ 
  (joinA $ map cpToBytecode $ array_cp c) ++
  (accessFlagsToBytecode $ acfg c) ++
  (intToTwoBytes $ index_th $ this c) ++
  (intToTwoBytes $ index_sp $ super $ c) ++
  (intToTwoBytes $ count_interfaces c) ++ 
  (joinA $ map (intToTwoBytes . index_if) $ array_interfaces c) ++
  (intToTwoBytes $ count_fields c) ++ 
  (joinA $ map fieldInfoToBytecode $ array_fields c) ++
  (intToTwoBytes $ count_methods c) ++
  (joinA $ map methodInfoToBytecode $ array_methods c) ++
  (intToTwoBytes $ count_attributes c) ++
  (joinA $ map attributeInfoToBytecode $ array_attributes c)

              
saveBytecode :: String -> [Word8] -> IO() 
saveBytecode filePath bytecode = BS.writeFile filePath $ BS.pack bytecode   
