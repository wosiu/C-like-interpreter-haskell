{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Pardeklaracja where
import Absdeklaracja
import Lexdeklaracja
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.19.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn6 :: (Ident) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Ident)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Integer) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Integer)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (String) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (String)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Program) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Program)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Compund_content) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Compund_content)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Namespace) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Namespace)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([Compund_content]) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([Compund_content])
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Dec) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Dec)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Type_specifier) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Type_specifier)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Dec_base) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Dec_base)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Variable) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Variable)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Uninitialized_variable) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Uninitialized_variable)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Initialized_variable) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Initialized_variable)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Initializer) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Initializer)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ([Initializer]) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ([Initializer])
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Function) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Function)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([Param]) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([Param])
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Param) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Param)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Stm) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Stm)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Selection_stm) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Selection_stm)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Switch_content) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Switch_content)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([Switch_content]) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([Switch_content])
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Iter_stm) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Iter_stm)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Exp_or_empty) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Exp_or_empty)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Jump_stm) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Jump_stm)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Print_stm) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Print_stm)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Exp) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Exp)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Exp) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Exp)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Exp) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Exp)
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Exp) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Exp)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Exp) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Exp)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Exp) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Exp)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Exp) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Exp)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (Exp) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (Exp)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Exp) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Exp)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Exp) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Exp)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Exp) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Exp)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ([Exp]) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> ([Exp])
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Constant) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Constant)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (CBool) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (CBool)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (LValue) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (LValue)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Constant_expression) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Constant_expression)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Exp) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Exp)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (Exp) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (Exp)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (Exp) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (Exp)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (Exp) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (Exp)
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (Exp) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (Exp)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (Exp) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (Exp)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (Exp) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (Exp)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (Unary_operator) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (Unary_operator)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (Assignment_op) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (Assignment_op)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xe1\x05\xa9\x01\x50\x06\x59\x01\x00\x00\x12\x00\x00\x00\x00\x00\x10\x00\x00\x00\x57\x01\x76\x01\x42\x00\x62\x00\xd1\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x06\x00\x00\x50\x06\x00\x00\x44\x01\x00\x00\x44\x01\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x64\x01\x62\x01\x6b\x01\x69\x01\x68\x01\x50\x06\x61\x01\x5e\x01\x24\x01\xe1\x05\x00\x00\x00\x00\x00\x00\x27\x01\x11\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x35\x01\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x05\x16\x01\x00\x00\x00\x00\x00\x00\x57\x00\x26\x06\x1c\x06\x00\x00\x00\x00\x50\x06\x50\x06\x99\x00\x50\x06\x50\x06\x50\x06\x00\x00\x00\x00\x29\x01\x00\x00\x00\x00\x36\x01\x81\x00\x00\x00\x50\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x50\x06\x12\x06\x50\x06\xa6\x00\x33\x01\x38\x01\x00\x00\x00\x00\x37\x01\x00\x00\x62\x00\x62\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x01\x25\x01\x07\x01\x0b\x01\x00\x00\x04\x01\xa3\x00\x06\x01\x00\x01\x00\x00\x00\x00\x50\x06\xf2\x00\x00\x00\x03\x01\xf6\x00\xd2\x00\x00\x00\x00\x00\xbb\x00\xd2\x00\x50\x06\xe3\x00\xb0\x00\xae\x00\x00\x00\xe1\x05\xe2\x01\xba\x00\xe1\x05\x50\x06\x00\x00\x50\x06\x00\x00\x00\x00\xaa\x00\xa0\x00\x00\x00\x00\x00\x50\x06\x9b\x00\xf1\xff\x00\x00\x50\x06\x00\x00\x50\x06\x00\x00\x00\x00\x00\x00\x79\x00\xf1\xff\x77\x00\xe1\x05\x3c\x00\xe1\x05\x50\x06\x95\x00\x00\x00\xe1\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\x05\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x01\x00\xd2\x02\x19\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x05\x00\x00\xe7\x03\x00\x00\x18\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\x03\x00\x00\x00\x00\x00\x00\xc9\x00\x00\x00\x00\x00\x00\x00\x68\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x97\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1e\x03\x94\x04\x3d\x05\x00\x00\x00\x00\x83\x03\x51\x03\x00\x00\x26\x05\x1f\x03\x27\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x05\x3b\x02\x09\x02\xd7\x01\x77\x04\xfe\x03\xcc\x03\x9a\x03\x68\x03\x6f\x02\x36\x03\x6f\x05\xdd\x04\x62\x04\xed\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\x02\x00\x00\x00\x00\x00\x00\x00\x00\x58\x00\x00\x00\x00\x00\x1a\x03\x43\x00\xab\x04\x00\x00\x00\x00\x00\x00\x00\x00\x91\x01\xfe\xff\x00\x00\x5f\x01\xf5\x01\x00\x00\x30\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\x02\x00\x00\x10\x01\x00\x00\x8b\x02\x00\x00\x59\x02\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\x00\x00\x65\x00\x00\x00\x2d\x01\xc3\x01\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x9d\xff\xa1\xff\xa0\xff\x00\x00\x9a\xff\x99\xff\xc1\xff\x96\xff\xbc\xff\x95\xff\xb4\xff\x94\xff\xad\xff\xaa\xff\xa6\xff\xa2\xff\xa7\xff\xc5\xff\xc3\xff\xbf\xff\x98\xff\x97\xff\xb7\xff\xb1\xff\x00\x00\x91\xff\x00\x00\x93\xff\x00\x00\x92\xff\x00\x00\x9e\xff\x9f\xff\xfb\xff\xfa\xff\x00\x00\xda\xff\xd9\xff\xd8\xff\xd7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xf5\xff\xf9\xff\xf7\xff\x00\x00\xe7\xff\x00\x00\xe8\xff\xe9\xff\xef\xff\xf8\xff\x00\x00\xeb\xff\xee\xff\xed\xff\xec\xff\x00\x00\x00\x00\xf3\xff\xf6\xff\xf0\xff\x00\x00\x00\x00\x00\x00\xea\xff\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xca\xff\xc9\xff\x9d\xff\xaf\xff\xb0\xff\x00\x00\xa7\xff\xae\xff\x00\x00\x8f\xff\xac\xff\x8d\xff\xab\xff\x8c\xff\x8e\xff\x90\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\xff\x00\x00\xa9\xff\xc6\xff\xc2\xff\xc0\xff\xbe\xff\xbd\xff\xb8\xff\xba\xff\xb9\xff\xbb\xff\xb5\xff\xb6\xff\xb2\xff\xb3\xff\xc4\xff\xa5\xff\x00\x00\xcb\xff\x00\x00\x00\x00\xc8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xe2\xff\x00\x00\xe7\xff\xdb\xff\x00\x00\xdd\xff\x00\x00\xf4\xff\xdf\xff\x00\x00\x00\x00\x00\x00\xe1\xff\x00\x00\x00\x00\xe6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\xa8\xff\x00\x00\x9c\xff\xa3\xff\x00\x00\xd6\xff\xc7\xff\xd4\xff\x00\x00\x00\x00\x00\x00\xce\xff\x00\x00\xe3\xff\x00\x00\xde\xff\xdc\xff\xe0\xff\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcc\xff\x00\x00\xd5\xff\x00\x00\xd1\xff\xd3\xff\xcf\xff\xe4\xff\xd2\xff\x00\x00\xcd\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x1e\x00\x0f\x00\x20\x00\x14\x00\x12\x00\x13\x00\x04\x00\x04\x00\x16\x00\x00\x00\x18\x00\x19\x00\x0b\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x15\x00\x26\x00\x27\x00\x28\x00\x19\x00\x19\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x28\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x28\x00\x06\x00\x0f\x00\x33\x00\x02\x00\x12\x00\x13\x00\x0b\x00\x05\x00\x16\x00\x0f\x00\x18\x00\x19\x00\x11\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x16\x00\x26\x00\x27\x00\x28\x00\x05\x00\x05\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x00\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x1b\x00\x1c\x00\x0f\x00\x13\x00\x14\x00\x12\x00\x13\x00\x17\x00\x18\x00\x16\x00\x25\x00\x18\x00\x19\x00\x28\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x09\x00\x26\x00\x27\x00\x28\x00\x0d\x00\x32\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x05\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0b\x00\x2e\x00\x0f\x00\x2e\x00\x05\x00\x12\x00\x13\x00\x12\x00\x11\x00\x16\x00\x0b\x00\x18\x00\x19\x00\x0b\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x12\x00\x26\x00\x27\x00\x28\x00\x1a\x00\x21\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x12\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x1b\x00\x1c\x00\x0f\x00\x08\x00\x2c\x00\x12\x00\x13\x00\x0c\x00\x2e\x00\x16\x00\x25\x00\x18\x00\x19\x00\x28\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x0b\x00\x26\x00\x27\x00\x28\x00\x14\x00\x15\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x2c\x00\x04\x00\x05\x00\x0b\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x05\x00\x05\x00\x0f\x00\x19\x00\x05\x00\x12\x00\x13\x00\x0b\x00\x05\x00\x16\x00\x0b\x00\x18\x00\x19\x00\x15\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1a\x00\x26\x00\x27\x00\x28\x00\x14\x00\x15\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x0b\x00\x04\x00\x05\x00\x12\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x03\x00\x05\x00\x0f\x00\x05\x00\x0b\x00\x12\x00\x13\x00\x0b\x00\x19\x00\x16\x00\x2e\x00\x18\x00\x19\x00\x12\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x12\x00\x26\x00\x27\x00\x28\x00\x2f\x00\x33\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x04\x00\x04\x00\x05\x00\x04\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x04\x00\x04\x00\x0f\x00\x04\x00\x33\x00\x12\x00\x13\x00\x2f\x00\x12\x00\x16\x00\x12\x00\x18\x00\x19\x00\x03\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x2d\x00\x26\x00\x27\x00\x28\x00\x2f\x00\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1d\x00\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\xff\xff\x29\x00\xff\xff\x2b\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x17\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\x1e\x00\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x17\x00\xff\xff\x2c\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x17\x00\xff\xff\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x0d\x00\x0e\x00\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\x0d\x00\x0e\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x07\x00\xff\xff\x09\x00\x0a\x00\xff\xff\xff\xff\x0d\x00\x0e\x00\xff\xff\x10\x00\x0d\x00\x0e\x00\x12\x00\x13\x00\x15\x00\xff\xff\x16\x00\xff\xff\x18\x00\x19\x00\xff\xff\x00\x00\x01\x00\x02\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x08\x00\x09\x00\xff\xff\x0b\x00\x08\x00\x09\x00\xff\xff\x0b\x00\x10\x00\x11\x00\xff\xff\xff\xff\x10\x00\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\x22\x00\x23\x00\x24\x00\xff\xff\x26\x00\x27\x00\x28\x00\x30\x00\x31\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\x30\x00\x31\x00\x08\x00\x09\x00\xff\xff\xff\xff\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\xff\xff\x1f\x00\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xff\xff\x2e\x00\x2f\x00\x30\x00\x31\x00\x01\x00\xff\xff\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\xff\xff\xff\xff\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\xff\xff\x1f\x00\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x01\x00\xff\xff\xff\xff\x04\x00\x05\x00\xff\xff\xff\xff\x08\x00\x09\x00\xff\xff\x01\x00\x0c\x00\x0d\x00\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\xff\xff\x01\x00\x0c\x00\x0d\x00\x04\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\xff\xff\xff\xff\x0c\x00\x0d\x00\x22\x00\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\x22\x00\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x2a\x00\xff\xff\x22\x00\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x2a\x00\x01\x00\x2c\x00\xff\xff\x04\x00\x2f\x00\x30\x00\x31\x00\x08\x00\x09\x00\xff\xff\xff\xff\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x05\x00\x06\x00\x07\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x58\x00\xb0\x00\x3f\x00\xb1\x00\xae\x00\x40\x00\x29\x00\x4c\x00\x74\x00\x2a\x00\x58\x00\x2b\x00\x2c\x00\x73\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x4d\x00\x13\x00\x14\x00\x15\x00\x4e\x00\x75\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x59\x00\x36\x00\x37\x00\xc7\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x5a\x00\x67\x00\x3f\x00\xff\xff\x6f\x00\x40\x00\x29\x00\x73\x00\xb6\x00\x2a\x00\x68\x00\x2b\x00\x2c\x00\xc3\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x70\x00\x13\x00\x14\x00\x15\x00\x99\x00\x9a\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x4e\x00\x36\x00\x37\x00\xc3\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x43\x00\x44\x00\x3f\x00\x6b\x00\x6c\x00\x40\x00\x29\x00\x6d\x00\x6e\x00\x2a\x00\x45\x00\x2b\x00\x2c\x00\x46\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x61\x00\x13\x00\x14\x00\x15\x00\x63\x00\x5e\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\xc9\x00\x36\x00\x37\x00\x47\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x73\x00\xc5\x00\x3f\x00\xc7\x00\xa3\x00\x40\x00\x29\x00\x8d\x00\xbd\x00\x2a\x00\x73\x00\x2b\x00\x2c\x00\x73\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xc0\x00\x13\x00\x14\x00\x15\x00\xaa\x00\xbf\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\xae\x00\x36\x00\x37\x00\x4f\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x43\x00\x44\x00\x3f\x00\x69\x00\xb4\x00\x40\x00\x29\x00\x6a\x00\xb5\x00\x2a\x00\x45\x00\x2b\x00\x2c\x00\x46\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xb6\x00\x13\x00\x14\x00\x15\x00\xba\x00\xc5\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x47\x00\xc9\x00\x37\x00\x9c\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x9d\x00\xa4\x00\x3f\x00\x9e\x00\xa6\x00\x40\x00\x29\x00\x73\x00\xa5\x00\x2a\x00\x73\x00\x2b\x00\x2c\x00\xa1\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xa2\x00\x13\x00\x14\x00\x15\x00\xba\x00\xbb\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x73\x00\xc1\x00\x37\x00\xa7\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x71\x00\x88\x00\x3f\x00\xa8\x00\xa9\x00\x40\x00\x29\x00\x73\x00\x75\x00\x2a\x00\x9a\x00\x2b\x00\x2c\x00\x4a\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x4b\x00\x13\x00\x14\x00\x15\x00\x05\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x51\x00\xac\x00\x37\x00\x52\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x54\x00\x55\x00\x3f\x00\x56\x00\xff\xff\x40\x00\x29\x00\x05\x00\x57\x00\x2a\x00\x58\x00\x2b\x00\x2c\x00\x71\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x72\x00\x13\x00\x14\x00\x15\x00\x05\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\xb2\x00\x37\x00\x00\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x00\x00\x40\x00\x29\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x41\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x30\x00\x31\x00\x00\x00\x32\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\xc0\x00\x00\x00\x00\x00\x89\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x82\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\xb0\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\xab\x00\x00\x00\xb2\x00\x89\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x83\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x88\x00\x00\x00\x00\x00\x89\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x9e\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x84\x00\x1d\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x92\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x7c\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x9e\x00\xb9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x1c\x00\x1d\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x92\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\xbd\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x60\x00\x00\x00\x61\x00\x62\x00\x00\x00\x00\x00\x63\x00\x64\x00\x00\x00\x65\x00\x9e\x00\x9f\x00\x28\x00\x29\x00\x66\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x2c\x00\x00\x00\x05\x00\x06\x00\x07\x00\x92\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x75\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x3a\x00\x94\x00\x00\x00\x95\x00\x3a\x00\x94\x00\x00\x00\x95\x00\xb7\x00\x97\x00\x00\x00\x00\x00\x96\x00\x97\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x8a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x7b\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x8d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x7d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x8e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x52\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x5b\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x76\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xaa\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x76\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x77\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x1c\x00\x1d\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x00\x00\x92\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x8f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x00\x00\x79\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x86\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x00\x00\x8b\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x8f\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x10\x00\x11\x00\x12\x00\x00\x00\x13\x00\x14\x00\x5c\x00\x85\x00\x1d\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x00\x00\x5d\x00\x1d\x00\x21\x00\x22\x00\x00\x00\x00\x00\x23\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x44\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x25\x00\x30\x00\x31\x00\x45\x00\x32\x00\x33\x00\x46\x00\x34\x00\x26\x00\x35\x00\x47\x00\x00\x00\x49\x00\x05\x00\x27\x00\x28\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x00\x00\x23\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x44\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x25\x00\x30\x00\x31\x00\x45\x00\x32\x00\x33\x00\x46\x00\x34\x00\x26\x00\x35\x00\x47\x00\x00\x00\x00\x00\x05\x00\x27\x00\x28\x00\x1f\x00\x00\x00\x00\x00\x20\x00\x79\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x1f\x00\x23\x00\x24\x00\x20\x00\x00\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x1f\x00\x23\x00\x24\x00\x20\x00\x00\x00\x00\x00\x00\x00\x21\x00\x22\x00\x00\x00\x00\x00\x23\x00\x24\x00\x25\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x25\x00\x00\x00\x00\x00\x05\x00\x27\x00\x28\x00\x00\x00\x00\x00\x26\x00\x00\x00\x25\x00\x00\x00\x00\x00\x05\x00\x27\x00\x28\x00\x00\x00\x00\x00\x26\x00\x1f\x00\x94\x00\x00\x00\x20\x00\x05\x00\x27\x00\x28\x00\x21\x00\x22\x00\x00\x00\x00\x00\x23\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x27\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (3, 115) [
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115)
	]

happy_n_terms = 52 :: Int
happy_n_nonterms = 51 :: Int

happyReduce_3 = happySpecReduce_1  0# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn6
		 (Ident happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn7
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_5 = happySpecReduce_1  2# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Progr happy_var_1
	)}

happyReduce_7 = happySpecReduce_1  4# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (ScompContentStm happy_var_1
	)}

happyReduce_8 = happySpecReduce_1  4# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (ScompContentDec happy_var_1
	)}

happyReduce_9 = happySpecReduce_2  4# happyReduction_9
happyReduction_9 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (ScompContentExp happy_var_1
	)}

happyReduce_10 = happySpecReduce_1  4# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (ScompContentSpace happy_var_1
	)}

happyReduce_11 = happySpecReduce_3  5# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (BlockSp happy_var_2
	)}

happyReduce_12 = happySpecReduce_2  5# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  happyIn11
		 (EmptyBlockSp
	)

happyReduce_13 = happySpecReduce_1  6# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ((:[]) happy_var_1
	)}

happyReduce_14 = happySpecReduce_2  6# happyReduction_14
happyReduction_14 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_15 = happySpecReduce_2  7# happyReduction_15
happyReduction_15 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (VariableDec happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  7# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (FuncDec happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  8# happyReduction_17
happyReduction_17 happy_x_1
	 =  happyIn14
		 (Tbool
	)

happyReduce_18 = happySpecReduce_1  8# happyReduction_18
happyReduction_18 happy_x_1
	 =  happyIn14
		 (Tint
	)

happyReduce_19 = happySpecReduce_1  8# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn14
		 (Tstring
	)

happyReduce_20 = happySpecReduce_1  8# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn14
		 (Tauto
	)

happyReduce_21 = happySpecReduce_2  9# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (DecBase happy_var_1 happy_var_2
	)}}

happyReduce_22 = happySpecReduce_1  10# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (InitDec happy_var_1
	)}

happyReduce_23 = happySpecReduce_1  10# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (UninitDec happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  11# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (UninitSimpleTypeDec happy_var_1
	)}

happyReduce_25 = happyReduce 4# 11# happyReduction_25
happyReduction_25 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (UninitArr happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_26 = happySpecReduce_3  12# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (InitSimpleTypeDec happy_var_1 happy_var_3
	)}}

happyReduce_27 = happyReduce 7# 12# happyReduction_27
happyReduction_27 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_6 of { happy_var_6 -> 
	happyIn18
		 (InitArr happy_var_1 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_28 = happyReduce 5# 12# happyReduction_28
happyReduction_28 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	happyIn18
		 (InitAutoArr happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_29 = happySpecReduce_1  13# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (InitExpr happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  14# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((:[]) happy_var_1
	)}

happyReduce_31 = happySpecReduce_3  14# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_32 = happyReduce 4# 15# happyReduction_32
happyReduction_32 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_4 of { happy_var_4 -> 
	happyIn21
		 (NoParamFunc happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_33 = happyReduce 5# 15# happyReduction_33
happyReduction_33 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	case happyOut11 happy_x_5 of { happy_var_5 -> 
	happyIn21
		 (ParamFunc happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_34 = happySpecReduce_1  16# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((:[]) happy_var_1
	)}

happyReduce_35 = happySpecReduce_3  16# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_36 = happySpecReduce_1  17# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (FuncParam happy_var_1
	)}

happyReduce_37 = happySpecReduce_1  18# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (SelS happy_var_1
	)}

happyReduce_38 = happySpecReduce_1  18# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (IterS happy_var_1
	)}

happyReduce_39 = happySpecReduce_1  18# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (JumpS happy_var_1
	)}

happyReduce_40 = happySpecReduce_1  18# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (PrintS happy_var_1
	)}

happyReduce_41 = happyReduce 5# 19# happyReduction_41
happyReduction_41 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	happyIn25
		 (Sif happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_42 = happyReduce 7# 19# happyReduction_42
happyReduction_42 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	case happyOut10 happy_x_7 of { happy_var_7 -> 
	happyIn25
		 (SifElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_43 = happyReduce 5# 19# happyReduction_43
happyReduction_43 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_3 of { happy_var_3 -> 
	case happyOut26 happy_x_5 of { happy_var_5 -> 
	happyIn25
		 (SswitchOne happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_44 = happyReduce 7# 19# happyReduction_44
happyReduction_44 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_3 of { happy_var_3 -> 
	case happyOut27 happy_x_6 of { happy_var_6 -> 
	happyIn25
		 (SswitchMany happy_var_3 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_45 = happyReduce 4# 20# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	happyIn26
		 (SswitchCase happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_46 = happySpecReduce_3  20# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (SswitchDef happy_var_3
	)}

happyReduce_47 = happySpecReduce_1  21# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ((:[]) happy_var_1
	)}

happyReduce_48 = happySpecReduce_2  21# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_49 = happyReduce 5# 22# happyReduction_49
happyReduction_49 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	happyIn28
		 (Swhile happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_50 = happyReduce 9# 22# happyReduction_50
happyReduction_50 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut29 happy_x_3 of { happy_var_3 -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	case happyOut29 happy_x_7 of { happy_var_7 -> 
	case happyOut10 happy_x_9 of { happy_var_9 -> 
	happyIn28
		 (Sfor happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_51 = happySpecReduce_0  23# happyReduction_51
happyReduction_51  =  happyIn29
		 (SemptyExp
	)

happyReduce_52 = happySpecReduce_1  23# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (SnonemptyExp happy_var_1
	)}

happyReduce_53 = happySpecReduce_2  24# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  happyIn30
		 (Scontinue
	)

happyReduce_54 = happySpecReduce_2  24# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  happyIn30
		 (Sbreak
	)

happyReduce_55 = happySpecReduce_3  24# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 (Sreturn happy_var_2
	)}

happyReduce_56 = happyReduce 5# 25# happyReduction_56
happyReduction_56 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 (SPrint happy_var_3
	) `HappyStk` happyRest}

happyReduce_57 = happySpecReduce_3  26# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (Ecomma happy_var_1 happy_var_3
	)}}

happyReduce_58 = happySpecReduce_1  26# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_59 = happySpecReduce_3  27# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (Eassign happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_60 = happySpecReduce_1  27# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  28# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (Elor happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_1  28# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_63 = happySpecReduce_3  29# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (Eland happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_1  29# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  30# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (Eeq happy_var_1 happy_var_3
	)}}

happyReduce_66 = happySpecReduce_3  30# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (Eneq happy_var_1 happy_var_3
	)}}

happyReduce_67 = happySpecReduce_1  30# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  31# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (Elthen happy_var_1 happy_var_3
	)}}

happyReduce_69 = happySpecReduce_3  31# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (Egrthen happy_var_1 happy_var_3
	)}}

happyReduce_70 = happySpecReduce_3  31# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (Ele happy_var_1 happy_var_3
	)}}

happyReduce_71 = happySpecReduce_3  31# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (Ege happy_var_1 happy_var_3
	)}}

happyReduce_72 = happySpecReduce_1  31# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (happy_var_1
	)}

happyReduce_73 = happySpecReduce_3  32# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (Eplus happy_var_1 happy_var_3
	)}}

happyReduce_74 = happySpecReduce_3  32# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (Eminus happy_var_1 happy_var_3
	)}}

happyReduce_75 = happySpecReduce_1  32# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_76 = happySpecReduce_3  33# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (Etimes happy_var_1 happy_var_3
	)}}

happyReduce_77 = happySpecReduce_3  33# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (Ediv happy_var_1 happy_var_3
	)}}

happyReduce_78 = happySpecReduce_1  33# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_79 = happySpecReduce_2  34# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (Epreinc happy_var_2
	)}

happyReduce_80 = happySpecReduce_2  34# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (Epredec happy_var_2
	)}

happyReduce_81 = happySpecReduce_2  34# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (Epreop happy_var_1 happy_var_2
	)}}

happyReduce_82 = happySpecReduce_1  34# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_83 = happySpecReduce_2  35# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (Epostinc happy_var_1
	)}

happyReduce_84 = happySpecReduce_2  35# happyReduction_84
happyReduction_84 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (Epostdec happy_var_1
	)}

happyReduce_85 = happySpecReduce_1  35# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  36# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (Efunk happy_var_1
	)}

happyReduce_87 = happyReduce 4# 36# happyReduction_87
happyReduction_87 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (Efunkpar happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_88 = happySpecReduce_1  36# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (Elval happy_var_1
	)}

happyReduce_89 = happySpecReduce_1  36# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (Econst happy_var_1
	)}

happyReduce_90 = happySpecReduce_3  36# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn42
		 (happy_var_2
	)}

happyReduce_91 = happySpecReduce_1  37# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((:[]) happy_var_1
	)}

happyReduce_92 = happySpecReduce_3  37# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_93 = happySpecReduce_1  38# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (Ebool happy_var_1
	)}

happyReduce_94 = happySpecReduce_1  38# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (Eint happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  38# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (Estring happy_var_1
	)}

happyReduce_96 = happySpecReduce_1  39# happyReduction_96
happyReduction_96 happy_x_1
	 =  happyIn45
		 (BTrue
	)

happyReduce_97 = happySpecReduce_1  39# happyReduction_97
happyReduction_97 happy_x_1
	 =  happyIn45
		 (BFalse
	)

happyReduce_98 = happySpecReduce_1  40# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (LVar happy_var_1
	)}

happyReduce_99 = happyReduce 4# 40# happyReduction_99
happyReduction_99 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (LArrEl happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_100 = happySpecReduce_1  41# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (Especial happy_var_1
	)}

happyReduce_101 = happySpecReduce_1  42# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_102 = happySpecReduce_1  43# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_103 = happySpecReduce_1  44# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_104 = happySpecReduce_1  45# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_105 = happySpecReduce_1  46# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_106 = happySpecReduce_1  47# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_107 = happySpecReduce_1  48# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_108 = happySpecReduce_1  49# happyReduction_108
happyReduction_108 happy_x_1
	 =  happyIn55
		 (Plus
	)

happyReduce_109 = happySpecReduce_1  49# happyReduction_109
happyReduction_109 happy_x_1
	 =  happyIn55
		 (Negative
	)

happyReduce_110 = happySpecReduce_1  49# happyReduction_110
happyReduction_110 happy_x_1
	 =  happyIn55
		 (Logicalneg
	)

happyReduce_111 = happySpecReduce_1  50# happyReduction_111
happyReduction_111 happy_x_1
	 =  happyIn56
		 (Assign
	)

happyReduce_112 = happySpecReduce_1  50# happyReduction_112
happyReduction_112 happy_x_1
	 =  happyIn56
		 (AssignMul
	)

happyReduce_113 = happySpecReduce_1  50# happyReduction_113
happyReduction_113 happy_x_1
	 =  happyIn56
		 (AssignDiv
	)

happyReduce_114 = happySpecReduce_1  50# happyReduction_114
happyReduction_114 happy_x_1
	 =  happyIn56
		 (AssignAdd
	)

happyReduce_115 = happySpecReduce_1  50# happyReduction_115
happyReduction_115 happy_x_1
	 =  happyIn56
		 (AssignSub
	)

happyNewToken action sts stk [] =
	happyDoAction 51# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TV happy_dollar_dollar) -> cont 47#;
	PT _ (TI happy_dollar_dollar) -> cont 48#;
	PT _ (TL happy_dollar_dollar) -> cont 49#;
	_ -> cont 50#;
	_ -> happyError' (tk:tks)
	}

happyError_ 51# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut9 x))

pStm tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut24 x))

pExp tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut32 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 45 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
	 check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
		  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 169 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
