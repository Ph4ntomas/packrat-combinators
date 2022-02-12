module Packrat.Internal.Errors (
  PackEDesc(..)
  , PackError(..)
  , 
) where

import Data.List (union)

import Packrat.Positions

data PackEDesc =  Expected    String
                | Unexpected  String
                | Internal    String
                | Message     String

data PackError = PackError {
                  pos :: PackPos,
                  descs :: [PackEDesc]
}

instance Eq PackEDesc where
  Expected lhs == Expected rhs = lhs == rhs
  Message lhs == Message rhs = lhs == rhs
  _ == _ = False

instance Show PackError where
  show (PackError pos []) = show pos ++ "parse error"
  show (PackError pos descs) = 
    prepPos (showExpecteds (getExpects descs)) ++
    prepPos (showUnexpecteds (getUnexpects descs)) ++ 
    prepPos (showMessages (getMessages descs))
    where
      prepPos [] = []
      prepPos s = show pos ++ s

showExpecteds [] = []
showExpecteds [Expected msg] = msg ++ " expected.\n"
showExpecteds [Expected msg, Expected msg'] = "Either " ++ 
  show msg ++ " or " ++ 
  show msg' ++ " expected.\n"
showExpecteds exps = "Expecting one of:\n" ++ expectList exps where 
  expectList [] = []
  expectList (Expected msg:tl) = "  " ++ show msg ++ ",\n" ++ expectList tl

showUnexpecteds [] = []
showUnexpecteds [Unexpected msg] = msg ++ " unexpected."
-- Following patterns make no sense because we should know what was unexpected here.
showUnexpecteds [Unexpected msg, Unexpected msg'] = "Either " ++
  show msg ++ " or " ++ 
  show msg' ++ " parsed but not expected.\n"
showUnexpecteds exps = "One of several unexpected token found:\n" ++ unexpectList exps where 
  unexpectList [] = []
  unexpectList (Unexpected msg:tl) = "  " ++ show msg ++ ",\n"

showMessages [] = []
showMessages [Message msg] = msg ++ "\n"
showMessages msgs = "Several error messages exists :\n" ++ msgList msgs where
  msgList [] = []
  msgList (Message msg:tl) = "  " ++ show msg ++ ",\n"

-- Utils function
isExpect :: PackEDesc -> Bool
isExpect (Expected _) = True
isExpect _ = False

isUnexpect :: PackEDesc -> Bool
isUnexpect (Unexpected _) = True
isUnexpect _ = False

isMessage :: PackEDesc -> Bool
isMessage (Message _) = True
isMessage _ = False

getExpects :: [PackEDesc] -> [PackEDesc] 
getExpects = filter isExpect

getUnexpects :: [PackEDesc] -> [PackEDesc] 
getUnexpects = filter isUnexpect

getMessages:: [PackEDesc] -> [PackEDesc] 
getMessages = filter isMessage
