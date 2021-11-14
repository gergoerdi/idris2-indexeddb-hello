import Utils

import JS
import Web.Dom
import Web.Html
import Web.Raw.IndexedDB

%foreign "browser:lambda:(w) => w.indexedDB"
prim__indexedDB : Window -> IDBFactory

SafeCast a => SafeCast (Array a) where
  safeCast = unsafeCastOnTypeof "object" -- XXX

data Date : Type where [external]

namespace Date
  export
  %foreign "browser:lambda:()=>new Date()"
  prim__new : PrimIO Date

  export
  %foreign "browser:lambda:(d)=>d.toString()"
  prim__toString : Date -> String

  export
  %foreign "browser:lambda:(s)=>new Date(s)"
  prim__fromString : String -> Date

new : JSIO Date
new = primIO prim__new

toString : Date -> String
toString = prim__toString

fromString : String -> Date
fromString = prim__fromString

record SaveGame where
  constructor MkSaveGame
  date : Date
  pic : Nat

toJSON : SaveGame -> IObject
toJSON sg = case getObject $ pairs [("saveDate", Str $ toString sg.date), ("pic", Num (cast sg.pic))] of
  Just obj => obj
  Nothing => assert_total $ idris_crash "toJSON"

fromJSON : IObject -> SaveGame
fromJSON obj = MkSaveGame
  { date = assert_total $ case valueAt obj "saveDate" of
      Just (Str s) => fromString s
  , pic = assert_total $ case  valueAt obj "pic" of
      Just (Num n) => cast n
  }

listFromDb : IDBDatabase -> String -> (Array Any -> JSIO ()) -> JSIO ()
listFromDb db name cb = do
  transaction <- transaction db (inject name) (Def Readonly) (Def $ !(IDBTransactionOptions.new Undef))
  req <- getAll !(objectStore transaction name) Undef Undef
  onsuccess req ?> do
    res <- castingTo "listFromDb" $ result req
    cb res

saveToDb : IDBDatabase -> String -> Any -> JSIO () -> JSIO ()
saveToDb db name x done = do
  transaction <- transaction db (inject name) (Def Readwrite) (Def $ !(IDBTransactionOptions.new Undef))
  req <- put !(objectStore transaction name) x Undef
  IDBRequest.onsuccess req ?> done
  IDBRequest.onerror req ?> done

main : IO ()
main = runJS $ do
  conn <- castingTo "IndexedDB" $ IDBFactory.open_ (prim__indexedDB !window) "hu.erdi.pokol" (Def 1)
  onupgradeneeded conn ?> do -- TODO
    db <- castingTo "IndexedDB" $ result conn
    params <- IDBObjectStoreParameters.new (Def Nothing) (Def True)
    store <- createObjectStore db "saves" (Def params)
    pure ()
  onsuccess conn ?> do
    db <- castingTo "IndexedDB" $ result conn
    let saveGame = MkSaveGame !new 1
    saveToDb db "saves" (MkAny . toJSON $ saveGame) $ printLn "Save done"
    listFromDb db "saves" $ \xs => do
      traceConsole xs $ pure ()
    pure ()
  pure ()
