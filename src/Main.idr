import Utils

import JS
import Web.Dom
import Web.Html
import Web.Raw.IndexedDB
import Web.Raw.Css

%foreign "browser:lambda:(w) => w.indexedDB"
prim__indexedDB : Window -> IDBFactory

SafeCast a => SafeCast (Array a) where
  safeCast = unsafeCastOnTypeof "object" -- HACK

-- MISSING
data Date : Type where [external]

namespace Date
  export
  %foreign "browser:lambda:()=>new Date()"
  prim__new : PrimIO Date

  export
  %foreign "browser:lambda:(d)=>d.toISOString()"
  prim__toISOString : Date -> String

  export
  %foreign "browser:lambda:(s)=>new Date(s)"
  prim__fromString : String -> Date

new : JSIO Date
new = primIO prim__new

toISOString : Date -> String
toISOString = prim__toISOString

fromString : String -> Date
fromString = prim__fromString

record SavedState a where
  constructor MkSavedState
  date : Date
  payload : a

Functor SavedState where
  map f = record { payload $= f }

toJSON : SavedState Value -> Value
toJSON ss = pairs
  [ ("date", Str $ toISOString ss.date)
  , ("payload", ss.payload)
  ]

-- MISSING: This should use JS.Object.toAny, but it's not exported
toAny : Value -> Any
toAny v = assert_total $ case getObject v of { Just obj => MkAny obj }

-- MISSING: This should use JS.Object.toVal, but it's not exported
fromAny : Any -> Maybe Value
fromAny (MkAny ptr) = Obj <$> unsafeCastOnTypeof "object" ptr

fromJSON : Value -> Maybe (SavedState Value)
fromJSON v = case v of
  Obj obj => Just $ MkSavedState
    { date = !(case valueAt obj "date" of { Just (Str s) => Just (fromString s); _ => Nothing })
    , payload = !(case  valueAt obj "payload" of { Just payload => Just payload; _ => Nothing })
    }
  _ => Nothing

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
  onclick !window !> \ev => do
    Just el <- the (Maybe ?) . (castTo HTMLElement =<<) <$> target ev
      | _ => pure ()
    when !(contains !(classList el) "modal") $
      ignore $ CSSStyleDeclaration.removeProperty !(style el) "display"

  Just save <- castElementById HTMLAnchorElement "btn-save"
    | _ => throwError $ IsNothing "btn-save"
  Just load <- castElementById HTMLAnchorElement "btn-load"
    | _ => throwError $ IsNothing "btn-load"
  Just loadSection <- castElementById HTMLElement "sec-load"
    | _ => throwError $ IsNothing "sec-load"
  Just loadList <- castElementById HTMLUListElement "load-list"
    | _ => throwError $ IsNothing "load-list"
  Just textbox <- castElementById HTMLInputElement "textbox"
    | _ => throwError $ IsNothing "textbox"

  conn <- castingTo "IndexedDB" $ IDBFactory.open_ (prim__indexedDB !window) "hu.erdi.hello-indexeddb" (Def 1)
  onupgradeneeded conn ?> do -- TODO
    db <- castingTo "IndexedDB" $ result conn
    params <- IDBObjectStoreParameters.new (Def Nothing) (Def True)
    store <- createObjectStore db "saves" (Def params)
    pure ()
  onsuccess conn ?> do
    db <- castingTo "IndexedDB" $ result conn
    onclick save ?> do
      s <- get textbox value
      let ss = MkSavedState !new (Str s)
      saveToDb db "saves" (toAny . toJSON $ ss) $ printLn "Save done"
    onclick load ?> do
      listFromDb db "saves" $ \xs => do
        items <- for [0 .. !(sizeIO xs)-1] $ \i => assert_total $ do
          Just x <- readIO xs i
          Just ss <- pure $ fromJSON =<< fromAny x
          traceConsole ss $ pure ()
          a <- newElement A [textContent =. toISOString (ss.date), href =. "#"]
          onclick a ?> do
            Str s <- pure $ ss.payload
            value textbox .= s
            ignore $ CSSStyleDeclaration.removeProperty !(style loadSection) "display"
          li <- createElement Li
          ignore $ appendChild li a
          pure $ inject $ li :> Node
        replaceChildren loadList items
        CSSStyleDeclaration.setProperty' !(style loadSection) "display" "block"
