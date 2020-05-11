{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-} -- TODO

module Backend.Schema where

import Common.Schema
import Database.Beam
import Data.Proxy
import Database.Beam.Migrate.New
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Obelisk.Beam.Migrate
import Data.ByteString.Lazy.Char8 as LChar8

dumpQuery :: _ => (DatabaseSettings Postgres Db -> Q Postgres Db QBaseScope res) -> IO ()
dumpQuery q =
  let SqlSelect (PgSelectSyntax s) = Database.Beam.select $ q db
  in LChar8.putStrLn (pgRenderSyntaxScript s)

db :: DatabaseSettings Postgres Db
db = deAnnotateDatabase checkedDb

checkedDb :: AnnotatedDatabaseSettings Postgres Db
checkedDb = defaultAnnotatedDbSettings $ useObeliskNamingConvention defaultDbSettings

hsSchema :: Schema
hsSchema = fromAnnotatedDbSettings checkedDb (Proxy @'[])
