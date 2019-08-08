module Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (
  aModule,
  c_QDateTime,
  ) where



import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpShl, OpShr),
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod',
  mkMethod
  )
  
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Comparable, Equatable, Copyable),
  classAddFeatures,
  )
  
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDate (c_QDate)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QTime (c_QTime)
--import Graphics.UI.Qtah.Generator.Interface.Core.QTimeZone (c_QTimeZone)
--import Graphics.UI.Qtah.Generator.Interface.Core.QDataStream (c_QDataStream)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64, e_TimeSpec, e_DateFormat)

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QDateTime"] $
  [QtExport $ ExportClass c_QDateTime]
  

c_QDateTime =
  addReqIncludes [ includeStd "QDateTime" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Comparable, Equatable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QDateTime") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithQDate" [refT $ constT $ objT c_QDate]
  , just $ mkCtor "newWithQDateQTime" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime]
  , just $ mkCtor "newWithQDateQTimeTimespec" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime, enumT e_TimeSpec]
  , test (qtVersion >= [5, 2]) $ mkCtor "newWithQDateQTimeTimespecOffset" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime, enumT e_TimeSpec, intT]
  --, test (qtVersion >= [5, 2]) $ mkCtor "newWithQDateQTimeQTimezone" [refT $ constT $ objT c_QDate, refT $ constT $ objT c_QTime, refT $ constT $ objT c_QTimeZone]
  , just $ mkConstMethod "addDays" [qint64] $ objT c_QDateTime
  , just $ mkConstMethod "addMSecs" [qint64] $ objT c_QDateTime
  , just $ mkConstMethod "addMonths" [intT] $ objT c_QDateTime
  , just $ mkConstMethod "addSecs" [qint64] $ objT c_QDateTime
  , just $ mkConstMethod "addYears" [intT] $ objT c_QDateTime
  , just $ mkStaticMethod "currentDateTime" [] $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkStaticMethod "currentDateTimeUtc" [] $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkStaticMethod "currentMSecsSinceEpoch" [] qint64
  , test (qtVersion >= [5, 8]) $ mkStaticMethod "currentSecsSinceEpoch" [] qint64
  , just $ mkConstMethod "date" [] $ objT c_QDate
  , just $ mkConstMethod "daysTo" [refT $ constT $ objT c_QDateTime] qint64
  --, test (qtVersion >= [5, 5]) $ mkStaticMethod "fromCFDate" [objT c_CFDateRef] $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpoch" [qint64] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpochWithTimespec" [qint64, enumT e_TimeSpec] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpochWithTimespecOffset" [qint64, enumT e_TimeSpec, intT] $ objT c_QDateTime
  --, test (qtVersion >= [5, 2]) $ mkStaticMethod' "fromMSecsSinceEpoch" "fromMSecsSinceEpochWithTimezone" [qint64, refT $ constT $ objT c_QTimeZone] $ objT c_QDateTime
  --, test (qtVersion >= [5, 5]) $ mkStaticMethod "fromNSDate" [ptrT $ constT $ objT c_NSDate] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpoch" [qint64] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpochWithTimespec" [qint64, enumT e_TimeSpec] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpochWithTimespecOffset" [qint64, enumT e_TimeSpec, intT] $ objT c_QDateTime
  --, test (qtVersion >= [5, 8]) $ mkStaticMethod' "fromSecsSinceEpoch" "fromSecsSinceEpochWithQTimezone" [qint64, refT $ constT $ objT c_QTimeZone] $ objT c_QDateTime
  , just $ mkStaticMethod' "fromString" "fromString" [refT $ constT $ objT c_QString] $ objT c_QDateTime
  , just $ mkStaticMethod' "fromString" "fromStringWithDateFormat" [refT $ constT $ objT c_QString, enumT e_DateFormat] $ objT c_QDateTime
  , just $ mkStaticMethod' "fromString" "fromStrings" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] $ objT c_QDateTime
  , test (qtVersion >= [5, 2]) $ mkConstMethod "isDaylightTime" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkConstMethod "msecsTo" [refT $ constT $ objT c_QDateTime] qint64
  , test (qtVersion >= [5, 2]) $ mkConstMethod "offsetFromUtc" [] intT
  , just $ mkConstMethod "secsTo" [refT $ constT $ objT c_QDateTime] qint64
  , just $ mkMethod "setDate" [refT $ constT $ objT c_QDate] voidT
  , test (qtVersion >= [4, 7]) $ mkMethod "setMSecsSinceEpoch" [qint64] voidT
  , test (qtVersion >= [5, 2]) $ mkMethod "setOffsetFromUtc" [intT] voidT
  , test (qtVersion >= [5, 8]) $ mkMethod "setSecsSinceEpoch" [qint64] voidT
  , just $ mkMethod "setTime" [refT $ constT $ objT c_QTime] voidT
  , just $ mkMethod "setTimeSpec" [enumT e_TimeSpec] voidT
  --, test (qtVersion >= [5, 2]) $ mkMethod "setTimeZone" [refT $ constT $ objT c_QTimeZone] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [refT $ objT c_QDateTime] voidT
  --, just $ mkConstMethod "time" [] $ objT c_QTime
  , just $ mkConstMethod "timeSpec" [] $ enumT e_TimeSpec
  --, test (qtVersion >= [5, 2]) $ mkConstMethod "timeZone" [] $ objT c_QTimeZone
  , test (qtVersion >= [5, 2]) $ mkConstMethod "timeZoneAbbreviation" [] $ objT c_QString
  --, test (qtVersion >= [5, 5]) $ mkConstMethod "toCFDate" [] $ objT c_CFDateRef
  , just $ mkConstMethod "toLocalTime" [] $ objT c_QDateTime
  , test (qtVersion >= [4, 7]) $ mkConstMethod "toMSecsSinceEpoch" [] qint64
  --, test (qtVersion >= [5, 5]) $ mkConstMethod "toNSDate" [] $ ptrT $ objT c_NSDate
  , test (qtVersion >= [5, 2]) $ mkConstMethod "toOffsetFromUtc" [intT] $ objT c_QDateTime
  , test (qtVersion >= [5, 8]) $ mkConstMethod "toSecsSinceEpoch" [] qint64
  , just $ mkConstMethod' "toString" "toString" [] $ objT c_QString
  , just $ mkConstMethod' "toString" "toStringWithDateFormat" [enumT e_DateFormat] $ objT c_QString
  , just $ mkConstMethod "toTimeSpec" [enumT e_TimeSpec] $ objT c_QDateTime
  --, test (qtVersion >= [5, 2]) $ mkConstMethod "toTimeZone" [refT $ constT $ objT c_QTimeZone] $ objT c_QDateTime
  , just $ mkConstMethod "toUTC" [] $ objT c_QDateTime
  --, just $ mkMethod OpShl [refT $ objT c_QDataStream, refT $ objT c_QDateTime] $ refT $ objT c_QDataStream
  --, just $ mkMethod OpShr [refT $ objT c_QDataStream, refT $ objT c_QDateTime] $ refT $ objT c_QDataStream
  ]
  
  

