{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Web.Segment
  (module Web.Segment
  ,module Web.Segment.Types
  )
  where

import           Cases                      (snakify)
import           Control.Monad              (guard)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Currency                   (Currency)
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, toJSON, (.=))
import           Data.Aeson.Types           (Pair)

import           Data.Aeson.QQ              (aesonQQ)
import qualified Data.ByteString.Char8      as BS8
import           Data.CountryCodes          (CountryCode)
import           Data.CountryCodes          (CountryCode)
import           Data.Decimal               (Decimal)
import           Data.IP
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)
import           Data.Time.Calendar         (Day)
import           Data.Typeable
import           Data.UUID
import qualified Data.UUID                  as UUID
import           Data.Version               (showVersion)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Network.HTTP.Client.TLS
import           Network.URI                (URI)
import           Network.URI                (uriToString)
import           Paths_segment_api          (version)
import           Servant.API
import           Servant.Client
import           Text.Email.Validate        (EmailAddress)
import qualified Text.Email.Validate        as TEV
import           Web.Segment.Types

type SegmentApi
  = BasicAuth "realm" ()
  :> ("batch" :> ReqBody '[JSON] BatchedMsg
      :> Post '[JSON] SegmentResponse)


mkRunner :: String -> IO (BatchedMsg -> IO (Either ServantError SegmentResponse))
mkRunner key = do
  manager <- newManager tlsManagerSettings
  let baseURL = BaseUrl Https "api.segment.io" 443 "/v1"
--  let baseURL = BaseUrl Http "localhost" 8888 "/v1"
      basicAuthData = BasicAuthData (BS8.pack key) ""
  return $ \msg ->
    runExceptT $ segmentClient basicAuthData msg manager baseURL


segmentApi :: Proxy SegmentApi
segmentApi = Proxy

segmentClient = client segmentApi


emptyIdentify :: Msg
emptyIdentify = Identify emptyIdTraits
emptyIdTraits :: IdTraits
emptyIdTraits = IdTraits Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON BatchedMsg  where
  toJSON (BatchedMsg uuid msgs sentAt) =
    object
    ["context"   .= defaultContext
    ,"batch"     .= msgs
    ,"type"      .= ("batch"::Text)
    ,"messageId" .= UUID.toText uuid
    ,"sentAt"    .= sentAt
      -- timestamp is meaningless here, so omit.
    ]

instance ToJSON FullMsg where
  toJSON (FullMsg freeform msg commonMsg ) =
    object (msgJson msg <> commonJson commonMsg)
    where
      commonJson  c  =
        let (k,u) = case cmUid c of
              IdentifiedUser t -> ("userId", t)
              Anonymous t      -> ("anonymousId", t)
        in
        [ k .= u
        , "timestamp" .= cmTimestamp c
        ]

      msgJson msg = case msg of
        Identify {..} ->
          [ "type" .= ("identify" :: Text)
          -- this is a bit sketchy: we should check the freeform list
          -- first, to make sure it's not using a reserved trait.
          -- however, no way to signal failure from within a ToJSON
          -- impl. FIX
          , "traits" .= object (idTraitsJSON idTraits <> freeformJSON freeform)
          -- uses "traits"
          ]
        Track {..}    ->
          [ "type" .= ("track" :: Text)
          , "event" .= trEvent
          , "properties" .= object (trPropertiesJSON trProperties <> freeformJSON freeform)
          -- uses "properties" wtf
--          , "properties" .= object (freeformJSON <> propertyJson props)
          ]
        Page pgname props  ->
          [
          ]
        Screen {..}    -> []
        Group {..} -> []
        Alias {..} -> []

--      propertyJson props = []
--      commonPropertyJson props = []
--       freeformJSON = Map.foldMapWithKey (\k v -> [k .= v]) freeForm

freeformJSON :: Freeform -> [(Text,Value)]
freeformJSON = M.toList

-- there must be a better way of getting a handle on it than this
idTraitsJSON :: IdTraits -> [Pair]
idTraitsJSON traits = map (\(k,fv) -> k .= fv traits) idSelectors

  where idSelectors =
          [ ("avatar", wrap (flattenURI idAvatar))
          , ("birthday", wrap idBirthday)
          , ("createdAt", wrap idCreatedAt)
          , ("description",  wrap idDescription)
          , ("email", wrap (\x -> BS8.unpack . TEV.toByteString <$> idEmail x))
          , ("firstName", wrap idFirstName)
          , ("gender", wrap idGender)
          , ("lastName", wrap idLastName)
          , ("phone", wrap idPhone)
          , ("title", wrap idTitle)
          , ("username", wrap idUsername)
          , ("website", wrap (flattenURI idWebsite))
          ]

        flattenURI x = \a -> (\x -> x "") . uriToString id <$> x a

        idKeys =  [ "avatar","birthday", "createdAt", "description", "email", "firstName"
                  , "gender","lastName", "gender", "phone", "title", "username", "website"]


        wrap :: ToJSON b => (a -> Maybe b) -> (a -> Maybe Value)
        wrap x = \a -> toJSON <$> x a

trPropertiesJSON :: TrackProperties -> [Pair]
trPropertiesJSON properties =  map (\(k,fv) -> k .= fv properties) idSelectors
  where
    idSelectors = [("name", wrap trName)
                  ,("revenue", wrap trRevenue)
                  ,("currency", wrap (\x -> show <$> trCurrency x))
                  ,("value", wrap trValue)
                  ]
    wrap :: ToJSON b => (a -> Maybe b) -> (a -> Maybe Value)
    wrap x = \a -> toJSON <$> x a
defaultContext = [aesonQQ|{"library" : { "name": "segment-api",
                                         "version" : #{showVersion version} } }|]





-- data Properties a = Properties [a]

-- -- group traits
-- data GroupTrait
--   = GTEmployees Integer
--   -- not entirely clear that this is useful; we already have a GroupId
--   | GTId Text
--   | GTIndustry Text
--   | GTName Text

-- -- reserved screen properties
-- data ScreenProperty
--   = SPName Text

-- -- reserved page properties
-- data PageProperty
--   = PPName Text
--   | PPPath String
--   | PPReferrer URI
--   | PPSearch Text
--   | PPTitle Text
--   | PPUrl URI


-- TODO
-- data Screen
-- data Track
-- data Group
-- data Identify

pagePropertiesOk :: [(Text,Text)] -> Bool
pagePropertiesOk = checkReserved reservedPageProperties
reservedPageProperties = mkReserved "name path referrer search title url"


-- Reserved track properties


-- data TrackProperty
--   = TPName Text
--   | TPRevenue
--   -- |  An abstract “value” to associate with an event.
--   --    This is typically used in situations where the event doesn’t generate real-dollar revenue,
--   --    but has an intrinsic value to a marketing team, like newsletter signups.
--   | TPValue Double

-- instance ToJSON [TrackProperty] where
--  toJSON = undefined


propsOk :: [(Text,Text)] -> Bool
propsOk = checkReserved reservedProps

reservedProps = mkReserved "name revenue currency value"

-- Reserved traits

identityTraitsOk :: [(Text,Text)] -> Bool
identityTraitsOk = checkReserved reservedIdentityTraits


checkReserved reserved source = Set.intersection (Set.fromList $ map fst source) reserved == Set.empty

-- Traits are case-insensitive, so in Javascript you can match the rest of your camel-case code by sending firstName,
-- while in Ruby you can match your snake-case code by sending first_name. That way the API never seems alien to your code base.
reservedIdentityTraits = mkReserved ("address age avatar birthday createdAt description email firstName gender id lastName name phone title username website"
                                     <> commonTraits)

commonTraits = "address avatar createdAt email website phone"


mkReserved base = Set.fromList $ ws <> map snakify ws
  where ws = T.words base
