{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.Segment where

import           Cases               (snakify)
import           Control.Monad       (guard)
import           Currency            (Currency)
import           Data.Aeson          (ToJSON, Value, object, toJSON, (.=))
import           Data.Aeson.QQ       (aesonQQ)
import           Data.CountryCodes   (CountryCode)
import           Data.Decimal        (Decimal)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Monoid         ((<>))
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Data.Time.Calendar  (Day)
import           Data.UUID
import qualified Data.UUID           as UUID
import           Data.Version        (showVersion)
import           Network.URI         (URI)
import           Paths_segment_api   (version)
import           Servant.Client
import           Text.Email.Validate (EmailAddress)
-- data Client =
--   Client {
--     writeKey :: Key
--   }


-- | The sent-at field should be applied as close to send-time as possible
data BatchedMsg
  = BatchedMsg UUID [FullMsg] UTCTime

instance ToJSON BatchedMsg  where
  toJSON (BatchedMsg uuid msgs sentAt) =
    object ["context"   .= defaultContext
           ,"batch"     .= msgs
           ,"type"      .= ("batch"::Text)
           ,"messageId" .= uuid
           ,"sentAt"    .= sentAt
           -- timestamp is meaningless here, so omit.
           ]

instance ToJSON FullMsg where
  toJSON (FullMsg freeForm msg commonMsg ) =
    object (msgJson msg <> commonJson commonMsg)
    where
      commonJson  (CommonMsg sentAt timestamp uid) = []

      msgJson msg = case msg of
        Identify id props commonProps ->
          [ "type" .= ("identify" :: Text)
          , "traits" .= object (freeformJSON <> propertyJson props <> commonPropertyJson commonProps)
          , "userId" .= toJSON id
          ]
        Track (Event event) props  ->
          [ "type" .= ("track" :: Text)
          , "event" .= event
          , "properties" .= object (freeformJSON <> propertyJson props)
          ]
        Page pgname props  -> undefined
        Screen screenName props  -> undefined
        Group groupid props commonProps -> undefined
        Alias from to -> undefined

      propertyJson props = []
      commonPropertyJson props = []
      freeformJSON = Map.foldMapWithKey (\k v -> [k .= v]) freeForm



instance ToJSON Id where
  toJSON = undefined

instance ToJSON UUID where
  toJSON = toJSON . UUID.toText

defaultContext = [aesonQQ|{"library" : { "name": "segment-api",
                                         "version" : #{showVersion version} } }|]

instance ToJSON TrackProperty where
  toJSON = undefined

data FullMsg = FullMsg Freeform Msg CommonMsg

-- We leave this as an empty datatype for now
-- When we add vocabularies, we can add them as arms here, and
-- avoid invalidating old code
data TrackDefinedVocabulary

data Msg
  = Identify Id               (Properties IdentifyTrait) (Properties CommonTrait)
  | TrackSemantic TrackDefinedVocabulary
  | Track Event               (Properties TrackProperty)
  | Page (Maybe PageName)     (Properties PageProperty)
  | Screen (Maybe ScreenName) (Properties ScreenProperty)
  | Group GroupId             (Properties GroupTrait) (Properties CommonTrait)
  | Alias Id Id

type Key = Text
type PageName = Text
type ScreenName = Text
type GroupId = Text

-- Anonymous should really be semi-anonymous - it's for when you have some info about the User, like a session ID,
-- but don't have a strong guarantee like a database Id or UUID.
data Id
  = Anonymous Text
  | IdentifiedUser Text

newtype Event = Event Text

-- this is controversial: possible Map Text Value makes more sense.
type Freeform = Map Text Text

data CommonMsg =
  CommonMsg
  { -- time when sent
    sentAt    :: UTCTime
    -- time when queued
  , timestamp :: UTCTime
  , uid       :: UUID
  -- TODO lots more fields https://segment.com/docs/spec/common/
  }


data Properties a = Properties [a]

-- group traits
data GroupTrait
  = GTEmployees Integer
  -- not entirely clear that this is useful; we already have a GroupId
  | GTId Text
  | GTIndustry Text
  | GTName Text

-- reserved screen properties
data ScreenProperty
  = SPName Text

-- reserved page properties
data PageProperty
  = PPName Text
  | PPPath String
  | PPReferrer URI
  | PPSearch Text
  | PPTitle Text
  | PPUrl URI


pagePropertiesOk :: [(Text,Text)] -> Bool
pagePropertiesOk = checkReserved reservedPageProperties

reservedPageProperties = mkReserved "name path referrer search title url"

-- Reserved track properties


data TrackProperty
  = TPName Text
  | TPRevenue Currency Decimal
  -- |  An abstract “value” to associate with an event.
  --    This is typically used in situations where the event doesn’t generate real-dollar revenue,
  --    but has an intrinsic value to a marketing team, like newsletter signups.
  | TPValue Double

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

commonTraits = "address avatar createdAt eail website phone"

data CommonTrait
  = Address  (Maybe City) (Maybe CountryCode) (Maybe Postcode) (Maybe StateLoc) (Maybe Street)
  | AvatarURL URI
  | CreatedAt UTCTime
  | Description Text
  | Email EmailAddress
  | Website URI
  | Phone Text

data IdentifyTrait
  = ITAge Int
  | ITBirthday Day
  | ITFirstName Text
  | ITGender Text
  | ITIdentifier Text
  | ITLastName Text
  | ITFullName Text
  | ITTitle Text
  | ITUsername Text


newtype City = City Text
newtype StateLoc = StateLoc Text
newtype Street = Street Text
newtype Postcode = Postcode Text


mkReserved base = Set.fromList $ ws <> map snakify ws
  where ws = T.words base
