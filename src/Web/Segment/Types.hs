{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Segment.Types where

import           Control.Monad             (guard, mzero)
import           Currency                  (ISO4217Currency)
import           Data.Aeson                (FromJSON, ToJSON, Value (..),
                                            object, parseJSON, toJSON, (.:),
                                            (.=))
import           Data.Aeson.QQ             (aesonQQ)
import           Data.Decimal              (Decimal)
import           Data.IP
import qualified Data.ISO3166_CountryCodes as CC
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import           Data.Time.Calendar        (Day)
import           Data.Typeable
import           Data.UUID                 (UUID)
import qualified Data.UUID                 as UUID
import           Data.Version              (showVersion)
import           GHC.Generics              (Generic)
import           Network.URI               (URI)
import           Paths_segment_api         (version)
import           Text.Email.Validate       (EmailAddress)
import           Text.Read                 (readMaybe)

data SegmentResponse = SegmentResponse
  deriving (Eq,Show)

instance FromJSON SegmentResponse where
  parseJSON (Object o) = do
    success <- o .: "success"
    guard (success == True)
    return $ SegmentResponse
  parseJSON _ = mzero

-- | The sent-at field should be applied as close to send-time as
--   possible - the supplied recordEvent function will add the time.
data BatchedMsg
  = BatchedMsg UUID [FullMsg] UTCTime
  deriving (Generic, Eq, Show, Typeable)


data FullMsg = FullMsg Freeform Msg CommonMsg Context
  deriving (Generic, Eq, Show, Typeable)


data Msg
  = Identify
    { idTraits :: IdTraits  }
  | Track
    { trEvent      :: Text
    , trProperties :: TrackProperties
    }
  | Page
    { paName       :: Maybe Text
    , paProperties :: PageProperties
    }

  | Screen
    { scName :: Maybe Text

    }
  | Group
    { grGroupId :: Text
    , grTraits  :: GroupTraits
    } --GroupId             Group -- Properties GroupTrait) (Properties CommonTrait)
  | Alias
    { alPreviousId :: SegmentId
    } -- Id
  | TrackSemantic
    {
    } -- TrackDefinedVocabulary
  deriving (Generic, Eq, Show, Typeable)


data IdTraits = IdTraits
  { idAvatar      :: Maybe URI
  , idBirthday    :: Maybe Day
  , idCreatedAt   :: Maybe UTCTime
  , idDescription :: Maybe Text
  , idEmail       :: Maybe EmailAddress
  , idFirstName   :: Maybe Text
  , idGender      :: Maybe Text
  , idLastName    :: Maybe Text
  , idPhone       :: Maybe Text
  , idTitle       :: Maybe Text
  , idUsername    :: Maybe Text
  , idWebsite     :: Maybe URI
  } deriving (Generic, Eq, Show, Typeable)

emptyTrackProperties = TrackProperties Nothing Nothing Nothing Nothing

data TrackProperties = TrackProperties {
  -- it is entirely unclear why they would reserve this when they've
  -- already named the event under the "event" tag.
   trName     :: Maybe Text
 , trRevenue  :: Maybe Double
 , trCurrency :: Maybe ISO4217Currency
 , trValue    :: Maybe Double
 } deriving (Generic, Eq, Show, Typeable)

data PageProperties = PageProperties
  { ppName     :: Maybe Text
  , ppPath     :: Maybe Text
  , ppReferrer :: Maybe URI
  , ppSearch   :: Maybe Text
  , ppTitle    :: Maybe Text
  , ppUrl      :: Maybe URI
  } deriving (Generic, Eq, Show, Typeable)

data GroupTraits = GroupTraits
  { grAddress     :: Maybe Address
  , grAvatar      :: Maybe URI
  , grCreatedAt   :: Maybe UTCTime
  , grDescription :: Maybe Text
  , grEmail       :: Maybe EmailAddress
    -- I don't know why this is a string. I
    -- bet it's for "10-100 employees" or something
    -- similarly stupid.
  , grEmployees   :: Maybe Text
  , grId          :: Maybe Text
  , grIndustry    :: Maybe Text
  , grName        :: Maybe Text
  , grPhone       :: Maybe Text
  , grWebsite     :: Maybe URI
  } deriving (Generic, Eq, Show, Typeable)

-- Anonymous should really be semi-anonymous - it's for when you have some info about the User, like a session ID,
-- but don't have a strong guarantee like a database Id or UUID.
--
-- these can be used just about anywhere - is legit to alias an
-- Anonymous ID to another Anonymous ID, for instance.
data SegmentId
  = Anonymous Text
  | IdentifiedUser Text
  deriving (Generic, Eq, Show, Typeable)

-- instance ToJSON Id where
--   toJSON (Anonymous t)      = object ["anonymousId" .= t]
--   toJSON (IdentifiedUser t) = object ["userId"      .= t]

type Context = Freeform

type Freeform = Map Text Value
emptyFreeform = Map.empty


-- instance ToJSON UUID where
--  toJSON = toJSON . UUID.toText

emptyCommonMsg time uid = CommonMsg time uid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
data CommonMsg =
  CommonMsg
  { -- time when queued
    cmTimestamp :: UTCTime
  , cmUid       :: SegmentId

  -- optional fields
  , cmActive    :: Maybe Bool
  , cmApp       :: Maybe App
  , cmCampaign  :: Maybe Campaign
  , cmDevice    :: Maybe Device
  , cmIP        :: Maybe IP
  , cmLibrary   :: Maybe Library
  , cmLocation  :: Maybe Location
  , cmNetwork   :: Maybe Network
  , cmOS        :: Maybe OS
  , cmUserAgent :: Maybe Text
  -- TODO lots more fields https://segment.com/docs/spec/common/
  } deriving (Generic, Eq, Show, Typeable)


data App = App
  { apName    :: Text
  , apVersion :: Text
  , apBuild   :: Text
  }  deriving (Generic, Eq, Show, Typeable)
-- FIX need standard field-munging machinery
instance ToJSON App

data Campaign = Campaign
  { caName    :: Text
  , caSource  :: Text
  , caMedium  :: Text
  , caTerm    :: Text
  , caContent :: Text
  }  deriving (Generic, Eq, Show, Typeable)
instance ToJSON Campaign

data Device = Device
  { deId           :: Text
  , deManufacturer :: Text
  , deModel        :: Text
  , deName         :: Text
  , deType         :: Text
  , deVersion      :: Text
  }  deriving (Generic, Eq, Show, Typeable)
instance ToJSON Device

data Library = Library
  { liName    :: Text
  , liVersion :: Text
  } deriving (Generic, Eq, Show, Typeable)
instance ToJSON Library

-- newtype to avoid needing to define Aeson instances for a datatype
-- we don't own
newtype OurCountryCode = OurCountryCode { unOurCountryCode :: CC.CountryCode }
  deriving (Eq,Show,Ord)

-- | to json: as a simple string
instance ToJSON OurCountryCode where
  toJSON = toJSON . CC.countryNameFromCode . unOurCountryCode

-- | from json: as a simple string
instance FromJSON OurCountryCode where
  parseJSON (String s)
    | Just a <- readMaybe (T.unpack s) = pure (OurCountryCode a)
  parseJSON _ = fail "CountryCode"

-- Unclear which of these are actually mandatory.
-- region is not in the example, so assuming it's optional.
data Location = Location
  { loCity      :: Text
  , loCountry   :: OurCountryCode
  , loLatitude  :: Double
  , loLongitude :: Double
  , loRegion    :: Maybe Text
  , loSpeed     :: Double
  } deriving (Generic, Eq, Show, Typeable)
instance ToJSON Location


data Network = Network
  { neBluetooth :: Bool
  , neCellular  :: Bool
  , neWifi      :: Bool
  -- can this be null?
  , neCarrier   :: Text
  }  deriving (Generic, Eq, Show, Typeable)
instance ToJSON Network

data OS = OS
  { osName    :: Text
  , osVersion :: Text
  } deriving (Generic, Eq, Show, Typeable)
instance ToJSON OS

data Address = Address
  { adCity     :: Maybe City
  , adCountry  :: Maybe OurCountryCode
  , adPostcode :: Maybe Postcode
  , adState    :: Maybe StateLoc
  , adStreet   :: Maybe Street
  } deriving (Generic, Eq, Show, Typeable)

newtype City = City Text
 deriving (Generic, Eq, Show, Typeable)
newtype StateLoc = StateLoc Text
 deriving (Generic, Eq, Show, Typeable)
newtype Street = Street Text
 deriving (Generic, Eq, Show, Typeable)
newtype Postcode = Postcode Text
 deriving (Generic, Eq, Show, Typeable)

-- We leave this as an empty datatype for now
-- When we add vocabularies, we can add them as arms here, and
-- avoid invalidating old code
data TrackDefinedVocabulary
  deriving (Generic, Typeable)

-- bogus instances because "deriving" don't mess with types without constructors
instance Eq TrackDefinedVocabulary where
  _ == _ = error "can never be called"
instance Show TrackDefinedVocabulary where
  show _ = error "can never be called"
