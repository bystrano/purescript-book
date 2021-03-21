module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, PhoneType, address, person, phoneNumber)
import Data.Either (Either)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhonesField
           | PhoneField PhoneType

derive instance eqField :: Eq Field

data ValidationError = ValidationError String Field

type Errors = Array ValidationError

nonEmpty :: String -> Field -> String -> V Errors String
nonEmpty fname ftype ""     = invalid [ ValidationError ("Field '" <> fname <> "' cannot be empty") ftype ]
nonEmpty _     _     value  = pure value

validatePhoneNumbers :: String -> Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers field []      =
  invalid [ ValidationError
            ("Field '" <> field <> "' must contain at least one value")
            PhonesField ]
validatePhoneNumbers _     phones  =
  traverse validatePhoneNumber phones

lengthIs :: String -> Field -> Int -> String -> V Errors String
lengthIs fname ftype len value | length value /= len =
  invalid [ ValidationError
            ("Field '" <> fname <> "' must have length " <> show len)
            ftype
          ]
lengthIs _     _     _   value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: String -> Field -> Regex -> String -> V Errors String
matches _ _ regex value | test regex value = pure value
matches fname ftype _ _
  = invalid [ ValidationError
              ("Field '" <> fname <> "' did not match the required format")
              ftype
            ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty "Street" StreetField  a.street
          <*> nonEmpty "City"   CityField    a.city
          <*> lengthIs "State"  StateField 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
              <*> matches "Number" (PhoneField pn."type") phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty "First Name" FirstNameField p.firstName
         <*> nonEmpty "Last Name"  LastNameField  p.lastName
         <*> validateAddress p.homeAddress
         <*> validatePhoneNumbers "Phone Numbers" p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
