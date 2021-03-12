module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filter filterStreet >>> head
                           where
                             filterStreet :: Entry -> Boolean
                             filterStreet = _.address.street >>> \s -> s == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = filter filterEntry >>> not null
                where
                  filterEntry :: Entry -> Boolean
                  filterEntry entry =
                    entry.firstName == firstName &&
                    entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq eqEntryName
  where
    eqEntryName :: Entry -> Entry -> Boolean
    eqEntryName e1 e2 =
      e1.firstName == e2.firstName &&
      e1.lastName == e2.lastName
