{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Segment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Segment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.
--
--
--
-- /See:/ 'segment' smart constructor.
data Segment = Segment'{_sSegmentNumber :: !Nat,
                        _sTotalSegments :: !Nat}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Segment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSegmentNumber' - The zero-based index number of the this segment. For example, if the total number of segments is 4, SegmentNumber values will range from zero through three.
--
-- * 'sTotalSegments' - The total numer of segments.
segment
    :: Natural -- ^ 'sSegmentNumber'
    -> Natural -- ^ 'sTotalSegments'
    -> Segment
segment pSegmentNumber_ pTotalSegments_
  = Segment'{_sSegmentNumber = _Nat # pSegmentNumber_,
             _sTotalSegments = _Nat # pTotalSegments_}

-- | The zero-based index number of the this segment. For example, if the total number of segments is 4, SegmentNumber values will range from zero through three.
sSegmentNumber :: Lens' Segment Natural
sSegmentNumber = lens _sSegmentNumber (\ s a -> s{_sSegmentNumber = a}) . _Nat

-- | The total numer of segments.
sTotalSegments :: Lens' Segment Natural
sTotalSegments = lens _sTotalSegments (\ s a -> s{_sTotalSegments = a}) . _Nat

instance Hashable Segment where

instance NFData Segment where

instance ToJSON Segment where
        toJSON Segment'{..}
          = object
              (catMaybes
                 [Just ("SegmentNumber" .= _sSegmentNumber),
                  Just ("TotalSegments" .= _sTotalSegments)])
