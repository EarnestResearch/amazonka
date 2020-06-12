{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the strategy for using unused Capacity Reservations for fulfilling On-Demand capacity.
--
--
-- For more information about Capacity Reservations, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html On-Demand Capacity Reservations> in the /Amazon Elastic Compute Cloud User Guide/ . For examples of using Capacity Reservations in an EC2 Fleet, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-examples.html EC2 Fleet Example Configurations> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- /See:/ 'capacityReservationOptions' smart constructor.
newtype CapacityReservationOptions = CapacityReservationOptions'{_croUsageStrategy
                                                                 ::
                                                                 Maybe
                                                                   FleetCapacityReservationUsageStrategy}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'CapacityReservationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'croUsageStrategy' - Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity. If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ). If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
capacityReservationOptions
    :: CapacityReservationOptions
capacityReservationOptions
  = CapacityReservationOptions'{_croUsageStrategy =
                                  Nothing}

-- | Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity. If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ). If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
croUsageStrategy :: Lens' CapacityReservationOptions (Maybe FleetCapacityReservationUsageStrategy)
croUsageStrategy = lens _croUsageStrategy (\ s a -> s{_croUsageStrategy = a})

instance FromXML CapacityReservationOptions where
        parseXML x
          = CapacityReservationOptions' <$>
              (x .@? "usageStrategy")

instance Hashable CapacityReservationOptions where

instance NFData CapacityReservationOptions where
