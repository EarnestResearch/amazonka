{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your subnets.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.DescribeSubnets
    (
    -- * Creating a Request
      describeSubnets
    , DescribeSubnets
    -- * Request Lenses
    , dsSubnetIds
    , dsFilters
    , dsDryRun

    -- * Destructuring the Response
    , describeSubnetsResponse
    , DescribeSubnetsResponse
    -- * Response Lenses
    , dsrsSubnets
    , dsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSubnets' smart constructor.
data DescribeSubnets = DescribeSubnets'
  { _dsSubnetIds :: !(Maybe [Text])
  , _dsFilters   :: !(Maybe [Filter])
  , _dsDryRun    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSubnets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSubnetIds' - One or more subnet IDs. Default: Describes all your subnets.
--
-- * 'dsFilters' - One or more filters.     * @availability-zone@ - The Availability Zone for the subnet. You can also use @availabilityZone@ as the filter name.     * @availability-zone-id@ - The ID of the Availability Zone for the subnet. You can also use @availabilityZoneId@ as the filter name.     * @available-ip-address-count@ - The number of IPv4 addresses in the subnet that are available.     * @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you specify must exactly match the subnet's CIDR block for information to be returned for the subnet. You can also use @cidr@ or @cidrBlock@ as the filter names.     * @default-for-az@ - Indicates whether this is the default subnet for the Availability Zone. You can also use @defaultForAz@ as the filter name.     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the subnet.     * @ipv6-cidr-block-association.association-id@ - An association ID for an IPv6 CIDR block associated with the subnet.     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the subnet.     * @owner-id@ - The ID of the AWS account that owns the subnet.     * @state@ - The state of the subnet (@pending@ | @available@ ).     * @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.     * @subnet-id@ - The ID of the subnet.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC for the subnet.
--
-- * 'dsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeSubnets
    :: DescribeSubnets
describeSubnets =
  DescribeSubnets'
    {_dsSubnetIds = Nothing, _dsFilters = Nothing, _dsDryRun = Nothing}


-- | One or more subnet IDs. Default: Describes all your subnets.
dsSubnetIds :: Lens' DescribeSubnets [Text]
dsSubnetIds = lens _dsSubnetIds (\ s a -> s{_dsSubnetIds = a}) . _Default . _Coerce

-- | One or more filters.     * @availability-zone@ - The Availability Zone for the subnet. You can also use @availabilityZone@ as the filter name.     * @availability-zone-id@ - The ID of the Availability Zone for the subnet. You can also use @availabilityZoneId@ as the filter name.     * @available-ip-address-count@ - The number of IPv4 addresses in the subnet that are available.     * @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you specify must exactly match the subnet's CIDR block for information to be returned for the subnet. You can also use @cidr@ or @cidrBlock@ as the filter names.     * @default-for-az@ - Indicates whether this is the default subnet for the Availability Zone. You can also use @defaultForAz@ as the filter name.     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the subnet.     * @ipv6-cidr-block-association.association-id@ - An association ID for an IPv6 CIDR block associated with the subnet.     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the subnet.     * @owner-id@ - The ID of the AWS account that owns the subnet.     * @state@ - The state of the subnet (@pending@ | @available@ ).     * @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.     * @subnet-id@ - The ID of the subnet.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC for the subnet.
dsFilters :: Lens' DescribeSubnets [Filter]
dsFilters = lens _dsFilters (\ s a -> s{_dsFilters = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsDryRun :: Lens' DescribeSubnets (Maybe Bool)
dsDryRun = lens _dsDryRun (\ s a -> s{_dsDryRun = a})

instance AWSRequest DescribeSubnets where
        type Rs DescribeSubnets = DescribeSubnetsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSubnetsResponse' <$>
                   (x .@? "subnetSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSubnets where

instance NFData DescribeSubnets where

instance ToHeaders DescribeSubnets where
        toHeaders = const mempty

instance ToPath DescribeSubnets where
        toPath = const "/"

instance ToQuery DescribeSubnets where
        toQuery DescribeSubnets'{..}
          = mconcat
              ["Action" =: ("DescribeSubnets" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "SubnetId" <$> _dsSubnetIds),
               toQuery (toQueryList "Filter" <$> _dsFilters),
               "DryRun" =: _dsDryRun]

-- | /See:/ 'describeSubnetsResponse' smart constructor.
data DescribeSubnetsResponse = DescribeSubnetsResponse'
  { _dsrsSubnets        :: !(Maybe [Subnet])
  , _dsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSubnetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsSubnets' - Information about one or more subnets.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeSubnetsResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DescribeSubnetsResponse
describeSubnetsResponse pResponseStatus_ =
  DescribeSubnetsResponse'
    {_dsrsSubnets = Nothing, _dsrsResponseStatus = pResponseStatus_}


-- | Information about one or more subnets.
dsrsSubnets :: Lens' DescribeSubnetsResponse [Subnet]
dsrsSubnets = lens _dsrsSubnets (\ s a -> s{_dsrsSubnets = a}) . _Default . _Coerce

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeSubnetsResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DescribeSubnetsResponse where
