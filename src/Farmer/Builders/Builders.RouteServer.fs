[<AutoOpen>]
module Farmer.Builders.Builders_RouteServer

open Farmer
open Farmer.Arm
open Farmer.RouteServer


type RSBGPConnectionConfig =
    {
        Name: string
        PeerIp: string
        PeerAsn: int
        Dependencies: Set<ResourceId>
    }

type RSBGPConnectionBuilder() =
    member _.Yield _ =
        {
            Name = ""
            PeerIp = ""
            PeerAsn = 0
            Dependencies = Set.empty
        }

    [<CustomOperation "name">]
    member _.ConnectionName(state: RSBGPConnectionConfig, name) = { state with Name = name }

    [<CustomOperation "peer_ip">]
    member _.PeerIp(state: RSBGPConnectionConfig, peerIp) = { state with PeerIp = peerIp }

    [<CustomOperation "peer_asn">]
    member _.PeerAsn(state: RSBGPConnectionConfig, peerAsn) = { state with PeerAsn = peerAsn }

    interface IDependable<RSBGPConnectionConfig> with
        /// Adds an explicit dependency to this Container App Environment.
        member _.Add state newDeps =
            { state with
                Dependencies = state.Dependencies + newDeps
            }

let routeServerBGPConnection = RSBGPConnectionBuilder()

type GenerateOrLinkSubnet =
    | AutoGenerate of VirtualNetwork:LinkedResource option * SubnetPrefix:IPAddressCidr option
    | LinkedSubnet of Subnet:LinkedResource

type RouteServerConfig =
    {
        Name: ResourceName
        Sku: RouteServer.Sku
        AllowBranchToBranchTraffic: FeatureFlag option
        HubRoutingPreference: HubRoutingPreference option
        BGPConnections: RSBGPConnectionConfig list
        SubnetScenario: GenerateOrLinkSubnet option
        Dependencies: Set<ResourceId>
        Tags: Map<string, string>
    }
    member private this.generateAnySubnetResources() : List<IArmResource> =
        match this.SubnetScenario with
        | None ->
            raiseFarmer "Must link to a subnet with 'link_to_subnet' or link to vnet with 'link_to_vnet' to generate a subnet."
        | Some(AutoGenerate(None, Some(_))) ->
            raiseFarmer "Must link to a vnet with 'link_to_vnet' when specifying a 'subnet_prefix'."
        | Some(AutoGenerate(_, None)) ->
            raiseFarmer "Must specify a 'subnet_prefix' when linking to a vnet with 'link_to_vnet'."
        | Some(AutoGenerate(Some(linkedVnet), Some(cidr))) ->
            let subnetId = {linkedVnet.ResourceId with Type = subnets; Segments = [ResourceName "RouteServerSubnet"] }
            [
                // subnet
                {
                    Subnet.Name = ResourceName "RouteServerSubnet"
                    Prefix = IPAddressCidr.format cidr
                    VirtualNetwork = Some(linkedVnet)
                    NetworkSecurityGroup = None
                    Delegations = []
                    NatGateway = None
                    ServiceEndpoints = []
                    AssociatedServiceEndpointPolicies = []
                    PrivateEndpointNetworkPolicies = None
                    PrivateLinkServiceNetworkPolicies = None
                }
                // ip configuration
                {
                    RouteServerIPConfig.Name = ResourceName $"{this.Name.Value}-ipconfig"
                    RouteServer = Managed(routeServers.resourceId this.Name)
                    PublicIpAddress = LinkedResource.Managed(publicIPAddresses.resourceId $"{this.Name.Value}-publicip")
                    SubnetId = LinkedResource.Managed(subnetId)
                }
            ]
        | Some(LinkedSubnet(linkedSubnet)) ->
            [
                //ip configuration
                {
                    RouteServerIPConfig.Name = ResourceName $"{this.Name.Value}-ipconfig"
                    RouteServer = Managed(routeServers.resourceId this.Name)
                    PublicIpAddress = LinkedResource.Managed(publicIPAddresses.resourceId $"{this.Name.Value}-publicip")
                    SubnetId = linkedSubnet
                }
            ]
    interface IBuilder with
        member this.ResourceId = routeServers.resourceId this.Name

        member this.BuildResources location =
            this.generateAnySubnetResources() @
            [
                //public ip
                {
                    PublicIpAddress.Name = ResourceName $"{this.Name.Value}-publicip"
                    AvailabilityZone = None
                    Location = location
                    Sku = PublicIpAddress.Sku.Standard
                    AllocationMethod = PublicIpAddress.AllocationMethod.Static
                    DomainNameLabel = None
                    Tags = this.Tags
                }

                //route server
                {
                    RouteServer.Name = this.Name
                    Location = location
                    Sku = this.Sku
                    AllowBranchToBranchTraffic =
                        this.AllowBranchToBranchTraffic |> Option.defaultValue FeatureFlag.Disabled
                    HubRoutingPreference =
                        this.HubRoutingPreference
                        |> Option.defaultValue HubRoutingPreference.ExpressRoute
                    Dependencies = this.Dependencies
                    Tags = this.Tags
                }

                //bgp connections
                for connection in this.BGPConnections do
                    {
                        RouteServerBGPConnection.Name = ResourceName connection.Name
                        RouteServer = Managed(routeServers.resourceId this.Name)
                        PeerIp = connection.PeerIp
                        PeerAsn = connection.PeerAsn
                        IpConfig =
                            LinkedResource.Managed(
                                routeServerIPConfigs.resourceId (
                                    ResourceName this.Name.Value,
                                    ResourceName $"{this.Name.Value}-ipconfig"
                                )
                            )
                        Dependencies = connection.Dependencies
                    }
            ]

type RouteServerBuilder() =
    member _.Yield _ =
        {
            Name = ResourceName.Empty
            Sku = Standard
            AllowBranchToBranchTraffic = None
            HubRoutingPreference = None
            BGPConnections = []
            SubnetScenario = None
            Dependencies = Set.empty
            Tags = Map.empty
        }

    [<CustomOperation "name">]
    member _.Name(state: RouteServerConfig, name: string) = { state with Name = ResourceName name }

    [<CustomOperation "sku">]
    member _.Sku(state: RouteServerConfig, sku) = { state with Sku = sku }

    [<CustomOperation "allow_branch_to_branch_traffic">]
    member _.AllowBranchToBranchTraffic(state: RouteServerConfig, flag: bool) =
        { state with
            AllowBranchToBranchTraffic = Some(FeatureFlag.ofBool flag)
        }

    [<CustomOperation "routing_preference">]
    member _.HubRoutingPreference(state: RouteServerConfig, routingPreference) =
        { state with
            HubRoutingPreference = Some(routingPreference)
        }

    [<CustomOperation "add_bgp_connections">]
    member _.AddIPConfigs(state: RouteServerConfig, connections: RSBGPConnectionConfig list) =
        { state with
            BGPConnections = connections @ state.BGPConnections
        }

    [<CustomOperation "subnet_prefix">]
    member _.SubnetPrefix(state: RouteServerConfig, prefix) =
        let prefix = IPAddressCidr.parse prefix
        let subnetScenario =
            match state.SubnetScenario with
            | None -> AutoGenerate(None, Some prefix)
            | Some(AutoGenerate(vnetId, _)) -> AutoGenerate(vnetId, Some prefix)
            | Some(LinkedSubnet(_)) -> raiseFarmer "Cannot specify 'subnet_prefix' when linking to an existing subnet."
        { state with
            SubnetScenario = Some subnetScenario
        }

    // linked to managed vnet created by Farmer and linked by user
    [<CustomOperation "link_to_vnet">]
    member _.LinkToVNetId(state: RouteServerConfig, vnetId: ResourceId) =
        let subnetScenario =
            match state.SubnetScenario with
            | None -> AutoGenerate(Some(Unmanaged(vnetId)), None)
            | Some(AutoGenerate(_, prefix)) -> AutoGenerate(Some(Unmanaged(vnetId)), prefix)
            | Some(LinkedSubnet(_)) -> raiseFarmer "Cannot specify 'link_to_vnet' when linking to an existing subnet."
        { state with
            SubnetScenario = Some subnetScenario
        }

    // linked to external existing vnet
    member _.LinkToVNetId(state: RouteServerConfig, vnetName: string) =
        let vnetId = virtualNetworks.resourceId (ResourceName vnetName)
        let subnetScenario =
            match state.SubnetScenario with
            | None -> AutoGenerate(Some(Unmanaged(vnetId)), None)
            | Some(AutoGenerate(_, prefix)) -> AutoGenerate(Some(Unmanaged(vnetId)), prefix)
            | Some(LinkedSubnet(_)) -> raiseFarmer "Cannot specify 'link_to_vnet' when linking to an existing subnet."
        { state with
            SubnetScenario = Some subnetScenario
        }

    // link to existing subnet
    [<CustomOperation "link_to_subnet">]
    member _.LinkToSubnet(state: RouteServerConfig, subnetId: ResourceId) =
        { state with
            SubnetScenario = Some (LinkedSubnet(Unmanaged subnetId))
        }

    interface ITaggable<RouteServerConfig> with
        member _.Add state tags =
            { state with
                Tags = state.Tags |> Map.merge tags
            }

    interface IDependable<RouteServerConfig> with
        /// Adds an explicit dependency to this Container App Environment.
        member _.Add state newDeps =
            { state with
                Dependencies = state.Dependencies + newDeps
            }
let routeServer = RouteServerBuilder()
