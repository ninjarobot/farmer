[<AutoOpen>]
module Farmer.Builders.LocalNetworkGateway

open Farmer
open Farmer.Arm.Network

type LocalNetworkGatewayConfig = {
    /// The name of the local network gateway.
    Name: ResourceName

    /// BGP speaker settings for the gateway.
    BgpSettings: BgpSettings option

    /// The FQDN of the local network gateway.
    Fqdn: string option

    /// The IP address of the local network gateway.
    GatewayIpAddress: System.Net.IPAddress option

    /// The address prefixes of the local network site.
    AddressSpacePrefixes: IPAddressCidr list

    Tags: Map<string, string>
} with

    interface IBuilder with
        member this.ResourceId = localNetworkGateways.resourceId this.Name

        member this.BuildResources location = [
            {
                Name = this.Name
                Location = location
                BgpSettings = this.BgpSettings
                Fqdn = this.Fqdn
                GatewayIpAddress = this.GatewayIpAddress
                AddressSpacePrefixes = this.AddressSpacePrefixes
                Tags = this.Tags
            }
        ]

type LocalNetworkGatewayBuilder() =
    member _.Yield _ = {
        Name = ResourceName.Empty
        BgpSettings = None
        Fqdn = None
        GatewayIpAddress = None
        AddressSpacePrefixes = []
        Tags = Map.empty
    }

    /// Specifies the name of the local network gateway.
    [<CustomOperation "name">]
    member _.Name(state: LocalNetworkGatewayConfig, name: ResourceName) = { state with Name = name }

    member _.Name(state: LocalNetworkGatewayConfig, name: string) = { state with Name = ResourceName name }

    /// Specifies the BGP speaker settings of the local network gateway.
    [<CustomOperation "bgp">]
    member _.Bgp(state: LocalNetworkGatewayConfig, bgpSettings: BgpSettings) = {
        state with
            BgpSettings = Some bgpSettings
    }

    /// Specifies the FQDN of the local network gateway.
    [<CustomOperation "fqdn">]
    member _.Fqdn(state: LocalNetworkGatewayConfig, fqdn: string) = { state with Fqdn = Some fqdn }

    /// Specifies the IP address of the local network gateway.
    [<CustomOperation "gateway_ip">]
    member _.GatewayIp(state: LocalNetworkGatewayConfig, gatewayIp: string) = {
        state with
            GatewayIpAddress = Some(System.Net.IPAddress.Parse gatewayIp)
    }

    member _.GatewayIp(state: LocalNetworkGatewayConfig, gatewayIp: System.Net.IPAddress) = {
        state with
            GatewayIpAddress = Some gatewayIp
    }

    /// Adds an address prefix to the local network site.
    [<CustomOperation "add_address_space">]
    member _.AddAddressSpace(state: LocalNetworkGatewayConfig, prefix: string) = {
        state with
            AddressSpacePrefixes = state.AddressSpacePrefixes @ [ IPAddressCidr.parse prefix ]
    }

    member _.AddAddressSpace(state: LocalNetworkGatewayConfig, prefix: IPAddressCidr) = {
        state with
            AddressSpacePrefixes = state.AddressSpacePrefixes @ [ prefix ]
    }

    interface ITaggable<LocalNetworkGatewayConfig> with
        member _.Add state tags = {
            state with
                Tags = state.Tags |> Map.merge tags
        }

let localNetworkGateway = LocalNetworkGatewayBuilder()