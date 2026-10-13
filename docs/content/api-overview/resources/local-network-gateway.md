---
title: "Local Network Gateway"
date: 2026-10-13T10:00:00-04:00
chapter: false
weight: 22
---

#### Overview
The Local Network Gateway builder creates `Microsoft.Network/localNetworkGateways`
resources. A local network gateway represents an on‑premises local network that
connects to a virtual network through a VPN device, and it carries BGP speaker
settings, an FQDN, a gateway IP address, and the on‑premises address space
prefixes.

* Local Network Gateway (`Microsoft.Network/localNetworkGateways`)

#### Builder Keywords

| Applies To       | Keyword           | Purpose                                                                        |
|------------------|-------------------|--------------------------------------------------------------------------------|
| localNetworkGateway | `name`      | Specifies the name of the local network gateway                                |
| localNetworkGateway | `gateway_ip`    | The IP address of the local network gateway used for the on‑premises peering device |
| localNetworkGateway | `fqdn`          | The fully qualified domain name of the local network gateway                   |
| localNetworkGateway | `bgp`           | The BGP speaker settings (ASN, peering address, peering addresses, peer weight) |
| localNetworkGateway | `add_address_space` | Adds an on‑premises address space prefix to the gateway                    |

#### Example

```fsharp
#r "nuget:Farmer"

open Farmer
open Farmer.Arm.Network
open Farmer.Builders

let bgp: BgpSettings =
    {
        Asn = 65000
        BgpPeeringAddress = "10.0.0.1"
        BgpPeeringAddresses =
            [
                {
                    CustomPeeringAddress = []
                    IpConfigurationId = "GatewaySubnet/ipConfig"
                }
            ]
        PeerWeight = 40
    }

arm {
    location Location.EastUS
    add_resource (
        localNetworkGateway {
            name "on-premises"
            gateway_ip "10.0.0.1"
            fqdn "example.com"
            bgp bgp
            add_address_space "10.0.0.0/16"
            add_address_space IPAddressCidr.parse "10.1.0.0/16"
        }
    )
}
```
