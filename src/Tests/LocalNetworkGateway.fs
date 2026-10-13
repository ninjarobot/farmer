module LocalNetworkGateway

open Expecto
open System
open Farmer
open Farmer.Arm.Network
open Farmer.Builders
open Newtonsoft.Json.Linq

let tests =
    testList "LocalNetworkGateway"
    [
        test "Can create a basic localNetworkGateway" {
            let b =
                localNetworkGateway {
                    name "lng"
                    gateway_ip "10.0.0.4"
                    add_address_space "10.0.0.0/24"
                }
                :> IBuilder

            let resources = b.BuildResources Location.WestEurope

            let lng =
                resources
                |> List.pick (function
                    | :? LocalNetworkGateway as l -> Some l
                    | _ -> None)

            Expect.equal lng.Name.Value "lng" "Incorrect resource name"
            Expect.equal lng.Location Location.WestEurope "Incorrect location"
            Expect.isSome lng.GatewayIpAddress "GatewayIpAddress should be set"
            Expect.equal
                lng.GatewayIpAddress.Value.ToString()
                "10.0.0.4"
                "Incorrect gateway IP address"
            Expect.hasLength lng.AddressSpacePrefixes 1 "Incorrect address space count"
        }

        test "localNetworkGateway emits the correct ARM type and api version" {
            let lng =
                localNetworkGateway {
                    name "lng"
                    gateway_ip "10.0.0.4"
                    add_address_space "10.0.0.0/24"
                }

            let json =
                arm { add_resource lng }
                |> fun d -> d.Template
                |> Writer.toJson

            let jobj = JObject.Parse json
            let resource = jobj.SelectToken("resources[?(@.name=='lng')]")

            Expect.equal
                resource.ToString(Newtonsoft.Json.Formatting.Indented).Trim()
                resource.ToString(Newtonsoft.Json.Formatting.Indented).Trim()
                "Sanity check the resource is present"

            Expect.equal
                resource("type").ToString()
                "Microsoft.Network/localNetworkGateways"
                "Incorrect resource type"

            Expect.equal
                resource("apiVersion").ToString()
                "2021-02-01"
                "Incorrect API version"

            Expect.equal
                resource("properties/gatewayIpAddress").ToString()
                "10.0.0.4"
                "Incorrect gatewayIpAddress"

            Expect.hasLength
                (resource("properties/localNetworkAddressSpace/addressPrefixes") :?> JArray)
                1
                "Incorrect address space prefixes count"

            Expect.equal
                (resource("properties/localNetworkAddressSpace/addressPrefixes")[0].ToString())
                "\"10.0.0.0/24\""
                "Incorrect address space prefix formatting"
        }

        test "localNetworkGateway emits bgpSettings when configured" {
            let settings =
                {
                    Asn = 65010
                    BgpPeeringAddress = "192.168.1.1"
                    BgpPeeringAddresses =
                        [
                            {
                                CustomPeeringAddress = [ "192.168.1.1" ]
                                IpConfigurationId = "ipconfig1"
                            }
                        ]
                    PeerWeight = 50
                }

            let lng =
                localNetworkGateway {
                    name "lng-bgp"
                    gateway_ip "10.0.0.4"
                    add_address_space "10.0.0.0/24"
                    bgp settings
                }

            let json =
                arm { add_resource lng }
                |> fun d -> d.Template
                |> Writer.toJson

            let jobj = JObject.Parse json
            let resource = jobj.SelectToken("resources[?(@.name=='lng-bgp')]")

            Expect.equal
                resource("properties/bgpSettings/asn").ToString()
                "65010"
                "Incorrect BGP ASN"

            Expect.equal
                resource("properties/bgpSettings/bgpPeeringAddress").ToString()
                "\"192.168.1.1\""
                "Incorrect BGP peering address"

            Expect.equal
                resource("properties/bgpSettings/peerWeight").ToString()
                "50"
                "Incorrect BGP peer weight"

            Expect.equal
                resource("properties/bgpSettings/bgpPeeringAddresses/ipconfigurationId").ToString()
                "\"ipconfig1\""
                "Incorrect BGP peering IP configuration id"

            Expect.hasLength
                (resource(
                    "properties/bgpSettings/bgpPeeringAddresses/customBgpIpAddresses"
                ) :?> JArray)
                1
                "Incorrect BGP peering address count"
        }
    ]
