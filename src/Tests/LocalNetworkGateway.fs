module LocalNetworkGateway

open Expecto
open System
open Farmer
open Farmer.Arm.Network
open Farmer.Builders
open Newtonsoft.Json.Linq

let tests =
    testList "LocalNetworkGateway" [
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
            match lng.GatewayIpAddress with
            | Some ip -> Expect.equal (ip.ToString()) "10.0.0.4" "Incorrect gateway IP address"
            | None -> failwith "Expected GatewayIpAddress to be set, but it was None"
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
            let resource =
                jobj.SelectToken("resources[?(@.name=='lng')]")
                :?> JObject
            let properties = (resource.Property "properties").Value :?> JObject
            let addressSpace = (properties.Property "localNetworkAddressSpace").Value :?> JObject
            let addressPrefixes = (addressSpace.Property "addressPrefixes").Value :?> JArray

            Expect.equal
                (string (resource.Property "type").Value)
                "Microsoft.Network/localNetworkGateways"
                "Incorrect resource type"
            Expect.equal
                (string (resource.Property "apiVersion").Value)
                "2021-02-01"
                "Incorrect API version"
            Expect.equal
                (string (properties.Property "gatewayIpAddress").Value)
                "10.0.0.4"
                "Incorrect gatewayIpAddress"
            Expect.hasLength addressPrefixes 1 "Incorrect address space prefixes count"
            Expect.equal
                (addressPrefixes.[0].Value<string>())
                "10.0.0.0/24"
                "Incorrect address space prefix"
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
            let resource =
                jobj.SelectToken("resources[?(@.name=='lng-bgp')]")
                :?> JObject
            let properties = (resource.Property "properties").Value :?> JObject
            let bgp = (properties.Property "bgpSettings").Value :?> JObject
            let peeringAddresses = (bgp.Property "bgpPeeringAddresses").Value :?> JArray
            let firstAddress = peeringAddresses.[0] :?> JObject
            let customAddresses = (firstAddress.Property "customBgpIpAddresses").Value :?> JArray

            Expect.equal
                (string (bgp.Property "asn").Value)
                "65010"
                "Incorrect BGP ASN"
            Expect.equal
                (string (bgp.Property "bgpPeeringAddress").Value)
                "192.168.1.1"
                "Incorrect BGP peering address"
            Expect.equal
                (string (bgp.Property "peerWeight").Value)
                "50"
                "Incorrect BGP peer weight"
            Expect.hasLength peeringAddresses 1 "Incorrect BGP peering addresses count"
            Expect.equal
                (string (firstAddress.Property "ipconfigurationId").Value)
                "ipconfig1"
                "Incorrect BGP peering IP configuration id"
            Expect.hasLength customAddresses 1 "Incorrect BGP peering address count"
            Expect.equal
                (customAddresses.[0].Value<string>())
                "192.168.1.1"
                "Incorrect BGP peering address"
        }
    ]
