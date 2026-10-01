module ApiManagement

open Expecto
open Farmer
open Farmer.Builders.ApiManagement
open Farmer.Arm.ApiManagement
open Newtonsoft.Json.Linq

let tests =
    testList "ApiManagement" [
        test "Creates an API Management service" {
            let service = apiManagementService {
                name "apim"
                location Location.NorthEurope
                publisher_name "Farmer"
                publisher_email "farmer@example.com"
                sku ApiManagementSku.Basic
                capacity 1
                add_tags [ "environment", "test" ]
            }

            let deployment = arm { add_resource service }
            let json = deployment.Template |> Writer.toJson |> JObject.Parse
            let resource = json.SelectToken("resources[?(@.name=='apim')]")

            Expect.equal (resource.SelectToken("type").ToString()) "Microsoft.ApiManagement/service" "Incorrect type"
            Expect.equal (resource.SelectToken("apiVersion").ToString()) "2021-08-01" "Incorrect API version"
            Expect.equal (resource.SelectToken("location").ToString()) "northeurope" "Incorrect location"
            Expect.equal (resource.SelectToken("sku.name").ToString()) "Basic" "Incorrect SKU"
            Expect.equal (resource.SelectToken("sku.capacity").ToString()) "1" "Incorrect capacity"

            Expect.equal
                (resource.SelectToken("properties.publisherName").ToString())
                "Farmer"
                "Incorrect publisher name"

            Expect.equal
                (resource.SelectToken("properties.publisherEmail").ToString())
                "farmer@example.com"
                "Incorrect publisher email"

            Expect.equal (resource.SelectToken("tags.environment").ToString()) "test" "Incorrect tag"
        }
    ]