[<AutoOpen>]
module Farmer.Arm.ApiManagement

open Farmer

let apiManagement = ResourceType("Microsoft.ApiManagement/service", "2021-08-01")

[<RequireQualifiedAccess>]
type ApiManagementSku =
    | Consumption
    | Developer
    | Basic
    | Standard
    | Premium

    member this.ArmValue =
        match this with
        | Consumption -> "Consumption"
        | Developer -> "Developer"
        | Basic -> "Basic"
        | Standard -> "Standard"
        | Premium -> "Premium"

type ApiManagementService = {
    Name: ResourceName
    Location: Location
    PublisherName: string
    PublisherEmail: string
    Sku: ApiManagementSku
    Capacity: int
    Tags: Map<string, string>
} with

    interface IArmResource with
        member this.ResourceId = apiManagement.resourceId this.Name

        member this.JsonModel = {|
            apiManagement.Create(this.Name, this.Location, tags = this.Tags) with
                sku = {|
                    name = this.Sku.ArmValue
                    capacity = this.Capacity
                |}
                properties = {|
                    publisherName = this.PublisherName
                    publisherEmail = this.PublisherEmail
                |}
        |}