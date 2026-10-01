[<AutoOpen>]
module Farmer.Builders.ApiManagement

open Farmer
open Farmer.Arm.ApiManagement

type ApiManagementConfig = {
    Name: ResourceName
    Location: Location
    PublisherName: string
    PublisherEmail: string
    Sku: ApiManagementSku
    Capacity: int
    Tags: Map<string, string>
} with

    interface IBuilder with
        member this.ResourceId = apiManagement.resourceId this.Name

        member this.BuildResources _ = [
            {
                ApiManagementService.Name = this.Name
                Location = this.Location
                PublisherName = this.PublisherName
                PublisherEmail = this.PublisherEmail
                Sku = this.Sku
                Capacity = this.Capacity
                Tags = this.Tags
            }
        ]

type ApiManagementBuilder() =
    member _.Yield _ = {
        Name = ResourceName.Empty
        Location = Location.WestEurope
        PublisherName = ""
        PublisherEmail = ""
        Sku = ApiManagementSku.Developer
        Capacity = 1
        Tags = Map.empty
    }

    [<CustomOperation "name">]
    member _.Name(state: ApiManagementConfig, name) = { state with Name = ResourceName name }

    [<CustomOperation "location">]
    member _.Location(state: ApiManagementConfig, location) = { state with Location = location }

    [<CustomOperation "publisher_name">]
    member _.PublisherName(state: ApiManagementConfig, publisherName) = {
        state with
            PublisherName = publisherName
    }

    [<CustomOperation "publisher_email">]
    member _.PublisherEmail(state: ApiManagementConfig, publisherEmail) = {
        state with
            PublisherEmail = publisherEmail
    }

    [<CustomOperation "sku">]
    member _.Sku(state: ApiManagementConfig, sku) = { state with Sku = sku }

    [<CustomOperation "capacity">]
    member _.Capacity(state: ApiManagementConfig, capacity) = { state with Capacity = capacity }

    [<CustomOperation "add_tags">]
    member _.AddTags(state: ApiManagementConfig, tags) = {
        state with
            Tags = state.Tags |> Map.merge tags
    }

    interface ITaggable<ApiManagementConfig> with
        member _.Add state tags = {
            state with
                Tags = state.Tags |> Map.merge tags
        }

let apiManagementService = ApiManagementBuilder()