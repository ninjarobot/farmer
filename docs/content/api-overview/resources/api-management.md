---
title: "API Management"
---

The API Management builder creates an Azure API Management service. The builder currently creates the service resource; APIs, products, and other child resources can be added separately as Farmer support expands.

## Builder operations

| Keyword | Description |
| --- | --- |
| `name` | Sets the API Management service name. |
| `location` | Sets the Azure region. |
| `publisher_name` | Sets the publisher name shown by the service. |
| `publisher_email` | Sets the publisher email address. |
| `sku` | Sets the service tier (`Consumption`, `Developer`, `Basic`, `Standard`, or `Premium`). |
| `capacity` | Sets the number of units for the selected SKU. |

Example:

```fsharp
open Farmer
open Farmer.Builders.ApiManagement

let apim =
    apiManagementService {
        name "my-api-management"
        location Location.WestEurope
        publisher_name "Contoso"
        publisher_email "api-admin@contoso.com"
        sku Developer
        capacity 1
    }

let template = arm { add_resource apim }
```
