variable "min_tls_version" {
  description = "The minimum supported TLS version for the storage account. Defaults to TLS1_2 (contrary to AZ default option)"
  type        = string
  //ERROR: match
  default     = "TLS1_2"
}

resource "azurerm_storage_account" "storage_account" {
  name                      = var.name
  //ERROR: match
  min_tls_version           = var.min_tls_version
}