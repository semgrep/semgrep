package main

type Rectangle struct {
    Top     int `json:"top"`
    Left     int `json:"left" bencode:"foo"`
    Width  int `bencode:"bar" json:"width"`
    Height int `json:"height"`
}