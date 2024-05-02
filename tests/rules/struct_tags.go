package main

type Rectangle struct {
    Top     int `json:"top"`
    //ruleid: match	
    Left     int `json:"left" bencode:"foo"`
    //ruleid: match	
    Width  int `bencode:"bar" json:"width"`
    Height int `json:"height"`
}