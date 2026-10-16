#!/usr/bin/env lua
local café = 1
local next, self, _G, _VERSION = f()
next(self)
local global = 2
function global() end
x = t.global
