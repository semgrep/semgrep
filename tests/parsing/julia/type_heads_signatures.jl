abstract type AbstractBox{T} <: Parent{T} end
primitive type Word{T} <: Number 32 end
mutable struct Box{T} <: AbstractBox{T}
    const value::T
    const tag = :box
    other::Int
end
function convert_value(x::T, y=1; flag=true)::T where {T <: Number}
    x
end
function (x::T)::T where T
    x
end
function declared end
