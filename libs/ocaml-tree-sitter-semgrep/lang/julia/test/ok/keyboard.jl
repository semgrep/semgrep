module MouseButtons
export MouseButton
@enum MouseButton::UInt8 begin
    LEFT = 1
    MIDDLE = 2
    RIGHT = 3
    WHEEL_UP = 4
    WHEEL_DOWN = 5
end
end

using .MouseButtons

# This somewhat convoluted code below is to map julia code
# like `Keys.A` to the SDLK_* enums, without actually enumerating
# all the values in Julia. We used to do that, but that duplication
# was somewhat problematic. 

# A Keys.X invocation returns the keyscan code (an UInt32) corresponding
# to the SDLK_X enum defined in SDL2. 
# In contrast, a keyboard.X returns true or false depending of whether the X
# key is currently pressed. 
# Thus user code to check if X is pressed is written as
#       ` if game.keyboard.X`
const singleCharStrings = string.(collect('a':'z'))
struct KeyHolder
end
const Keys = KeyHolder()
function getproperty(k::KeyHolder, s::Symbol)
    st=string(s)
    if findfirst(x->x==lowercase(st), singleCharStrings) !== nothing
        st = lowercase(st)
    end
    st = "SDLK_" * st
    s=Symbol(st)
    try 
        return getproperty(GameZero.SimpleDirectMediaLayer.LibSDL2, s)
    catch 
        @error "Unknown key: $s"
        return nothing
    end 
end

struct Keyboard
    pressed::Array{UInt32, 1}
end

Keyboard() = Keyboard(Array{Any, 1}())

function getproperty(k::Keyboard, s::Symbol)
    return getproperty(GameZero.Keys, s) in getfield(k, :pressed)
end

push!(k::Keyboard, item) = push!(getfield(k, :pressed), item)
function delete!(k::Keyboard, item)
    a = getfield(k, :pressed)
    deleteat!(a, a.==item)
end
