require 'securerandom'

generator = SecureRandom

#ERROR: match
generator.hex

#ERROR: match
test = generator.hex

#ERROR: match
puts generator.hex
