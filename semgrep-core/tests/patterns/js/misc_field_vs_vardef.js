//ERROR: match
x = {password: "str"}
x = {y: {password: 123}}
// this should not match here
x = {y: {password: "str"}}
x = {y: {password: {z: 123}}}
// this should also not match here
x = {y: {password: {z: "str"}}}
