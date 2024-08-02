--------------------------------------------------------------------------------
Python -> OCaml RPC
--------------------------------------------------------------------------------

The mechanism implemented here and in `rpc.py` in pysemgrep allows us to
incrementally migrate functionality from Python to OCaml in support of our
osemgrep effort. Instead of being limited to the one main call out to
semgrep-core, we can now make additional calls from arbitrary points in the
Python code.

To add a function:

- Update the `function_call` and `function_return` types in
  `semgrep_output_v1.atd` in the `semgrep-interfaces` module.
- Update `RPC.ml` to handle the function call and return the appropriate value.
- Update `rpc_call.py` to make the function call.
- Call the function in `rpc_call.py` from wherever you need to in the Python code.

Suggested best practices:

- Do not perform any business logic in `rpc_call.py`. It should just construct the
  function call object, send it, receive the result, and return it.

Invariants/limitations:

- Function calls cannot be interleaved. The response to one call must be
  received before a second call is sent.
- We pay the performance cost of serializing and deserializing each argument and
  return value. That cost is negligible for some workloads and is likely
  unacceptable for others. Carefully consider how best to use this.
- Further, the semgrep-core rpc process handles only a single call and then
  dies. We could change that, but there would be a little bit more complexity
  involved with managing a long-lived child process. Starting a process each
  time adds some overhead, effectively preventing us from using this for
  functionality that requires numerous function calls from Python to OCaml.

RPC protocol details:

- [Python] Serialize the function call to a string using ATD.
- [Python] Send the string over the output channel:
  - Compute the size of the string in bytes.
  - Convert the size integer to a string, and terminate it with a newline.
  - Send the newline-terminated size string over the output channel.
  - Then send the payload itself.
- [OCaml] Read the call from the input channel, following the same protocol
  (size in bytes, terminated by newline, followed by payload).
- [OCaml] Compute the result of the function call.
- [OCaml] Serialize the result and send it following the same protocol.
- [Python] Read the result from the channel following the same protocol.

We use stdin and stdout here. This is the simplest option for communication
between the two processes. The main concern would be that we might inadvertently
print something on the OCaml process' stdout. However, we already use stdout for
the main call into semgrep-core from pysemgrep, so we are already disciplined
about printing to stdout.

Future work:
- Leave the RPC process running and let it serve multiple back-to-back calls.
- Provide some seamless way to print to the pysemgrep stdout from OCaml code.
