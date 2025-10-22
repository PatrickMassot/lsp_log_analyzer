import LSPLogAnalyzer.Basic
import Lean.Data.Json
import Lean.Data.Lsp
-- import Lean.Data.JsonRpc

partial def collectMessages (stream : IO.FS.Stream) : IO (List Lean.JsonRpc.Message) := do
  try
    let m ← IO.FS.Stream.readLspMessage stream
    let tail ← collectMessages stream
    pure (m :: tail)
  catch _ => pure []

def messageSummary : Lean.JsonRpc.Message → String
| .request id method params? => toString method
| .notification method params? => toString method
| _ => "!!! unknown JsonRpc message type"

-- def dumpMessageTypes (messages : List Lean.JsonRpc.Messages)
