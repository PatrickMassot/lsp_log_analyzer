import LSPLogAnalyzer

open LSPLogAnalyzer

def main (args : List String) : IO UInt32 := do
  match args with
  | ["pp", filename] =>
      -- only on new-style logs
      let stream ← fileStream filename
      let entries ← collectLogEntries stream (fun _ => true)
      entries.forM IO.println
      return 0
  | ["collect", filename] =>
      -- only on old-style logs
      let stream ← fileStream filename
      let messages ← collectMessages stream (fun _ => true)
      let summaries := messages.map messageSummary
      summaries.forM IO.println
      return 0
  | ["states", filename] =>
      -- only on old-style logs
      let stream ← fileStream filename
      let messages ← collectMessages stream (fun _ => true)
      match buildDocumentStates messages with
      | .ok states =>
          IO.println states
          pure 0
      | .error e =>
          IO.println e
          pure 1
  | _ => return 1
