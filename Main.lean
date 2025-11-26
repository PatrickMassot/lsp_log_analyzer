import LSPLogAnalyzer


def fileStream (filename : System.FilePath) : IO IO.FS.Stream := do
  let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
  pure (IO.FS.Stream.ofHandle handle)


partial def process
    (stream : IO.FS.Stream)
    (processor : String → Except String α)
    : IO (List α) := do
  match (← stream.getLine) with
  | "" => pure []
  | line =>
      let head := processor line
      let tail ← process stream processor
      match head with
      | .error e =>
          (← IO.getStderr).putStrLn e
          return tail
      | .ok entry =>
          return entry :: tail


def main (args : List String) : IO UInt32 := do
  match args with
  | ["pp", filename] =>
      -- only on new-style logs
      let stream ← fileStream filename
      let entries ← process stream parseLogLine
      entries.forM IO.println
      return 0
  | ["collect", filename] =>
      -- only on old-style logs
      let stream ← fileStream filename
      let messages ← collectMessages stream
      let summaries := messages.map messageSummary
      summaries.forM IO.println
      return 0
  | ["states", filename] =>
      -- only on old-style logs
      let stream ← fileStream filename
      let messages ← collectMessages stream
      match buildDocumentStates messages with
      | .ok states =>
          IO.println states
          pure 0
      | .error e =>
          IO.println e
          pure 1
  | _ => return 1
