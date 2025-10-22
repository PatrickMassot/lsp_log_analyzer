import LSPLogAnalyzer

def main : IO Unit := do
  let input ← IO.getStdin
  let messages ← collectMessages input
  -- dumpMessages messages
  let summaries := messages.map messageSummary
  IO.println (Lean.Json.pretty messages.toJson)
  pure ()
