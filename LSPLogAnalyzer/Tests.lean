import LSPLogAnalyzer

open LSPLogAnalyzer

def noFilter (_ : α) := true

#eval show _ from do
    let exampleStream ← fileStream "logs/LSP_2025-11-25-16-54-08-9505+0100.log"
    let entries ← collectLogEntries exampleStream noFilter
    let action := entries.forM processLogEntry
    let (_, st) ← action.run {}
    return st
