import Lean.Data.JsonRpc
import Lean.Data.Lsp
import Lean.Data.RBMap
import Lean.Server.Logging

namespace LSPLogAnalyzer

open Lean.Json Lean.ToJson Lean.FromJson Lean.JsonRpc Lean.Lsp
open Std.Time

abbrev Uri := String

local instance : Lean.ToJson ZonedDateTime where
  toJson dt := dt.toISO8601String

local instance : Lean.FromJson ZonedDateTime where
  fromJson?
    | .str s => ZonedDateTime.fromISO8601String s
    | _ => throw "Expected string when converting JSON to ZonedDateTime"


section Structures

/-- LSP log entry (new format). -/
structure LogEntry where
  time : ZonedDateTime
  direction : MessageDirection
  kind : MessageKind
  msg : Message
  deriving Lean.FromJson, Lean.ToJson

instance : ToString LogEntry where
  toString le := pretty $ toJson le

instance : ToString LogEntry where
  toString le := pretty $ toJson le

instance : Coe LogEntry Message := ⟨LogEntry.msg⟩


/-- File change event. -/
structure ChangeEvent where
  time : ZonedDateTime
  change : DidChangeTextDocumentParams

instance : ToString ChangeEvent where
  toString ev := s!"[{ev.time}]"


/-- File snapshot. -/
structure Snapshot where
  time : ZonedDateTime
  doc : TextDocumentItem

instance : ToString Snapshot where
  toString snap := s!"[{snap.time} - version {snap.doc.version}]\n{snap.doc.text}"


/-- Per-file replay state. -/
structure FileState where
  snapshots : Array Snapshot := #[]
  changes : Array ChangeEvent := #[]
  diagnostics : Array Diagnostic := #[]
deriving Inhabited

instance : ToString FileState where
  toString fs :=
    let ssnaps := String.intercalate "\n" $ fs.snapshots.toList.map toString
    let schanges := String.intercalate "\n" $ fs.changes.toList.map toString
    "Snapshots:\n\n" ++ ssnaps
    ++ "\nPending changes:\n" ++ schanges

/-- Log entry tracking error. -/
structure TrackingError where
  message : String
  entry : LogEntry

instance : ToString TrackingError where
  toString te := te.message


/-- Global tracker. -/
structure Tracker where
  files : Std.TreeMap Uri FileState Ord.compare := {}
  errors : Array TrackingError := #[]
deriving Inhabited

instance : ToString Tracker where
  toString tracker :=
    String.intercalate "\n" $ tracker.files.toList.map aux
    where aux
    | (uri, fs) => s!"State for file {uri}:\n\n" ++ toString fs

/-- Tracker monad. -/
abbrev TrackerM := StateT Tracker IO

end Structures

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

def ensureFile (doc : TextDocumentItem) : TrackerM FileState := do
  let st ← get
  match st.files.get? doc.uri with
  | some fs => return fs
  | none =>
    let fs : FileState := {}
    set { st with files := st.files.insert doc.uri fs }
    return fs

def onError (e : String) (entry : LogEntry) : TrackerM Unit := do
  let st ← get
  let message := s!"Error while processing entry : {e} "
  set { st with errors := st.errors.push ⟨message, entry⟩ }

def onDidOpen (time : ZonedDateTime) (params : DidOpenTextDocumentParams) : TrackerM Unit := do
  let doc := params.textDocument
  let fs ← ensureFile doc
  let fs := { fs with
    snapshots := fs.snapshots.push ⟨time, doc⟩
    changes := #[] }
  let st ← get
  set { st with files := st.files.insert doc.uri fs }

def processLogEntry (entry : LogEntry) : TrackerM Unit := do
  match entry.direction with
  | .clientToServer =>
    match entry.msg with
    | .notification "textDocument/didOpen" (some params) =>
        match fromJson? (toJson params) with
        | .ok params => onDidOpen entry.time params
        | .error e => onError e entry
    | .request id method params? => return ()
    | _ => return ()
  | .serverToClient => return ()


def parseLogLine (line : String) : Except String LogEntry := do
  let j ← parse line
  Lean.fromJson? j

def getUri (msg : Message) : Option String :=
    match msg with
    | .request _ _ (some params)
    | .notification _ (some params) =>
        match do
          let params := params.toJson
          let textDoc ← params.getObjVal? "textDocument"
          let uri ← textDoc.getObjVal? "uri"
          uri.getStr?
        with
        | .ok s => some s
        | _ => none
    | _ => none

def exampleMessageFilter (msg : Message) : Bool :=
  match msg with
  | .request _ "initialize" .. => true
  | .notification "textDocument/didOpen" ..
  | .notification "textDocument/didChange" .. =>
    match getUri msg with
    | none => false
    | some s => s.endsWith "Example.lean"
  | _ => false

def messageSummary : Message → String
  | .request id method _params? => s!"Request: {id} {method}"
  | .notification method _params? => s!"Notification: {method}"
  | .response id _result => s!"Response: {id}"
  | .responseError id _errCode _msg _params? => s!"Response error: {id}"

def logEntrySummary (e : LogEntry) : String := messageSummary e.msg

partial def collectMessages (stream : IO.FS.Stream) (filter : Message → Bool) : IO (List Message) := do
  try
    let msg ← IO.FS.Stream.readLspMessage stream
    let tail ← collectMessages stream filter
    if filter msg then
      pure (msg :: tail)
    else
      pure tail
  catch e =>
    if e.toString.endsWith "Stream was closed" then
      pure []
    else
      let stderr ← IO.getStderr
      stderr.putStrLn s!"{e}"
      collectMessages stream filter

partial def collectLogEntries (stream : IO.FS.Stream) (filter : Message → Bool) : IO (List LogEntry) := do
  match (← stream.getLine) with
  | "" => pure []
  | line =>
      let j ← IO.ofExcept $ parse line
      let entry : Except String LogEntry := Lean.fromJson? j
      match entry with
      | .error e =>
          (← IO.getStderr).putStrLn e
          tail
      | .ok entry =>
          if filter entry then
            return entry :: (← tail)
          else
            tail
      where tail := collectLogEntries stream filter


-- def dumpMessageTypes (messages : List Messages)
