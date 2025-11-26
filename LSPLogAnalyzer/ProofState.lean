import Lean
open Lean Server Elab

def parseDocument (doc : Lsp.TextDocumentItem) : CoreM Lean.Syntax := do
  let env ← getEnv
  let stx?: Except String Lean.Syntax :=
    Lean.Parser.runParserCategory env `term doc.text
  Lean.ofExcept stx?

def parseString (text : String) : CoreM Lean.Syntax := do
  let env ← getEnv
  let stx?: Except String Lean.Syntax :=
    Lean.Parser.runParserCategory env `command text
  Lean.ofExcept stx?

def myText := "
variable {p q r : Prop}

theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  apply hqr  -- et pas hpq !
  apply hpq
  exact hp
"

def myText2 := "
theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  apply hqr  -- et pas hpq !
  apply hpq
  exact hp
"

#eval parseString myText2

-- def collectGoals (doc : Lsp.TextDocumentItem) : IO (List (List MVarId)) := do

--   -- Build server context
--   let serverCtx ← mkInitContext {}
--   let docCtx ← FileWorker.mkInitialDocumentContext tdItem serverCtx
--   -- Compile the file, building the info tree
--   let snap ← FileWorker.compileNextCmdSnapshots docCtx
--   -- Traverse the info tree for goals
--   let rec collect (t : InfoTree) (acc : List (List MVarId)) :=
--     match t with
--     | InfoTree.node i cs =>
--       match i with
--       | Info.ofTacticInfo ti => collectGoalsInTacticInfo ti acc
--       | _ => cs.foldl (fun acc' c => collect c acc') acc
--     | InfoTree.context _ c  => collect c acc
--     | _ => acc
--   -- You have to implement collectGoalsInTacticInfo, extracting the goals from the tactic info node.
--   pure (collect snap.infoTree [])
