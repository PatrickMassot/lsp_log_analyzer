À chaque frappe de touche / modification :
- notification 'textDocument/didChange'
- suivie de plusieurs request '$/lean/rpc/call :
    - Lean.Widget.getInteractiveGoals
    - Lean.Widget.getInteractiveTermGoal
    - Lean.Widget.getWidgets
    - Lean.Widget.getInteractiveDiagnostics

Regarder "Received notification 'textDocument/publishDiagnostics'"
- liste des messages