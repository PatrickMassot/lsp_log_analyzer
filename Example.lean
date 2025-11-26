variable {p q r : Prop}

theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  apply hqr
  apply hpq
  exact hp

theorem or_comm' (h : p ∨ q) : q ∨ p := by
  right
  assumption
