variable {p q r : Prop}

theorem imp_trans (hpq : p → q) (hqr : q → r) : p → r := by
  intro hp
  apply hqr  -- et pas hpq !
  apply hpq
  exact hp
