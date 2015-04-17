eval :: BExpr -> Bool
data BExpr = F | T | Not BExpr
			| BExpr :&: BExpr
			| BExpr :|: BExpr

eval T = True
eval F = False
eval (Not x) = not (eval x)
eval (x :&: y) = eval x && eval y
eval (x :|: y) = eval x || eval y
