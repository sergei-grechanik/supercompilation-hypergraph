data Nat = Z | S Nat;
data Action = R1 | R2 | R3 | R4 | R5 | R6;
-- x2 x3 x4 x5 x6 x7
data State = State Nat Nat Nat Nat Nat Nat;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

chState (loop1 (State (S Z) Z Z (S i) Z Z) acts) where

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State x2 x3 x4 x5 x6 x7  -> loop1 (State x2 x3 x4 x5 x6 x7) as;
	};
};

react = \state action -> case state of {State x2 x3 x4 x5 x6 x7 ->
	case action of {
		R1 -> r1 x2 x3 x4 x5 x6 x7;
		R2 -> r2 x2 x3 x4 x5 x6 x7;
		R3 -> r3 x2 x3 x4 x5 x6 x7;
		R4 -> r4 x2 x3 x4 x5 x6 x7;
		R5 -> r5 x2 x3 x4 x5 x6 x7;
		R6 -> r6 x2 x3 x4 x5 x6 x7;
	};
};
r1 = \x2 x3 x4 x5 x6 x7 -> 
	case x2 of {S x21 -> case x4 of {Z -> case x7 of {S x71 ->
		State x21 (S x3) x4 x5 x6 x71;
};};};
r2 = \x2 x3 x4 x5 x6 x7 -> 
	case x2 of {S x21 -> case x6 of {S x61 ->
		State (S x21) x3 (S x4) x5 x61 x7;
};};
r3 = \x2 x3 x4 x5 x6 x7 -> 
	case x3 of {S x31 -> 
		State x2 x31 x4 (S x5) x6 x7;
};
r4 = \x2 x3 x4 x5 x6 x7 -> 
	case x4 of {S x41 -> 
		State x2 x3 x41 (S x5) x6 x7;
};
r5 = \x2 x3 x4 x5 x6 x7 -> 
	case x5 of {S x51 -> 
		State x2 x3 x4 x51 (S x6) x7;
};
r6 = \x2 x3 x4 x5 x6 x7 -> 
	case x5 of {S x51 -> 
		State x2 x3 x4 x51 x6 (S x7);
};

chState = \state -> case state of {State x2 x3 x4 x5 x6 x7 -> 
	case x3 of {S x31 -> case x4 of {S x41 -> False;};};};