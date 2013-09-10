data Number = Z | S Number;
data Action = PrWr1 | PrWr2 | PrRd;
data State = State Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

-- TODO: workaround (f s)
check (loop (State (S i) Z Z) (f s)) where

loop = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> loop (react state a) as;
};

react = \state action -> case state of {State invalid modified shared ->
	case action of {
		PrWr1 -> prwr1 invalid modified shared;
		PrWr2 -> prwr2 invalid modified shared;
		PrRd -> prrd invalid modified shared;
	};
};

prwr1 = \invalid modified shared ->
	case invalid of {
		S i -> State (add (add i modified) shared) (S Z) Z;
	};
	
prwr2 = \invalid modified shared ->
	case shared of {
		S s -> State (add (add s modified) shared) (S Z) Z;
	};
	
prrd = \invalid modified shared ->
	case invalid of {
		S i -> State i Z (S (add modified shared));
	};
	


check = \state -> case state of {State invalid modified shared ->
	case modified of {
		S m1 -> 
			case m1 of {
				S m2 -> False;
				Z -> 
					case shared of {
						S s1 -> False;
						Z -> True;
					};
			};
		Z -> True;
	};
};

add = \x y ->
  case x of {
    Z -> y;
    S x1 -> S (add x1 y);
};