data Number = Z | S Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);
data State = State Number Number Number;
data Action = R1 | R2 | R3 | R4 ;

-- TODO: workaround for [f actions] 
check (loop1 (State out Z Z) xxx) where

loop = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> loop (react state a) as;
};

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State out cs scs -> loop1 (State out cs scs) as;
	};
};

react = \state action -> case state of {State out cs scs ->
	case action of {
		R1 -> r1 out cs scs;
		R2 -> r2 out cs scs;
		R3 -> r3 out cs scs;
		R4 -> r4 out cs scs;
	};
};

r1 = \out cs scs ->
	case out of {
		S out1 -> 
			case cs of {
				Z -> 
					case scs of {
						Z -> State out1 (S Z) Z;
					};
			};
	};
	
r2 = \out cs scs ->
	case out of {
		S out1 ->
			case cs of {
				Z -> State out1 Z (S scs);
			};
	};
	
r3 = \out cs scs ->
	case cs of {
		S cs1 -> State (S out) cs1 scs; 
	};
	
r4 = \out cs scs ->
	case scs of {
		S scs1 -> State (S out) cs scs1; 
	};
	
check = \state -> case state of {State out cs scs ->
	case cs of {
		S cs1 -> case scs of {S scs1 -> False; Z -> True;};
		Z -> True;
	};
};