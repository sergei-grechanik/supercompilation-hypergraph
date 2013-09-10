data Nat = Z | S Nat;
data Action = RM1 | RM21 | RM22 | RM23 | RM24 | 
	WM1 | WM21 | WM22 | WM23 | WM24 | WH1 | WH2 | WH3 | WH4;
-- Invalid shared_clean shared_dirty dirty Exclusive
data State = State Nat Nat Nat Nat Nat;
data Boolean = True | False;
data List a = Nil | Cons a (List a);

(chState1 (loop1 (State (S i) Z Z Z Z) (f s))) where

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State i sc sd d e -> loop1 (State i sc sd d e) as;
	};
};

react = \state action -> case state of {State i sc sd d e ->
	case action of {
		RM1 -> rm1 i sc sd d e;
		RM21 -> rm21 i sc sd d e;
		RM22 -> rm22 i sc sd d e;
		RM23 -> rm23 i sc sd d e;
		RM24 -> rm24 i sc sd d e;
		WM1 -> wm1 i sc sd d e;
		WM21 -> wm21 i sc sd d e;
		WM22 -> wm22 i sc sd d e;
		WM23 -> wm23 i sc sd d e;
		WM24 -> wm24 i sc sd d e;
		WH1 -> wh1 i sc sd d e;
		WH2 -> wh2 i sc sd d e;
		WH3 -> wh3 i sc sd d e;
	};
};
rm1 = \i sc sd d e -> case i of {
	S i1 -> case sc of {
		Z -> case sd of {
			Z -> case d of {
				Z -> case e of {
					Z -> State i1 sc sd d (S e);
				};
			};
		};
	};	
};
rm21 = \i sc sd d e -> case i of {
	S i1 -> case sc of {S n1 -> 
		State i1 (add sc e) (add sd d) Z Z;};};
rm22 = \i sc sd d e -> case i of {
	S i1 -> case sd of {S n1 -> 
		State i1 (add sc e) (add sd d) Z Z;};};
rm23 = \i sc sd d e -> case i of {
	S i1 -> case d of {S sc1 -> 
		State i1 (add sc e) (add sd d) Z Z;};};
rm24 = \i sc sd d e -> case i of {
	S i1 -> case e of {S sc1 -> 
		State i1 (add sc e) (add sd d) Z Z;};};
wm1 = \i sc sd d e -> case i of {
	S i1 -> case sc of {
		Z -> case sd of {
			Z -> case d of {
				Z -> case e of {
					Z -> State i1 sc sd (S d) e;
				};
			};
		};
	};	
};
-- sd??
wm21 = \i sc sd d e -> case i of {
	S i1 -> case sc of {S n1 -> 
		State i1 (add sc (add sd (add d e))) (S Z) Z Z;};};
wm22 = \i sc sd d e -> case i of {
	S i1 -> case sd of {S n1 -> 
		State i1 (add sc (add sd (add d e))) (S Z) Z Z;};};
wm23 = \i sc sd d e -> case i of {
	S i1 -> case d of {S n1 -> 
		State i1 (add sc (add sd (add d e))) (S Z) Z Z;};};
wm24 = \i sc sd d e -> case i of {
	S i1 -> case e of {S n1 -> 
		State i1 (add sc (add sd (add d e))) (S Z) Z Z;};};
wh1 = \i sc sd d e -> case d of {
	S d1 -> State (S i) sc sd d1 e; 
};
wh2 = \i sc sd d e -> case sc of {
	S sc1 -> State (S i) sc1 sd d e; 
};
wh3 = \i sc sd d e -> case sd of {
	S sd1 -> State (S i) sc sd1 d e; 
};
wh4 = \i sc sd d e -> case e of {
	S e1 -> State (S i) sc sd d e1; 
};

chState1 = \state -> case state of {State i sc sd d e -> 
	case d of {S d1 -> case e of {S e1 -> False;};};};

chState2 = \state -> case state of {State i sc sd d e -> 
	case d of {S d1 -> case sc of {S e1 -> False;};};};
chState3 = \state -> case state of {State i sc sd d e -> 
	case d of {S d1 -> case sd of {S e1 -> False;};};};
--
chState4 = \state -> case state of {State i sc sd d e -> 
	case e of {S d1 -> case sc of {S e1 -> False;};};};
chState5 = \state -> case state of {State i sc sd d e -> 
	case e of {S d1 -> case sd of {S e1 -> False;};};};
--
chState6 = \state -> case state of {State i sc sd d e -> 
	case d of {S d1 -> case d1 of {S d2 -> False;};};};
chState7 = \state -> case state of {State i sc sd d e -> 
	case e of {S e1 -> case e1 of {S e2 -> False;};};};
	
add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};