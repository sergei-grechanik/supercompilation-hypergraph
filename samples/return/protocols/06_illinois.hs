data Number = Z | S Number;
data Action = R2 | R3 | R4A | R4B | R6 | R7 | R8 | R9 | R10 | R11;
data State = State Number Number Number Number;
data Boolean = True | False;
data List a = Nil | Cons a (List a);
data Result = Result Boolean Boolean Boolean Boolean Boolean Boolean Boolean;

(chState2 (loop (State (S i) Z Z Z) acts))
where

loop = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State invalid dirty shared exclusive -> loop (State invalid dirty shared exclusive) as;
	};
};

react = \state action -> case state of {State invalid dirty shared exclusive ->
	case action of {
		R2 -> r2 invalid dirty shared exclusive;
		R3 -> r3 invalid dirty shared exclusive;
		R4A -> r4a invalid dirty shared exclusive;
		R4B -> r4b invalid dirty shared exclusive;
		R6 -> r6 invalid dirty shared exclusive;
		R7 -> r7 invalid dirty shared exclusive;
		R8 -> r8 invalid dirty shared exclusive;
		R9 -> r9 invalid dirty shared exclusive;
		R10 -> r10 invalid dirty shared exclusive;
		R11 -> r11 invalid dirty shared exclusive;
	};
};

r2 = \i d s e -> case i of {
	S i1 -> 
	case d of {Z -> case s of { Z -> case e of { Z -> State i1 Z Z (S Z);};};};
};
r3 = \i d s e -> case i of {
	S i1 -> case d of {S d1 -> State i1 d1 (S (S s)) e;};
};
r4a = \i d s e -> case i of {
	S i1 -> case s of {S s1 -> State i1 d (add (S (S s1)) e) Z;};
};
r4b = \i d s e -> case i of {
	S i1 -> case e of {S e1 -> State i1 d (add (S (S s)) e1) Z;};
};
r6 = \i d s e -> case e of {
	S e1 -> State i (S d) s e1;
};
r7 = \i d s e -> case s of {
	S s1 -> State (add i s1) (S d) Z e;
};
r8 = \i d s e -> case i of {
	S i1 -> State (add i1 (add s (add s e))) (S Z) Z Z;
};
r9 = \i d s e -> case d of {
	S d1 -> State (S i) d1 s e;
};
r10 = \i d s e -> case s of {
	S s1 -> State (S i) d s1 e;
};
r11 = \i d s e -> case e of {
	S e1 -> State (S i) d s e1;
};

chState1 = \state -> case state of {State invalid dirty shared exclusive ->
	check1 invalid dirty shared exclusive;
};
chState2 = \state -> case state of {State invalid dirty shared exclusive ->
	check2 invalid dirty shared exclusive;
};
check1 = \i d s e -> case d of {S d1 -> case s of {S s1 -> False;Z->True;};Z->True;};
check2 = \i d s e -> case d of {S d1 -> case d1 of {S d2 -> False;Z->True;};Z->True;};
add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};