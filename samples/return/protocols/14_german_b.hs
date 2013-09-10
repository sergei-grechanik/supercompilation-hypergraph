data Nat = Z | S Nat;
data Action = ReqS | ReqE1 | ReqE2 | Inv | Nonex1 | AInvS | AInvE | Nonex2 |
			AGrantS | AGrantE | ServG | EE1 | EE2;
--                 server bool null  waitS shared waitE Exclusive
data State = State Server Bool Nat   Nat   Nat    Nat   Nat;
data Server = Idle | ServS | ServE | InvE | GrantS | GrantE;
data Bool = Yes | No;
data Boolean = True | False;
data Trans = NotBusy | Busy;
data List a = Nil | Cons a (List a);

chState1 (loop1 (State Idle No (S n) Z Z Z Z) (f acts)) where

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State ser b n wS s wE e -> loop1 (State ser b n wS s wE e) as;
	};
};
react = \state action -> case state of {State ser b n wS s wE e ->
	case action of {
		ReqS -> reqs ser b n wS s wE e;
		ReqE1 -> reqE1 ser b n wS s wE e;
		ReqE2 -> reqE2 ser b n wS s wE e;
		Inv -> inv ser b n wS s wE e;
		Nonex1 -> nonex1 ser b n wS s wE e;
		AInvS -> invS ser b n wS s wE e;
		AInvE -> invE ser b n wS s wE e;
		Nonex2 -> nonex2 ser b n wS s wE e;
		AGrantS -> grantS ser b n wS s wE e;
		AGrantE -> grantE ser b n wS s wE e;
	};};
reqs = \ser b n wS s wE e -> 
	case ser of {Idle -> case n of {S n1 ->
		State ServS b n1 (S wS) s wE e;
	};};
reqE1 = \ser b n wS s wE e -> 
	case ser of {Idle -> case n of {S n1 ->
		State ServE b n1 wS s (S wE) e;
	};};
reqE2 = \ser b n wS s wE e -> 
	case ser of {Idle -> case s of {S s1 ->
		State ServE b n wS s1 (S wE) e;
	};};
inv = \ser b n wS s wE e -> 
	case ser of {ServS -> case b of {Yes -> case e of {S e1 ->
		State GrantS No (S n) wS s wE e1;
	};};};
nonex1 = \ser b n wS s wE e -> 
	case ser of {ServS -> case b of {No ->
		State GrantS No n wS s wE e;
	};};
invS = \ser b n wS s wE e -> 
	case ser of {ServE -> 
		State InvE b (add n s) wS Z wE e;
	};
invE = \ser b n wS s wE e -> 
	case ser of {InvE -> case b of {Yes -> case e of {S e1 ->
		State GrantE No (S n) wS s wE e1;
	};};};
nonex2 = \ser b n wS s wE e -> 
	case ser of {InvE -> case b of {No ->
		State GrantE No n wS s wE e;
	};};
grantS = \ser b n wS s wE e -> 
	case ser of {GrantS -> case wS of {S wS1 ->
		State Idle b n wS1 (S s) wE e;
	};};
grantE = \ser b n wS s wE e -> 
	case ser of {GrantE -> case wE of {S wE1 ->
		State Idle Yes n wS s wE1 (S e);
	};};
chState = \state -> case state of {State ser b n wS s wE e -> 
	case e of {S e1 -> case e1 of {
		S e2 -> False;
		Z -> case s of {S s1 -> False;};};};};
chState1 = \state -> case state of {State ser b n wS s wE e -> 
	case e of {S e1 -> case e1 of {S e2 -> False;};};};
chState2 = \state -> case state of {State ser b n wS s wE e -> 
	case e of {S e1 -> case s of {S s1 -> False;};};};
--add = \x y -> case x of { Z -> y; S x1 -> S (add x1 y);};
add = \x y -> case x of { Z -> y; S x1 -> (add x1 (S y));};