data Nat = Z | S Nat;
data Action = GF | PF | GS | PS | RQ | REL | GO;
--                 trans race busy idle owner handin handout wait
data State = State Trans Race Nat  Nat  Nat   Nat    Nat     Nat;
data Race = H0 | H1 | H2 | H3;
data Boolean = True | False;
data Trans = NotBusy | Busy;
data List a = Nil | Cons a (List a);

chState (loop1 (State NotBusy H0 Z (S i) Z Z Z Z) acts) where

loop1 = \state actions -> case actions of {
	Nil -> state;
	Cons a as -> case (react state a) of {
		State tr r b i o hin hout w  -> loop1 (State tr r b i o hin hout w) as;
	};
};
react = \state action -> case state of {State tr r b i o hin hout w ->
	case action of {
		GF -> gf tr r b i o hin hout w;
		PF -> pf tr r b i o hin hout w;
		GS -> gs tr r b i o hin hout w;
		PS -> ps tr r b i o hin hout w;
		RQ -> rq tr r b i o hin hout w;
		REL -> rel tr r b i o hin hout w;
		GO -> go tr r b i o hin hout w;
	};
};
gf = \tr r b i o hin hout w -> 
	case tr of {NotBusy -> case i of {S i1 ->
		State Busy r Z i1 (S o) hin hout w; 
	};};
pf = \tr r b i o hin hout w ->
	case tr of {Busy -> case b of {Z -> case o of {S o1 ->
		State NotBusy r Z (S i) o1 hin hout w;
};};};
gs = \tr r b i o hin hout w -> 
	case tr of {Busy -> case i of {S i1 ->
		State Busy r (S b) i1 o (S hin) hout w; 
	};};
ps = \tr r b i o hin hout w ->
	case tr of {Busy -> case b of {S b1 -> case o of {S o1 ->
		State Busy r b1 i o1 hin (S hout) w;
};};};
rq = \tr r b i o hin hout w ->
	case hin of {S hin1 -> 
		case r of {
			H0 -> State tr H1 b i o hin1 hout (S w);
			H2 -> State tr H3 b i o hin1 hout (S w);
		};};
rel = \tr r b i o hin hout w ->
	case hout of {S hout1 -> 
		case r of {
			H0 -> State tr H2 b (S i) o hin hout1 w;
			H1 -> State tr H3 b (S i) o hin hout1 w;
		};};
go = \tr r b i o hin hout w ->
	case w of {S w1 -> 
		case r of {
			H3 -> State tr H3 b i (S o) hin hout w1;
		};};
chState = \state -> case state of {State tr r b i o hin hout w -> 
	case o of {
		Z -> case hout of {
			S hout1 -> case hout1 of {S hout2 -> False;};};
		S o1 -> case o1 of {
			Z -> case hout of {S hout1 -> False;};
			S o2 -> False;
		};
	};
};