f_interleave f_z f_y2 = (case f_z of {
                           C_nil -> f_y2;
                           C_cons f_z2 f_xs -> (C_cons f_z2 (f_interleave f_y2 f_xs)); });
f_evens f_z = (case f_z of {
                 C_nil -> (C_nil);
                 C_cons f_y2 f_xs -> (C_cons f_y2 (f_odds f_xs)); });
f_odds f_z = (case f_z of {
                C_nil -> (C_nil);
                C_cons f_y2 f_xs -> (f_evens f_xs); });
prove: (forall f_xs . ((f_interleave (f_evens f_xs) (f_odds f_xs)) = f_xs));
