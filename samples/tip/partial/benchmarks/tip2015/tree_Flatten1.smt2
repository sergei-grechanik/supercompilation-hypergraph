f_flatten1 f_z = (case f_z of {
                    C_nil -> (C_nil);
                    C_cons f_y2 f_ps -> (case f_y2 of {
                                           C_Node f_z2 f_x2 f_q -> (case f_z2 of {
                                                                      C_Node f_x3 f_x4 f_x5 -> (f_flatten1 (C_cons f_z2 (C_cons (C_Node (C_Nil) f_x2 f_q) f_ps)));
                                                                      C_Nil -> (C_cons f_x2 (f_flatten1 (C_cons f_q f_ps))); });
                                           C_Nil -> (f_flatten1 f_ps); }); });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
f_flatten0 f_z = (case f_z of {
                    C_Node f_p f_y2 f_q -> (f_append (f_append (f_flatten0 f_p) (C_cons f_y2 (C_nil))) (f_flatten0 f_q));
                    C_Nil -> (C_nil); });
prove: (forall f_p . ((f_flatten1 (C_cons f_p (C_nil))) = (f_flatten0 f_p)));
