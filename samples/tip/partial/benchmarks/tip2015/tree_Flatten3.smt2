f_flatten3 f_z = (case f_z of {
                    C_Node f_y2 f_z2 f_r -> (case f_y2 of {
                                               C_Node f_p f_x2 f_q -> (f_flatten3 (C_Node f_p f_x2 (C_Node f_q f_z2 f_r)));
                                               C_Nil -> (C_cons f_z2 (f_flatten3 f_r)); });
                    C_Nil -> (C_nil); });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
f_flatten0 f_z = (case f_z of {
                    C_Node f_p f_y2 f_q -> (f_append (f_append (f_flatten0 f_p) (C_cons f_y2 (C_nil))) (f_flatten0 f_q));
                    C_Nil -> (C_nil); });
prove: (forall f_p . ((f_flatten3 f_p) = (f_flatten0 f_p)));
