f_flatten2 f_z f_y2 = (case f_z of {
                         C_Node f_p f_z2 f_q -> (f_flatten2 f_p (C_cons f_z2 (f_flatten2 f_q f_y2)));
                         C_Nil -> f_y2; });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
f_flatten0 f_z = (case f_z of {
                    C_Node f_p f_y2 f_q -> (f_append (f_append (f_flatten0 f_p) (C_cons f_y2 (C_nil))) (f_flatten0 f_q));
                    C_Nil -> (C_nil); });
prove: (forall f_p . ((f_flatten2 f_p (C_nil)) = (f_flatten0 f_p)));
