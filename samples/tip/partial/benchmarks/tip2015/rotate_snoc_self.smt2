f_snoc f_z f_y2 = (case f_y2 of {
                     C_nil -> (C_cons f_z (C_nil));
                     C_cons f_z2 f_ys -> (C_cons f_z2 (f_snoc f_z f_ys)); });
f_rotate f_z f_y2 = (case f_z of {
                       C_Z -> f_y2;
                       C_S f_z2 -> (case f_y2 of {
                                      C_nil -> (C_nil);
                                      C_cons f_x2 f_x3 -> (f_rotate f_z2 (f_snoc f_x2 f_x3)); }); });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
prove: (forall f_n f_xs . ((f_rotate f_n (f_append f_xs f_xs)) = (f_append (f_rotate f_n f_xs) (f_rotate f_n f_xs))));
