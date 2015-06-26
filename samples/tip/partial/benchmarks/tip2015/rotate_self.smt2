f_append f_z f_y2 = (case f_z of {
                       C_Cons f_z2 f_xs -> (C_Cons f_z2 (f_append f_xs f_y2));
                       C_Nil -> f_y2; });
f_rotate f_z f_y2 = (case f_z of {
                       C_S f_z2 -> (case f_y2 of {
                                      C_Cons f_x2 f_x3 -> (f_rotate f_z2 (f_append f_x3 (C_Cons f_x2 (C_Nil))));
                                      C_Nil -> (C_Nil); });
                       C_Z -> f_y2; });
prove: (forall f_n f_xs . ((f_rotate f_n (f_append f_xs f_xs)) = (f_append (f_rotate f_n f_xs) (f_rotate f_n f_xs))));
