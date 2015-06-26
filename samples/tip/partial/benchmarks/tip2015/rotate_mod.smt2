f_take f_z f_y2 = (case f_z of {
                     C_S f_z2 -> (case f_y2 of {
                                    C_Cons f_x2 f_x3 -> (C_Cons f_x2 (f_take f_z2 f_x3));
                                    C_Nil -> (C_Nil); });
                     C_Z -> (C_Nil); });
f_minus f_z f_y2 = (case f_z of {
                      C_S f_z2 -> (case f_y2 of {
                                     C_S f_x2 -> (f_minus f_z2 f_x2);
                                     C_Z -> f_z; });
                      C_Z -> (C_Z); });
f_lt f_z f_y2 = (case f_y2 of {
                   C_S f_z2 -> (case f_z of {
                                  C_S f_x2 -> (f_lt f_x2 f_z2);
                                  C_Z -> (C_True); });
                   C_Z -> (C_False); });
f_mod2 f_z f_y2 = (case f_y2 of {
                     C_S f_z2 -> (case (f_lt f_z f_y2) of {
                                    C_True -> f_z;
                                    C_False -> (f_mod2 (f_minus f_z f_y2) f_y2); });
                     C_Z -> (C_Z); });
f_length f_z = (case f_z of {
                  C_Cons f_y2 f_xs -> (C_S (f_length f_xs));
                  C_Nil -> (C_Z); });
f_drop f_z f_y2 = (case f_z of {
                     C_S f_z2 -> (case f_y2 of {
                                    C_Cons f_x2 f_x3 -> (f_drop f_z2 f_x3);
                                    C_Nil -> (C_Nil); });
                     C_Z -> f_y2; });
f_append f_z f_y2 = (case f_z of {
                       C_Cons f_z2 f_xs -> (C_Cons f_z2 (f_append f_xs f_y2));
                       C_Nil -> f_y2; });
f_rotate f_z f_y2 = (case f_z of {
                       C_S f_z2 -> (case f_y2 of {
                                      C_Cons f_x2 f_x3 -> (f_rotate f_z2 (f_append f_x3 (C_Cons f_x2 (C_Nil))));
                                      C_Nil -> (C_Nil); });
                       C_Z -> f_y2; });
prove: (forall f_n f_xs . ((f_rotate f_n f_xs) = (f_append (f_drop (f_mod2 f_n (f_length f_xs)) f_xs) (f_take (f_mod2 f_n (f_length f_xs)) f_xs))));
