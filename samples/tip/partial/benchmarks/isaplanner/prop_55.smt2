f_minus f_z f_y2 = (case f_z of {
                      C_Z -> (C_Z);
                      C_S f_z2 -> (case f_y2 of {
                                     C_Z -> f_z;
                                     C_S f_x2 -> (f_minus f_z2 f_x2); }); });
f_len f_z = (case f_z of {
               C_nil -> (C_Z);
               C_cons f_y2 f_xs -> (C_S (f_len f_xs)); });
f_drop f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_z2 -> (case f_y2 of {
                                    C_nil -> (C_nil);
                                    C_cons f_x2 f_x3 -> (f_drop f_z2 f_x3); }); });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
prove: (forall f_n f_xs f_ys . ((f_drop f_n (f_append f_xs f_ys)) = (f_append (f_drop f_n f_xs) (f_drop (f_minus f_n (f_len f_xs)) f_ys))));
