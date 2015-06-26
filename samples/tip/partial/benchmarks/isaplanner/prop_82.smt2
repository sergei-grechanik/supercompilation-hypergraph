f_zip f_z f_y2 = (case f_z of {
                    C_nil -> (C_nil);
                    C_cons f_z2 f_x2 -> (case f_y2 of {
                                           C_nil -> (C_nil);
                                           C_cons f_x3 f_x4 -> (C_cons (C_Pair2 f_z2 f_x3) (f_zip f_x2 f_x4)); }); });
f_take f_z f_y2 = (case f_z of {
                     C_Z -> (C_nil);
                     C_S f_z2 -> (case f_y2 of {
                                    C_nil -> (C_nil);
                                    C_cons f_x2 f_x3 -> (C_cons f_x2 (f_take f_z2 f_x3)); }); });
prove: (forall f_n f_xs f_ys . ((f_take f_n (f_zip f_xs f_ys)) = (f_zip (f_take f_n f_xs) (f_take f_n f_ys))));
