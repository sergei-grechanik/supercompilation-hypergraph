f_zip f_z f_y2 = (case f_z of {
                    C_nil -> (C_nil);
                    C_cons f_z2 f_x2 -> (case f_y2 of {
                                           C_nil -> (C_nil);
                                           C_cons f_x3 f_x4 -> (C_cons (C_Pair2 f_z2 f_x3) (f_zip f_x2 f_x4)); }); });
f_zipConcat f_z f_y2 f_z2 = (case f_z2 of {
                               C_nil -> (C_nil);
                               C_cons f_y22 f_ys -> (C_cons (C_Pair2 f_z f_y22) (f_zip f_y2 f_ys)); });
prove: (forall f_z f_xs f_ys . ((f_zip (C_cons f_z f_xs) f_ys) = (f_zipConcat f_z f_xs f_ys)));
