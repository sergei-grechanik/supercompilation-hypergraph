f_zip f_z f_y2 = (case f_z of {
                    C_nil -> (C_nil);
                    C_cons f_z2 f_x2 -> (case f_y2 of {
                                           C_nil -> (C_nil);
                                           C_cons f_x3 f_x4 -> (C_cons (C_Pair2 f_z2 f_x3) (f_zip f_x2 f_x4)); }); });
prove: (forall f_xs . ((f_zip (C_nil) f_xs) = (C_nil)));
