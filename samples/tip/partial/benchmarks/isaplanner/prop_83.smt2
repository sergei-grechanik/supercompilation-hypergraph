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
prove: (forall f_xs f_ys f_zs . ((f_zip (f_append f_xs f_ys) f_zs) = (f_append (f_zip f_xs (f_take (f_len f_xs) f_zs)) (f_zip f_ys (f_drop (f_len f_xs) f_zs)))));
