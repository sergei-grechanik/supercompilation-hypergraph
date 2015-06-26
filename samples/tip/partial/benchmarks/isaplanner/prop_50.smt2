f_take f_z f_y2 = (case f_z of {
                     C_Z -> (C_nil);
                     C_S f_z2 -> (case f_y2 of {
                                    C_nil -> (C_nil);
                                    C_cons f_x2 f_x3 -> (C_cons f_x2 (f_take f_z2 f_x3)); }); });
f_minus f_z f_y2 = (case f_z of {
                      C_Z -> (C_Z);
                      C_S f_z2 -> (case f_y2 of {
                                     C_Z -> f_z;
                                     C_S f_x2 -> (f_minus f_z2 f_x2); }); });
f_len f_z = (case f_z of {
               C_nil -> (C_Z);
               C_cons f_y2 f_xs -> (C_S (f_len f_xs)); });
f_butlast f_z = (case f_z of {
                   C_nil -> (C_nil);
                   C_cons f_y2 f_z2 -> (case f_z2 of {
                                          C_nil -> (C_nil);
                                          C_cons f_x2 f_x3 -> (C_cons f_y2 (f_butlast f_z2)); }); });
prove: (forall f_xs . ((f_butlast f_xs) = (f_take (f_minus (f_len f_xs) (C_S (C_Z))) f_xs)));
