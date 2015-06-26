f_s f_z = (case f_z of {
             C_One -> (C_ZeroAnd (C_One));
             C_ZeroAnd f_xs -> (C_OneAnd f_xs);
             C_OneAnd f_ys -> (C_ZeroAnd (f_s f_ys)); });
f_plus f_z f_y2 = (case f_z of {
                     C_One -> (f_s f_y2);
                     C_ZeroAnd f_z2 -> (case f_y2 of {
                                          C_One -> (f_s f_z);
                                          C_ZeroAnd f_ys -> (C_ZeroAnd (f_plus f_z2 f_ys));
                                          C_OneAnd f_xs -> (C_OneAnd (f_plus f_z2 f_xs)); });
                     C_OneAnd f_x2 -> (case f_y2 of {
                                         C_One -> (f_s f_z);
                                         C_ZeroAnd f_zs -> (C_OneAnd (f_plus f_x2 f_zs));
                                         C_OneAnd f_ys2 -> (C_ZeroAnd (f_s (f_plus f_x2 f_ys2))); }); });
f_times f_z f_y2 = (case f_z of {
                      C_One -> f_y2;
                      C_ZeroAnd f_xs -> (C_ZeroAnd (f_times f_xs f_y2));
                      C_OneAnd f_ys -> (f_plus (C_ZeroAnd (f_times f_ys f_y2)) f_y2); });
prove: (forall f_z f_y2 f_z2 . ((f_times f_z (f_times f_y2 f_z2)) = (f_times (f_times f_z f_y2) f_z2)));
