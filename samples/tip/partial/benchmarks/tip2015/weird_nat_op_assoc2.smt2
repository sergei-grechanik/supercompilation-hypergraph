f_op f_z f_y2 f_z2 f_x2 = (case f_z of {
                             C_Z -> (case f_z2 of {
                                       C_Z -> f_x2;
                                       C_S f_x3 -> (f_op (C_Z) f_y2 f_x3 (C_S f_x2)); });
                             C_S f_x4 -> (case f_z2 of {
                                            C_Z -> (f_op f_x4 f_y2 f_y2 f_x2);
                                            C_S f_c -> (f_op f_z f_y2 f_c (C_S f_x2)); }); });
prove: (forall f_z f_a f_b f_c f_d . ((f_op (f_op f_z f_a f_a f_a) f_b f_c f_d) = (f_op f_a (f_op f_b f_z f_b f_b) f_c f_d)));
