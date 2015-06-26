f_mirror f_z = (case f_z of {
                  C_Leaf -> (C_Leaf);
                  C_Node f_l f_y2 f_r -> (C_Node (f_mirror f_r) f_y2 (f_mirror f_l)); });
f_max2 f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_z2 -> (case f_y2 of {
                                    C_Z -> f_z;
                                    C_S f_x2 -> (C_S (f_max2 f_z2 f_x2)); }); });
f_height f_z = (case f_z of {
                  C_Leaf -> (C_Z);
                  C_Node f_l f_y2 f_r -> (C_S (f_max2 (f_height f_l) (f_height f_r))); });
prove: (forall f_b . ((f_height (f_mirror f_b)) = (f_height f_b)));
