f_plus f_z f_y2 = (case f_z of {
                     C_Z -> f_y2;
                     C_S f_n -> (C_S (f_plus f_n f_y2)); });
f_minimum f_z = (case f_z of {
                   C_Node f_y2 f_z2 f_x2 -> (C_Just f_z2);
                   C_Nil -> (C_Nothing); });
f_listMinimum f_z = (case f_z of {
                       C_nil -> (C_Nothing);
                       C_cons f_y2 f_z2 -> (C_Just f_y2); });
f_le f_z f_y2 = (case f_z of {
                   C_Z -> (C_True);
                   C_S f_z2 -> (case f_y2 of {
                                  C_Z -> (C_False);
                                  C_S f_x2 -> (f_le f_z2 f_x2); }); });
f_merge f_z f_y2 = (case f_z of {
                      C_Node f_z2 f_x2 f_x3 -> (case f_y2 of {
                                                  C_Node f_x4 f_x5 f_x6 -> (case (f_le f_x2 f_x5) of {
                                                                              C_True -> (C_Node (f_merge f_x3 f_y2) f_x2 f_z2);
                                                                              C_False -> (C_Node (f_merge f_z f_x6) f_x5 f_x4); });
                                                  C_Nil -> f_z; });
                      C_Nil -> f_y2; });
f_toList f_z f_y2 = (case f_z of {
                       C_Z -> (C_nil);
                       C_S f_z2 -> (case f_y2 of {
                                      C_Node f_x2 f_x3 f_x4 -> (C_cons f_x3 (f_toList f_z2 (f_merge f_x2 f_x4)));
                                      C_Nil -> (C_nil); }); });
f_heapSize f_z = (case f_z of {
                    C_Node f_l f_y2 f_r -> (C_S (f_plus (f_heapSize f_l) (f_heapSize f_r)));
                    C_Nil -> (C_Z); });
f_toList2 f_z = (f_toList (f_heapSize f_z) f_z);
prove: (forall f_h . ((f_listMinimum (f_toList2 f_h)) = (f_minimum f_h)));
