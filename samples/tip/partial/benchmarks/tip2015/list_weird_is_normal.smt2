f_weird_concat f_z = (case f_z of {
                        C_nil -> (C_nil);
                        C_cons f_y2 f_xss -> (case f_y2 of {
                                                C_nil -> (f_weird_concat f_xss);
                                                C_cons f_z2 f_xs -> (C_cons f_z2 (f_weird_concat (C_cons f_xs f_xss))); }); });
f_append f_z f_y2 = (case f_z of {
                       C_nil -> f_y2;
                       C_cons f_z2 f_xs -> (C_cons f_z2 (f_append f_xs f_y2)); });
f_concat2 f_z = (case f_z of {
                   C_nil -> (C_nil);
                   C_cons f_xs f_xss -> (f_append f_xs (f_concat2 f_xss)); });
prove: (forall f_z . ((f_concat2 f_z) = (f_weird_concat f_z)));
