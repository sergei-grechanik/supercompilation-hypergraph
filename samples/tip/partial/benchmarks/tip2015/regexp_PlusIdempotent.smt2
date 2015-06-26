f_seq f_z f_y2 = (case f_z of {
                    C_Eps -> (case f_y2 of {
                                C_Eps -> (case f_z of {
                                            C_Nil -> (case f_y2 of {
                                                        C_Nil -> (C_Seq f_z f_y2);
                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                        C_Eps -> f_z; });
                                            C_Atom f_Atom_0 -> (case f_y2 of {
                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                  C_Eps -> f_z; });
                                            C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                           C_Eps -> f_z; });
                                            C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                        C_Eps -> f_z; });
                                            C_Star f_Star_0 -> (case f_y2 of {
                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                  C_Eps -> f_z; });
                                            C_Eps -> f_y2; });
                                C_Atom f_Atom_0 -> (case f_z of {
                                                      C_Nil -> (case f_y2 of {
                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                  C_Eps -> f_z; });
                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Eps -> f_y2; });
                                C_Plus f_Plus_0 f_Plus_1 -> (case f_z of {
                                                               C_Nil -> (case f_y2 of {
                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                           C_Eps -> f_z; });
                                                               C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                               C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                              C_Nil -> (C_Seq f_z f_y2);
                                                                                              C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                              C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                              C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                              C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                              C_Eps -> f_z; });
                                                               C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                           C_Eps -> f_z; });
                                                               C_Star f_Star_0 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                               C_Eps -> f_y2; });
                                C_Seq f_Seq_0 f_Seq_1 -> (case f_z of {
                                                            C_Nil -> (case f_y2 of {
                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                        C_Eps -> f_z; });
                                                            C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                            C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                           C_Eps -> f_z; });
                                                            C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                        C_Eps -> f_z; });
                                                            C_Star f_Star_0 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                            C_Eps -> f_y2; });
                                C_Star f_Star_0 -> (case f_z of {
                                                      C_Nil -> (case f_y2 of {
                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                  C_Eps -> f_z; });
                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Eps -> f_y2; });
                                C_Nil -> (C_Nil); });
                    C_Atom f_Atom_0 -> (case f_y2 of {
                                          C_Eps -> (case f_z of {
                                                      C_Nil -> (case f_y2 of {
                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                  C_Eps -> f_z; });
                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Eps -> f_y2; });
                                          C_Atom f_Atom_0 -> (case f_z of {
                                                                C_Nil -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                                C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                C_Star f_Star_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Eps -> f_y2; });
                                          C_Plus f_Plus_0 f_Plus_1 -> (case f_z of {
                                                                         C_Nil -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                                         C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Eps -> f_z; });
                                                                         C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                         C_Star f_Star_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Eps -> f_y2; });
                                          C_Seq f_Seq_0 f_Seq_1 -> (case f_z of {
                                                                      C_Nil -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Eps -> f_z; });
                                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Eps -> f_y2; });
                                          C_Star f_Star_0 -> (case f_z of {
                                                                C_Nil -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                                C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                C_Star f_Star_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Eps -> f_y2; });
                                          C_Nil -> (C_Nil); });
                    C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                   C_Eps -> (case f_z of {
                                                               C_Nil -> (case f_y2 of {
                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                           C_Eps -> f_z; });
                                                               C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                               C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                              C_Nil -> (C_Seq f_z f_y2);
                                                                                              C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                              C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                              C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                              C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                              C_Eps -> f_z; });
                                                               C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                           C_Eps -> f_z; });
                                                               C_Star f_Star_0 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                               C_Eps -> f_y2; });
                                                   C_Atom f_Atom_0 -> (case f_z of {
                                                                         C_Nil -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                                         C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Eps -> f_z; });
                                                                         C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                         C_Star f_Star_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Eps -> f_y2; });
                                                   C_Plus f_Plus_0 f_Plus_1 -> (case f_z of {
                                                                                  C_Nil -> (case f_y2 of {
                                                                                              C_Nil -> (C_Seq f_z f_y2);
                                                                                              C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                              C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                              C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                              C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                              C_Eps -> f_z; });
                                                                                  C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Eps -> f_z; });
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                                 C_Nil -> (C_Seq f_z f_y2);
                                                                                                                 C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                                 C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                                 C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                                 C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                                 C_Eps -> f_z; });
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                              C_Nil -> (C_Seq f_z f_y2);
                                                                                                              C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                              C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                              C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                              C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                              C_Eps -> f_z; });
                                                                                  C_Star f_Star_0 -> (case f_y2 of {
                                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Eps -> f_z; });
                                                                                  C_Eps -> f_y2; });
                                                   C_Seq f_Seq_0 f_Seq_1 -> (case f_z of {
                                                                               C_Nil -> (case f_y2 of {
                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                           C_Eps -> f_z; });
                                                                               C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                               C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                              C_Nil -> (C_Seq f_z f_y2);
                                                                                                              C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                              C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                              C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                              C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                              C_Eps -> f_z; });
                                                                               C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                           C_Eps -> f_z; });
                                                                               C_Star f_Star_0 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                               C_Eps -> f_y2; });
                                                   C_Star f_Star_0 -> (case f_z of {
                                                                         C_Nil -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                                         C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Eps -> f_z; });
                                                                         C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                         C_Star f_Star_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Eps -> f_y2; });
                                                   C_Nil -> (C_Nil); });
                    C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                C_Eps -> (case f_z of {
                                                            C_Nil -> (case f_y2 of {
                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                        C_Eps -> f_z; });
                                                            C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                            C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                           C_Eps -> f_z; });
                                                            C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                        C_Eps -> f_z; });
                                                            C_Star f_Star_0 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                            C_Eps -> f_y2; });
                                                C_Atom f_Atom_0 -> (case f_z of {
                                                                      C_Nil -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Eps -> f_z; });
                                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Eps -> f_y2; });
                                                C_Plus f_Plus_0 f_Plus_1 -> (case f_z of {
                                                                               C_Nil -> (case f_y2 of {
                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                           C_Eps -> f_z; });
                                                                               C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                               C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                              C_Nil -> (C_Seq f_z f_y2);
                                                                                                              C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                              C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                              C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                              C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                              C_Eps -> f_z; });
                                                                               C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                           C_Eps -> f_z; });
                                                                               C_Star f_Star_0 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                               C_Eps -> f_y2; });
                                                C_Seq f_Seq_0 f_Seq_1 -> (case f_z of {
                                                                            C_Nil -> (case f_y2 of {
                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                        C_Eps -> f_z; });
                                                                            C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Eps -> f_z; });
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                           C_Nil -> (C_Seq f_z f_y2);
                                                                                                           C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                           C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                           C_Eps -> f_z; });
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Eps -> f_z; });
                                                                            C_Star f_Star_0 -> (case f_y2 of {
                                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Eps -> f_z; });
                                                                            C_Eps -> f_y2; });
                                                C_Star f_Star_0 -> (case f_z of {
                                                                      C_Nil -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Eps -> f_z; });
                                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Eps -> f_y2; });
                                                C_Nil -> (C_Nil); });
                    C_Star f_Star_0 -> (case f_y2 of {
                                          C_Eps -> (case f_z of {
                                                      C_Nil -> (case f_y2 of {
                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                  C_Eps -> f_z; });
                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                      C_Eps -> f_y2; });
                                          C_Atom f_Atom_0 -> (case f_z of {
                                                                C_Nil -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                                C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                C_Star f_Star_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Eps -> f_y2; });
                                          C_Plus f_Plus_0 f_Plus_1 -> (case f_z of {
                                                                         C_Nil -> (case f_y2 of {
                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                     C_Eps -> f_z; });
                                                                         C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                        C_Nil -> (C_Seq f_z f_y2);
                                                                                                        C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                        C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                        C_Eps -> f_z; });
                                                                         C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                         C_Star f_Star_0 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                         C_Eps -> f_y2; });
                                          C_Seq f_Seq_0 f_Seq_1 -> (case f_z of {
                                                                      C_Nil -> (case f_y2 of {
                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                  C_Eps -> f_z; });
                                                                      C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                                     C_Nil -> (C_Seq f_z f_y2);
                                                                                                     C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                     C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                     C_Eps -> f_z; });
                                                                      C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                                  C_Nil -> (C_Seq f_z f_y2);
                                                                                                  C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                                  C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                                  C_Eps -> f_z; });
                                                                      C_Star f_Star_0 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                      C_Eps -> f_y2; });
                                          C_Star f_Star_0 -> (case f_z of {
                                                                C_Nil -> (case f_y2 of {
                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                            C_Eps -> f_z; });
                                                                C_Atom f_Atom_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                                                               C_Nil -> (C_Seq f_z f_y2);
                                                                                               C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                               C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                               C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                               C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                               C_Eps -> f_z; });
                                                                C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                                                            C_Nil -> (C_Seq f_z f_y2);
                                                                                            C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                            C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                            C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                            C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                            C_Eps -> f_z; });
                                                                C_Star f_Star_0 -> (case f_y2 of {
                                                                                      C_Nil -> (C_Seq f_z f_y2);
                                                                                      C_Atom f_Atom_0 -> (C_Seq f_z f_y2);
                                                                                      C_Plus f_Plus_0 f_Plus_1 -> (C_Seq f_z f_y2);
                                                                                      C_Seq f_Seq_0 f_Seq_1 -> (C_Seq f_z f_y2);
                                                                                      C_Star f_Star_0 -> (C_Seq f_z f_y2);
                                                                                      C_Eps -> f_z; });
                                                                C_Eps -> f_y2; });
                                          C_Nil -> (C_Nil); });
                    C_Nil -> (C_Nil); });
f_plus f_z f_y2 = (case f_z of {
                     C_Eps -> (case f_y2 of {
                                 C_Eps -> (C_Plus f_z f_y2);
                                 C_Atom f_Atom_0 -> (C_Plus f_z f_y2);
                                 C_Plus f_Plus_0 f_Plus_1 -> (C_Plus f_z f_y2);
                                 C_Seq f_Seq_0 f_Seq_1 -> (C_Plus f_z f_y2);
                                 C_Star f_Star_0 -> (C_Plus f_z f_y2);
                                 C_Nil -> f_z; });
                     C_Atom f_Atom_0 -> (case f_y2 of {
                                           C_Eps -> (C_Plus f_z f_y2);
                                           C_Atom f_Atom_0 -> (C_Plus f_z f_y2);
                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Plus f_z f_y2);
                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Plus f_z f_y2);
                                           C_Star f_Star_0 -> (C_Plus f_z f_y2);
                                           C_Nil -> f_z; });
                     C_Plus f_Plus_0 f_Plus_1 -> (case f_y2 of {
                                                    C_Eps -> (C_Plus f_z f_y2);
                                                    C_Atom f_Atom_0 -> (C_Plus f_z f_y2);
                                                    C_Plus f_Plus_0 f_Plus_1 -> (C_Plus f_z f_y2);
                                                    C_Seq f_Seq_0 f_Seq_1 -> (C_Plus f_z f_y2);
                                                    C_Star f_Star_0 -> (C_Plus f_z f_y2);
                                                    C_Nil -> f_z; });
                     C_Seq f_Seq_0 f_Seq_1 -> (case f_y2 of {
                                                 C_Eps -> (C_Plus f_z f_y2);
                                                 C_Atom f_Atom_0 -> (C_Plus f_z f_y2);
                                                 C_Plus f_Plus_0 f_Plus_1 -> (C_Plus f_z f_y2);
                                                 C_Seq f_Seq_0 f_Seq_1 -> (C_Plus f_z f_y2);
                                                 C_Star f_Star_0 -> (C_Plus f_z f_y2);
                                                 C_Nil -> f_z; });
                     C_Star f_Star_0 -> (case f_y2 of {
                                           C_Eps -> (C_Plus f_z f_y2);
                                           C_Atom f_Atom_0 -> (C_Plus f_z f_y2);
                                           C_Plus f_Plus_0 f_Plus_1 -> (C_Plus f_z f_y2);
                                           C_Seq f_Seq_0 f_Seq_1 -> (C_Plus f_z f_y2);
                                           C_Star f_Star_0 -> (C_Plus f_z f_y2);
                                           C_Nil -> f_z; });
                     C_Nil -> f_y2; });
f_eqA f_z f_y2 = (case f_z of {
                    C_X -> (case f_y2 of {
                              C_X -> (C_True);
                              C_Y -> (C_False); });
                    C_Y -> (case f_y2 of {
                              C_X -> (C_False);
                              C_Y -> (C_True); }); });
f_eps f_z = (case f_z of {
               C_Nil -> (C_False);
               C_Atom f_Atom_0 -> (C_False);
               C_Eps -> (C_True);
               C_Plus f_p f_q -> (case (f_eps f_p) of {
                                    C_True -> (C_True);
                                    C_False -> (f_eps f_q); });
               C_Seq f_p2 f_q2 -> (case (f_eps f_p2) of {
                                     C_True -> (f_eps f_q2);
                                     C_False -> (C_False); });
               C_Star f_y2 -> (C_True); });
f_epsR f_z = (case (f_eps f_z) of {
                C_True -> (C_Eps);
                C_False -> (C_Nil); });
f_step f_z f_y2 = (case f_z of {
                     C_Nil -> (C_Nil);
                     C_Eps -> (C_Nil);
                     C_Atom f_a -> (case (f_eqA f_a f_y2) of {
                                      C_True -> (C_Eps);
                                      C_False -> (C_Nil); });
                     C_Plus f_p f_q -> (f_plus (f_step f_p f_y2) (f_step f_q f_y2));
                     C_Seq f_p2 f_q2 -> (f_plus (f_seq (f_step f_p2 f_y2) f_q2) (f_seq (f_epsR f_p2) (f_step f_q2 f_y2)));
                     C_Star f_p3 -> (f_seq (f_step f_p3 f_y2) f_z); });
f_recognise f_z f_y2 = (case f_y2 of {
                          C_nil -> (f_eps f_z);
                          C_cons f_z2 f_xs -> (f_recognise (f_step f_z f_z2) f_xs); });
prove: (forall f_p f_s . ((f_recognise (C_Plus f_p f_p) f_s) = (f_recognise f_p f_s)));
