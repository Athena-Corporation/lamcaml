(*
Call-by-value (eager evaluation) semantics :
  in every functional application f ∙ arg1 ∙ ... ∙ argn,you first reduce the arguments of the functions to values, then pass the arguments to the function... (intuition...)


(β-axiom) for call-by-value (complete the rules...)
small-step semantics for call-by-value

        s v : expr    is_value(s) = false                     s  ->ᵥ s'                is_value(s) = true   v v v'              is_value(v) = true
    --------------------------------------- (ax)   ---------------------------     ---------------------------------      -------------------------------
                App(s, v)                             App(s, v) ->ᵥ App(s', v)          App(s, v)  ->ᵥ App(s, v')               (λ x. s) ∙ v ->ᵥ s[x := v]

       s t u : expr    is_value(s) = false                           s ->ᵥ s'                   s = true                s = false
    ----------------------------------------- (If-False)       --------------------------    -------------------     --------------------       
                  if(s, t, u)                                 if(s, t, u) ->ᵥ if(s', t, u)    if(s', t, u) ->ᵥ t       if(s', t, u) ->ᵥ u

      B f s : expr    is_value(f) = false    is_value(s) = false                         f ->ᵥ f'               is_value(f) = true   s ->ᵥ s'                 is_value(s) = true
    ---------------------------------------------------------- (Bin-Add)     ----------------------------   ----------------------------------        -----------------------------
                        Bin(f,s)                                               Bin(f, s) ->ᵥ Bin(f', s)             Bin(f, s) ->ᵥ Bin(f, s')               Bin(f, s) ->ᵥ f + s ->ᵥ (f + s)sum

     B f s : expr    is_value(f) = false    is_value(s) = false                        f ->ᵥ f'                is_value(f) = true   s ->ᵥ s'       is_value(s) = true   f > s = true           is_value(s) = true   f > s = false 
    ---------------------------------------------------------- (Bin-Geq)     -------------------------    ----------------------------------      ----------------------------------         ---------------------------------------
                        Bin(f,s)                                              Bin(f, s) ->ᵥ Bin(f', s)           Bin(f, s) ->ᵥ Bin(f, s')            Bin(f, s) ->ᵥ f > s ->ᵥ True                 f s ->ᵥ f > s ->ᵥ false

     B f s : expr    is_value(f) = false    is_value(s) = false                     f ->ᵥ f'                is_value(f) = true   s ->ᵥ s'            is_value(s) = true   f < s = true         is_value(s) = true   f < s = false 
    ---------------------------------------------------------- (Bin-Leq)     -----------------------   ----------------------------------      ----------------------------------          --------------------------------------
                        Bin(f,s)                                             Bin(f, s) ->ᵥ Bin(f', s)          Bin(f, s) ->ᵥ Bin(f, s')                     Bin(f, s) ->ᵥ f < s ->ᵥ True             Bin(f, s) ->ᵥ f < s ->ᵥ false

     B f s : expr    is_value(f) = false                     f ->ᵥ f'                    is_value(f) = true          
    ------------------------------------- (Unp-Inv)     -----------------            --------------------------
                        Unp(f)                           Unp(f) ->ᵥ Unp(f')                 Unp(f) ->ᵥ (-f)                                            

     B f s : expr    is_value(f) = false                       f ->ᵥ f'            is_value(f) = true   f = true      is_value(f) = true  f = false         
    ------------------------------------- (Unp-Not)     ------------------      ---------------------------------   --------------------------------
                        Unp(f)                           Unp(f) ->ᵥ Unp(f')           Unp(f) ->ᵥ False                        Unp(f) ->ᵥ True
    
*)