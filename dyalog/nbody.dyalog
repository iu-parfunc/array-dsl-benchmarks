⍝ Dyalog NBODY Benchmark
⍝ ──────────────────────
⍝ 
⍝ This document describes the namespace NBODY, which implements 
⍝ a series of approaches for doing an NBody calculation. To 
⍝ use a given nbody calculator NBC, you can run the following:
⍝
⍝     NBODY.NBC NBODY.GEN∆DATA N
⍝
⍝ Here N is the number of the particles to use in the simulation.
⍝ This is calculated using the PBBS uniform 3-D points generator.

:Namespace NBODY
    ⎕IO ⎕ML ⎕WX←0 0 3                    
  

    ∇ D←GEN∆DATA N;T
      ⎕SH'../pbbs/nBody/geometryData/uniform -s -d 3 ',(⍕N),' pbbs_data'
      T←'pbbs_data'⎕NTIE 0 0
      D←⎕NREAD T,80,(⎕NSIZE T),0
      D←(∨\D=⎕UCS 10)/D
      D[(D='-')/⍳⍴D]←'¯'
      D[(D=⎕UCS 10)/⍳⍴D]←' '
      D←(D=' ')⊂D
      D←⍎¨¯1↓D
      D←⍉((3÷⍨⍴D),3)⍴D
    ∇

  ⍝ This is the generic definition of the force function
  ⍝ since we are using constant weights of 1.0, we eliminate 
  ⍝ those from the equation.
  
    F←{V÷3*⍨|V←⍵-⍺}
    
  ⍝ The reference implementation of NBody against which we compare 
  ⍝ out other implementations is called REF. It does the naive
  ⍝ outer product using F defined above. 

    REF←{⍉↑+/∘.F⍨⊂[0]⍵}
    
    NEACH←{↑{+/V÷(|V←X-[0]⍵)*3}¨⊂[0]X←⍵}
    
      CEACH←{
          RS←(⍴⍵)⍴0 ⋄ I0 I1 I2←⊂[1]⍵
          FS←{+/V÷U×U×U←|V←⍵-⍺}
          RS[0;]←{⍵ FS I0}¨I0
          RS[1;]←{⍵ FS I1}¨I1
          RS[2;]←{⍵ FS I2}¨I2
          RS
      }
   
      OEACH←{
          ⎕IO←1 ⋄ RS←(⍴⍵)⍴0 ⋄ C←⊃⌽⍴⍵
          FS←{+/V÷U×U×U←|V←⍵-⍺}
          OP←{⍺[⍵]FS ⍺[⍵+⍳C-⍵]}
          RS←↑{(⊂⍵)OP¨⍳C}¨⊂[2]⍵
          1+RS+⌽RS
      }

:EndNamespace 
