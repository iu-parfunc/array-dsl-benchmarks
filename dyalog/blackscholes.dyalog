:Namespace Blackscholes

    ⎕IO←0

⍝ These are some constants that are defined by default in the 
⍝ Accelerate version.

    riskfree←0.02
    volatility←0.03

⍝ The Accelerate solution starts with a horner function:

    HORNER←{X←⍺ ⋄ X×{⍺+X×⍵}/⍵}

⍝ It follows up with the following CNDP function:

    coeff←0.31938153 -0.356563782 1.781477937 -1.821255978 1.33027442
    rsqrt2pi←÷0.5*⍨○2

      CNDP←{
          k←÷1+0.2316419×L←|⍵
          rsqrt2pi×(*¯2÷⍨L×L)×k HORNER coeff
      }

⍝ Finally, this is a transliteration of the reference implementation 
⍝ in the Accelerate program.

      BS∆REF←{
          r←riskfree ⋄ v←volatility ⋄
          ↑{
              price strike years←⍵
              sqrtT←years*0.5
              d1←((⍟price÷strike)+(r+(v×v)÷2)×years)÷v×sqrtT
              d2←d1-v×sqrtT
              expRT←*(-r)×years
              cnd←{⍵>0:1-CNDP ⍵ ⋄ CNDP ⍵}
              cndD1←cnd d1 ⋄ cndD2←cnd d2
              R←((price×cndD1)-strike×expRT×cndD2)
              R((strike×expRT×1-cndD2)-price×1-cndD1)
          }¨⊂[1]⍵
      }

      GEN∆DATA←{
          sp←5+?⍵⍴25
          os←1+?⍵⍴100
          oy←0.25+100÷⍨?⍵⍴1000
          ⍉↑sp os oy
      }

:EndNamespace
