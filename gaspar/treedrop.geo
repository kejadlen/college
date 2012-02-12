MODULE W {
   BODY Hill {
      Point {p1,p2,p3}
      Orient {cos(p4)*cos(p6)-sin(p4)*cos(p5)*sin(p6),-cos(p4)*sin(p6)-sin(p4)*cos(p5)*cos(p6),sin(p4)*sin(p5),sin(p4)*cos(p6)+cos(p4)*cos(p5)*sin(p6),-sin(p4)*sin(p6)+cos(p4)*cos(p5)*cos(p6),-cos(p4)*sin(p5),sin(p5)*sin(p6),sin(p5)*cos(p6),cos(p5)}
      Block {
         Xlength 10
         Ylength 10
         Zlength .1e-1
         Color {0,0.39,0}
      }
   }
   BODY Tree {
      Point {q1,q2,q3}
      Orient {cos(q4)*cos(q6)-sin(q4)*cos(q5)*sin(q6),-cos(q4)*sin(q6)-sin(q4)*cos(q5)*cos(q6),sin(q4)*sin(q5),sin(q4)*cos(q6)+cos(q4)*cos(q5)*sin(q6),-sin(q4)*sin(q6)+cos(q4)*cos(q5)*cos(q6),-cos(q4)*sin(q5),sin(q5)*sin(q6),sin(q5)*cos(q6),cos(q5)}
      Cylinder {
         Radius R
         Length L
         Color {0.55,0.27,0.07}
      }
   }
}
