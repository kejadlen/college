maple('Restart()');

maple('DeclareObservers(W,Tree,Hill)');
maple('DeclarePoints(W,Tree,Hill)');
maple('DeclareTriads(w,tree,hill)');

maple(['DefineObservers('...
    '[W,W,w],[Tree,Tree,tree],[Hill,Hill,hill])']);
maple(['DefinePoints('...
    '[W,Tree,w,q1,q2,q3],'...
    '[W,Hill,w,p1,p2,p3])']);
maple(['DefineTriads('...
    '[w,tree,[q4,3],[q5,1],[q6,3]],'...
    '[w,hill,[p4,3],[p5,1],[p6,3]])']);

maple('DefineNeighbors([W,Tree],[W,Hill])');

maple(['DefineObjects('...
    '[Tree,''Cylinder'',length=2.5,radius=.1,color=red],'...
    '[Hill,''Block'',xlength=50,ylength=50,'...
        'zlength=.01,color=white])']);

maple('DeclareStates(q1,q2,q3,q4,q5,q6,u1,u2,u3,u4,u5,u6)');

maple(['kde:={'...
    'seq(LinearVelocity(W,Tree)'...
        '&oo MakeTranslations(w,i)=cat(u,i),i=1..3),'...
    'seq(AngularVelocity(w,tree)'...
        '&oo MakeTranslations(tree,i)=cat(u,i+3),i=1..3)}']);

maple('p:=subs(kde,LinearMomentum(W,Tree))');
maple('h:=subs(kde,AngularMomentum(W,tree))');

maple('scale:=1');
maple('gravity:=MakeTranslations(w,0,0,-M*g)');
maple('bounce:=MakeTranslations(w,0,-1,-scale*M*g*q3)');

maple(['dde:={'...
    'seq(((case &** (DiffTime(p,w) &-- gravity))'...
        '&++ ((1-case) &** (DiffTime(p,w) &-- bounce)))'...
        '&oo MakeTranslations(w,i)=0,i=1..3),'...
    'seq(DiffTime(h,w)'...
        '&oo MakeTranslations(w,i)=0,i=1..3)}']);

maple(['GeometryOutput('...
    'main=W,states=[q1,q2,q3=2,q4=1,q5=1,q6=1,'...
    'u1,u2,u3,u4,u5,u6,case],'...
    'parameters=[p1,p2,p3,p4,p5=0.785,p6,'...
    'seq(seq(cat(Inertia,i,j),i=1..3),j=1..3),M=1,g=10],'...
    'checkargs,filename="treedrop.geo")']);
maple(['MotionOutput('...
    'ode=kde union dde,states=[q1,q2,q3=2,q4=1,q5=1,q6=1,'...
    'u1,u2,u3,u4,u5,u6],'...
    'insignals=[case=((q3-q2)&>0)],'...
    'parameters=[p1,p2,p3,p4,p5=0.785,p6,'...
    'Inertia11=2,Inertia12,Inertia13,'...
    'Inertia21,Inertia22=1,Inertia23,'...
    'Inertia31,Inertia32,Inertia33=.5,M=1,g=10],'...
    'checksings,checkargs,filename="treedrop.dyn")']);