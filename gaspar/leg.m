maple('Restart()');

maple(['DeclareObservers('...
    'Base,'...
    'UpperArm,'...
    'AuxArm,'...
    'Knee,'...
    'LowerArm,'...
    'Foot,'...
    'Shaft1,'...
    'Cylinder1,'...
    'Cylinder2,'...
    'Shaft2,'...
    'Cylinder3,'...
    'Shaft3)']);

maple(['DeclarePoints('...
    'Base,'...
    'UpperArm,'...
    'AuxArm,'...
    'Knee,'...
    'LowerArm,'...
    'Foot,'...
    'Shaft1,'...
    'Cylinder2,'...
    'Shaft2,'...
    'Cylinder3,'...
    'Shaft3)']);

% Cylinder1 = AuxArm

maple(['DeclareTriads('...
    'base,'...
    'upperarm,'...
    'knee,'...
    'lowerarm,'...
    'foot,'...
    'actuator1,'...
    'actuator2,'...
    'actuator3)']);

% auxarm = upperarm

maple('s:=1');

maple(['DefineObservers('...
    '[Base,Base,base],'...
    '[UpperArm,UpperArm,upperarm],'...
    '[AuxArm,AuxArm,upperarm],'...
    '[Knee,Knee,knee],'...
    '[LowerArm,LowerArm,lowerarm],'...
    '[Foot,Foot,foot],'...
    '[Cylinder1,AuxArm,actuator1],'...
    '[Shaft1,Shaft1,actuator1],'...
    '[Cylinder2,Cylinder2,actuator2],'...
    '[Shaft2,Shaft2,actuator2],'...
    '[Cylinder3,Cylinder3,actuator3],'...
    '[Shaft3,Shaft3,actuator3])']);

maple(['DefinePoints('...
    '[Base,UpperArm,base,q1,q2,q3],'...
    '[UpperArm,AuxArm,knee,0,0,2*s],'...
    '[UpperArm,Knee,upperarm,0,0,7*s],'...
    '[Knee,LowerArm,knee,0,0,0],'...
    '[LowerArm,Foot,lowerarm,0,0,7*s],'...
    '[UpperArm,Shaft1,upperarm,0,0,5*s],'...
    '[Knee,Cylinder2,knee,-2*s,0,2*s],'...
    '[LowerArm,Shaft2,lowerarm,1.5*s,0,1*s],'...
    '[Knee,Cylinder3,knee,0,0,2*s],'...
    '[LowerArm,Shaft3,lowerarm,0,0,4*s])']);

maple(['DefineTriads('...
    '[base,upperarm,[q4,2],[q5,1],[q6,3]],'...
    '[upperarm,knee,q7,2],'...
    '[knee,lowerarm,q8,2],'...
    '[lowerarm,foot,[q9,3],[q10,1],[q11,3]],'...
    '[upperarm,actuator1,q12,2],'...
    '[knee,actuator2,q13,2],'...
    '[knee,actuator3,q14,2])']);

maple(['DefineNeighbors('...
    '[Base,UpperArm],'...
    '[Base,AuxArm],'...
    '[UpperArm,Knee],'...
    '[Knee,LowerArm],'...
    '[LowerArm,Foot],'...
    '[AuxArm,Cylinder1],'...
    '[UpperArm,Shaft1],'...
    '[Knee,Cylinder2],'...
    '[LowerArm,Shaft2],'...
    '[Knee,Cylinder3],'...
    '[LowerArm,Shaft3])']);

maple(['DefineObjects('...
'[Cylinder2,''Sphere'',radius=.2,color=green],'...
'[Shaft2,''Sphere'',radius=.3,color=green],'...
'[Cylinder3,''Sphere'',radius=.2,color=red],'...
'[Shaft3,''Sphere'',radius=.3,color=red],'...
    '[UpperArm,''Cylinder'',point=MakeTranslations(upperarm,0,0,3.5*s),'...
        'length=7*s,radius=0.1],'...
    '[AuxArm,''Cylinder'',point=MakeTranslations(upperarm,0,0,3.5*s),'...
        'length=7*s,radius=0.1],'...
    '[Knee,''Cylinder'',point=MakeTranslations(knee,0,0,1*s),'...
        'length=2*s,radius=0.1,color=green],'...
    '[Knee,''Cylinder'',point=MakeTranslations(knee,-1*s,0,2*s),'...
        'orient=MakeRotations(Pi/2,2),length=2*s,radius=0.1,color=green],'...
    '[LowerArm,''Cylinder'',point=MakeTranslations(lowerarm,0,0,3.5*s),'...
        'length=7*s,radius=0.1],'...
    '[Foot,''Cylinder'',length=0.1,radius=1*s],'...
    '[Cylinder1,''Cylinder'',point=MakeTranslations(actuator1,0,0,1.5*s),'...
        'length=3*s,radius=0.2,color=blue],'...
    '[Shaft1,''Cylinder'',point=MakeTranslations(actuator1,0,0,-1.5*s),'...
        'length=3*s,radius=0.3,color=blue],'...
    '[Cylinder2,''Cylinder'',point=MakeTranslations(actuator2,0,0,1*s),'...
        'length=2*s,radius=0.2,color=blue],'...
    '[Shaft2,''Cylinder'',point=MakeTranslations(actuator2,0,0,-1*s),'...
        'length=2*s,radius=0.3,color=blue],'...
    '[Cylinder3,''Cylinder'',point=MakeTranslations(actuator3,0,0,.5*s),'...
        'length=1*s,radius=0.2,color=blue],'...
    '[Shaft3,''Cylinder'',point=MakeTranslations(actuator3,0,0,-.5*s),'...
        'length=1*s,radius=0.3,color=blue])']);

% constraints

maple('r1:=FindTranslation(AuxArm,Shaft1)');
maple('r2:=FindTranslation(Cylinder2,Shaft2)');
maple('r3:=FindTranslation(Cylinder3,Shaft3)');

maple('f1:=r1 &oo MakeTranslations(actuator1,1) = 0');
maple('f2:=r2 &oo MakeTranslations(actuator2,1) = 0');
maple('f3:=r3 &oo MakeTranslations(actuator3,1) = 0');

maple(['GeometryOutput('...
    'main=Base,'...
    'states=[q1,q2,q3=-7,q4,q5,'...
        'q6,q7=-1,q8=0.2,q9,q10,'...
        'q11,q12=0.406,q13=1.92,q14=0.37],'...
    'filename="leg.geo")']);

maple(['MotionOutput('...
    'states=[q1,q2,q3=-7,q4,q5,'...
        'q6,q7=-1,q8=0.2,q9,q10,'...
        'q11,q12=0.406,q13=1.92,q14=0.37],'...
    'filename="leg.dyn")']);