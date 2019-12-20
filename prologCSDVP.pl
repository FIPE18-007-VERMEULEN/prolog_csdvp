skill([
[ [1,75], [4,50] ],
[ [3,50], [1,25], [4,50] ],
[ [0,50], [2,50], [5,25] ],
[ [5,50]],
[ [1,35], [2,25], [5,5] ],
[ [6,40], [3,45], [1,10]],
[ [0,25], [2,40] ]
]).

prereq([
[ [] ],
[ [1,3] ],
[ [5,3] ],
[ [1,4], [4,2] ],
[ [2,2], [4,4] ],
[ [3,4] ]
]).

timeFrame([
[ 0 ],
[ 1, 2 ],
[ 2, 3 ],
[ 1, 2 ],
[ 2, 3, 4],
[ 2, 5, 6],
[ 3, 5, 6]
]).

ects([
[ 2 ],
[ 2 ],
[ 2 ],
[ 2 ],
[ 2 ],
[ 2 ],
[ 2 ]
]).

finalSkills([
[ 0, 0 ],
[ 1, 2 ],
[ 5, 3 ]
]).

courses([
  [ -1, /* EMPTY COURSE */
    [ [] ], /* skills given */
    [ [] ], /* prerequisite */
    [ 0, 1, 2, 3, 4 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 0, /* course 0*/
    [ [1, 75], [4, 50] ], /* skills given */
    [ [] ], /* prerequisite */
    [ 0 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 1, /* course 1*/
    [ [3, 50], [1, 25] ], /* skills given */
    [ [1, 3] ], /* prerequisite */
    [ 1, 2 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 2, /* course 2 */
    [ [0,50], [2,50], [5,25] ], /* skills given */
    [ [5, 2] ], /* prerequisite */
    [ 2, 3 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 3, /* course 3 */
    [ [5,50] ], /* skills given */
    [ [1,4], [4,2] ], /* prerequisite */
    [ 1, 2 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 4, /* course 4 */
    [ [1,35], [2,25], [5,5] ], /* skills given */
    [ [2,2], [4,4] ], /* prerequisite */
    [ 2, 3, 4 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 5, /* course 5 */
    [ [6,40], [3,45], [1,10] ], /* skills given */
    [ [2,2], [4,4] ], /* prerequisite */
    [ 2, 5, 6 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 6, /* course 6 */
    [ [0,25], [2,40] ], /* skills given */
    [ [3,4] ], /* prerequisite */
    [ 3, 5, 6 ], /* time frame */
    [ 3 ] /* ects */
  ]
]).

/* === GLOBAL CONFIGURATION */
minECTS(8).
nbSemester(4).
nbCourses(5).
nbCourseBySemester(1).

/* === GETTER AND UTILITY === */
getIDCourse([I|_], I).

isNbCoursesIsEQ(S):- length(S,L), nbCourseBySemester(A), nbSemester(B), L =:= A * B. %correct size of S.
isNbCoursesIsLT(S):- length(S,L), nbCourseBySemester(A), nbSemester(B), L < A * B.
isNbCoursesIsGT(S):- length(S,L), nbCourseBySemester(A), nbSemester(B), L > A * B. %correct size of S.

displaySolution([]).
displaySolution([ [ID|_]|Y]):- write("\n"), write("Course"), write(ID), displaySolution(Y).

/* === ECTS CONSTRAINTS === */
ectsConstraintsSolver([X],V):- minECTS(M), (X+V) >= M.
ectsConstraintsSolver([X|Y],V):- ectsConstraintsSolver(Y,V+X).

% Used to build a good list looking for ectsConstraintsSolver
ectsListBuilder([X],A):-nth0(4,X,A).
ectsListBuilder([X|Y],K):-nth0(4,X,A),ectsListBuilder(Y,L), append(A,L,K).

ectsConstraintsCaller(X):- ectsListBuilder(X,L), ectsConstraintsSolver(L,0).

/* === FUZZYFICATION PLACEBO ENGINE  === */
fuzzifyMastery(X,0):-X >= 0, X < 20.%fuzzifyMastery(Value,MasterySet)
fuzzifyMastery(X,1):-X >= 20,X < 40.
fuzzifyMastery(X,2):-X >= 40,X < 60.
fuzzifyMastery(X,3):-X >= 60,X < 80.
fuzzifyMastery(X,4):-X >=80.

/* === DECAY FUNCTION === */
decayFunction(X,Y):- Y is exp((X / 1.25)) + 5.
% decayFunction(X,X).

/* === ALL DIFFERENTS === */
buildIDCoursesList([X],[I]):-getIDCourse(X,I),!.
buildIDCoursesList([X|Y],[I|L]):-buildIDCoursesList(Y,L),getIDCourse(X,I).

allDiff(S):-buildIDCoursesList(S,L), subtract(L, [-1], C), allUniqueExceptEmpty(C). %subtract remove empty course of the current solution to check redudancy of courses

allUniqueExceptEmpty([]).
allUniqueExceptEmpty([X|Y]):- \+ member(X,Y), allUniqueExceptEmpty(Y).

/* === TIME CONSTRAINTS */

timeConstraintsSolver(Course,PositionInSol):- nbCourseBySemester(C), X is div(PositionInSol, C) , nth0(3,Course,TimeFrame), member(X,TimeFrame).

timeConstraintsCaller(Course, PositionInSol):-timeConstraintsSolver(Course, PositionInSol).

/* === PREREQUISITE CONSTRAINTS */
prerequisiteConstraintsSolverFzDc(_, []).
prerequisiteConstraintsSolverFzDc(_, [[]]).
prerequisiteConstraintsSolverFzDc(FuzzySet, [[P,VP]|Prereq]):- member([P,V],FuzzySet),V>=VP, prerequisiteConstraintsSolverFzDc(FuzzySet, Prereq).

% e.g.: courses(C), exploreMasteryByCourse(C,0,[],N), nbSemester(S),exploreForComputeDecay(N,S,[],L), fuzzifyCurrentSkills(L,F).

fuzzifyCurrentSkills([],[]).
fuzzifyCurrentSkills([[SID,M]|Skills],[[SID,Set]|L1]):-fuzzifyCurrentSkills(Skills,L1),fuzzifyMastery(M,Set).

exploreForComputeDecay([],_,L, L).
exploreForComputeDecay([ [SID,M,T] | Skills ], UpperTF, L, SLevels):- exploreForComputeDecay(Skills, UpperTF,L,TmpS),computeDecay([SID,M,T],UpperTF,SLevel), append(TmpS,[[SID,SLevel]],SLevels).

computeDecay([],_,0).
computeDecay([_, [M], [T]], UpperTF, SLevel):- nbCourseBySemester(N), CurrentSemester is div(T,N), CS2 is div(UpperTF,N), Range is CS2-CurrentSemester, decayFunction(Range,Y), SLevel is M - Y.
% computeDecay([_, [M1,M2], [T1,T2]], UpperTF, SLevel):- nbCourseBySemester(N), CS1 is div(T1,N), CS2 is div(T2,N), Range is CS2 - CS1, decayFunction(Range,Y), SLevel is M-Y.
computeDecay([SID, [M1,M2|M], [T1,T2|T]], UpperTF, SLevel):-
  nbCourseBySemester(N),
  CS1 is div(T1,N), CS2 is div(T2,N), Range is CS2 - CS1,
  decayFunction(Range,Y), TmpSLevel is M1-Y, computeDecay([SID, [M2|M], [T2|T]], UpperTF, TmpSLevel2), SLevel is TmpSLevel + TmpSLevel2.

exploreMasteryByCourse([],_,Struct,Struct):-!.
exploreMasteryByCourse([[_,Skills|_]],Depth,Struct,NewStruc):-exploreMasteryByCourseSkills(Skills, Depth,Struct,NewStruc),!.
exploreMasteryByCourse([ [_,Skills|_]|Y ],Depth,Struct,NewStruc):-exploreMasteryByCourseSkills(Skills,Depth,Struct,TmpStruc),!,
D is Depth + 1, exploreMasteryByCourse(Y,D,TmpStruc,NewStruc).

exploreMasteryByCourseSkills([],_,Struct,Struct):-!.
exploreMasteryByCourseSkills([S],Depth,Struct,NewStruc):-constructOccSkillList(S,Depth,Struct,NewStruc).
exploreMasteryByCourseSkills([S|Skills],Depth, Struct, NewStruc):-
  constructOccSkillList(S,Depth,Struct,TmpStruc), exploreMasteryByCourseSkills(Skills,Depth,TmpStruc,NewStruc).

%Struct is [ [SkillID, [SkillV1,...,SkillVn], [Depth1,...,Depthn]], ... m ]
constructOccSkillList([],_,Struct,Struct):-!.
constructOccSkillList([S,M],Depth,Struct,NewStruc):-
  member([S,Values,When],Struct),!,append(Values,[M],NV),append(When,[Depth],ND),
  subtract(Struct,[[S,Values,When]],TmpS),append(TmpS,[[S,NV,ND]],NewStruc).
constructOccSkillList([S,M],Depth,Struct,NewStruc):- !, append(Struct,[[S,[M],[Depth]]],NewStruc).

decayManager([S,_],Range,DecayedSkills, NewDS):- member([S,K],DecayedSkills),!,decayFunction(Range,Y), V is K - Y, subtract(DecayedSkills,[[S,K]],TmpDS),append(TmpDS,[[S,V]],NewDS).
decayManager([S,M],Range,DecayedSkills, NewDS):- decayFunction(Range,Y), V is M - Y, append(DecayedSkills,[[S,V]],NewDS).


prerequisiteConstraintsSolver(S,C):-getOnlySkills(S,Skills),getOnlyPrereq([C],Prereq), subset(Prereq,Skills),!.

prerequisiteConstraintsCaller(CurrentSol,CtoCheck):- prerequisiteConstraintsSolver(CurrentSol,CtoCheck).
prerequisiteConstraintsCallerFzDc(CurrentSol, [CtoCheckID,_,Prereq|_]):-
  prerequisiteConstraintsSolver(CurrentSol,[CtoCheckID,_,Prereq,_]), %we keep this call. even with fuzzy+decay : faster than fuzzy if prereq does not exist !
  exploreMasteryByCourse(CurrentSol,0,[],N), length(CurrentSol,Size), exploreForComputeDecay(N,Size,[],L), fuzzifyCurrentSkills(L,F),
  prerequisiteConstraintsSolverFzDc(F,Prereq).

/* === FINAL SKILLS CONSTRAINTS */
finalSkillsConstraintsSolver(CurrentSol, FinalSkills):- getOnlySkills(CurrentSol,S), flattenSkill(FinalSkills,F), subset(F,S),!.

finalSkillsConstraintsCaller(CurrentSol):- finalSkills(F), finalSkillsConstraintsSolver(CurrentSol, F).
finalSkillsConstraintsCallerFzDc(CurrentSol):- finalSkills(FS),
  exploreMasteryByCourse(CurrentSol,0,[],N), length(CurrentSol,Size), exploreForComputeDecay(N,Size,[],L), fuzzifyCurrentSkills(L,F),
  prerequisiteConstraintsSolverFzDc(F,FS).
/* === SKILLS ACQUIRED */
  /* === SKILLS + MASTERY */
    getSkillsValueAcquired([],[]).
    getSkillsValueAcquired([[_,Skill|_]|Y],L):-getSkillsValueAcquired(Y,Z), append(Skill,Z,L).

    getPrereqValueAcquired([],[]).
    getPrereqValueAcquired([[_,_,Prereq|_]|Y],L):-getPrereqValueAcquired(Y,Z), append(Prereq,Z,L).

  /* === ONLY SKILLS */
    flattenSkill([],[]):-!.
    flattenSkill([[]],[]):-!.
    flattenSkill([[X|_]],[X]):-!. %Used to remove the sublist, and create a nice depth-1 list with only skill id
    flattenSkill([[X|_]|Y],L):-flattenSkill(Y,Z), append([X],Z,L).

    getOnlySkills([],[]).
    getOnlySkills([[_,Skill|_]|Y],L):-getOnlySkills(Y,Z), flattenSkill(Skill,FlatSkill), append(FlatSkill,Z,L).

    getOnlyPrereq([],[]).
    getOnlyPrereq([[_,_,Prereq|_]|Y],L):-getOnlyPrereq(Y,Z), flattenSkill(Prereq,FlatSkill), append(FlatSkill,Z,L).

/* === SOLVER === */
solve(S):-
  courses(C),
  searchSolutions(C,[],S,[]).

chooseNewCourse(C,OneC,Tabou):- member(OneC, C).

searchSolutions(_,S,S,_):- isNbCoursesIsEQ(S),!, ectsConstraintsCaller(S), finalSkillsConstraintsCallerFzDc(S), displaySolution(S).
searchSolutions(C,S,Sol,Tabou):-  isNbCoursesIsLT(S), length(S, SizeOfS),
                                  chooseNewCourse(C,OneC,Tabou), %Backtracker
                                  timeConstraintsCaller(OneC, SizeOfS), %check if the course picked is ok
                                  prerequisiteConstraintsCallerFzDc(S,OneC),
                                  append(S,[OneC],N), allDiff(N),
                                  searchSolutions(C,N,Sol,Tabou).
