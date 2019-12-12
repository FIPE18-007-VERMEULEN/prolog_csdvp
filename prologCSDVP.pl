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
[ 0, 2 ],
[ 1, 4 ],
[ 5, 4 ]
]).

courses([
  [ -1, /* course 0 */
    [ [] ], /* skills given */
    [ [] ], /* prerequisite */
    [ 0, 1, 2, 3, 4 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 0, /* course 0 */
    [ [1, 75], [4, 50] ], /* skills given */
    [ [] ], /* prerequisite */
    [ 0 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 1, /* course 0 */
    [ [3, 50], [1, 25] ], /* skills given */
    [ [1, 3] ], /* prerequisite */
    [ 1, 2 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 2, /* course 0 */
    [ [0,50], [2,50], [5,25] ], /* skills given */
    [ [5, 3] ], /* prerequisite */
    [ 2, 3 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 3, /* course 0 */
    [ [5,50] ], /* skills given */
    [ [1,4], [4,2] ], /* prerequisite */
    [ 1, 2 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 4, /* course 0 */
    [ [1,35], [2,25], [5,5] ], /* skills given */
    [ [2,2], [4,4] ], /* prerequisite */
    [ 2, 3, 4 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 5, /* course 0 */
    [ [6,40], [3,45], [1,10] ], /* skills given */
    [ [2,2], [4,4] ], /* prerequisite */
    [ 2, 5, 6 ], /* time frame */
    [ 2 ] /* ects */
  ],
  [ 6, /* course 0 */
    [ [0,25], [2,40] ], /* skills given */
    [ [3,4] ], /* prerequisite */
    [ 3, 5, 6 ], /* time frame */
    [ 3 ] /* ects */
  ]
]).


minECTS(8).
nbSemester(4).
nbCourses(5).
nbCourseBySemester(1).

% === GETTER AND UTILITY ===
getIDCourse([I|_], I).

isNbCoursesIsEQ(S):- length(S,L), nbCourseBySemester(A), nbSemester(B), L =:= A * B. %correct size of S.
isNbCoursesIsLT(S):- length(S,L), nbCourseBySemester(A), nbSemester(B), L < A * B.
isNbCoursesIsGT(S):- length(S,L), nbCourseBySemester(A), nbSemester(B), L > A * B. %correct size of S.

% === ECTS CONSTRAINTS ===
ectsConstraintsSolver([X],V):- minECTS(M), (X+V) >= M.
ectsConstraintsSolver([X|Y],V):- ectsConstraintsSolver(Y,V+X).

% Used to build a good list looking for ectsConstraintsSolver
ectsListBuilder([X],A):-nth0(4,X,A).
ectsListBuilder([X|Y],K):-nth0(4,X,A),ectsListBuilder(Y,L), append(A,L,K).

ectsConstraintsCaller(X):- ectsListBuilder(X,L), write(X), ectsConstraintsSolver(L,0).

% === ALL DIFFERENTS ===
isDiffTwo(X,Y):- getIDCourse(X,XID), getIDCourse(Y,YID), XID \= YID.

allDiff([_]):-!.
allDiff([X,Y]):- !, isDiffTwo(X,Y).
allDiff([X,Y|Z]):- isDiffTwo(X,Y), allDiff([X|Z]), allDiff([Y|Z]).

% === TIME CONSTRAINTS

timeConstraintsSolver(Course,PositionInSol):- nbCourseBySemester(C), X is PositionInSol / C, nth0(3,Course,TimeFrame), member(X,TimeFrame).

timeConstraintsCaller(Course, PositionInSol):-timeConstraintsSolver(Course, PositionInSol).

% === PREREQUISITE CONSTRAINTS
prerequisiteConstraintsSolver(S,C):-getOnlySkills(S,Skills),getOnlyPrereq([C],Prereq), subset(Prereq,Skills),!.

prerequisiteConstraintsCaller(CurrentSol,CtoCheck):- prerequisiteConstraintsSolver(CurrentSol,CtoCheck).

% === FINAL SKILLS CONSTRAINTS
finalSkillsConstraintsSolver(CurrentSol, FinalSkills):- getOnlySkills(CurrentSol,S), flattenSkill(FinalSkills,F), subset(F,S),!.

finalSkillsConstraintsCaller(CurrentSol):- finalSkills(F), finalSkillsConstraintsSolver(CurrentSol, F).

% === SKILLS ACQUIRED
  % === SKILLS + MASTERY
    getSkillsValueAcquired([],[]).
    getSkillsValueAcquired([[_,Skill|_]|Y],L):-getSkillsValueAcquired(Y,Z), append(Skill,Z,L).
  % === ONLY SKILLS
    flattenSkill([],[]).
    flattenSkill([[]],[]).
    flattenSkill([[X|_]],[X]). %Used to remove the sublist, and create a nice depth-1 list with only skill id
    flattenSkill([[X|_]|Y],L):-flattenSkill(Y,Z), append([X],Z,L).

    getOnlySkills([],[]).
    getOnlySkills([[_,Skill|_]|Y],L):-getOnlySkills(Y,Z), flattenSkill(Skill,FlatSkill), append(FlatSkill,Z,L).

    getOnlyPrereq([],[]).
    getOnlyPrereq([[_,_,Prereq|_]|Y],L):-getOnlyPrereq(Y,Z), flattenSkill(Prereq,FlatSkill), append(FlatSkill,Z,L).

% === SOLVER ===
solve(S):-
  courses(C),
  searchSolutions(C,[],S,[]).

chooseNewCourse(C,OneC,Tabou):- member(OneC, C).

searchSolutions(_,S,S,_):- isNbCoursesIsEQ(S),!, ectsConstraintsCaller(S), finalSkillsConstraintsCaller(S), displaySolution(S).
searchSolutions(C,S,Sol,Tabou):-  isNbCoursesIsLT(S), length(S, SizeOfS),
                                  chooseNewCourse(C,OneC,Tabou), %Backtracker
                                  timeConstraintsCaller(OneC, SizeOfS), %check if the course picked is ok
                                  prerequisiteConstraintsCaller(S,OneC),
                                  append(S,[OneC],N), allDiff(N),
                                  searchSolutions(C,N,Sol,Tabou).

displaySolution([]).
displaySolution([ [ID|_]|Y]):- write("\n"), write("Course"), write(ID), displaySolution(Y).
