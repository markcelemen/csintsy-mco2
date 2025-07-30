% ==================================================================== %
%                       FamiLink Knowledge Base                        %
% ==================================================================== %

% --- PREDICATE DIRECTIVES ---
% These predicates can be modified at runtime by the Python application.
:- dynamic
    person_male/1,
    person_female/1,
    has_parent/2,
    has_spouse/2,
    explicit_sibling/2,
    explicit_uncle/2,
    explicit_aunt/2,
    explicit_grandparent/2.

:- multifile
    person_male/1,
    person_female/1,
    has_parent/2,
    has_spouse/2,
    explicit_sibling/2,
    explicit_uncle/2,
    explicit_aunt/2,
    explicit_grandparent/2.

% --- UTILITY DIRECTIVES ---
% Allows clauses for these predicates to be defined in different parts of the file.
:- discontiguous
    logical_error/1,
    has_ancestor/2,
    family_related/2,
    is_blood_relative/2,
    are_siblings/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CORE RELATIONSHIP INFERENCE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_child(Parent, Child) :-
    has_parent(Child, Parent).

is_dad(Father, Child) :-
    has_parent(Child, Father),
    person_male(Father).

is_mom(Mother, Child) :-
    has_parent(Child, Mother),
    person_female(Mother).

is_son(Son, Parent) :-
    has_child(Parent, Son),
    person_male(Son).

is_daughter(Daughter, Parent) :-
    has_child(Parent, Daughter),
    person_female(Daughter).

% --- Sibling Logic ---
% A link exists if two different people share a parent.
sibling_link(A, B) :-
    has_parent(A, P),
    has_parent(B, P),
    A \= B.
% A link also exists if they are explicitly stated as siblings.
sibling_link(A, B) :-
    explicit_sibling(A, B),
    A \= B.

% Helper for recursive pathfinding with a visited list to prevent loops.
find_connection(Start, End, Visited) :-
    sibling_link(Start, C),
    \+ member(C, Visited),
    (   End = C
    ;   find_connection(C, End, [C|Visited])
    ).

% Defines sibling relationship transitively.
are_siblings(A, B) :-
    (   nonvar(A)
    ->  find_connection(A, B, [A])
    ;   find_connection(B, A, [B])
    ).

is_brother(Brother, Person) :-
    are_siblings(Brother, Person),
    person_male(Brother).

is_sister(Sister, Person) :-
    are_siblings(Sister, Person),
    person_female(Sister).

% --- Grandparent, Aunt, Uncle Logic ---
has_grandparent(Grandchild, Grandparent) :-
    (   explicit_grandparent(Grandparent, Grandchild)
    ;   has_parent(Grandchild, Parent),
        has_parent(Parent, Grandparent)
    ),
    Grandchild \= Grandparent.

is_grandfather(Grandchild, Grandfather) :-
    has_grandparent(Grandchild, Grandfather),
    person_male(Grandfather).

is_grandmother(Grandchild, Grandmother) :-
    has_grandparent(Grandchild, Grandmother),
    person_female(Grandmother).

is_uncle(NephewNiece, Uncle) :-
    person_male(Uncle),
    NephewNiece \= Uncle,
    (   explicit_uncle(Uncle, NephewNiece)
    ;   has_parent(NephewNiece, Parent), are_siblings(Parent, Uncle)
    ;   has_grandparent(NephewNiece, Grandparent), has_child(Grandparent, Uncle), \+ has_parent(NephewNiece, Uncle)
    ).

is_aunt(NephewNiece, Aunt) :-
    person_female(Aunt),
    NephewNiece \= Aunt,
    (   explicit_aunt(Aunt, NephewNiece)
    ;   has_parent(NephewNiece, Parent), are_siblings(Parent, Aunt)
    ;   has_grandparent(NephewNiece, Grandparent), has_child(Grandparent, Aunt), \+ has_parent(NephewNiece, Aunt)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXTENDED & IN-LAW RELATIONSHIPS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- Ancestors and Descendants ---
has_ancestor(Desc, Anc) :- has_parent(Desc, Anc).
has_ancestor(Desc, Anc) :-
    has_parent(Desc, Intermediate),
    has_ancestor(Intermediate, Anc).

is_great_grandparent(GreatGrandchild, GreatGrandparent) :-
    has_parent(GreatGrandchild, Parent),
    has_grandparent(Parent, GreatGrandparent).

% --- Cousins (First Cousins) ---
are_cousins(Cousin1, Cousin2) :-
    has_parent(Cousin1, Parent1),
    has_parent(Cousin2, Parent2),
    are_siblings(Parent1, Parent2),
    Cousin1 \= Cousin2.

% --- In-Law Definitions (First-Degree) ---
is_parent_in_law(Person, PIL) :- has_spouse(Person, Spouse), has_parent(Spouse, PIL).
is_sibling_in_law(Person, SIL) :- has_spouse(Person, Spouse), are_siblings(Spouse, SIL).
is_sibling_in_law(Person, SIL) :- are_siblings(Person, Sibling), has_spouse(Sibling, SIL).
is_child_in_law(Person, CIL) :- has_child(Person, Child), has_spouse(Child, CIL).

are_in_laws(A, B) :- A \= B, ( is_parent_in_law(A, B) ; is_parent_in_law(B, A) ).
are_in_laws(A, B) :- A \= B, ( is_sibling_in_law(A, B) ; is_sibling_in_law(B, A) ).
are_in_laws(A, B) :- A \= B, ( is_child_in_law(A, B) ; is_child_in_law(B, A) ).

% --- Broad Relationship Categories ---

% is_blood_relative/2: Used for incest rule. Does NOT include in-laws.
is_blood_relative(A, B) :- A \= B, (has_ancestor(A, B) ; has_ancestor(B, A)).
is_blood_relative(A, B) :- are_siblings(A, B).
is_blood_relative(A, B) :- are_cousins(A, B).
is_blood_relative(A, B) :- A \= B, has_ancestor(A, Common), has_ancestor(B, Common).

% family_related/2: Broadest term. Includes blood relatives and in-laws.
family_related(A, B) :- is_blood_relative(A, B).
family_related(A, B) :- has_spouse(A, B).
family_related(A, B) :- are_in_laws(A, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOGICAL CONSTRAINT CHECKING %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- Foundational Constraints ---
logical_error('a person cannot be their own parent') :- has_parent(X, X), !.
logical_error('a person cannot be both male and female') :- person_male(X), person_female(X), !.
logical_error('there is a circular ancestor relationship') :- has_ancestor(X, Y), has_ancestor(Y, X), !.

% --- Parent Constraints ---
logical_error('a person cannot have more than two parents') :-
    has_parent(Child, _),
    findall(P, has_parent(Child, P), Parents),
    sort(Parents, UniqueParents),
    length(UniqueParents, Count),
    Count > 2, !.

logical_error('a child cannot have two parents of the same gender') :-
    has_parent(Child, Parent1),
    has_parent(Child, Parent2),
    Parent1 \= Parent2,
    (   (person_male(Parent1), person_male(Parent2))
    ;   (person_female(Parent1), person_female(Parent2))
    ), !.

logical_error('close blood relatives cannot have children together') :-
    has_parent(Child, Parent1),
    has_parent(Child, Parent2),
    Parent1 \= Parent2,
    is_blood_relative(Parent1, Parent2), !.

% --- Spouse Constraints ---
logical_error('a person cannot be their own spouse') :- has_spouse(X, X), !.
logical_error('spouses must be of different genders') :- has_spouse(A, B), person_male(A), person_male(B), !.
logical_error('spouses must be of different genders') :- has_spouse(A, B), person_female(A), person_female(B), !.
logical_error('a person cannot have more than one spouse') :-
    has_spouse(Person, Spouse1),
    has_spouse(Person, Spouse2),
    Spouse1 \= Spouse2, !.
logical_error('cannot marry a close blood relative') :- has_spouse(A, B), is_blood_relative(A, B), !.
logical_error('cannot marry a parent-in-law') :-
    has_spouse(Person, Spouse),
    has_parent(Spouse, ParentInLaw),
    has_spouse(Person, ParentInLaw), !.
logical_error('cannot marry a child-in-law') :-
    has_child(Person, Child),
    has_spouse(Child, ChildInLaw),
    has_spouse(Person, ChildInLaw), !.

% --- Other Relational Paradoxes ---
logical_error('a person cannot be their own explicit relative (e.g., uncle)') :-
    (   explicit_sibling(X, X) ; explicit_uncle(X, X)
    ;   explicit_aunt(X, X)    ; explicit_grandparent(X, X)
    ), !.

logical_error('a person cannot be both a parent and grandparent to the same person') :-
    has_parent(Child, Person), has_grandparent(Child, Person), !.

logical_error('a person cannot be a sibling of their ancestor or descendant') :-
    are_siblings(A, B), (has_ancestor(A, B) ; has_ancestor(B, A)), !.
